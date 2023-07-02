# Cerberus 0.0.1

## Intro

While working with [Ningle](http://8arrow.org/ningle/), I found myself in need of an authentication system, and while I found [hermetic](https://github.com/eudoxia0/hermetic) and managed to run the demo app, it just didn't work predictably for me so with no other obvious solution (and if there is one, please [let me know](mailto:nmunro@duck.com)) I decided to write my own.

This utilizes the [lack](https://github.com/fukamachi/lack) session middleware that ningle can be configured with, indeed your ningle app *MUST* use the lack session middleware for this to work.

It is important to note that this is also based around users and roles, and a user can have any number of roles. An example of the database structure is below, however, you may have other columns and may call them whatever you want, but you must have a means to store users (and hashed passwords) and roles for this to work as intended.

In this example there is a permissions table that maps users to roles, however, how you choose to implement this is left up to you.

### Users

| id         | name     | password |
|--------------|-----------|------------|
| 1 | admin      | *sdfgdsfgs7*        |

### Roles

| id         | name     |
|--------------|-----------|
| 1 | admin      |
| 2 | user      |

### Permissions

| id         | user_id     | role_id |
|--------------|-----------|------------|
| 1 | 1 | 1 |
| 1 | 1 | 2 |

It is left up to you to write some functions to tell cerberus _how_ to find users and roles etc.

## Setup

As per the ningle documentation you have to wrap a ningle up in lack session middleware.

    (clack:clackup (lack.builder:builder :session +app+) :server server :address address :port port)
    
You must also, as previously mentioned, write functions to inform cerberus how to work with your data, `setup` is a function of cerberus that sets up the functions to allow the session to be used. Three functions are required, `user-p`, `user-pass`, and `user-roles` these are functions of one argument that take a string `user` which is the name of the user. 

### user-p

`user-p` is a function that determines _if_ a user exists and takes a username (a string) and returns a generalized boolean.

### user-pass

`user-pass` is a function that retrieves the hashed password for a user, it takes a username (a string) and returns the password hash.

### user-roles

`user-roles` is a function that retrieves the list of roles a user has, it takes a username (a string) and returns a list.

## API

### Conditions

#### invalid-password
    
    A condition that may be raised during `login` if the password doesn't match the password hash.

#### invalid-user

    A condition that may be raised during `login` if there is no matching user.

### Functions

#### setup 

As mentioned in the setup section, the `setup` function takes three key arguments, as you can see below.

    (cerberus:setup
        :user-p #'(lambda (user)
                (controllers:get controllers:+user+ :name user))

        :user-pass #'(lambda (user)
                   (slot-value (controllers:get controllers:+user+ :name user) 'models:password))

        :user-roles #'(lambda (user)
                    (loop :for role
                          :in (controllers:search controllers:+permissions+ :player (controllers:get controllers:+user+ :name user))
                          :collect (slot-value (slot-value role 'models:role) 'models:name))))

#### login

Login is the function that will put the username and roles into the browser session, it _may_ raise an `invalid-user`, or `invalid-password` condition if there is no such user, or the password doesn't match the hash.

One way login can be integrated is this:

    (handler-case (cerberus:login :user (cdr (assoc "username" params :test #'equal)) :password (cdr (assoc "password" params :test #'equal)))
        (cerberus:invalid-user (err)
          (return-from login (render "login.html" :msg (cerberus:msg err))))

        (cerberus:invalid-password (err)
          (return-from login (render "login.html" :msg (cerberus:msg err)))))
          
If no condition is raised then you may safely assume that the username and roles are in the session.

#### logged-in-p 

A function that returns a generalized boolean that returns the username of the currently logged in user or nil.

    (if (cerberus:logged-in-p)
            (render "profile.html" :msg (format nil "Welcome, ~A!" (cerberus:user-name)))
            (render "login.html"))

#### user-name

A synonym for `logged-in-p`

    (when (cerberus:user-name)
        (cerberus:logout)
        (return-from logout (render "login.html" :msg "You are logged out")))

#### roles

A function that returns a generalized boolean that returns the roles of the currently logged in user or nil.

    (cerberus:roles)

#### role-p 

A function that takes a string and returns a generalized boolean testing if the role exists in the logged in users roles. 

    (cerberus:role-p "admin")

#### logout

Logs the current user out by clearing the username and roles from the session.

    (when (cerberus:user-name)
        (cerberus:logout)
        (return-from logout (render "login.html" :msg "You are logged out")))

#### auth

Takes a number of roles as [&rest](http://clhs.lisp.se/Body/03_da.htm#AMrest) and determines if the logged in user has any of the roles.

    (unless (cerberus:auth "admin")
        (setf (lack.response:response-status ningle:*response*) 403)
        (return-from admin (render "403.html")))

## Author

[nmunro](https://nmunro.github.io)

## Licence

BSD3-Clause
