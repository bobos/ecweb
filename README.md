About
======
Cowboy depends on Erlang OTP R18

How To Run It
======
./build
./app/start

server is listening on port 8080

REST APIs
======

create a table:
request:
http POST serveraddress:8080/tablename

response:
success: 
    status code: 200 
    body: "ok"
fail: 
    status code: 400 
    body: fail reason in plain text

delete a table:
request:
http DELETE serveraddress:8080/tablename

response:
success: 
    status code: 200 
    body: "ok"
fail: 
    status code: 400 
    body: fail reason in plain text

insert a record:
request:
http POST serveraddress:8080/tablename
body: {key: value}

response:
success: 
    status code: 200 
    body: "ok"
fail: 
    status code: 400 
    body: fail reason in plain text

update a record:
request:
http UPDATE serveraddress:8080/tablename
body: {key: value}

response:
success: 
    status code: 200 
    body: "ok"
fail: 
    status code: 400 
    body: fail reason in plain text

lookup a record:
request:
http GET serveraddress:8080/tablename/key

response:
success: 
    status code: 200 
    body: {key: value}
fail: 
    status code: 400 
    body: fail reason in plain text

dump table:
request:
http GET serveraddress:8080/tablename

response:
success: 
    status code: 200 
    body: [{key: value}]
fail: 
    status code: 400 
    body: fail reason in plain text
