
This project is a small, focused attempt to provide processes within
your application a means to monitor and be notified of changes to essential configuration -
most commonly IPs and host names that are subject to change with little to no notice.

While it's possible to do this already using any number of pub/sub
libraries and some custom monitoring code, the intent here is to make
a small application that is focused on providing one very specific service.

Plans are to support multiple different configuration sources and
providers including:

* files on disk
* dns
* etcd
* redis
* other erlang nodes
* ... support extending these providers easily within your own project.



