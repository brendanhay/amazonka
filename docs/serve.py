#!/usr/bin/env python3

import http.server
import socketserver
import sys
import tempfile
import tarfile

with tempfile.TemporaryDirectory() as root:
    with tarfile.TarFile(sys.argv[1], "r") as archive:
        archive.extractall(root)

    class Handler(http.server.SimpleHTTPRequestHandler):
        def __init__(self, *args, **kwargs):
            super(Handler, self).__init__(*args, directory=root, **kwargs)

    with socketserver.TCPServer(("127.0.0.1", 0), Handler) as httpd:
        print("http://%s:%s" % httpd.server_address)
        httpd.serve_forever()
