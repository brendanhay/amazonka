-- |
-- Module      : Amazonka.Request
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Request
  ( -- * Requests
    head',
    delete,
    get,

    -- ** Empty body
    post,
    put,

    -- ** Specialised body
    patchJSON,
    postXML,
    postJSON,
    postQuery,
    postBody,
    putXML,
    putJSON,
    putBody,

    -- ** Constructors
    defaultRequest,

    -- ** Operation Plugins
    contentMD5Header,
    expectHeader,
    glacierVersionHeader,
    s3vhost,

    -- ** Lenses
    clientRequestHeaders,
    clientRequestQuery,
    clientRequestURL,
  )
where

import Amazonka.Core
import Amazonka.Lens ((%~), (.~))
import Amazonka.Prelude
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (StdMethod (..))
import qualified Network.HTTP.Types as HTTP
import Text.Regex.Posix

type ToRequest a = (ToPath a, ToQuery a, ToHeaders a)

head' :: ToRequest a => Service -> a -> Request a
head' s x = get s x & requestMethod .~ HEAD

delete :: ToRequest a => Service -> a -> Request a
delete s x = get s x & requestMethod .~ DELETE

get :: ToRequest a => Service -> a -> Request a
get s = defaultRequest s

post :: ToRequest a => Service -> a -> Request a
post s x = get s x & requestMethod .~ POST

put :: ToRequest a => Service -> a -> Request a
put s x = get s x & requestMethod .~ PUT

patchJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
patchJSON s x = putJSON s x & requestMethod .~ PATCH

postXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
postXML s x = putXML s x & requestMethod .~ POST

postJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
postJSON s x = putJSON s x & requestMethod .~ POST

postQuery :: ToRequest a => Service -> a -> Request a
postQuery s x =
  Request
    { _requestService = s,
      _requestMethod = POST,
      _requestPath = rawPath x,
      _requestQuery = mempty,
      _requestBody = toBody (toQuery x),
      _requestHeaders = hdr hContentType hFormEncoded (toHeaders x)
    }

postBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
postBody s x =
  defaultRequest s x
    & requestMethod .~ POST
    & requestBody .~ toBody x

putXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
putXML s x =
  defaultRequest s x
    & requestMethod .~ PUT
    & requestBody .~ maybe "" toBody (maybeElement x)

putJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
putJSON s x =
  defaultRequest s x
    & requestMethod .~ PUT
    & requestBody .~ toBody (toJSON x)

putBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
putBody s x =
  defaultRequest s x
    & requestMethod .~ PUT
    & requestBody .~ toBody x

defaultRequest :: ToRequest a => Service -> a -> Request a
defaultRequest s x =
  Request
    { _requestService = s,
      _requestMethod = GET,
      _requestPath = rawPath x,
      _requestQuery = toQuery x,
      _requestHeaders = toHeaders x,
      _requestBody = ""
    }

clientRequestQuery :: Lens' ClientRequest ByteString
clientRequestQuery f x =
  f (Client.queryString x) <&> \y -> x {Client.queryString = y}

clientRequestHeaders :: Lens' ClientRequest HTTP.RequestHeaders
clientRequestHeaders f x =
  f (Client.requestHeaders x) <&> \y -> x {Client.requestHeaders = y}

clientRequestURL :: ClientRequest -> ByteString
clientRequestURL x =
  scheme
    <> toBS (Client.host x)
    <> port (Client.port x)
    <> toBS (Client.path x)
    <> toBS (Client.queryString x)
  where
    scheme
      | secure = "https://"
      | otherwise = "http://"

    port = \case
      80 -> ""
      443 | secure -> ""
      n -> ":" <> toBS n

    secure = Client.secure x

contentMD5Header :: Request a -> Request a
contentMD5Header rq
  | isMissing, Just x <- maybeMD5 = rq {_requestHeaders = hdr HTTP.hContentMD5 x headers}
  | otherwise = rq
  where
    maybeMD5 = md5Base64 (_requestBody rq)
    isMissing = isNothing (lookup HTTP.hContentMD5 headers)
    headers = _requestHeaders rq

expectHeader :: Request a -> Request a
expectHeader rq =
  rq {_requestHeaders = hdr hExpect "100-continue" (_requestHeaders rq)}

glacierVersionHeader :: ByteString -> Request a -> Request a
glacierVersionHeader version rq =
  rq {_requestHeaders = hdr "x-amz-glacier-version" version (_requestHeaders rq)}

-- This function used to rewrite a request to use virtual-hosted-style buckets where
-- possible.  A request to endpoint "s3.region.amazonaws.com" with
-- path "/foo/bar" means "object bar in bucket foo". Rewrite it to
-- endpoint "foo.s3.region.amazonaws.com" and path "/bar".
--
-- See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html
--
-- But it doesn't rewrite the request anymore since this doesn't work for some
-- setups, e.g. minio without DNS configs. The rewrite of s3 endpoint also
-- doesn't work if there is an override for this in the 'Env'. This has a
-- drawback, buckets on AWS which were created after Sep 2020 will not work as
-- they cannot be addressed using path-style URLs.
s3vhost :: Request a -> Request a
s3vhost = id
