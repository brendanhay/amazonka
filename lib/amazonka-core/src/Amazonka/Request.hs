-- |
-- Module      : Amazonka.Request
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    patchBody,
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
import Amazonka.Data
import Amazonka.Prelude
import qualified Data.ByteString.Char8 as B8
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (StdMethod (..))
import qualified Network.HTTP.Types as HTTP
import Text.Regex.Posix

type ToRequest a = (ToPath a, ToQuery a, ToHeaders a)

head' :: ToRequest a => Service -> a -> Request a
head' s x = (get s x) {method = HEAD}

delete :: ToRequest a => Service -> a -> Request a
delete s x = (get s x) {method = DELETE}

get :: ToRequest a => Service -> a -> Request a
get s = defaultRequest s

post :: ToRequest a => Service -> a -> Request a
post s x = (get s x) {method = POST}

put :: ToRequest a => Service -> a -> Request a
put s x = (get s x) {method = PUT}

patchBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
patchBody s x = (putBody s x) {method = PATCH}

patchJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
patchJSON s x = (putJSON s x) {method = PATCH}

postXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
postXML s x = (putXML s x) {method = POST}

postJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
postJSON s x = (putJSON s x) {method = POST}

postQuery :: ToRequest a => Service -> a -> Request a
postQuery service x =
  Request
    { service,
      method = POST,
      path = rawPath x,
      query = mempty,
      body = toBody (toQuery x),
      headers = hdr hContentType hFormEncoded (toHeaders x)
    }

postBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
postBody s x = (post s x) {body = toBody x}

putXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
putXML s x = (put s x) {body = maybe "" toBody (maybeElement x)}

putJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
putJSON s x = (put s x) {body = toBody (toJSON x)}

putBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
putBody s x = (put s x) {body = toBody x}

defaultRequest :: ToRequest a => Service -> a -> Request a
defaultRequest service x =
  Request
    { service,
      method = GET,
      path = rawPath x,
      query = toQuery x,
      headers = toHeaders x,
      body = ""
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
contentMD5Header rq@Request {headers, body}
  | isMissing, Just x <- maybeMD5 = rq {headers = hdr HTTP.hContentMD5 x headers}
  | otherwise = rq
  where
    maybeMD5 = md5Base64 body
    isMissing = isNothing (lookup HTTP.hContentMD5 headers)

expectHeader :: Request a -> Request a
expectHeader rq@Request {headers} =
  rq {headers = hdr hExpect "100-continue" headers}

glacierVersionHeader :: ByteString -> Request a -> Request a
glacierVersionHeader version rq@Request {headers} =
  rq {headers = hdr "x-amz-glacier-version" version headers}

-- Rewrite a request to use virtual-hosted-style buckets where
-- possible and requested.
--
-- Example: A request to endpoint "s3.region.amazonaws.com" with path
-- "/foo/bar" means "object bar in bucket foo". Rewrite it to endpoint
-- "foo.s3.region.amazonaws.com" and path "/bar".
--
-- This is basically the logic in
-- https://github.com/boto/botocore/blob/04d1fae43b657952e49b21d16daa86378ddb4253/botocore/utils.py#L1922-L1941
-- except that we can't tell if an endpoint has been overridden, as a
-- 'Request' contains a 'Service' after all overrides have been
-- applied.
--
-- See: https://boto3.amazonaws.com/v1/documentation/api/1.9.42/guide/s3.html#changing-the-addressing-style
-- See: https://docs.aws.amazon.com/AmazonS3/latest/userguide/VirtualHosting.html
s3vhost :: Request a -> Request a
s3vhost
  rq@Request
    { path = path,
      service = service@Service {endpoint, s3AddressingStyle}
    } = case path of
    Raw [] -> rq -- Impossible?
    Raw (bucketName : p) ->
      let bucketNameLen = B8.length bucketName

          -- Inspired by:
          -- https://github.com/boto/botocore/blob/04d1fae43b657952e49b21d16daa86378ddb4253/botocore/utils.py#L1067
          rewritePossible
            | '.' `B8.elem` bucketName = False
            | bucketNameLen < 3 || bucketNameLen > 63 = False
            | not $ bucketName =~ ("^[a-z0-9][a-z0-9\\-]*[a-z0-9]$" :: ByteString) = False
            | otherwise = True

          doRewrite = case s3AddressingStyle of
            S3AddressingStyleAuto -> rewritePossible
            S3AddressingStylePath -> False
            S3AddressingStyleVirtual -> True

          path' = Raw p
          service' =
            service
              { endpoint = \r ->
                  let e@Endpoint {host} = endpoint r
                   in e {host = mconcat [bucketName, ".", host]}
              }
       in if doRewrite
            then rq {path = path', service = service'}
            else rq
