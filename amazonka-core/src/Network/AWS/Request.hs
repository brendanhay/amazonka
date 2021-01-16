-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Network.AWS.Request
  ( -- * Plugins
    addContentMD5Header,
    addExpectHeader,

    -- * Lenses
    -- requestHeaders,
    -- queryString,
    requestURL,
  )
where

import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Builder as Builder
import qualified Data.Maybe  as Maybe
import Network.AWS.Data
import qualified Network.AWS.Lens as Lens
import Network.AWS.Types
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types (StdMethod (..))
import qualified Data.Map.Strict as Map
import qualified Network.HTTP.Types as HTTP
import Network.AWS.Prelude

-- head' ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- head' service path query headers body =
--   (get service path query headers)
--     { requestMethod = HEAD
--     }

-- delete ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- delete service path query headers body =
--   (get service path query headers)
--     { requestMethod = DELETE
--     }

-- get ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    Request a
-- get service path query headers =
--  Request
--     { requestService = service,
--       requestMethod = GET,
--       requestPath = path,
--       requestQuery = query,
--       requestHeaders = headers,
--       requestBody = ""
--     }

-- post ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- post service path query headers body =
--   (get service path query headers)
--     { requestMethod = POST,
--       requestBody = body
--     }

-- put ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- put service path query headers body =
--   (get service path query headers)
--    { requestMethod = PUT
--    }

-- postForm ::
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postForm service path query headers body =
--   Request
--     { requestService = s,
--       requestMethod = POST,
--       requestPath = rawPath x,
--       requestQuery = mempty,
--       requestBody = toBody (toQuery x),
--       requestHeaders = hdr hContentType hFormEncoded headers
--     }

-- mkRequest ::
--   Service ->
--   StdMethod ->
--   ByteString ->
--   QueryString ->
--   [Header] ->
--   RqBody ->
--   Request a
-- mkRequest service method path query headers body =
--   Request
--     { requestService = service,
--       requestMethod = method,
--       requestPath = path,
--       requestQuery = query,
--       requestHeaders = headers,
--       requestBody = body
--     }

-- postXML ::
--    ToElement a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postXML service path query headers body =
--    (putXML service path query headers body)
--      { requestMethod = POST
--      }

-- putXML ::
--    ToElement a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- putXML service path query headers body =
--   (get service path query headers)
--     { requestMethod = PUT
--     , requestBody = maybe "" toBody (maybeElement x)
--     }

-- patchJSON ::
--    ToJSON a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- patchJSON service path query headers body =
--    (putJSON service path query headers body)
--      { requestMethod = PATCH
--      }

-- postJSON ::
--    ToJSON a => Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- postJSON service path query headers body =
--    (putJSON service path query headers body)
--      { requestMethod = POST
--      }

-- json ::
--    ToJSON a =>
--    Service ->
--    ByteString ->
--    QueryString ->
--    [Header] ->
--    a ->
--    Request a
-- json service method path query headers body =
--    Request
--     { requestService = service,
--       requestMethod = method,
--       requestPath = path,
--       requestQuery = query,
--       requestHeaders = headers,
--       requestBody = toBody (toJSON x)
--     }

-- queryString :: Lens' Client.Request ByteString
-- queryString f x =
--   f (Client.queryString x) <&> \y -> x {Client.queryString = y}

-- requestHeaders :: Lens' Client.Request HTTP.RequestHeaders
-- requestHeaders f x =
--   f (Client.requestHeaders x) <&> \y -> x {Client.requestHeaders = y}

requestURL :: ClientRequest -> ByteString
requestURL x =
  ByteString.Lazy.toStrict
   . Builder.toLazyByteString
   $ scheme
    <> Builder.byteString (Client.host x)
    <> port (Client.port x)
    <> Builder.byteString (Client.path x)
    <> Builder.byteString (Client.queryString x)
  where
    scheme
      | secure = "https://"
      | otherwise = "http://"

    port = \case
      80 -> ""
      443 | secure -> ""
      n -> ":" <> Builder.intDec n

    secure =
      Client.secure x

addContentMD5Header :: Request a -> Request a
addContentMD5Header rq
  | Map.member HTTP.hContentMD5 (requestHeaders rq) = rq
  | Just md5 <- md5Base64 (requestBody rq) = addHeader HTTP.hContentMD5 md5 rq
  | otherwise = rq

addExpectHeader :: Request a -> Request a
addExpectHeader = addHeader hExpect "100-continue"

addHeader :: HeaderName -> ByteString -> Request a -> Request a
addHeader key val rq =
  rq { requestHeaders = Map.insert key val (requestHeaders rq)
     }
