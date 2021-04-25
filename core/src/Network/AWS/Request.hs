{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Request
    (
    -- * Requests
      head'
    , delete
    , get

    -- ** Empty body
    , post
    , put

    -- ** Specialised body
    , patchJSON

    , postXML
    , postJSON
    , postQuery
    , postBody

    , putXML
    , putJSON
    , putBody

    -- ** Constructors
    , defaultRequest

    -- ** Operation Plugins
    , contentMD5Header
    , expectHeader

    -- ** Lenses
    , requestHeaders
    , requestQuery
    , requestURL
    ) where

import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.XML
import           Network.AWS.Lens            (Lens')
import           Network.AWS.Lens            ((%~), (&), (.~), (<&>))
import           Network.AWS.Types
import qualified Network.HTTP.Conduit        as Client
import           Network.HTTP.Types          (StdMethod (..))
import qualified Network.HTTP.Types          as HTTP

type ToRequest a = (ToPath a, ToQuery a, ToHeaders a)

head' :: ToRequest a => Service -> a -> Request a
head' s x = get s x & rqMethod .~ HEAD

delete :: ToRequest a => Service -> a -> Request a
delete s x = get s x & rqMethod .~ DELETE

get :: ToRequest a => Service -> a -> Request a
get s = defaultRequest s

post :: ToRequest a => Service -> a -> Request a
post s x = get s x & rqMethod .~ POST

put :: ToRequest a => Service -> a -> Request a
put s x = get s x & rqMethod .~ PUT

patchJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
patchJSON s x = putJSON s x & rqMethod .~ PATCH

postXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
postXML s x = putXML s x & rqMethod .~ POST

postJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
postJSON s x = putJSON s x & rqMethod .~ POST

postQuery :: ToRequest a => Service -> a -> Request a
postQuery s x = Request
    { _rqService = s
    , _rqMethod  = POST
    , _rqPath    = rawPath x
    , _rqQuery   = mempty
    , _rqBody    = toBody (toQuery x)
    , _rqHeaders = hdr hContentType hFormEncoded (toHeaders x)
    }

postBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
postBody s x = defaultRequest s x
    & rqMethod .~ POST
    & rqBody   .~ toBody x

putXML :: (ToRequest a, ToElement a) => Service -> a -> Request a
putXML s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ maybe "" toBody (maybeElement x)

putJSON :: (ToRequest a, ToJSON a) => Service -> a -> Request a
putJSON s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toJSON x)

putBody :: (ToRequest a, ToBody a) => Service -> a -> Request a
putBody s x = defaultRequest s x
    & rqMethod .~ PUT
    & rqBody   .~ toBody x

defaultRequest :: ToRequest a => Service -> a -> Request a
defaultRequest s x = Request
    { _rqService = s
    , _rqMethod  = GET
    , _rqPath    = rawPath x
    , _rqQuery   = toQuery x
    , _rqHeaders = toHeaders x
    , _rqBody    = ""
    }

requestQuery :: Lens' Client.Request ByteString
requestQuery f x =
    f (Client.queryString x) <&> \y -> x { Client.queryString = y }

requestHeaders :: Lens' Client.Request HTTP.RequestHeaders
requestHeaders f x =
    f (Client.requestHeaders x) <&> \y -> x { Client.requestHeaders = y }

requestURL :: ClientRequest -> ByteString
requestURL x = scheme
    <> toBS (Client.host        x)
    <> port (Client.port        x)
    <> toBS (Client.path        x)
    <> toBS (Client.queryString x)
  where
    scheme
        | secure    = "https://"
        | otherwise = "http://"

    port = \case
        80           -> ""
        443 | secure -> ""
        n            -> ":" <> toBS n

    secure = Client.secure x

contentMD5Header :: Request a -> Request a
contentMD5Header rq
    | missing, Just x <- md5 = rq & rqHeaders %~ hdr HTTP.hContentMD5 x
    | otherwise              = rq
  where
    missing = isNothing $ lookup HTTP.hContentMD5 (_rqHeaders rq)
    md5     = md5Base64 (_rqBody rq)

expectHeader :: Request a -> Request a
expectHeader = rqHeaders %~ hdr hExpect "100-continue"
