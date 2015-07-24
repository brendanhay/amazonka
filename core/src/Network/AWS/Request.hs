{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    , postXML
    , postJSON
    , postQuery
    , postBody

    , putXML
    , putJSON
    , putBody

    -- ** Constructors
    , defaultRequest

    -- ** Preflight Plugins
    , contentSHA256
    , contentMD5

    -- * Lenses
    , method
    , host
    , path
    , queryString
    , requestBody
    , requestHeaders
    , requestURL
    ) where

import           Control.Lens
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text.Encoding           as Text
import           Network.AWS.Data.Body
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.JSON
import           Network.AWS.Data.Path
import           Network.AWS.Data.Query
import           Network.AWS.Data.XML
import           Network.AWS.Types
import qualified Network.HTTP.Client.Internal as HTTP
import           Network.HTTP.Types           (StdMethod (..))
import qualified Network.HTTP.Types           as HTTP

head' :: (ToPath a, ToQuery a, ToHeaders a)
      => Operation
      -> a
      -> Request a
head' o x = get o x & rqMethod .~ HEAD

delete :: (ToPath a, ToQuery a, ToHeaders a)
       => Operation
       -> a
       -> Request a
delete o x = get o x & rqMethod .~ DELETE

get :: (ToPath a, ToQuery a, ToHeaders a)
    => Operation
    -> a
    -> Request a
get o = contentSHA256 . defaultRequest o

post :: (ToPath a, ToQuery a, ToHeaders a)
     => Operation
     -> a
     -> Request a
post o x = get o x & rqMethod .~ POST

put :: (ToPath a, ToQuery a, ToHeaders a)
    => Operation
    -> a
    -> Request a
put o x = get o x & rqMethod .~ PUT

postXML :: (ToQuery a, ToPath a, ToHeaders a, ToElement a)
        => Operation
        -> a
        -> Request a
postXML o x = putXML o x & rqMethod .~ POST

postJSON :: (ToQuery a, ToPath a, ToHeaders a, ToJSON a)
         => Operation
         -> a
         -> Request a
postJSON o x = putJSON o x & rqMethod .~ POST

postQuery :: (ToQuery a, ToPath a, ToHeaders a)
          => Operation
          -> a
          -> Request a
postQuery o x = defaultRequest o x
    & rqMethod .~ POST
    & rqQuery <>~ toQuery x
    & contentSHA256

postBody :: (ToPath a, ToQuery a, ToHeaders a, ToBody a)
         => Operation
         -> a
         -> Request a
postBody o x = putBody o x & rqMethod .~ POST

putXML :: (ToPath a, ToQuery a, ToHeaders a, ToElement a)
       => Operation
       -> a
       -> Request a
putXML o x = defaultRequest o x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toElement x)
    & contentSHA256

putJSON :: (ToQuery a, ToPath a, ToHeaders a, ToJSON a)
        => Operation
        -> a
        -> Request a
putJSON o x = defaultRequest o x
    & rqMethod .~ PUT
    & rqBody   .~ toBody (toJSON x)
    & contentSHA256

putBody :: (ToPath a, ToQuery a, ToHeaders a, ToBody a)
        => Operation
        -> a
        -> Request a
putBody o x = defaultRequest o x
    & rqMethod .~ PUT
    & rqBody   .~ toBody x
    & contentSHA256

defaultRequest :: (ToPath a, ToQuery a, ToHeaders a)
               => Operation
               -> a
               -> Request a
defaultRequest o x = Request
    { _rqOperation = o
    , _rqMethod    = GET
    , _rqPath      = Text.encodeUtf8 (toPath x)
    , _rqQuery     = toQuery x
    , _rqHeaders   = toHeaders x
    , _rqBody      = ""
    }

contentSHA256 :: Request a -> Request a
contentSHA256 rq = rq
    & rqHeaders %~ hdr hAMZContentSHA256 (rq ^. rqBody . to bodySHA256)

contentMD5 :: Request a -> Request a
contentMD5 rq
    | missing, Just x <- md5 = rq & rqHeaders %~ hdr HTTP.hContentMD5 x
    | otherwise              = rq
  where
    missing = isNothing $ lookup HTTP.hContentMD5 (rq ^. rqHeaders)
    md5     = rq ^. rqBody . to bodyCalculateMD5

method :: Lens' HTTP.Request HTTP.Method
method f x = f (HTTP.method x) <&> \y -> x { HTTP.method = y }

host :: Lens' HTTP.Request ByteString
host f x = f (HTTP.host x) <&> \y -> x { HTTP.host = y }

path :: Lens' HTTP.Request ByteString
path f x = f (HTTP.path x) <&> \y -> x { HTTP.path = y }

queryString :: Lens' HTTP.Request ByteString
queryString f x = f (HTTP.queryString x) <&> \y -> x { HTTP.queryString = y }

requestBody :: Lens' HTTP.Request HTTP.RequestBody
requestBody f x = f (HTTP.requestBody x) <&> \y -> x { HTTP.requestBody = y }

requestHeaders :: Lens' HTTP.Request HTTP.RequestHeaders
requestHeaders f x =
    f (HTTP.requestHeaders x) <&> \y -> x { HTTP.requestHeaders = y }

requestURL :: ClientRequest -> ByteString
requestURL = toBS . uri
  where
    uri x = scheme (HTTP.secure      x)
         <> build  (HTTP.host        x)
         <> port   (HTTP.port        x)
         <> build  (HTTP.path        x)
         <> build  (HTTP.queryString x)

    scheme True = "https://"
    scheme _    = "http://"

    port = \case
        80  -> ""
        443 -> ""
        n   -> build ':' <> build n

