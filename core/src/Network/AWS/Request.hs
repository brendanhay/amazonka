{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request
    (
    -- * Requests
    , get
    , delete
    , head'

    , defaultRequest
    , contentSHA256

    -- * Lenses
    , method
    , host
    , path
    , queryString
    , requestBody
    , requestHeaders
    ) where

import           Control.Lens
import           Data.ByteString              (ByteString)
import           Data.Default.Class
import qualified Data.Text.Encoding           as Text
import           Network.AWS.Data
import           Network.AWS.Types
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types           as HTTP

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get = content . defaultRequest

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE

head' :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head' x = get x & rqMethod .~ HEAD

defaultRequest :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
defaultRequest x = def
    & rqPath    .~ Text.encodeUtf8 (toPath x)
    & rqQuery   .~ toQuery x
    & rqHeaders .~ toHeaders x

    hs = toHeader hAMZTarget   target
      ++ toHeader hContentType content

    target  = (\p -> p <> "." <> toBS a) <$> _svcTargetPrefix svc
    content = ("application/x-amz-json-" <>) <$> _svcJSONVersion svc

    svc :: Service (Sv a)
    svc = service

contentSHA256 :: Request a -> Request a
contentSHA256 rq = rq
    & rqHeaders %~ hdr hAMZContentSHA256 (bodyHash (rq ^. rqBody))

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
