{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Request.Internal
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.Internal
    (
    -- * Requests
      defaultRequest

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

defaultRequest :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
defaultRequest x = def
    & rqPath    .~ Text.encodeUtf8 (toPath x)
    & rqQuery   .~ toQuery x
    & rqHeaders .~ toHeaders x
{-# INLINE defaultRequest #-}

method :: Lens' HTTP.Request HTTP.Method
method f x = f (HTTP.method x) <&> \y -> x { HTTP.method = y }
{-# INLINE method #-}

host :: Lens' HTTP.Request ByteString
host f x = f (HTTP.host x) <&> \y -> x { HTTP.host = y }
{-# INLINE host #-}

path :: Lens' HTTP.Request ByteString
path f x = f (HTTP.path x) <&> \y -> x { HTTP.path = y }
{-# INLINE path #-}

queryString :: Lens' HTTP.Request ByteString
queryString f x = f (HTTP.queryString x) <&> \y -> x { HTTP.queryString = y }
{-# INLINE queryString #-}

requestBody :: Lens' HTTP.Request HTTP.RequestBody
requestBody f x = f (HTTP.requestBody x) <&> \y -> x { HTTP.requestBody = y }
{-# INLINE requestBody #-}

requestHeaders :: Lens' HTTP.Request HTTP.RequestHeaders
requestHeaders f x =
    f (HTTP.requestHeaders x) <&> \y -> x { HTTP.requestHeaders = y }
{-# INLINE requestHeaders #-}
