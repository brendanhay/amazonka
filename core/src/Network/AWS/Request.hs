{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Request
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
      get'
    , head'
    , delete'
    , post'
    , put'

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
import           Network.AWS.Data
import           Network.AWS.Types
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types           as HTTP
import           Network.HTTP.Types.Method

get' :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get' x = def
    & rqPath    .~ toPath x
    & rqQuery   .~ toQuery x
    & rqHeaders .~ toHeaders x
{-# INLINE get' #-}

head' :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head' x = get' x & rqMethod .~ HEAD
{-# INLINE head' #-}

delete' :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete' x = get' x & rqMethod .~ DELETE
{-# INLINE delete' #-}

post' :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
post' x = put' x & rqMethod .~ POST
{-# INLINE post' #-}

put' :: (ToPath a, ToQuery a, ToHeaders a, ToBody a) => a -> Request a
put' x = get' x & rqMethod .~ PUT & rqBody .~ toBody x
{-# INLINE put' #-}

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
