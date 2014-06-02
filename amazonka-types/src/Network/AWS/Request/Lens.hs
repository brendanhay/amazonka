{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Request.Lens
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.Lens
    (
    -- * Requests
      blank
    , get
    , Network.AWS.Request.Lens.head
    , delete

    -- * Lenses
    , rqMethod
    , rqPath
    , rqQuery
    , rqHeaders
    , rqBody
    , rqPayload

    -- * Re-exports
    , (&)
    , (^.)
    , (.~)
    , (%~)
    , (<>~)
    ) where

import Control.Applicative
import Control.Lens
import Data.ByteString           (ByteString)
import Data.Monoid
import Network.AWS.Data
import Network.AWS.Types
import Network.HTTP.Client       (RequestBody(..))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

blank :: Request a
blank = Request GET "/" mempty mempty (RequestBodyBS "") ""

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get x = blank
    & rqPath    .~ toPath x
    & rqQuery   .~ toQuery x
    & rqHeaders .~ toHeaders x

head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head x = get x & rqMethod .~ HEAD

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE

rqMethod :: Functor f => LensLike' f (Request a) StdMethod
rqMethod f x = (\y -> x { _rqMethod = y }) <$> f (_rqMethod x)

rqPath :: Functor f => LensLike' f (Request a) ByteString
rqPath f x = (\y -> x { _rqPath = y }) <$> f (_rqPath x)

rqQuery :: Functor f => LensLike' f (Request a) Query
rqQuery f x = (\y -> x { _rqQuery = y }) <$> f (_rqQuery x)

rqHeaders :: Functor f => LensLike' f (Request a) [Header]
rqHeaders f x = (\y -> x { _rqHeaders = y }) <$> f (_rqHeaders x)

rqBody :: Functor f => LensLike' f (Request a) RequestBody
rqBody f x = (\y -> x { _rqBody = y }) <$> f (_rqBody x)

rqPayload :: Functor f => LensLike' f (Request a) ByteString
rqPayload f x = (\y -> x { _rqPayload = y }) <$> f (_rqPayload x)
