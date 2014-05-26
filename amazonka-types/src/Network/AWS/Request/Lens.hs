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
    -- * Default
      req

    -- * Requests
    , get
    , Network.AWS.Request.Lens.head
    , delete

    -- * Lenses
    , meth
    , path
    , qry
    , hdrs
    , bdy

    -- * Re-exports
    , (&)
    , (^.)
    , (.~)
    , (%~)
    , (<>~)
    ) where

import Control.Applicative
import Data.CaseInsensitive      (CI)
import Data.Monoid
import Data.Text                 (Text)
import Lens.Family
import Network.AWS.Data
import Network.AWS.Types
import Network.HTTP.Client       (RequestBody(..))
import Network.HTTP.Types.Method

-- what about md5 for v4?

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request (Sg (Sv a))
get x = req & path .~ x & qry .~ x & hdrs .~ x

head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request (Sg (Sv a))
head x = get x & meth .~ HEAD

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request (Sg (Sv a))
delete x = get x & meth .~ DELETE

req :: Request a
req = Request GET "/" mempty mempty (RequestBodyBS "")

meth :: Functor f => LensLike' f (Request a) StdMethod
meth f x = (\y -> x { rqMethod = y }) <$> f (rqMethod x)

path :: (ToPath b, Functor f)
     => (Text -> f b)
     -> Request a
     -> f (Request a)
path f x = (\y -> x { rqPath = toPath y }) <$> f (rqPath x)

qry :: (ToQuery b, Functor f)
    => (Query -> f b)
    -> Request a
    -> f (Request a)
qry f x = (\y -> x { rqQuery = toQuery y }) <$> f (rqQuery x)

hdrs :: (ToHeaders b, Functor f)
     => ([(CI Text, Text)] -> f b)
     -> Request a
     -> f (Request a)
hdrs f x = (\y -> x { rqHeaders = toHeaders y }) <$> f (rqHeaders x)

bdy :: (ToBody b, Functor f)
    => (RequestBody -> f b)
    -> Request a
    -> f (Request a)
bdy f x = (\y -> x { rqBody = toBody y }) <$> f (rqBody x)
