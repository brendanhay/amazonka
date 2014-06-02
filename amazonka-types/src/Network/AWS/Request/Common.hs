{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Request.Common
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.Common
    (
    -- * Requests
      get
    , Network.AWS.Request.Common.head
    , delete
    ) where

import Control.Lens
import Data.Default
import Network.AWS.Data
import Network.AWS.Types
import Network.HTTP.Types.Method

get :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
get x = def
    & rqPath    .~ toPath x
    & rqQuery   .~ toQuery x
    & rqHeaders .~ toHeaders x

head :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
head x = get x & rqMethod .~ HEAD

delete :: (ToPath a, ToQuery a, ToHeaders a) => a -> Request a
delete x = get x & rqMethod .~ DELETE
