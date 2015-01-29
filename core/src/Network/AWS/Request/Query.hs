{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Request.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Request.Query
    ( post
    ) where

import Control.Lens                 hiding (Action)
import Data.Monoid
import Network.AWS.Data
import Network.AWS.Request.Internal
import Network.AWS.Types
import Network.HTTP.Types.Method

post :: forall a. (AWSService (Sv a), ToQuery a, ToPath a, ToHeaders a)
     => Action
     -> a
     -> Request a
post a x = defaultRequest x & rqMethod .~ POST & rqQuery <>~ qry
  where
    qry = pair "Version" (_svcVersion svc)
        . pair "Action"  (toBS a)
        $ mempty

    svc :: Service (Sv a)
    svc = service
{-# INLINE post #-}
