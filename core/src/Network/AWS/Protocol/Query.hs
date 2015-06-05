{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Module      : Network.AWS.Protocol.Query
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Protocol.Query where

import           Control.Lens              hiding (Action)
import           Data.Monoid
import           Network.AWS.Request
import           Network.AWS.Types
import           Network.HTTP.Types.Method

postQuery :: forall a. (AWSService (Sv a), ToQuery a, ToPath a, ToHeaders a)
          => Action
          -> a
          -> Request a
postQuery a x = defaultRequest x
    & rqMethod .~ POST
    & rqQuery <>~ qry
    & contentSHA256
  where
    qry = "Version" =: _svcVersion svc
       <> "Action"  =: toBS a

    svc = service :: Service (Sv a)
