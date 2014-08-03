{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables actions for the specified alarms.
module Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

data EnableAlarmActions = EnableAlarmActions
    { _eaaiAlarmNames :: [Text]
      -- ^ The names of the alarms to enable actions for.
    } deriving (Generic)

makeLenses ''EnableAlarmActions

instance ToQuery EnableAlarmActions where
    toQuery = genericToQuery def

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Show, Generic)

makeLenses ''EnableAlarmActionsResponse

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request = post "EnableAlarmActions"
    response _ _ = return (Right EnableAlarmActionsResponse)
