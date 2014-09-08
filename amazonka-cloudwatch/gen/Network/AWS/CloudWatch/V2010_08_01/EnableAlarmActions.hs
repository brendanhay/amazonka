{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
    (
    -- * Request
      EnableAlarmActions
    -- ** Request constructor
    , mkEnableAlarmActions
    -- ** Request lenses
    , eaaAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    -- ** Response constructor
    , mkEnableAlarmActionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | 
newtype EnableAlarmActions = EnableAlarmActions
    { _eaaAlarmNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableAlarmActions' request.
mkEnableAlarmActions :: [Text] -- ^ 'eaaAlarmNames'
                     -> EnableAlarmActions
mkEnableAlarmActions p1 = EnableAlarmActions
    { _eaaAlarmNames = p1
    }

-- | The names of the alarms to enable actions for.
eaaAlarmNames :: Lens' EnableAlarmActions [Text]
eaaAlarmNames = lens _eaaAlarmNames (\s a -> s { _eaaAlarmNames = a })

instance ToQuery EnableAlarmActions where
    toQuery = genericQuery def

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableAlarmActionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkEnableAlarmActionsResponse :: EnableAlarmActionsResponse
mkEnableAlarmActionsResponse = EnableAlarmActionsResponse

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request = post "EnableAlarmActions"
    response _ = nullaryResponse EnableAlarmActionsResponse
