{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.SetLoadBasedAutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specify the load-based auto scaling configuration for a specified layer.
-- For more information, see Managing Load with Time-based and Load-based
-- Instances. To use load-based auto scaling, you must create a set of
-- load-based auto scaling instances. Load-based auto scaling operates only on
-- the instances from that set, so you must ensure that you have created
-- enough instances to handle the maximum anticipated load. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see Managing User
-- Permissions.
module Network.AWS.OpsWorks.V2013_02_18.SetLoadBasedAutoScaling where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'SetLoadBasedAutoScaling' request.
setLoadBasedAutoScaling :: Text -- ^ '_slbasrLayerId'
                        -> SetLoadBasedAutoScaling
setLoadBasedAutoScaling p1 = SetLoadBasedAutoScaling
    { _slbasrLayerId = p1
    , _slbasrUpScaling = Nothing
    , _slbasrDownScaling = Nothing
    , _slbasrEnable = Nothing
    }

data SetLoadBasedAutoScaling = SetLoadBasedAutoScaling
    { _slbasrLayerId :: Text
      -- ^ The layer ID.
    , _slbasrUpScaling :: Maybe AutoScalingThresholds
      -- ^ An AutoScalingThresholds object with the upscaling threshold
      -- configuration. If the load exceeds these thresholds for a
      -- specified amount of time, AWS OpsWorks starts a specified number
      -- of instances.
    , _slbasrDownScaling :: Maybe AutoScalingThresholds
      -- ^ An AutoScalingThresholds object with the downscaling threshold
      -- configuration. If the load falls below these thresholds for a
      -- specified amount of time, AWS OpsWorks stops a specified number
      -- of instances.
    , _slbasrEnable :: Maybe Bool
      -- ^ Enables load-based auto scaling for the layer.
    } deriving (Show, Generic)

makeLenses ''SetLoadBasedAutoScaling

instance ToPath SetLoadBasedAutoScaling

instance ToQuery SetLoadBasedAutoScaling

instance ToHeaders SetLoadBasedAutoScaling

instance ToJSON SetLoadBasedAutoScaling

data SetLoadBasedAutoScalingResponse = SetLoadBasedAutoScalingResponse
    deriving (Eq, Show, Generic)

makeLenses ''SetLoadBasedAutoScalingResponse

instance AWSRequest SetLoadBasedAutoScaling where
    type Sv SetLoadBasedAutoScaling = OpsWorks
    type Rs SetLoadBasedAutoScaling = SetLoadBasedAutoScalingResponse

    request = get
    response _ = nullaryResponse SetLoadBasedAutoScalingResponse
