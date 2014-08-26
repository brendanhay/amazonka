{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the desired size of the specified AutoScalingGroup.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &HonorCooldown=false &DesiredCapacity=2 &Version=2011-01-01
-- &Action=SetDesiredCapacity &AUTHPARAMS
-- 9fb7e2db-6998-11e2-a985-57c82EXAMPLE.
module Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetDesiredCapacity' request.
setDesiredCapacity :: Integer -- ^ '_sdctDesiredCapacity'
                   -> Text -- ^ '_sdctAutoScalingGroupName'
                   -> SetDesiredCapacity
setDesiredCapacity p1 p2 = SetDesiredCapacity
    { _sdctDesiredCapacity = p1
    , _sdctAutoScalingGroupName = p2
    , _sdctHonorCooldown = Nothing
    }

data SetDesiredCapacity = SetDesiredCapacity
    { _sdctDesiredCapacity :: Integer
      -- ^ The new capacity setting for the Auto Scaling group.
    , _sdctAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _sdctHonorCooldown :: Maybe Bool
      -- ^ By default, SetDesiredCapacity overrides any cooldown period
      -- associated with the Auto Scaling group. Set to True if you want
      -- Auto Scaling to wait for the cooldown period associated with the
      -- Auto Scaling group to complete before initiating a scaling
      -- activity to set your Auto Scaling group to the new capacity
      -- setting.
    } deriving (Show, Generic)

makeLenses ''SetDesiredCapacity

instance ToQuery SetDesiredCapacity where
    toQuery = genericQuery def

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    deriving (Eq, Show, Generic)

makeLenses ''SetDesiredCapacityResponse

instance AWSRequest SetDesiredCapacity where
    type Sv SetDesiredCapacity = AutoScaling
    type Rs SetDesiredCapacity = SetDesiredCapacityResponse

    request = post "SetDesiredCapacity"
    response _ = nullaryResponse SetDesiredCapacityResponse
