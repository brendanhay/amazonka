{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.AutoScaling.V2011_01_01.SetDesiredCapacity
    (
    -- * Request
      SetDesiredCapacity
    -- ** Request constructor
    , mkSetDesiredCapacityType
    -- ** Request lenses
    , sdctAutoScalingGroupName
    , sdctDesiredCapacity
    , sdctHonorCooldown

    -- * Response
    , SetDesiredCapacityResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetDesiredCapacity' request.
mkSetDesiredCapacityType :: Text -- ^ 'sdctAutoScalingGroupName'
                         -> Integer -- ^ 'sdctDesiredCapacity'
                         -> SetDesiredCapacity
mkSetDesiredCapacityType p1 p2 = SetDesiredCapacity
    { _sdctAutoScalingGroupName = p1
    , _sdctDesiredCapacity = p2
    , _sdctHonorCooldown = Nothing
    }
{-# INLINE mkSetDesiredCapacityType #-}

data SetDesiredCapacity = SetDesiredCapacity
    { _sdctAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group.
    , _sdctDesiredCapacity :: Integer
      -- ^ The new capacity setting for the Auto Scaling group.
    , _sdctHonorCooldown :: Maybe Bool
      -- ^ By default, SetDesiredCapacity overrides any cooldown period
      -- associated with the Auto Scaling group. Set to True if you want
      -- Auto Scaling to wait for the cooldown period associated with the
      -- Auto Scaling group to complete before initiating a scaling
      -- activity to set your Auto Scaling group to the new capacity
      -- setting.
    } deriving (Show, Generic)

-- | The name of the Auto Scaling group.
sdctAutoScalingGroupName :: Lens' SetDesiredCapacity (Text)
sdctAutoScalingGroupName = lens _sdctAutoScalingGroupName (\s a -> s { _sdctAutoScalingGroupName = a })
{-# INLINE sdctAutoScalingGroupName #-}

-- | The new capacity setting for the Auto Scaling group.
sdctDesiredCapacity :: Lens' SetDesiredCapacity (Integer)
sdctDesiredCapacity = lens _sdctDesiredCapacity (\s a -> s { _sdctDesiredCapacity = a })
{-# INLINE sdctDesiredCapacity #-}

-- | By default, SetDesiredCapacity overrides any cooldown period associated
-- with the Auto Scaling group. Set to True if you want Auto Scaling to wait
-- for the cooldown period associated with the Auto Scaling group to complete
-- before initiating a scaling activity to set your Auto Scaling group to the
-- new capacity setting.
sdctHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdctHonorCooldown = lens _sdctHonorCooldown (\s a -> s { _sdctHonorCooldown = a })
{-# INLINE sdctHonorCooldown #-}

instance ToQuery SetDesiredCapacity where
    toQuery = genericQuery def

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetDesiredCapacity where
    type Sv SetDesiredCapacity = AutoScaling
    type Rs SetDesiredCapacity = SetDesiredCapacityResponse

    request = post "SetDesiredCapacity"
    response _ = nullaryResponse SetDesiredCapacityResponse
