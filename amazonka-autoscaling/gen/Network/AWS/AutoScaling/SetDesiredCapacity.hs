{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
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
module Network.AWS.AutoScaling.SetDesiredCapacity
    (
    -- * Request
      SetDesiredCapacity
    -- ** Request constructor
    , setDesiredCapacity
    -- ** Request lenses
    , sdcAutoScalingGroupName
    , sdcDesiredCapacity
    , sdcHonorCooldown

    -- * Response
    , SetDesiredCapacityResponse
    -- ** Response constructor
    , setDesiredCapacityResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

-- | 
data SetDesiredCapacity = SetDesiredCapacity
    { _sdcAutoScalingGroupName :: Text
    , _sdcDesiredCapacity :: !Integer
    , _sdcHonorCooldown :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetDesiredCapacity' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @DesiredCapacity ::@ @Integer@
--
-- * @HonorCooldown ::@ @Maybe Bool@
--
setDesiredCapacity :: Text -- ^ 'sdcAutoScalingGroupName'
                   -> Integer -- ^ 'sdcDesiredCapacity'
                   -> SetDesiredCapacity
setDesiredCapacity p1 p2 = SetDesiredCapacity
    { _sdcAutoScalingGroupName = p1
    , _sdcDesiredCapacity = p2
    , _sdcHonorCooldown = Nothing
    }

-- | The name of the Auto Scaling group.
sdcAutoScalingGroupName :: Lens' SetDesiredCapacity Text
sdcAutoScalingGroupName =
    lens _sdcAutoScalingGroupName
         (\s a -> s { _sdcAutoScalingGroupName = a })

-- | The new capacity setting for the Auto Scaling group.
sdcDesiredCapacity :: Lens' SetDesiredCapacity Integer
sdcDesiredCapacity =
    lens _sdcDesiredCapacity (\s a -> s { _sdcDesiredCapacity = a })

-- | By default, SetDesiredCapacity overrides any cooldown period associated
-- with the Auto Scaling group. Set to True if you want Auto Scaling to wait
-- for the cooldown period associated with the Auto Scaling group to complete
-- before initiating a scaling activity to set your Auto Scaling group to the
-- new capacity setting.
sdcHonorCooldown :: Lens' SetDesiredCapacity (Maybe Bool)
sdcHonorCooldown =
    lens _sdcHonorCooldown (\s a -> s { _sdcHonorCooldown = a })

instance ToQuery SetDesiredCapacity where
    toQuery = genericQuery def

data SetDesiredCapacityResponse = SetDesiredCapacityResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetDesiredCapacityResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
setDesiredCapacityResponse :: SetDesiredCapacityResponse
setDesiredCapacityResponse = SetDesiredCapacityResponse

instance AWSRequest SetDesiredCapacity where
    type Sv SetDesiredCapacity = AutoScaling
    type Rs SetDesiredCapacity = SetDesiredCapacityResponse

    request = post "SetDesiredCapacity"
    response _ = nullaryResponse SetDesiredCapacityResponse
