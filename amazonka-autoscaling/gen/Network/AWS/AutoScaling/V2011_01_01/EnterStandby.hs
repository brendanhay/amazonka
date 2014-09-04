{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.EnterStandby
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Move instances in an Auto Scaling group into a Standby mode. To learn more
-- about how to put instances into a Standby mode, see Auto Scaling InService
-- State.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-asg&ShouldDecrementDesiredCapacity=true&InstanceIds.member.1=i-5b73d709&Version=2011-01-01&Action=
-- 
-- EnterStandby&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-13T22%3A35%3A50.567Z&AUTHPARAMS
-- 462b4bc3-ad3b-4e67-a58d-96cd00f02f9e 50 InProgress 2014-06-13T22:35:50.884Z
-- At 2014-06-13T22:35:50Z instance i-5b73d709 was moved to standby in
-- response to a user request, shrinking the capacity from 4 to 3. my-asg
-- {"Availability Zone":"us-east-1a"} Moving EC2 instance to Standby:
-- i-5b73d709 126f2f31-f34b-11e3-bc51-b35178f0274f.
module Network.AWS.AutoScaling.V2011_01_01.EnterStandby
    (
    -- * Request
      EnterStandby
    -- ** Request constructor
    , mkEnterStandbyQuery
    -- ** Request lenses
    , esqInstanceIds
    , esqAutoScalingGroupName
    , esqShouldDecrementDesiredCapacity

    -- * Response
    , EnterStandbyResponse
    -- ** Response lenses
    , esaActivities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnterStandby' request.
mkEnterStandbyQuery :: Text -- ^ 'esqAutoScalingGroupName'
                    -> Bool -- ^ 'esqShouldDecrementDesiredCapacity'
                    -> EnterStandby
mkEnterStandbyQuery p1 p2 = EnterStandby
    { _esqInstanceIds = mempty
    , _esqAutoScalingGroupName = p2
    , _esqShouldDecrementDesiredCapacity = p3
    }
{-# INLINE mkEnterStandbyQuery #-}

data EnterStandby = EnterStandby
    { _esqInstanceIds :: [Text]
      -- ^ The instances to move into Standby mode. You must specify at
      -- least one instance ID.
    , _esqAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group from which to move instances
      -- into Standby mode.
    , _esqShouldDecrementDesiredCapacity :: Bool
      -- ^ Specifies whether the instances moved to Standby mode count as
      -- part of the Auto Scaling group's desired capacity. If set, the
      -- desired capacity for the Auto Scaling group decrements by the
      -- number of instances moved to Standby mode.
    } deriving (Show, Generic)

-- | The instances to move into Standby mode. You must specify at least one
-- instance ID.
esqInstanceIds :: Lens' EnterStandby ([Text])
esqInstanceIds = lens _esqInstanceIds (\s a -> s { _esqInstanceIds = a })
{-# INLINE esqInstanceIds #-}

-- | The name of the Auto Scaling group from which to move instances into
-- Standby mode.
esqAutoScalingGroupName :: Lens' EnterStandby (Text)
esqAutoScalingGroupName = lens _esqAutoScalingGroupName (\s a -> s { _esqAutoScalingGroupName = a })
{-# INLINE esqAutoScalingGroupName #-}

-- | Specifies whether the instances moved to Standby mode count as part of the
-- Auto Scaling group's desired capacity. If set, the desired capacity for the
-- Auto Scaling group decrements by the number of instances moved to Standby
-- mode.
esqShouldDecrementDesiredCapacity :: Lens' EnterStandby (Bool)
esqShouldDecrementDesiredCapacity = lens _esqShouldDecrementDesiredCapacity (\s a -> s { _esqShouldDecrementDesiredCapacity = a })
{-# INLINE esqShouldDecrementDesiredCapacity #-}

instance ToQuery EnterStandby where
    toQuery = genericQuery def

newtype EnterStandbyResponse = EnterStandbyResponse
    { _esaActivities :: [Activity]
      -- ^ A list describing the activities related to moving instances into
      -- Standby mode.
    } deriving (Show, Generic)

-- | A list describing the activities related to moving instances into Standby
-- mode.
esaActivities :: Lens' EnterStandbyResponse ([Activity])
esaActivities = lens _esaActivities (\s a -> s { _esaActivities = a })
{-# INLINE esaActivities #-}

instance FromXML EnterStandbyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnterStandby where
    type Sv EnterStandby = AutoScaling
    type Rs EnterStandby = EnterStandbyResponse

    request = post "EnterStandby"
    response _ = xmlResponse
