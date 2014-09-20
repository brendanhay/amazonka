{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.EnterStandby
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
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Request
      EnterStandby
    -- ** Request constructor
    , enterStandby
    -- ** Request lenses
    , esInstanceIds
    , esAutoScalingGroupName
    , esShouldDecrementDesiredCapacity

    -- * Response
    , EnterStandbyResponse
    -- ** Response constructor
    , enterStandbyResponse
    -- ** Response lenses
    , esrActivities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data EnterStandby = EnterStandby
    { _esInstanceIds :: [Text]
    , _esAutoScalingGroupName :: Text
    , _esShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnterStandby' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceIds ::@ @[Text]@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @ShouldDecrementDesiredCapacity ::@ @Bool@
--
enterStandby :: Text -- ^ 'esAutoScalingGroupName'
             -> Bool -- ^ 'esShouldDecrementDesiredCapacity'
             -> EnterStandby
enterStandby p2 p3 = EnterStandby
    { _esInstanceIds = mempty
    , _esAutoScalingGroupName = p2
    , _esShouldDecrementDesiredCapacity = p3
    }

-- | The instances to move into Standby mode. You must specify at least one
-- instance ID.
esInstanceIds :: Lens' EnterStandby [Text]
esInstanceIds = lens _esInstanceIds (\s a -> s { _esInstanceIds = a })

-- | The name of the Auto Scaling group from which to move instances into
-- Standby mode.
esAutoScalingGroupName :: Lens' EnterStandby Text
esAutoScalingGroupName =
    lens _esAutoScalingGroupName (\s a -> s { _esAutoScalingGroupName = a })

-- | Specifies whether the instances moved to Standby mode count as part of the
-- Auto Scaling group's desired capacity. If set, the desired capacity for the
-- Auto Scaling group decrements by the number of instances moved to Standby
-- mode.
esShouldDecrementDesiredCapacity :: Lens' EnterStandby Bool
esShouldDecrementDesiredCapacity =
    lens _esShouldDecrementDesiredCapacity
         (\s a -> s { _esShouldDecrementDesiredCapacity = a })

instance ToQuery EnterStandby where
    toQuery = genericQuery def

-- | The output of the EnterStandby action.
newtype EnterStandbyResponse = EnterStandbyResponse
    { _esrActivities :: [Activity]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnterStandbyResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Activities ::@ @[Activity]@
--
enterStandbyResponse :: EnterStandbyResponse
enterStandbyResponse = EnterStandbyResponse
    { _esrActivities = mempty
    }

-- | A list describing the activities related to moving instances into Standby
-- mode.
esrActivities :: Lens' EnterStandbyResponse [Activity]
esrActivities = lens _esrActivities (\s a -> s { _esrActivities = a })

instance FromXML EnterStandbyResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnterStandby where
    type Sv EnterStandby = AutoScaling
    type Rs EnterStandby = EnterStandbyResponse

    request = post "EnterStandby"
    response _ = xmlResponse
