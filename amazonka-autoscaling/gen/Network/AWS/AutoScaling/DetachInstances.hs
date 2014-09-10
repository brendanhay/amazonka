{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Using DetachInstances, you can remove an instance from an Auto Scaling
-- group. After the instances are detached, you can manage them independently
-- from the rest of the Auto Scaling group. To learn more about detaching
-- instances, see Detach Amazon EC2 Instances From Your Auto Scaling Group.
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-asg&ShouldDecrementDesiredCapacity=true&InstanceIds.member.1=i-5f2e8a0d&Version=2011-01-01
-- 
-- &Action=DetachInstances&SignatureVersion=2&SignatureMethod=HmacSHA256&Timestamp=2014-06-14T00%3A07%3A29.962Z&AUTHPARAMS
-- e54ff599-bf05-4076-8b95-a0f090ed90bb 50 InProgress 2014-06-14T00:07:30.280Z
-- At 2014-06-14T00:07:30Z instance i-5f2e8a0d was detached in response to a
-- user request, shrinking the capacity from 4 to 3. my-asg {"Availability
-- Zone":"us-east-1a"} Detaching EC2 instance: i-5f2e8a0d
-- e04f3b11-f357-11e3-a434-7f10009d5849.
module Network.AWS.AutoScaling
    (
    -- * Request
      DetachInstances
    -- ** Request constructor
    , mkDetachInstances
    -- ** Request lenses
    , diInstanceIds
    , diAutoScalingGroupName
    , diShouldDecrementDesiredCapacity

    -- * Response
    , DetachInstancesResponse
    -- ** Response constructor
    , mkDetachInstancesResponse
    -- ** Response lenses
    , dirActivities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DetachInstances = DetachInstances
    { _diInstanceIds :: [Text]
    , _diAutoScalingGroupName :: !Text
    , _diShouldDecrementDesiredCapacity :: !Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceIds ::@ @[Text]@
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @ShouldDecrementDesiredCapacity ::@ @Bool@
--
mkDetachInstances :: Text -- ^ 'diAutoScalingGroupName'
                  -> Bool -- ^ 'diShouldDecrementDesiredCapacity'
                  -> DetachInstances
mkDetachInstances p2 p3 = DetachInstances
    { _diInstanceIds = mempty
    , _diAutoScalingGroupName = p2
    , _diShouldDecrementDesiredCapacity = p3
    }

-- | A list of instances to detach from the Auto Scaling group. You must specify
-- at least one instance ID.
diInstanceIds :: Lens' DetachInstances [Text]
diInstanceIds = lens _diInstanceIds (\s a -> s { _diInstanceIds = a })

-- | The name of the Auto Scaling group from which to detach instances.
diAutoScalingGroupName :: Lens' DetachInstances Text
diAutoScalingGroupName =
    lens _diAutoScalingGroupName (\s a -> s { _diAutoScalingGroupName = a })

-- | Specifies if the detached instance should decrement the desired capacity
-- value for the Auto Scaling group. If set to True, the Auto Scaling group
-- decrements the desired capacity value by the number of instances detached.
diShouldDecrementDesiredCapacity :: Lens' DetachInstances Bool
diShouldDecrementDesiredCapacity =
    lens _diShouldDecrementDesiredCapacity
         (\s a -> s { _diShouldDecrementDesiredCapacity = a })

instance ToQuery DetachInstances where
    toQuery = genericQuery def

-- | The output of the DetachInstances action.
newtype DetachInstancesResponse = DetachInstancesResponse
    { _dirActivities :: [Activity]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Activities ::@ @[Activity]@
--
mkDetachInstancesResponse :: DetachInstancesResponse
mkDetachInstancesResponse = DetachInstancesResponse
    { _dirActivities = mempty
    }

-- | A list describing the activities related to detaching the instances from
-- the Auto Scaling group.
dirActivities :: Lens' DetachInstancesResponse [Activity]
dirActivities = lens _dirActivities (\s a -> s { _dirActivities = a })

instance FromXML DetachInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachInstances where
    type Sv DetachInstances = AutoScaling
    type Rs DetachInstances = DetachInstancesResponse

    request = post "DetachInstances"
    response _ = xmlResponse
