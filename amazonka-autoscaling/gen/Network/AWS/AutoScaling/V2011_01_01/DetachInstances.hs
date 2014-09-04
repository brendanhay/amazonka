{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DetachInstances
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
module Network.AWS.AutoScaling.V2011_01_01.DetachInstances
    (
    -- * Request
      DetachInstances
    -- ** Request constructor
    , mkDetachInstancesQuery
    -- ** Request lenses
    , diqInstanceIds
    , diqAutoScalingGroupName
    , diqShouldDecrementDesiredCapacity

    -- * Response
    , DetachInstancesResponse
    -- ** Response lenses
    , diaActivities
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachInstances' request.
mkDetachInstancesQuery :: Text -- ^ 'diqAutoScalingGroupName'
                       -> Bool -- ^ 'diqShouldDecrementDesiredCapacity'
                       -> DetachInstances
mkDetachInstancesQuery p1 p2 = DetachInstances
    { _diqInstanceIds = mempty
    , _diqAutoScalingGroupName = p2
    , _diqShouldDecrementDesiredCapacity = p3
    }
{-# INLINE mkDetachInstancesQuery #-}

data DetachInstances = DetachInstances
    { _diqInstanceIds :: [Text]
      -- ^ A list of instances to detach from the Auto Scaling group. You
      -- must specify at least one instance ID.
    , _diqAutoScalingGroupName :: Text
      -- ^ The name of the Auto Scaling group from which to detach
      -- instances.
    , _diqShouldDecrementDesiredCapacity :: Bool
      -- ^ Specifies if the detached instance should decrement the desired
      -- capacity value for the Auto Scaling group. If set to True, the
      -- Auto Scaling group decrements the desired capacity value by the
      -- number of instances detached.
    } deriving (Show, Generic)

-- | A list of instances to detach from the Auto Scaling group. You must specify
-- at least one instance ID.
diqInstanceIds :: Lens' DetachInstances ([Text])
diqInstanceIds = lens _diqInstanceIds (\s a -> s { _diqInstanceIds = a })
{-# INLINE diqInstanceIds #-}

-- | The name of the Auto Scaling group from which to detach instances.
diqAutoScalingGroupName :: Lens' DetachInstances (Text)
diqAutoScalingGroupName = lens _diqAutoScalingGroupName (\s a -> s { _diqAutoScalingGroupName = a })
{-# INLINE diqAutoScalingGroupName #-}

-- | Specifies if the detached instance should decrement the desired capacity
-- value for the Auto Scaling group. If set to True, the Auto Scaling group
-- decrements the desired capacity value by the number of instances detached.
diqShouldDecrementDesiredCapacity :: Lens' DetachInstances (Bool)
diqShouldDecrementDesiredCapacity = lens _diqShouldDecrementDesiredCapacity (\s a -> s { _diqShouldDecrementDesiredCapacity = a })
{-# INLINE diqShouldDecrementDesiredCapacity #-}

instance ToQuery DetachInstances where
    toQuery = genericQuery def

newtype DetachInstancesResponse = DetachInstancesResponse
    { _diaActivities :: [Activity]
      -- ^ A list describing the activities related to detaching the
      -- instances from the Auto Scaling group.
    } deriving (Show, Generic)

-- | A list describing the activities related to detaching the instances from
-- the Auto Scaling group.
diaActivities :: Lens' DetachInstancesResponse ([Activity])
diaActivities = lens _diaActivities (\s a -> s { _diaActivities = a })
{-# INLINE diaActivities #-}

instance FromXML DetachInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachInstances where
    type Sv DetachInstances = AutoScaling
    type Rs DetachInstances = DetachInstancesResponse

    request = post "DetachInstances"
    response _ = xmlResponse
