{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DetachInstances
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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DetachInstances.html>
module Network.AWS.AutoScaling.DetachInstances
    (
    -- * Request
      DetachInstances
    -- ** Request constructor
    , detachInstances
    -- ** Request lenses
    , diAutoScalingGroupName
    , diInstanceIds
    , diShouldDecrementDesiredCapacity

    -- * Response
    , DetachInstancesResponse
    -- ** Response constructor
    , detachInstancesResponse
    -- ** Response lenses
    , dirActivities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DetachInstances = DetachInstances
    { _diAutoScalingGroupName           :: Text
    , _diInstanceIds                    :: List "InstanceIds" Text
    , _diShouldDecrementDesiredCapacity :: Bool
    } deriving (Eq, Ord, Show)

-- | 'DetachInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diAutoScalingGroupName' @::@ 'Text'
--
-- * 'diInstanceIds' @::@ ['Text']
--
-- * 'diShouldDecrementDesiredCapacity' @::@ 'Bool'
--
detachInstances :: Text -- ^ 'diAutoScalingGroupName'
                -> Bool -- ^ 'diShouldDecrementDesiredCapacity'
                -> DetachInstances
detachInstances p1 p2 = DetachInstances
    { _diAutoScalingGroupName           = p1
    , _diShouldDecrementDesiredCapacity = p2
    , _diInstanceIds                    = mempty
    }

-- | The name of the Auto Scaling group from which to detach instances.
diAutoScalingGroupName :: Lens' DetachInstances Text
diAutoScalingGroupName =
    lens _diAutoScalingGroupName (\s a -> s { _diAutoScalingGroupName = a })

-- | A list of instances to detach from the Auto Scaling group. You must
-- specify at least one instance ID.
diInstanceIds :: Lens' DetachInstances [Text]
diInstanceIds = lens _diInstanceIds (\s a -> s { _diInstanceIds = a }) . _List

-- | Specifies if the detached instance should decrement the desired capacity
-- value for the Auto Scaling group. If set to True, the Auto Scaling group
-- decrements the desired capacity value by the number of instances
-- detached.
diShouldDecrementDesiredCapacity :: Lens' DetachInstances Bool
diShouldDecrementDesiredCapacity =
    lens _diShouldDecrementDesiredCapacity
        (\s a -> s { _diShouldDecrementDesiredCapacity = a })

newtype DetachInstancesResponse = DetachInstancesResponse
    { _dirActivities :: List "Activities" Activity
    } deriving (Eq, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DetachInstancesResponse where
    type Item DetachInstancesResponse = Activity

    fromList = DetachInstancesResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _dirActivities

-- | 'DetachInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dirActivities' @::@ ['Activity']
--
detachInstancesResponse :: DetachInstancesResponse
detachInstancesResponse = DetachInstancesResponse
    { _dirActivities = mempty
    }

-- | A list describing the activities related to detaching the instances from
-- the Auto Scaling group.
dirActivities :: Lens' DetachInstancesResponse [Activity]
dirActivities = lens _dirActivities (\s a -> s { _dirActivities = a }) . _List

instance ToPath DetachInstances where
    toPath = const "/"

instance ToQuery DetachInstances where
    toQuery DetachInstances{..} = mconcat
        [ "AutoScalingGroupName"           =? _diAutoScalingGroupName
        , "InstanceIds"                    =? _diInstanceIds
        , "ShouldDecrementDesiredCapacity" =? _diShouldDecrementDesiredCapacity
        ]

instance ToHeaders DetachInstances

query

instance AWSRequest DetachInstances where
    type Sv DetachInstances = AutoScaling
    type Rs DetachInstances = DetachInstancesResponse

    request  = post "DetachInstances"
    response = xmlResponse

instance FromXML DetachInstancesResponse where
    parseXML = withElement "DetachInstancesResult" $ \x -> DetachInstancesResponse
        <$> x .@  "Activities"
