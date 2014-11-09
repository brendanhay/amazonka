{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.AutoScaling.DetachInstances
    (
    -- * Request
      DetachInstancesQuery
    -- ** Request constructor
    , detachInstancesQuery
    -- ** Request lenses
    , diqAutoScalingGroupName
    , diqInstanceIds
    , diqShouldDecrementDesiredCapacity

    -- * Response
    , DetachInstancesAnswer
    -- ** Response constructor
    , detachInstancesAnswer
    -- ** Response lenses
    , diaActivities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DetachInstancesQuery = DetachInstancesQuery
    { _diqAutoScalingGroupName           :: Text
    , _diqInstanceIds                    :: [Text]
    , _diqShouldDecrementDesiredCapacity :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachInstancesQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diqAutoScalingGroupName' @::@ 'Text'
--
-- * 'diqInstanceIds' @::@ ['Text']
--
-- * 'diqShouldDecrementDesiredCapacity' @::@ 'Bool'
--
detachInstancesQuery :: Text -- ^ 'diqAutoScalingGroupName'
                     -> Bool -- ^ 'diqShouldDecrementDesiredCapacity'
                     -> DetachInstancesQuery
detachInstancesQuery p1 p2 = DetachInstancesQuery
    { _diqAutoScalingGroupName           = p1
    , _diqShouldDecrementDesiredCapacity = p2
    , _diqInstanceIds                    = mempty
    }

-- | The name of the Auto Scaling group from which to detach instances.
diqAutoScalingGroupName :: Lens' DetachInstancesQuery Text
diqAutoScalingGroupName =
    lens _diqAutoScalingGroupName (\s a -> s { _diqAutoScalingGroupName = a })

-- | A list of instances to detach from the Auto Scaling group. You must
-- specify at least one instance ID.
diqInstanceIds :: Lens' DetachInstancesQuery [Text]
diqInstanceIds = lens _diqInstanceIds (\s a -> s { _diqInstanceIds = a })

-- | Specifies if the detached instance should decrement the desired capacity
-- value for the Auto Scaling group. If set to True, the Auto Scaling group
-- decrements the desired capacity value by the number of instances
-- detached.
diqShouldDecrementDesiredCapacity :: Lens' DetachInstancesQuery Bool
diqShouldDecrementDesiredCapacity =
    lens _diqShouldDecrementDesiredCapacity
        (\s a -> s { _diqShouldDecrementDesiredCapacity = a })

instance ToPath DetachInstancesQuery where
    toPath = const "/"

instance ToQuery DetachInstancesQuery

newtype DetachInstancesAnswer = DetachInstancesAnswer
    { _diaActivities :: [Activity]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'DetachInstancesAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diaActivities' @::@ ['Activity']
--
detachInstancesAnswer :: DetachInstancesAnswer
detachInstancesAnswer = DetachInstancesAnswer
    { _diaActivities = mempty
    }

-- | A list describing the activities related to detaching the instances from
-- the Auto Scaling group.
diaActivities :: Lens' DetachInstancesAnswer [Activity]
diaActivities = lens _diaActivities (\s a -> s { _diaActivities = a })

instance AWSRequest DetachInstancesQuery where
    type Sv DetachInstancesQuery = AutoScaling
    type Rs DetachInstancesQuery = DetachInstancesAnswer

    request  = post "DetachInstances"
    response = const . xmlResponse $ \h x -> DetachInstancesAnswer
newtype
