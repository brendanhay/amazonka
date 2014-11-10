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
module Network.AWS.AutoScaling.EnterStandby
    (
    -- * Request
      EnterStandbyQuery
    -- ** Request constructor
    , enterStandby
    -- ** Request lenses
    , esqAutoScalingGroupName
    , esqInstanceIds
    , esqShouldDecrementDesiredCapacity

    -- * Response
    , EnterStandbyAnswer
    -- ** Response constructor
    , enterStandbyResponse
    -- ** Response lenses
    , esaActivities
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data EnterStandbyQuery = EnterStandbyQuery
    { _esqAutoScalingGroupName           :: Text
    , _esqInstanceIds                    :: [Text]
    , _esqShouldDecrementDesiredCapacity :: Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnterStandbyQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esqAutoScalingGroupName' @::@ 'Text'
--
-- * 'esqInstanceIds' @::@ ['Text']
--
-- * 'esqShouldDecrementDesiredCapacity' @::@ 'Bool'
--
enterStandby :: Text -- ^ 'esqAutoScalingGroupName'
             -> Bool -- ^ 'esqShouldDecrementDesiredCapacity'
             -> EnterStandbyQuery
enterStandby p1 p2 = EnterStandbyQuery
    { _esqAutoScalingGroupName           = p1
    , _esqShouldDecrementDesiredCapacity = p2
    , _esqInstanceIds                    = mempty
    }

-- | The name of the Auto Scaling group from which to move instances into
-- Standby mode.
esqAutoScalingGroupName :: Lens' EnterStandbyQuery Text
esqAutoScalingGroupName =
    lens _esqAutoScalingGroupName (\s a -> s { _esqAutoScalingGroupName = a })

-- | The instances to move into Standby mode. You must specify at least one
-- instance ID.
esqInstanceIds :: Lens' EnterStandbyQuery [Text]
esqInstanceIds = lens _esqInstanceIds (\s a -> s { _esqInstanceIds = a })

-- | Specifies whether the instances moved to Standby mode count as part of
-- the Auto Scaling group's desired capacity. If set, the desired capacity
-- for the Auto Scaling group decrements by the number of instances moved to
-- Standby mode.
esqShouldDecrementDesiredCapacity :: Lens' EnterStandbyQuery Bool
esqShouldDecrementDesiredCapacity =
    lens _esqShouldDecrementDesiredCapacity
        (\s a -> s { _esqShouldDecrementDesiredCapacity = a })

instance ToPath EnterStandbyQuery where
    toPath = const "/"

instance ToQuery EnterStandbyQuery

newtype EnterStandbyAnswer = EnterStandbyAnswer
    { _esaActivities :: [Activity]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'EnterStandbyAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esaActivities' @::@ ['Activity']
--
enterStandbyResponse :: EnterStandbyAnswer
enterStandbyResponse = EnterStandbyAnswer
    { _esaActivities = mempty
    }

-- | A list describing the activities related to moving instances into Standby
-- mode.
esaActivities :: Lens' EnterStandbyAnswer [Activity]
esaActivities = lens _esaActivities (\s a -> s { _esaActivities = a })

instance AWSRequest EnterStandbyQuery where
    type Sv EnterStandbyQuery = AutoScaling
    type Rs EnterStandbyQuery = EnterStandbyAnswer

    request  = post "EnterStandby"
    response = xmlResponse $ \h x -> EnterStandbyAnswer
        <$> x %| "Activities"
