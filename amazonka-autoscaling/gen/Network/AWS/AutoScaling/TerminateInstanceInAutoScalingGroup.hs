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

-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Terminates the specified instance. Optionally, the desired group size can
-- be adjusted.
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    (
    -- * Request
      TerminateInstanceInAutoScalingGroupType
    -- ** Request constructor
    , terminateInstanceInAutoScalingGroupType
    -- ** Request lenses
    , tiiasgtInstanceId
    , tiiasgtShouldDecrementDesiredCapacity

    -- * Response
    , ActivityType
    -- ** Response constructor
    , activityType
    -- ** Response lenses
    , atActivity
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data TerminateInstanceInAutoScalingGroupType = TerminateInstanceInAutoScalingGroupType
    { _tiiasgtInstanceId                     :: Text
    , _tiiasgtShouldDecrementDesiredCapacity :: Bool
    } (Eq, Ord, Show, Generic)

-- | 'TerminateInstanceInAutoScalingGroupType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiiasgtInstanceId' @::@ 'Text'
--
-- * 'tiiasgtShouldDecrementDesiredCapacity' @::@ 'Bool'
--
terminateInstanceInAutoScalingGroupType :: Text -- ^ 'tiiasgtInstanceId'
                                        -> Bool -- ^ 'tiiasgtShouldDecrementDesiredCapacity'
                                        -> TerminateInstanceInAutoScalingGroupType
terminateInstanceInAutoScalingGroupType p1 p2 = TerminateInstanceInAutoScalingGroupType
    { _tiiasgtInstanceId                     = p1
    , _tiiasgtShouldDecrementDesiredCapacity = p2
    }

-- | The ID of the Amazon EC2 instance to be terminated.
tiiasgtInstanceId :: Lens' TerminateInstanceInAutoScalingGroupType Text
tiiasgtInstanceId =
    lens _tiiasgtInstanceId (\s a -> s { _tiiasgtInstanceId = a })

-- | Specifies whether (true) or not (false) terminating this instance should
-- also decrement the size of the AutoScalingGroup.
tiiasgtShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroupType Bool
tiiasgtShouldDecrementDesiredCapacity =
    lens _tiiasgtShouldDecrementDesiredCapacity
        (\s a -> s { _tiiasgtShouldDecrementDesiredCapacity = a })
instance ToQuery TerminateInstanceInAutoScalingGroupType

instance ToPath TerminateInstanceInAutoScalingGroupType where
    toPath = const "/"

newtype ActivityType = ActivityType
    { _atActivity :: Maybe Activity
    } (Eq, Show, Generic)

-- | 'ActivityType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atActivity' @::@ 'Maybe' 'Activity'
--
activityType :: ActivityType
activityType = ActivityType
    { _atActivity = Nothing
    }

-- | A scaling Activity.
atActivity :: Lens' ActivityType (Maybe Activity)
atActivity = lens _atActivity (\s a -> s { _atActivity = a })

instance FromXML ActivityType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ActivityType"

instance AWSRequest TerminateInstanceInAutoScalingGroupType where
    type Sv TerminateInstanceInAutoScalingGroupType = AutoScaling
    type Rs TerminateInstanceInAutoScalingGroupType = ActivityType

    request  = post "TerminateInstanceInAutoScalingGroup"
    response = xmlResponse $ \h x -> ActivityType
        <$> x %| "Activity"
