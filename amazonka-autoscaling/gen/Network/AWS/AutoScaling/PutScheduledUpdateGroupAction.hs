{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a scheduled scaling action for an Auto Scaling group.
-- When updating a scheduled scaling action, if you leave a parameter
-- unspecified, the corresponding value remains unchanged in the affected Auto
-- Scaling group. For information on creating or updating a scheduled action
-- for your Auto Scaling group, see Scale Based on a Schedule. Auto Scaling
-- supports the date and time expressed in "YYYY-MM-DDThh:mm:ssZ" format in
-- UTC/GMT only. Schedule based on a specific date and time
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=ScaleUp &StartTime=2013-05-25T08:00:00Z
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE Recurring Schedule
-- https://autoscaling.amazonaws.com/?AutoScalingGroupName=my-test-asg
-- &ScheduledActionName=scaleup-schedule-year &Recurrence="30 0 1 1,6,12 *"
-- &DesiredCapacity=3 &Version=2011-01-01
-- &Action=PutScheduledUpdateGroupAction &AUTHPARAMS
-- 3bc8c9bc-6a62-11e2-8a51-4b8a1EXAMPLE.
module Network.AWS.AutoScaling.PutScheduledUpdateGroupAction
    (
    -- * Request
      PutScheduledUpdateGroupAction
    -- ** Request constructor
    , putScheduledUpdateGroupAction
    -- ** Request lenses
    , psugaAutoScalingGroupName
    , psugaScheduledActionName
    , psugaTime
    , psugaStartTime
    , psugaEndTime
    , psugaRecurrence
    , psugaMinSize
    , psugaMaxSize
    , psugaDesiredCapacity

    -- * Response
    , PutScheduledUpdateGroupActionResponse
    -- ** Response constructor
    , putScheduledUpdateGroupActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { _psugaAutoScalingGroupName :: Text
    , _psugaScheduledActionName :: Text
    , _psugaTime :: Maybe ISO8601
    , _psugaStartTime :: Maybe ISO8601
    , _psugaEndTime :: Maybe ISO8601
    , _psugaRecurrence :: Maybe Text
    , _psugaMinSize :: Maybe Integer
    , _psugaMaxSize :: Maybe Integer
    , _psugaDesiredCapacity :: Maybe Integer
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutScheduledUpdateGroupAction' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @ScheduledActionName ::@ @Text@
--
-- * @Time ::@ @Maybe ISO8601@
--
-- * @StartTime ::@ @Maybe ISO8601@
--
-- * @EndTime ::@ @Maybe ISO8601@
--
-- * @Recurrence ::@ @Maybe Text@
--
-- * @MinSize ::@ @Maybe Integer@
--
-- * @MaxSize ::@ @Maybe Integer@
--
-- * @DesiredCapacity ::@ @Maybe Integer@
--
putScheduledUpdateGroupAction :: Text -- ^ 'psugaAutoScalingGroupName'
                              -> Text -- ^ 'psugaScheduledActionName'
                              -> PutScheduledUpdateGroupAction
putScheduledUpdateGroupAction p1 p2 = PutScheduledUpdateGroupAction
    { _psugaAutoScalingGroupName = p1
    , _psugaScheduledActionName = p2
    , _psugaTime = Nothing
    , _psugaStartTime = Nothing
    , _psugaEndTime = Nothing
    , _psugaRecurrence = Nothing
    , _psugaMinSize = Nothing
    , _psugaMaxSize = Nothing
    , _psugaDesiredCapacity = Nothing
    }

-- | The name or ARN of the Auto Scaling group.
psugaAutoScalingGroupName :: Lens' PutScheduledUpdateGroupAction Text
psugaAutoScalingGroupName =
    lens _psugaAutoScalingGroupName
         (\s a -> s { _psugaAutoScalingGroupName = a })

-- | The name of this scaling action.
psugaScheduledActionName :: Lens' PutScheduledUpdateGroupAction Text
psugaScheduledActionName =
    lens _psugaScheduledActionName
         (\s a -> s { _psugaScheduledActionName = a })

-- | Time is deprecated. The time for this action to start. Time is an alias for
-- StartTime and can be specified instead of StartTime, or vice versa. If both
-- Time and StartTime are specified, their values should be identical.
-- Otherwise, PutScheduledUpdateGroupAction will return an error.
psugaTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugaTime = lens _psugaTime (\s a -> s { _psugaTime = a })

-- | The time for this action to start, as in --start-time 2010-06-01T00:00:00Z.
-- If you try to schedule your action in the past, Auto Scaling returns an
-- error message. When StartTime and EndTime are specified with Recurrence,
-- they form the boundaries of when the recurring action will start and stop.
psugaStartTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugaStartTime = lens _psugaStartTime (\s a -> s { _psugaStartTime = a })

-- | The time for this action to end.
psugaEndTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugaEndTime = lens _psugaEndTime (\s a -> s { _psugaEndTime = a })

-- | The time when recurring future actions will start. Start time is specified
-- by the user following the Unix cron syntax format. For information about
-- cron syntax, go to Wikipedia, The Free Encyclopedia. When StartTime and
-- EndTime are specified with Recurrence, they form the boundaries of when the
-- recurring action will start and stop.
psugaRecurrence :: Lens' PutScheduledUpdateGroupAction (Maybe Text)
psugaRecurrence = lens _psugaRecurrence (\s a -> s { _psugaRecurrence = a })

-- | The minimum size for the new Auto Scaling group.
psugaMinSize :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugaMinSize = lens _psugaMinSize (\s a -> s { _psugaMinSize = a })

-- | The maximum size for the Auto Scaling group.
psugaMaxSize :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugaMaxSize = lens _psugaMaxSize (\s a -> s { _psugaMaxSize = a })

-- | The number of Amazon EC2 instances that should be running in the group.
psugaDesiredCapacity :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugaDesiredCapacity =
    lens _psugaDesiredCapacity (\s a -> s { _psugaDesiredCapacity = a })

instance ToQuery PutScheduledUpdateGroupAction where
    toQuery = genericQuery def

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutScheduledUpdateGroupActionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
putScheduledUpdateGroupActionResponse :: PutScheduledUpdateGroupActionResponse
putScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse

instance AWSRequest PutScheduledUpdateGroupAction where
    type Sv PutScheduledUpdateGroupAction = AutoScaling
    type Rs PutScheduledUpdateGroupAction = PutScheduledUpdateGroupActionResponse

    request = post "PutScheduledUpdateGroupAction"
    response _ = nullaryResponse PutScheduledUpdateGroupActionResponse
