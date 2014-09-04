{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction
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
module Network.AWS.AutoScaling.V2011_01_01.PutScheduledUpdateGroupAction
    (
    -- * Request
      PutScheduledUpdateGroupAction
    -- ** Request constructor
    , mkPutScheduledUpdateGroupActionType
    -- ** Request lenses
    , psugatAutoScalingGroupName
    , psugatScheduledActionName
    , psugatTime
    , psugatStartTime
    , psugatEndTime
    , psugatRecurrence
    , psugatMinSize
    , psugatMaxSize
    , psugatDesiredCapacity

    -- * Response
    , PutScheduledUpdateGroupActionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PutScheduledUpdateGroupAction' request.
mkPutScheduledUpdateGroupActionType :: Text -- ^ 'psugatAutoScalingGroupName'
                                    -> Text -- ^ 'psugatScheduledActionName'
                                    -> PutScheduledUpdateGroupAction
mkPutScheduledUpdateGroupActionType p1 p2 = PutScheduledUpdateGroupAction
    { _psugatAutoScalingGroupName = p1
    , _psugatScheduledActionName = p2
    , _psugatTime = Nothing
    , _psugatStartTime = Nothing
    , _psugatEndTime = Nothing
    , _psugatRecurrence = Nothing
    , _psugatMinSize = Nothing
    , _psugatMaxSize = Nothing
    , _psugatDesiredCapacity = Nothing
    }
{-# INLINE mkPutScheduledUpdateGroupActionType #-}

data PutScheduledUpdateGroupAction = PutScheduledUpdateGroupAction
    { _psugatAutoScalingGroupName :: Text
      -- ^ The name or ARN of the Auto Scaling group.
    , _psugatScheduledActionName :: Text
      -- ^ The name of this scaling action.
    , _psugatTime :: Maybe ISO8601
      -- ^ Time is deprecated. The time for this action to start. Time is an
      -- alias for StartTime and can be specified instead of StartTime, or
      -- vice versa. If both Time and StartTime are specified, their
      -- values should be identical. Otherwise,
      -- PutScheduledUpdateGroupAction will return an error.
    , _psugatStartTime :: Maybe ISO8601
      -- ^ The time for this action to start, as in --start-time
      -- 2010-06-01T00:00:00Z. If you try to schedule your action in the
      -- past, Auto Scaling returns an error message. When StartTime and
      -- EndTime are specified with Recurrence, they form the boundaries
      -- of when the recurring action will start and stop.
    , _psugatEndTime :: Maybe ISO8601
      -- ^ The time for this action to end.
    , _psugatRecurrence :: Maybe Text
      -- ^ The time when recurring future actions will start. Start time is
      -- specified by the user following the Unix cron syntax format. For
      -- information about cron syntax, go to Wikipedia, The Free
      -- Encyclopedia. When StartTime and EndTime are specified with
      -- Recurrence, they form the boundaries of when the recurring action
      -- will start and stop.
    , _psugatMinSize :: Maybe Integer
      -- ^ The minimum size for the new Auto Scaling group.
    , _psugatMaxSize :: Maybe Integer
      -- ^ The maximum size for the Auto Scaling group.
    , _psugatDesiredCapacity :: Maybe Integer
      -- ^ The number of Amazon EC2 instances that should be running in the
      -- group.
    } deriving (Show, Generic)

-- | The name or ARN of the Auto Scaling group.
psugatAutoScalingGroupName :: Lens' PutScheduledUpdateGroupAction (Text)
psugatAutoScalingGroupName = lens _psugatAutoScalingGroupName (\s a -> s { _psugatAutoScalingGroupName = a })
{-# INLINE psugatAutoScalingGroupName #-}

-- | The name of this scaling action.
psugatScheduledActionName :: Lens' PutScheduledUpdateGroupAction (Text)
psugatScheduledActionName = lens _psugatScheduledActionName (\s a -> s { _psugatScheduledActionName = a })
{-# INLINE psugatScheduledActionName #-}

-- | Time is deprecated. The time for this action to start. Time is an alias for
-- StartTime and can be specified instead of StartTime, or vice versa. If both
-- Time and StartTime are specified, their values should be identical.
-- Otherwise, PutScheduledUpdateGroupAction will return an error.
psugatTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugatTime = lens _psugatTime (\s a -> s { _psugatTime = a })
{-# INLINE psugatTime #-}

-- | The time for this action to start, as in --start-time 2010-06-01T00:00:00Z.
-- If you try to schedule your action in the past, Auto Scaling returns an
-- error message. When StartTime and EndTime are specified with Recurrence,
-- they form the boundaries of when the recurring action will start and stop.
psugatStartTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugatStartTime = lens _psugatStartTime (\s a -> s { _psugatStartTime = a })
{-# INLINE psugatStartTime #-}

-- | The time for this action to end.
psugatEndTime :: Lens' PutScheduledUpdateGroupAction (Maybe ISO8601)
psugatEndTime = lens _psugatEndTime (\s a -> s { _psugatEndTime = a })
{-# INLINE psugatEndTime #-}

-- | The time when recurring future actions will start. Start time is specified
-- by the user following the Unix cron syntax format. For information about
-- cron syntax, go to Wikipedia, The Free Encyclopedia. When StartTime and
-- EndTime are specified with Recurrence, they form the boundaries of when the
-- recurring action will start and stop.
psugatRecurrence :: Lens' PutScheduledUpdateGroupAction (Maybe Text)
psugatRecurrence = lens _psugatRecurrence (\s a -> s { _psugatRecurrence = a })
{-# INLINE psugatRecurrence #-}

-- | The minimum size for the new Auto Scaling group.
psugatMinSize :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugatMinSize = lens _psugatMinSize (\s a -> s { _psugatMinSize = a })
{-# INLINE psugatMinSize #-}

-- | The maximum size for the Auto Scaling group.
psugatMaxSize :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugatMaxSize = lens _psugatMaxSize (\s a -> s { _psugatMaxSize = a })
{-# INLINE psugatMaxSize #-}

-- | The number of Amazon EC2 instances that should be running in the group.
psugatDesiredCapacity :: Lens' PutScheduledUpdateGroupAction (Maybe Integer)
psugatDesiredCapacity = lens _psugatDesiredCapacity (\s a -> s { _psugatDesiredCapacity = a })
{-# INLINE psugatDesiredCapacity #-}

instance ToQuery PutScheduledUpdateGroupAction where
    toQuery = genericQuery def

data PutScheduledUpdateGroupActionResponse = PutScheduledUpdateGroupActionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest PutScheduledUpdateGroupAction where
    type Sv PutScheduledUpdateGroupAction = AutoScaling
    type Rs PutScheduledUpdateGroupAction = PutScheduledUpdateGroupActionResponse

    request = post "PutScheduledUpdateGroupAction"
    response _ = nullaryResponse PutScheduledUpdateGroupActionResponse
