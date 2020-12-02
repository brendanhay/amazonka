{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.ScheduledUpdateGroupActionRequest where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes information used for one or more scheduled scaling action updates in a 'BatchPutScheduledUpdateGroupAction' operation.
--
--
-- When updating a scheduled scaling action, all optional parameters are left unchanged if not specified.
--
--
-- /See:/ 'scheduledUpdateGroupActionRequest' smart constructor.
data ScheduledUpdateGroupActionRequest = ScheduledUpdateGroupActionRequest'
  { _sugarStartTime ::
      !(Maybe ISO8601),
    _sugarMaxSize ::
      !(Maybe Int),
    _sugarRecurrence ::
      !(Maybe Text),
    _sugarDesiredCapacity ::
      !(Maybe Int),
    _sugarMinSize ::
      !(Maybe Int),
    _sugarEndTime ::
      !(Maybe ISO8601),
    _sugarScheduledActionName ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledUpdateGroupActionRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sugarStartTime' - The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ). If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence. If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
--
-- * 'sugarMaxSize' - The maximum size of the Auto Scaling group.
--
-- * 'sugarRecurrence' - The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> . When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
--
-- * 'sugarDesiredCapacity' - The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
--
-- * 'sugarMinSize' - The minimum size of the Auto Scaling group.
--
-- * 'sugarEndTime' - The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
--
-- * 'sugarScheduledActionName' - The name of the scaling action.
scheduledUpdateGroupActionRequest ::
  -- | 'sugarScheduledActionName'
  Text ->
  ScheduledUpdateGroupActionRequest
scheduledUpdateGroupActionRequest pScheduledActionName_ =
  ScheduledUpdateGroupActionRequest'
    { _sugarStartTime = Nothing,
      _sugarMaxSize = Nothing,
      _sugarRecurrence = Nothing,
      _sugarDesiredCapacity = Nothing,
      _sugarMinSize = Nothing,
      _sugarEndTime = Nothing,
      _sugarScheduledActionName = pScheduledActionName_
    }

-- | The date and time for the action to start, in YYYY-MM-DDThh:mm:ssZ format in UTC/GMT only and in quotes (for example, @"2019-06-01T00:00:00Z"@ ). If you specify @Recurrence@ and @StartTime@ , Amazon EC2 Auto Scaling performs the action at this time, and then performs the action based on the specified recurrence. If you try to schedule the action in the past, Amazon EC2 Auto Scaling returns an error message.
sugarStartTime :: Lens' ScheduledUpdateGroupActionRequest (Maybe UTCTime)
sugarStartTime = lens _sugarStartTime (\s a -> s {_sugarStartTime = a}) . mapping _Time

-- | The maximum size of the Auto Scaling group.
sugarMaxSize :: Lens' ScheduledUpdateGroupActionRequest (Maybe Int)
sugarMaxSize = lens _sugarMaxSize (\s a -> s {_sugarMaxSize = a})

-- | The recurring schedule for the action, in Unix cron syntax format. This format consists of five fields separated by white spaces: [Minute] [Hour] [Day_of_Month] [Month_of_Year] [Day_of_Week]. The value must be in quotes (for example, @"30 0 1 1,6,12 *"@ ). For more information about this format, see <http://crontab.org Crontab> . When @StartTime@ and @EndTime@ are specified with @Recurrence@ , they form the boundaries of when the recurring action starts and stops.
sugarRecurrence :: Lens' ScheduledUpdateGroupActionRequest (Maybe Text)
sugarRecurrence = lens _sugarRecurrence (\s a -> s {_sugarRecurrence = a})

-- | The desired capacity is the initial capacity of the Auto Scaling group after the scheduled action runs and the capacity it attempts to maintain.
sugarDesiredCapacity :: Lens' ScheduledUpdateGroupActionRequest (Maybe Int)
sugarDesiredCapacity = lens _sugarDesiredCapacity (\s a -> s {_sugarDesiredCapacity = a})

-- | The minimum size of the Auto Scaling group.
sugarMinSize :: Lens' ScheduledUpdateGroupActionRequest (Maybe Int)
sugarMinSize = lens _sugarMinSize (\s a -> s {_sugarMinSize = a})

-- | The date and time for the recurring schedule to end. Amazon EC2 Auto Scaling does not perform the action after this time.
sugarEndTime :: Lens' ScheduledUpdateGroupActionRequest (Maybe UTCTime)
sugarEndTime = lens _sugarEndTime (\s a -> s {_sugarEndTime = a}) . mapping _Time

-- | The name of the scaling action.
sugarScheduledActionName :: Lens' ScheduledUpdateGroupActionRequest Text
sugarScheduledActionName = lens _sugarScheduledActionName (\s a -> s {_sugarScheduledActionName = a})

instance Hashable ScheduledUpdateGroupActionRequest

instance NFData ScheduledUpdateGroupActionRequest

instance ToQuery ScheduledUpdateGroupActionRequest where
  toQuery ScheduledUpdateGroupActionRequest' {..} =
    mconcat
      [ "StartTime" =: _sugarStartTime,
        "MaxSize" =: _sugarMaxSize,
        "Recurrence" =: _sugarRecurrence,
        "DesiredCapacity" =: _sugarDesiredCapacity,
        "MinSize" =: _sugarMinSize,
        "EndTime" =: _sugarEndTime,
        "ScheduledActionName" =: _sugarScheduledActionName
      ]
