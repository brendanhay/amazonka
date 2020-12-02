{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Schedule where

import Network.AWS.Glue.Types.ScheduleState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A scheduling object using a @cron@ statement to schedule an event.
--
--
--
-- /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
  { _sState :: !(Maybe ScheduleState),
    _sScheduleExpression :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sState' - The state of the schedule.
--
-- * 'sScheduleExpression' - A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
schedule ::
  Schedule
schedule =
  Schedule' {_sState = Nothing, _sScheduleExpression = Nothing}

-- | The state of the schedule.
sState :: Lens' Schedule (Maybe ScheduleState)
sState = lens _sState (\s a -> s {_sState = a})

-- | A @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
sScheduleExpression :: Lens' Schedule (Maybe Text)
sScheduleExpression = lens _sScheduleExpression (\s a -> s {_sScheduleExpression = a})

instance FromJSON Schedule where
  parseJSON =
    withObject
      "Schedule"
      ( \x ->
          Schedule' <$> (x .:? "State") <*> (x .:? "ScheduleExpression")
      )

instance Hashable Schedule

instance NFData Schedule
