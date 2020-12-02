{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration details about the monitoring schedule.
--
--
--
-- /See:/ 'scheduleConfig' smart constructor.
newtype ScheduleConfig = ScheduleConfig'
  { _scScheduleExpression ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduleConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scScheduleExpression' - A cron expression that describes details about the monitoring schedule. Currently the only supported cron expressions are:     * If you want to set the job to start every hour, please use the following: @Hourly: cron(0 * ? * * *)@      * If you want to start the job daily: @cron(0 [00-23] ? * * *)@  For example, the following are valid cron expressions:     * Daily at noon UTC: @cron(0 12 ? * * *)@      * Daily at midnight UTC: @cron(0 0 ? * * *)@  To support running every 6, 12 hours, the following are also supported: @cron(0 [00-23]/[01-24] ? * * *)@  For example, the following are valid cron expressions:     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@      * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@
scheduleConfig ::
  -- | 'scScheduleExpression'
  Text ->
  ScheduleConfig
scheduleConfig pScheduleExpression_ =
  ScheduleConfig' {_scScheduleExpression = pScheduleExpression_}

-- | A cron expression that describes details about the monitoring schedule. Currently the only supported cron expressions are:     * If you want to set the job to start every hour, please use the following: @Hourly: cron(0 * ? * * *)@      * If you want to start the job daily: @cron(0 [00-23] ? * * *)@  For example, the following are valid cron expressions:     * Daily at noon UTC: @cron(0 12 ? * * *)@      * Daily at midnight UTC: @cron(0 0 ? * * *)@  To support running every 6, 12 hours, the following are also supported: @cron(0 [00-23]/[01-24] ? * * *)@  For example, the following are valid cron expressions:     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@      * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@
scScheduleExpression :: Lens' ScheduleConfig Text
scScheduleExpression = lens _scScheduleExpression (\s a -> s {_scScheduleExpression = a})

instance FromJSON ScheduleConfig where
  parseJSON =
    withObject
      "ScheduleConfig"
      (\x -> ScheduleConfig' <$> (x .: "ScheduleExpression"))

instance Hashable ScheduleConfig

instance NFData ScheduleConfig

instance ToJSON ScheduleConfig where
  toJSON ScheduleConfig' {..} =
    object
      (catMaybes [Just ("ScheduleExpression" .= _scScheduleExpression)])
