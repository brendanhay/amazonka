-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ScheduleConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ScheduleConfig
  ( ScheduleConfig (..),

    -- * Smart constructor
    mkScheduleConfig,

    -- * Lenses
    scScheduleExpression,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration details about the monitoring schedule.
--
-- /See:/ 'mkScheduleConfig' smart constructor.
newtype ScheduleConfig = ScheduleConfig'
  { scheduleExpression ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ScheduleConfig' with the minimum fields required to make a request.
--
-- * 'scheduleExpression' - A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
--     * If you want to set the job to start every hour, please use the following:
-- @Hourly: cron(0 * ? * * *)@
--
--
--     * If you want to start the job daily:
-- @cron(0 [00-23] ? * * *)@
--
--
-- For example, the following are valid cron expressions:
--
--     * Daily at noon UTC: @cron(0 12 ? * * *)@
--
--
--     * Daily at midnight UTC: @cron(0 0 ? * * *)@
--
--
-- To support running every 6, 12 hours, the following are also supported:
-- @cron(0 [00-23]/[01-24] ? * * *)@
-- For example, the following are valid cron expressions:
--
--     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@
--
--
--     * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@
mkScheduleConfig ::
  -- | 'scheduleExpression'
  Lude.Text ->
  ScheduleConfig
mkScheduleConfig pScheduleExpression_ =
  ScheduleConfig' {scheduleExpression = pScheduleExpression_}

-- | A cron expression that describes details about the monitoring schedule.
--
-- Currently the only supported cron expressions are:
--
--     * If you want to set the job to start every hour, please use the following:
-- @Hourly: cron(0 * ? * * *)@
--
--
--     * If you want to start the job daily:
-- @cron(0 [00-23] ? * * *)@
--
--
-- For example, the following are valid cron expressions:
--
--     * Daily at noon UTC: @cron(0 12 ? * * *)@
--
--
--     * Daily at midnight UTC: @cron(0 0 ? * * *)@
--
--
-- To support running every 6, 12 hours, the following are also supported:
-- @cron(0 [00-23]/[01-24] ? * * *)@
-- For example, the following are valid cron expressions:
--
--     * Every 12 hours, starting at 5pm UTC: @cron(0 17/12 ? * * *)@
--
--
--     * Every two hours starting at midnight: @cron(0 0/2 ? * * *)@
--
--
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scScheduleExpression :: Lens.Lens' ScheduleConfig Lude.Text
scScheduleExpression = Lens.lens (scheduleExpression :: ScheduleConfig -> Lude.Text) (\s a -> s {scheduleExpression = a} :: ScheduleConfig)
{-# DEPRECATED scScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

instance Lude.FromJSON ScheduleConfig where
  parseJSON =
    Lude.withObject
      "ScheduleConfig"
      (\x -> ScheduleConfig' Lude.<$> (x Lude..: "ScheduleExpression"))

instance Lude.ToJSON ScheduleConfig where
  toJSON ScheduleConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ScheduleExpression" Lude..= scheduleExpression)]
      )
