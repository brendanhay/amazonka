{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TimeBasedAutoScalingConfiguration
  ( TimeBasedAutoScalingConfiguration (..),

    -- * Smart constructor
    mkTimeBasedAutoScalingConfiguration,

    -- * Lenses
    tbascInstanceId,
    tbascAutoScalingSchedule,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance's time-based auto scaling configuration.
--
-- /See:/ 'mkTimeBasedAutoScalingConfiguration' smart constructor.
data TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration'
  { -- | The instance ID.
    instanceId :: Lude.Maybe Lude.Text,
    -- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
    autoScalingSchedule :: Lude.Maybe WeeklyAutoScalingSchedule
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeBasedAutoScalingConfiguration' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance ID.
-- * 'autoScalingSchedule' - A @WeeklyAutoScalingSchedule@ object with the instance schedule.
mkTimeBasedAutoScalingConfiguration ::
  TimeBasedAutoScalingConfiguration
mkTimeBasedAutoScalingConfiguration =
  TimeBasedAutoScalingConfiguration'
    { instanceId = Lude.Nothing,
      autoScalingSchedule = Lude.Nothing
    }

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbascInstanceId :: Lens.Lens' TimeBasedAutoScalingConfiguration (Lude.Maybe Lude.Text)
tbascInstanceId = Lens.lens (instanceId :: TimeBasedAutoScalingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: TimeBasedAutoScalingConfiguration)
{-# DEPRECATED tbascInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A @WeeklyAutoScalingSchedule@ object with the instance schedule.
--
-- /Note:/ Consider using 'autoScalingSchedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbascAutoScalingSchedule :: Lens.Lens' TimeBasedAutoScalingConfiguration (Lude.Maybe WeeklyAutoScalingSchedule)
tbascAutoScalingSchedule = Lens.lens (autoScalingSchedule :: TimeBasedAutoScalingConfiguration -> Lude.Maybe WeeklyAutoScalingSchedule) (\s a -> s {autoScalingSchedule = a} :: TimeBasedAutoScalingConfiguration)
{-# DEPRECATED tbascAutoScalingSchedule "Use generic-lens or generic-optics with 'autoScalingSchedule' instead." #-}

instance Lude.FromJSON TimeBasedAutoScalingConfiguration where
  parseJSON =
    Lude.withObject
      "TimeBasedAutoScalingConfiguration"
      ( \x ->
          TimeBasedAutoScalingConfiguration'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "AutoScalingSchedule")
      )
