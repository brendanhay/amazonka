{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.AlarmConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.AlarmConfiguration
  ( AlarmConfiguration (..),

    -- * Smart constructor
    mkAlarmConfiguration,

    -- * Lenses
    acIgnorePollAlarmFailure,
    acEnabled,
    acAlarms,
  )
where

import Network.AWS.CodeDeploy.Types.Alarm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about alarms associated with the deployment group.
--
-- /See:/ 'mkAlarmConfiguration' smart constructor.
data AlarmConfiguration = AlarmConfiguration'
  { ignorePollAlarmFailure ::
      Lude.Maybe Lude.Bool,
    enabled :: Lude.Maybe Lude.Bool,
    alarms :: Lude.Maybe [Alarm]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AlarmConfiguration' with the minimum fields required to make a request.
--
-- * 'alarms' - A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
-- * 'enabled' - Indicates whether the alarm configuration is enabled.
-- * 'ignorePollAlarmFailure' - Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.
--
--
--     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.
--
--
--     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
mkAlarmConfiguration ::
  AlarmConfiguration
mkAlarmConfiguration =
  AlarmConfiguration'
    { ignorePollAlarmFailure = Lude.Nothing,
      enabled = Lude.Nothing,
      alarms = Lude.Nothing
    }

-- | Indicates whether a deployment should continue if information about the current state of alarms cannot be retrieved from Amazon CloudWatch. The default value is false.
--
--
--     * @true@ : The deployment proceeds even if alarm status information can't be retrieved from Amazon CloudWatch.
--
--
--     * @false@ : The deployment stops if alarm status information can't be retrieved from Amazon CloudWatch.
--
--
--
-- /Note:/ Consider using 'ignorePollAlarmFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acIgnorePollAlarmFailure :: Lens.Lens' AlarmConfiguration (Lude.Maybe Lude.Bool)
acIgnorePollAlarmFailure = Lens.lens (ignorePollAlarmFailure :: AlarmConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {ignorePollAlarmFailure = a} :: AlarmConfiguration)
{-# DEPRECATED acIgnorePollAlarmFailure "Use generic-lens or generic-optics with 'ignorePollAlarmFailure' instead." #-}

-- | Indicates whether the alarm configuration is enabled.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acEnabled :: Lens.Lens' AlarmConfiguration (Lude.Maybe Lude.Bool)
acEnabled = Lens.lens (enabled :: AlarmConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AlarmConfiguration)
{-# DEPRECATED acEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | A list of alarms configured for the deployment group. A maximum of 10 alarms can be added to a deployment group.
--
-- /Note:/ Consider using 'alarms' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAlarms :: Lens.Lens' AlarmConfiguration (Lude.Maybe [Alarm])
acAlarms = Lens.lens (alarms :: AlarmConfiguration -> Lude.Maybe [Alarm]) (\s a -> s {alarms = a} :: AlarmConfiguration)
{-# DEPRECATED acAlarms "Use generic-lens or generic-optics with 'alarms' instead." #-}

instance Lude.FromJSON AlarmConfiguration where
  parseJSON =
    Lude.withObject
      "AlarmConfiguration"
      ( \x ->
          AlarmConfiguration'
            Lude.<$> (x Lude..:? "ignorePollAlarmFailure")
            Lude.<*> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "alarms" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AlarmConfiguration where
  toJSON AlarmConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ignorePollAlarmFailure" Lude..=)
              Lude.<$> ignorePollAlarmFailure,
            ("enabled" Lude..=) Lude.<$> enabled,
            ("alarms" Lude..=) Lude.<$> alarms
          ]
      )
