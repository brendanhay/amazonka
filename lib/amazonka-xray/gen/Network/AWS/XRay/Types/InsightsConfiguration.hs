-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InsightsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightsConfiguration
  ( InsightsConfiguration (..),

    -- * Smart constructor
    mkInsightsConfiguration,

    -- * Lenses
    icNotificationsEnabled,
    icInsightsEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The structure containing configurations related to insights.
--
-- /See:/ 'mkInsightsConfiguration' smart constructor.
data InsightsConfiguration = InsightsConfiguration'
  { notificationsEnabled ::
      Lude.Maybe Lude.Bool,
    insightsEnabled :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InsightsConfiguration' with the minimum fields required to make a request.
--
-- * 'insightsEnabled' - Set the InsightsEnabled value to true to enable insights or false to disable insights.
-- * 'notificationsEnabled' - Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
mkInsightsConfiguration ::
  InsightsConfiguration
mkInsightsConfiguration =
  InsightsConfiguration'
    { notificationsEnabled = Lude.Nothing,
      insightsEnabled = Lude.Nothing
    }

-- | Set the NotificationsEnabled value to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
-- /Note:/ Consider using 'notificationsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icNotificationsEnabled :: Lens.Lens' InsightsConfiguration (Lude.Maybe Lude.Bool)
icNotificationsEnabled = Lens.lens (notificationsEnabled :: InsightsConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {notificationsEnabled = a} :: InsightsConfiguration)
{-# DEPRECATED icNotificationsEnabled "Use generic-lens or generic-optics with 'notificationsEnabled' instead." #-}

-- | Set the InsightsEnabled value to true to enable insights or false to disable insights.
--
-- /Note:/ Consider using 'insightsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInsightsEnabled :: Lens.Lens' InsightsConfiguration (Lude.Maybe Lude.Bool)
icInsightsEnabled = Lens.lens (insightsEnabled :: InsightsConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {insightsEnabled = a} :: InsightsConfiguration)
{-# DEPRECATED icInsightsEnabled "Use generic-lens or generic-optics with 'insightsEnabled' instead." #-}

instance Lude.FromJSON InsightsConfiguration where
  parseJSON =
    Lude.withObject
      "InsightsConfiguration"
      ( \x ->
          InsightsConfiguration'
            Lude.<$> (x Lude..:? "NotificationsEnabled")
            Lude.<*> (x Lude..:? "InsightsEnabled")
      )

instance Lude.ToJSON InsightsConfiguration where
  toJSON InsightsConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NotificationsEnabled" Lude..=) Lude.<$> notificationsEnabled,
            ("InsightsEnabled" Lude..=) Lude.<$> insightsEnabled
          ]
      )
