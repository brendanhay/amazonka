{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.MonitoringSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.MonitoringSubscription
  ( MonitoringSubscription (..),

    -- * Smart constructor
    mkMonitoringSubscription,

    -- * Lenses
    msRealtimeMetricsSubscriptionConfig,
  )
where

import Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /See:/ 'mkMonitoringSubscription' smart constructor.
newtype MonitoringSubscription = MonitoringSubscription'
  { -- | A subscription configuration for additional CloudWatch metrics.
    realtimeMetricsSubscriptionConfig :: Lude.Maybe RealtimeMetricsSubscriptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MonitoringSubscription' with the minimum fields required to make a request.
--
-- * 'realtimeMetricsSubscriptionConfig' - A subscription configuration for additional CloudWatch metrics.
mkMonitoringSubscription ::
  MonitoringSubscription
mkMonitoringSubscription =
  MonitoringSubscription'
    { realtimeMetricsSubscriptionConfig =
        Lude.Nothing
    }

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /Note:/ Consider using 'realtimeMetricsSubscriptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRealtimeMetricsSubscriptionConfig :: Lens.Lens' MonitoringSubscription (Lude.Maybe RealtimeMetricsSubscriptionConfig)
msRealtimeMetricsSubscriptionConfig = Lens.lens (realtimeMetricsSubscriptionConfig :: MonitoringSubscription -> Lude.Maybe RealtimeMetricsSubscriptionConfig) (\s a -> s {realtimeMetricsSubscriptionConfig = a} :: MonitoringSubscription)
{-# DEPRECATED msRealtimeMetricsSubscriptionConfig "Use generic-lens or generic-optics with 'realtimeMetricsSubscriptionConfig' instead." #-}

instance Lude.FromXML MonitoringSubscription where
  parseXML x =
    MonitoringSubscription'
      Lude.<$> (x Lude..@? "RealtimeMetricsSubscriptionConfig")

instance Lude.ToXML MonitoringSubscription where
  toXML MonitoringSubscription' {..} =
    Lude.mconcat
      [ "RealtimeMetricsSubscriptionConfig"
          Lude.@= realtimeMetricsSubscriptionConfig
      ]
