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

import qualified Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A monitoring subscription. This structure contains information about whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /See:/ 'mkMonitoringSubscription' smart constructor.
newtype MonitoringSubscription = MonitoringSubscription'
  { -- | A subscription configuration for additional CloudWatch metrics.
    realtimeMetricsSubscriptionConfig :: Core.Maybe Types.RealtimeMetricsSubscriptionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'MonitoringSubscription' value with any optional fields omitted.
mkMonitoringSubscription ::
  MonitoringSubscription
mkMonitoringSubscription =
  MonitoringSubscription'
    { realtimeMetricsSubscriptionConfig =
        Core.Nothing
    }

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /Note:/ Consider using 'realtimeMetricsSubscriptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRealtimeMetricsSubscriptionConfig :: Lens.Lens' MonitoringSubscription (Core.Maybe Types.RealtimeMetricsSubscriptionConfig)
msRealtimeMetricsSubscriptionConfig = Lens.field @"realtimeMetricsSubscriptionConfig"
{-# DEPRECATED msRealtimeMetricsSubscriptionConfig "Use generic-lens or generic-optics with 'realtimeMetricsSubscriptionConfig' instead." #-}

instance Core.ToXML MonitoringSubscription where
  toXML MonitoringSubscription {..} =
    Core.toXMLNode "RealtimeMetricsSubscriptionConfig"
      Core.<$> realtimeMetricsSubscriptionConfig

instance Core.FromXML MonitoringSubscription where
  parseXML x =
    MonitoringSubscription'
      Core.<$> (x Core..@? "RealtimeMetricsSubscriptionConfig")
