{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionConfig
  ( RealtimeMetricsSubscriptionConfig (..)
  -- * Smart constructor
  , mkRealtimeMetricsSubscriptionConfig
  -- * Lenses
  , rmscRealtimeMetricsSubscriptionStatus
  ) where

import qualified Network.AWS.CloudFront.Types.RealtimeMetricsSubscriptionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A subscription configuration for additional CloudWatch metrics.
--
-- /See:/ 'mkRealtimeMetricsSubscriptionConfig' smart constructor.
newtype RealtimeMetricsSubscriptionConfig = RealtimeMetricsSubscriptionConfig'
  { realtimeMetricsSubscriptionStatus :: Types.RealtimeMetricsSubscriptionStatus
    -- ^ A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RealtimeMetricsSubscriptionConfig' value with any optional fields omitted.
mkRealtimeMetricsSubscriptionConfig
    :: Types.RealtimeMetricsSubscriptionStatus -- ^ 'realtimeMetricsSubscriptionStatus'
    -> RealtimeMetricsSubscriptionConfig
mkRealtimeMetricsSubscriptionConfig
  realtimeMetricsSubscriptionStatus
  = RealtimeMetricsSubscriptionConfig'{realtimeMetricsSubscriptionStatus}

-- | A flag that indicates whether additional CloudWatch metrics are enabled for a given CloudFront distribution.
--
-- /Note:/ Consider using 'realtimeMetricsSubscriptionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rmscRealtimeMetricsSubscriptionStatus :: Lens.Lens' RealtimeMetricsSubscriptionConfig Types.RealtimeMetricsSubscriptionStatus
rmscRealtimeMetricsSubscriptionStatus = Lens.field @"realtimeMetricsSubscriptionStatus"
{-# INLINEABLE rmscRealtimeMetricsSubscriptionStatus #-}
{-# DEPRECATED realtimeMetricsSubscriptionStatus "Use generic-lens or generic-optics with 'realtimeMetricsSubscriptionStatus' instead"  #-}

instance Core.ToXML RealtimeMetricsSubscriptionConfig where
        toXML RealtimeMetricsSubscriptionConfig{..}
          = Core.toXMLElement "RealtimeMetricsSubscriptionStatus"
              realtimeMetricsSubscriptionStatus

instance Core.FromXML RealtimeMetricsSubscriptionConfig where
        parseXML x
          = RealtimeMetricsSubscriptionConfig' Core.<$>
              (x Core..@ "RealtimeMetricsSubscriptionStatus")
