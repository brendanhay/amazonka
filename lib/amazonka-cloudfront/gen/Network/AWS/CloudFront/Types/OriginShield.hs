{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginShield
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginShield
  ( OriginShield (..),

    -- * Smart constructor
    mkOriginShield,

    -- * Lenses
    osEnabled,
    osOriginShieldRegion,
  )
where

import qualified Network.AWS.CloudFront.Types.OriginShieldRegion as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | CloudFront Origin Shield.
--
-- Using Origin Shield can help reduce the load on your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkOriginShield' smart constructor.
data OriginShield = OriginShield'
  { -- | A flag that specifies whether Origin Shield is enabled.
    --
    -- When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
    enabled :: Core.Bool,
    -- | The AWS Region for Origin Shield.
    --
    -- Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ .
    -- When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
    originShieldRegion :: Core.Maybe Types.OriginShieldRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OriginShield' value with any optional fields omitted.
mkOriginShield ::
  -- | 'enabled'
  Core.Bool ->
  OriginShield
mkOriginShield enabled =
  OriginShield' {enabled, originShieldRegion = Core.Nothing}

-- | A flag that specifies whether Origin Shield is enabled.
--
-- When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEnabled :: Lens.Lens' OriginShield Core.Bool
osEnabled = Lens.field @"enabled"
{-# DEPRECATED osEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The AWS Region for Origin Shield.
--
-- Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ .
-- When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originShieldRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOriginShieldRegion :: Lens.Lens' OriginShield (Core.Maybe Types.OriginShieldRegion)
osOriginShieldRegion = Lens.field @"originShieldRegion"
{-# DEPRECATED osOriginShieldRegion "Use generic-lens or generic-optics with 'originShieldRegion' instead." #-}

instance Core.ToXML OriginShield where
  toXML OriginShield {..} =
    Core.toXMLNode "Enabled" enabled
      Core.<> Core.toXMLNode "OriginShieldRegion" Core.<$> originShieldRegion

instance Core.FromXML OriginShield where
  parseXML x =
    OriginShield'
      Core.<$> (x Core..@ "Enabled") Core.<*> (x Core..@? "OriginShieldRegion")
