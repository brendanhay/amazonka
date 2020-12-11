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
    osOriginShieldRegion,
    osEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | CloudFront Origin Shield.
--
-- Using Origin Shield can help reduce the load on your origin. For more information, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html Using Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- /See:/ 'mkOriginShield' smart constructor.
data OriginShield = OriginShield'
  { originShieldRegion ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OriginShield' with the minimum fields required to make a request.
--
-- * 'enabled' - A flag that specifies whether Origin Shield is enabled.
--
-- When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
-- * 'originShieldRegion' - The AWS Region for Origin Shield.
--
-- Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ .
-- When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
mkOriginShield ::
  -- | 'enabled'
  Lude.Bool ->
  OriginShield
mkOriginShield pEnabled_ =
  OriginShield'
    { originShieldRegion = Lude.Nothing,
      enabled = pEnabled_
    }

-- | The AWS Region for Origin Shield.
--
-- Specify the AWS Region that has the lowest latency to your origin. To specify a region, use the region code, not the region name. For example, specify the US East (Ohio) region as @us-east-2@ .
-- When you enable CloudFront Origin Shield, you must specify the AWS Region for Origin Shield. For the list of AWS Regions that you can specify, and for help choosing the best Region for your origin, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/origin-shield.html#choose-origin-shield-region Choosing the AWS Region for Origin Shield> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'originShieldRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOriginShieldRegion :: Lens.Lens' OriginShield (Lude.Maybe Lude.Text)
osOriginShieldRegion = Lens.lens (originShieldRegion :: OriginShield -> Lude.Maybe Lude.Text) (\s a -> s {originShieldRegion = a} :: OriginShield)
{-# DEPRECATED osOriginShieldRegion "Use generic-lens or generic-optics with 'originShieldRegion' instead." #-}

-- | A flag that specifies whether Origin Shield is enabled.
--
-- When it’s enabled, CloudFront routes all requests through Origin Shield, which can help protect your origin. When it’s disabled, CloudFront might send requests directly to your origin from multiple edge locations or regional edge caches.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osEnabled :: Lens.Lens' OriginShield Lude.Bool
osEnabled = Lens.lens (enabled :: OriginShield -> Lude.Bool) (\s a -> s {enabled = a} :: OriginShield)
{-# DEPRECATED osEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML OriginShield where
  parseXML x =
    OriginShield'
      Lude.<$> (x Lude..@? "OriginShieldRegion") Lude.<*> (x Lude..@ "Enabled")

instance Lude.ToXML OriginShield where
  toXML OriginShield' {..} =
    Lude.mconcat
      [ "OriginShieldRegion" Lude.@= originShieldRegion,
        "Enabled" Lude.@= enabled
      ]
