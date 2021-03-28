{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestriction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.GeoRestriction
  ( GeoRestriction (..)
  -- * Smart constructor
  , mkGeoRestriction
  -- * Lenses
  , grRestrictionType
  , grQuantity
  , grItems
  ) where

import qualified Network.AWS.CloudFront.Types.GeoRestrictionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases. 
--
-- /See:/ 'mkGeoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
  { restrictionType :: Types.GeoRestrictionType
    -- ^ The method that you want to use to restrict distribution of your content by country:
--
--
--     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.
--
--
--     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.
--
--
--     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
--
--
  , quantity :: Core.Int
    -- ^ When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
  , items :: Core.Maybe [Core.Text]
    -- ^ A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ).
--
-- The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country.
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoRestriction' value with any optional fields omitted.
mkGeoRestriction
    :: Types.GeoRestrictionType -- ^ 'restrictionType'
    -> Core.Int -- ^ 'quantity'
    -> GeoRestriction
mkGeoRestriction restrictionType quantity
  = GeoRestriction'{restrictionType, quantity, items = Core.Nothing}

-- | The method that you want to use to restrict distribution of your content by country:
--
--
--     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.
--
--
--     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.
--
--
--     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
--
--
--
-- /Note:/ Consider using 'restrictionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRestrictionType :: Lens.Lens' GeoRestriction Types.GeoRestrictionType
grRestrictionType = Lens.field @"restrictionType"
{-# INLINEABLE grRestrictionType #-}
{-# DEPRECATED restrictionType "Use generic-lens or generic-optics with 'restrictionType' instead"  #-}

-- | When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grQuantity :: Lens.Lens' GeoRestriction Core.Int
grQuantity = Lens.field @"quantity"
{-# INLINEABLE grQuantity #-}
{-# DEPRECATED quantity "Use generic-lens or generic-optics with 'quantity' instead"  #-}

-- | A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ).
--
-- The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country.
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grItems :: Lens.Lens' GeoRestriction (Core.Maybe [Core.Text])
grItems = Lens.field @"items"
{-# INLINEABLE grItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

instance Core.ToXML GeoRestriction where
        toXML GeoRestriction{..}
          = Core.toXMLElement "RestrictionType" restrictionType Core.<>
              Core.toXMLElement "Quantity" quantity
              Core.<>
              Core.toXMLElement "Items"
                (Core.maybe Core.mempty (Core.toXMLList "Location") items)

instance Core.FromXML GeoRestriction where
        parseXML x
          = GeoRestriction' Core.<$>
              (x Core..@ "RestrictionType") Core.<*> x Core..@ "Quantity"
                Core.<*> x Core..@? "Items" Core..<@> Core.parseXMLList "Location"
