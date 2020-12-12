{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestriction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.GeoRestriction
  ( GeoRestriction (..),

    -- * Smart constructor
    mkGeoRestriction,

    -- * Lenses
    grItems,
    grRestrictionType,
    grQuantity,
  )
where

import Network.AWS.CloudFront.Types.GeoRestrictionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that controls the countries in which your content is distributed. CloudFront determines the location of your users using @MaxMind@ GeoIP databases.
--
-- /See:/ 'mkGeoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
  { items ::
      Lude.Maybe [Lude.Text],
    restrictionType :: GeoRestrictionType,
    quantity :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoRestriction' with the minimum fields required to make a request.
--
-- * 'items' - A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ).
--
-- The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country.
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
-- * 'quantity' - When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
-- * 'restrictionType' - The method that you want to use to restrict distribution of your content by country:
--
--
--     * @none@ : No geo restriction is enabled, meaning access to content is not restricted by client geo location.
--
--
--     * @blacklist@ : The @Location@ elements specify the countries in which you don't want CloudFront to distribute your content.
--
--
--     * @whitelist@ : The @Location@ elements specify the countries in which you want CloudFront to distribute your content.
mkGeoRestriction ::
  -- | 'restrictionType'
  GeoRestrictionType ->
  -- | 'quantity'
  Lude.Int ->
  GeoRestriction
mkGeoRestriction pRestrictionType_ pQuantity_ =
  GeoRestriction'
    { items = Lude.Nothing,
      restrictionType = pRestrictionType_,
      quantity = pQuantity_
    }

-- | A complex type that contains a @Location@ element for each country in which you want CloudFront either to distribute your content (@whitelist@ ) or not distribute your content (@blacklist@ ).
--
-- The @Location@ element is a two-letter, uppercase country code for a country that you want to include in your @blacklist@ or @whitelist@ . Include one @Location@ element for each country.
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the current list of countries and the corresponding codes, see @ISO 3166-1-alpha-2@ code on the /International Organization for Standardization/ website. You can also refer to the country list on the CloudFront console, which includes both country names and codes.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grItems :: Lens.Lens' GeoRestriction (Lude.Maybe [Lude.Text])
grItems = Lens.lens (items :: GeoRestriction -> Lude.Maybe [Lude.Text]) (\s a -> s {items = a} :: GeoRestriction)
{-# DEPRECATED grItems "Use generic-lens or generic-optics with 'items' instead." #-}

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
grRestrictionType :: Lens.Lens' GeoRestriction GeoRestrictionType
grRestrictionType = Lens.lens (restrictionType :: GeoRestriction -> GeoRestrictionType) (\s a -> s {restrictionType = a} :: GeoRestriction)
{-# DEPRECATED grRestrictionType "Use generic-lens or generic-optics with 'restrictionType' instead." #-}

-- | When geo restriction is @enabled@ , this is the number of countries in your @whitelist@ or @blacklist@ . Otherwise, when it is not enabled, @Quantity@ is @0@ , and you can omit @Items@ .
--
-- /Note:/ Consider using 'quantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grQuantity :: Lens.Lens' GeoRestriction Lude.Int
grQuantity = Lens.lens (quantity :: GeoRestriction -> Lude.Int) (\s a -> s {quantity = a} :: GeoRestriction)
{-# DEPRECATED grQuantity "Use generic-lens or generic-optics with 'quantity' instead." #-}

instance Lude.FromXML GeoRestriction where
  parseXML x =
    GeoRestriction'
      Lude.<$> ( x Lude..@? "Items" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Location")
               )
      Lude.<*> (x Lude..@ "RestrictionType")
      Lude.<*> (x Lude..@ "Quantity")

instance Lude.ToXML GeoRestriction where
  toXML GeoRestriction' {..} =
    Lude.mconcat
      [ "Items"
          Lude.@= Lude.toXML (Lude.toXMLList "Location" Lude.<$> items),
        "RestrictionType" Lude.@= restrictionType,
        "Quantity" Lude.@= quantity
      ]
