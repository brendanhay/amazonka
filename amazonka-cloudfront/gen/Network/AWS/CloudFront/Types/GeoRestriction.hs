{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.GeoRestriction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.GeoRestriction where

import Network.AWS.CloudFront.Types.GeoRestrictionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A complex type that controls the countries in which your content is
-- distributed. CloudFront determines the location of your users using
-- @MaxMind@ GeoIP databases.
--
-- /See:/ 'newGeoRestriction' smart constructor.
data GeoRestriction = GeoRestriction'
  { -- | A complex type that contains a @Location@ element for each country in
    -- which you want CloudFront either to distribute your content
    -- (@whitelist@) or not distribute your content (@blacklist@).
    --
    -- The @Location@ element is a two-letter, uppercase country code for a
    -- country that you want to include in your @blacklist@ or @whitelist@.
    -- Include one @Location@ element for each country.
    --
    -- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the
    -- current list of countries and the corresponding codes, see
    -- @ISO 3166-1-alpha-2@ code on the /International Organization for
    -- Standardization/ website. You can also refer to the country list on the
    -- CloudFront console, which includes both country names and codes.
    items :: Core.Maybe [Core.Text],
    -- | The method that you want to use to restrict distribution of your content
    -- by country:
    --
    -- -   @none@: No geo restriction is enabled, meaning access to content is
    --     not restricted by client geo location.
    --
    -- -   @blacklist@: The @Location@ elements specify the countries in which
    --     you don\'t want CloudFront to distribute your content.
    --
    -- -   @whitelist@: The @Location@ elements specify the countries in which
    --     you want CloudFront to distribute your content.
    restrictionType :: GeoRestrictionType,
    -- | When geo restriction is @enabled@, this is the number of countries in
    -- your @whitelist@ or @blacklist@. Otherwise, when it is not enabled,
    -- @Quantity@ is @0@, and you can omit @Items@.
    quantity :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GeoRestriction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'geoRestriction_items' - A complex type that contains a @Location@ element for each country in
-- which you want CloudFront either to distribute your content
-- (@whitelist@) or not distribute your content (@blacklist@).
--
-- The @Location@ element is a two-letter, uppercase country code for a
-- country that you want to include in your @blacklist@ or @whitelist@.
-- Include one @Location@ element for each country.
--
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the
-- current list of countries and the corresponding codes, see
-- @ISO 3166-1-alpha-2@ code on the /International Organization for
-- Standardization/ website. You can also refer to the country list on the
-- CloudFront console, which includes both country names and codes.
--
-- 'restrictionType', 'geoRestriction_restrictionType' - The method that you want to use to restrict distribution of your content
-- by country:
--
-- -   @none@: No geo restriction is enabled, meaning access to content is
--     not restricted by client geo location.
--
-- -   @blacklist@: The @Location@ elements specify the countries in which
--     you don\'t want CloudFront to distribute your content.
--
-- -   @whitelist@: The @Location@ elements specify the countries in which
--     you want CloudFront to distribute your content.
--
-- 'quantity', 'geoRestriction_quantity' - When geo restriction is @enabled@, this is the number of countries in
-- your @whitelist@ or @blacklist@. Otherwise, when it is not enabled,
-- @Quantity@ is @0@, and you can omit @Items@.
newGeoRestriction ::
  -- | 'restrictionType'
  GeoRestrictionType ->
  -- | 'quantity'
  Core.Int ->
  GeoRestriction
newGeoRestriction pRestrictionType_ pQuantity_ =
  GeoRestriction'
    { items = Core.Nothing,
      restrictionType = pRestrictionType_,
      quantity = pQuantity_
    }

-- | A complex type that contains a @Location@ element for each country in
-- which you want CloudFront either to distribute your content
-- (@whitelist@) or not distribute your content (@blacklist@).
--
-- The @Location@ element is a two-letter, uppercase country code for a
-- country that you want to include in your @blacklist@ or @whitelist@.
-- Include one @Location@ element for each country.
--
-- CloudFront and @MaxMind@ both use @ISO 3166@ country codes. For the
-- current list of countries and the corresponding codes, see
-- @ISO 3166-1-alpha-2@ code on the /International Organization for
-- Standardization/ website. You can also refer to the country list on the
-- CloudFront console, which includes both country names and codes.
geoRestriction_items :: Lens.Lens' GeoRestriction (Core.Maybe [Core.Text])
geoRestriction_items = Lens.lens (\GeoRestriction' {items} -> items) (\s@GeoRestriction' {} a -> s {items = a} :: GeoRestriction) Core.. Lens.mapping Lens._Coerce

-- | The method that you want to use to restrict distribution of your content
-- by country:
--
-- -   @none@: No geo restriction is enabled, meaning access to content is
--     not restricted by client geo location.
--
-- -   @blacklist@: The @Location@ elements specify the countries in which
--     you don\'t want CloudFront to distribute your content.
--
-- -   @whitelist@: The @Location@ elements specify the countries in which
--     you want CloudFront to distribute your content.
geoRestriction_restrictionType :: Lens.Lens' GeoRestriction GeoRestrictionType
geoRestriction_restrictionType = Lens.lens (\GeoRestriction' {restrictionType} -> restrictionType) (\s@GeoRestriction' {} a -> s {restrictionType = a} :: GeoRestriction)

-- | When geo restriction is @enabled@, this is the number of countries in
-- your @whitelist@ or @blacklist@. Otherwise, when it is not enabled,
-- @Quantity@ is @0@, and you can omit @Items@.
geoRestriction_quantity :: Lens.Lens' GeoRestriction Core.Int
geoRestriction_quantity = Lens.lens (\GeoRestriction' {quantity} -> quantity) (\s@GeoRestriction' {} a -> s {quantity = a} :: GeoRestriction)

instance Core.FromXML GeoRestriction where
  parseXML x =
    GeoRestriction'
      Core.<$> ( x Core..@? "Items" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Location")
               )
      Core.<*> (x Core..@ "RestrictionType")
      Core.<*> (x Core..@ "Quantity")

instance Core.Hashable GeoRestriction

instance Core.NFData GeoRestriction

instance Core.ToXML GeoRestriction where
  toXML GeoRestriction' {..} =
    Core.mconcat
      [ "Items"
          Core.@= Core.toXML
            (Core.toXMLList "Location" Core.<$> items),
        "RestrictionType" Core.@= restrictionType,
        "Quantity" Core.@= quantity
      ]
