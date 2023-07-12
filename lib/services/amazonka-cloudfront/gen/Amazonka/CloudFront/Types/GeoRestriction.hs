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
-- Module      : Amazonka.CloudFront.Types.GeoRestriction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.GeoRestriction where

import Amazonka.CloudFront.Types.GeoRestrictionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
    items :: Prelude.Maybe [Prelude.Text],
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
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GeoRestriction
newGeoRestriction pRestrictionType_ pQuantity_ =
  GeoRestriction'
    { items = Prelude.Nothing,
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
geoRestriction_items :: Lens.Lens' GeoRestriction (Prelude.Maybe [Prelude.Text])
geoRestriction_items = Lens.lens (\GeoRestriction' {items} -> items) (\s@GeoRestriction' {} a -> s {items = a} :: GeoRestriction) Prelude.. Lens.mapping Lens.coerced

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
geoRestriction_quantity :: Lens.Lens' GeoRestriction Prelude.Int
geoRestriction_quantity = Lens.lens (\GeoRestriction' {quantity} -> quantity) (\s@GeoRestriction' {} a -> s {quantity = a} :: GeoRestriction)

instance Data.FromXML GeoRestriction where
  parseXML x =
    GeoRestriction'
      Prelude.<$> ( x
                      Data..@? "Items"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Location")
                  )
      Prelude.<*> (x Data..@ "RestrictionType")
      Prelude.<*> (x Data..@ "Quantity")

instance Prelude.Hashable GeoRestriction where
  hashWithSalt _salt GeoRestriction' {..} =
    _salt
      `Prelude.hashWithSalt` items
      `Prelude.hashWithSalt` restrictionType
      `Prelude.hashWithSalt` quantity

instance Prelude.NFData GeoRestriction where
  rnf GeoRestriction' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf restrictionType
      `Prelude.seq` Prelude.rnf quantity

instance Data.ToXML GeoRestriction where
  toXML GeoRestriction' {..} =
    Prelude.mconcat
      [ "Items"
          Data.@= Data.toXML
            (Data.toXMLList "Location" Prelude.<$> items),
        "RestrictionType" Data.@= restrictionType,
        "Quantity" Data.@= quantity
      ]
