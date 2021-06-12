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
-- Module      : Network.AWS.Snowball.Types.Address
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.Address where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The address that you want the Snow device(s) associated with a specific
-- job to be shipped to. Addresses are validated at the time of creation.
-- The address you provide must be located within the serviceable area of
-- your region. Although no individual elements of the @Address@ are
-- required, if the address is invalid or unsupported, then an exception is
-- thrown.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The phone number associated with an address that a Snow device is to be
    -- delivered to.
    phoneNumber :: Core.Maybe Core.Text,
    -- | The name of the company to receive a Snow device at an address.
    company :: Core.Maybe Core.Text,
    -- | If the address you are creating is a primary address, then set this
    -- option to true. This field is not supported in most regions.
    isRestricted :: Core.Maybe Core.Bool,
    -- | The postal code in an address that a Snow device is to be delivered to.
    postalCode :: Core.Maybe Core.Text,
    -- | The first line in a street address that a Snow device is to be delivered
    -- to.
    street1 :: Core.Maybe Core.Text,
    -- | This field is no longer used and the value is ignored.
    landmark :: Core.Maybe Core.Text,
    -- | The city in an address that a Snow device is to be delivered to.
    city :: Core.Maybe Core.Text,
    -- | The name of a person to receive a Snow device at an address.
    name :: Core.Maybe Core.Text,
    -- | The unique ID for an address.
    addressId :: Core.Maybe Core.Text,
    -- | The second line in a street address that a Snow device is to be
    -- delivered to.
    street2 :: Core.Maybe Core.Text,
    -- | The state or province in an address that a Snow device is to be
    -- delivered to.
    stateOrProvince :: Core.Maybe Core.Text,
    -- | The country in an address that a Snow device is to be delivered to.
    country :: Core.Maybe Core.Text,
    -- | This field is no longer used and the value is ignored.
    prefectureOrDistrict :: Core.Maybe Core.Text,
    -- | The third line in a street address that a Snow device is to be delivered
    -- to.
    street3 :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'address_phoneNumber' - The phone number associated with an address that a Snow device is to be
-- delivered to.
--
-- 'company', 'address_company' - The name of the company to receive a Snow device at an address.
--
-- 'isRestricted', 'address_isRestricted' - If the address you are creating is a primary address, then set this
-- option to true. This field is not supported in most regions.
--
-- 'postalCode', 'address_postalCode' - The postal code in an address that a Snow device is to be delivered to.
--
-- 'street1', 'address_street1' - The first line in a street address that a Snow device is to be delivered
-- to.
--
-- 'landmark', 'address_landmark' - This field is no longer used and the value is ignored.
--
-- 'city', 'address_city' - The city in an address that a Snow device is to be delivered to.
--
-- 'name', 'address_name' - The name of a person to receive a Snow device at an address.
--
-- 'addressId', 'address_addressId' - The unique ID for an address.
--
-- 'street2', 'address_street2' - The second line in a street address that a Snow device is to be
-- delivered to.
--
-- 'stateOrProvince', 'address_stateOrProvince' - The state or province in an address that a Snow device is to be
-- delivered to.
--
-- 'country', 'address_country' - The country in an address that a Snow device is to be delivered to.
--
-- 'prefectureOrDistrict', 'address_prefectureOrDistrict' - This field is no longer used and the value is ignored.
--
-- 'street3', 'address_street3' - The third line in a street address that a Snow device is to be delivered
-- to.
newAddress ::
  Address
newAddress =
  Address'
    { phoneNumber = Core.Nothing,
      company = Core.Nothing,
      isRestricted = Core.Nothing,
      postalCode = Core.Nothing,
      street1 = Core.Nothing,
      landmark = Core.Nothing,
      city = Core.Nothing,
      name = Core.Nothing,
      addressId = Core.Nothing,
      street2 = Core.Nothing,
      stateOrProvince = Core.Nothing,
      country = Core.Nothing,
      prefectureOrDistrict = Core.Nothing,
      street3 = Core.Nothing
    }

-- | The phone number associated with an address that a Snow device is to be
-- delivered to.
address_phoneNumber :: Lens.Lens' Address (Core.Maybe Core.Text)
address_phoneNumber = Lens.lens (\Address' {phoneNumber} -> phoneNumber) (\s@Address' {} a -> s {phoneNumber = a} :: Address)

-- | The name of the company to receive a Snow device at an address.
address_company :: Lens.Lens' Address (Core.Maybe Core.Text)
address_company = Lens.lens (\Address' {company} -> company) (\s@Address' {} a -> s {company = a} :: Address)

-- | If the address you are creating is a primary address, then set this
-- option to true. This field is not supported in most regions.
address_isRestricted :: Lens.Lens' Address (Core.Maybe Core.Bool)
address_isRestricted = Lens.lens (\Address' {isRestricted} -> isRestricted) (\s@Address' {} a -> s {isRestricted = a} :: Address)

-- | The postal code in an address that a Snow device is to be delivered to.
address_postalCode :: Lens.Lens' Address (Core.Maybe Core.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address)

-- | The first line in a street address that a Snow device is to be delivered
-- to.
address_street1 :: Lens.Lens' Address (Core.Maybe Core.Text)
address_street1 = Lens.lens (\Address' {street1} -> street1) (\s@Address' {} a -> s {street1 = a} :: Address)

-- | This field is no longer used and the value is ignored.
address_landmark :: Lens.Lens' Address (Core.Maybe Core.Text)
address_landmark = Lens.lens (\Address' {landmark} -> landmark) (\s@Address' {} a -> s {landmark = a} :: Address)

-- | The city in an address that a Snow device is to be delivered to.
address_city :: Lens.Lens' Address (Core.Maybe Core.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address)

-- | The name of a person to receive a Snow device at an address.
address_name :: Lens.Lens' Address (Core.Maybe Core.Text)
address_name = Lens.lens (\Address' {name} -> name) (\s@Address' {} a -> s {name = a} :: Address)

-- | The unique ID for an address.
address_addressId :: Lens.Lens' Address (Core.Maybe Core.Text)
address_addressId = Lens.lens (\Address' {addressId} -> addressId) (\s@Address' {} a -> s {addressId = a} :: Address)

-- | The second line in a street address that a Snow device is to be
-- delivered to.
address_street2 :: Lens.Lens' Address (Core.Maybe Core.Text)
address_street2 = Lens.lens (\Address' {street2} -> street2) (\s@Address' {} a -> s {street2 = a} :: Address)

-- | The state or province in an address that a Snow device is to be
-- delivered to.
address_stateOrProvince :: Lens.Lens' Address (Core.Maybe Core.Text)
address_stateOrProvince = Lens.lens (\Address' {stateOrProvince} -> stateOrProvince) (\s@Address' {} a -> s {stateOrProvince = a} :: Address)

-- | The country in an address that a Snow device is to be delivered to.
address_country :: Lens.Lens' Address (Core.Maybe Core.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address)

-- | This field is no longer used and the value is ignored.
address_prefectureOrDistrict :: Lens.Lens' Address (Core.Maybe Core.Text)
address_prefectureOrDistrict = Lens.lens (\Address' {prefectureOrDistrict} -> prefectureOrDistrict) (\s@Address' {} a -> s {prefectureOrDistrict = a} :: Address)

-- | The third line in a street address that a Snow device is to be delivered
-- to.
address_street3 :: Lens.Lens' Address (Core.Maybe Core.Text)
address_street3 = Lens.lens (\Address' {street3} -> street3) (\s@Address' {} a -> s {street3 = a} :: Address)

instance Core.FromJSON Address where
  parseJSON =
    Core.withObject
      "Address"
      ( \x ->
          Address'
            Core.<$> (x Core..:? "PhoneNumber")
            Core.<*> (x Core..:? "Company")
            Core.<*> (x Core..:? "IsRestricted")
            Core.<*> (x Core..:? "PostalCode")
            Core.<*> (x Core..:? "Street1")
            Core.<*> (x Core..:? "Landmark")
            Core.<*> (x Core..:? "City")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "AddressId")
            Core.<*> (x Core..:? "Street2")
            Core.<*> (x Core..:? "StateOrProvince")
            Core.<*> (x Core..:? "Country")
            Core.<*> (x Core..:? "PrefectureOrDistrict")
            Core.<*> (x Core..:? "Street3")
      )

instance Core.Hashable Address

instance Core.NFData Address

instance Core.ToJSON Address where
  toJSON Address' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("Company" Core..=) Core.<$> company,
            ("IsRestricted" Core..=) Core.<$> isRestricted,
            ("PostalCode" Core..=) Core.<$> postalCode,
            ("Street1" Core..=) Core.<$> street1,
            ("Landmark" Core..=) Core.<$> landmark,
            ("City" Core..=) Core.<$> city,
            ("Name" Core..=) Core.<$> name,
            ("AddressId" Core..=) Core.<$> addressId,
            ("Street2" Core..=) Core.<$> street2,
            ("StateOrProvince" Core..=) Core.<$> stateOrProvince,
            ("Country" Core..=) Core.<$> country,
            ("PrefectureOrDistrict" Core..=)
              Core.<$> prefectureOrDistrict,
            ("Street3" Core..=) Core.<$> street3
          ]
      )
