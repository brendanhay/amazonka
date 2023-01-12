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
-- Module      : Amazonka.Snowball.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The address that you want the Snow device(s) associated with a specific
-- job to be shipped to. Addresses are validated at the time of creation.
-- The address you provide must be located within the serviceable area of
-- your region. Although no individual elements of the @Address@ are
-- required, if the address is invalid or unsupported, then an exception is
-- thrown.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The unique ID for an address.
    addressId :: Prelude.Maybe Prelude.Text,
    -- | The city in an address that a Snow device is to be delivered to.
    city :: Prelude.Maybe Prelude.Text,
    -- | The name of the company to receive a Snow device at an address.
    company :: Prelude.Maybe Prelude.Text,
    -- | The country in an address that a Snow device is to be delivered to.
    country :: Prelude.Maybe Prelude.Text,
    -- | If the address you are creating is a primary address, then set this
    -- option to true. This field is not supported in most regions.
    isRestricted :: Prelude.Maybe Prelude.Bool,
    -- | This field is no longer used and the value is ignored.
    landmark :: Prelude.Maybe Prelude.Text,
    -- | The name of a person to receive a Snow device at an address.
    name :: Prelude.Maybe Prelude.Text,
    -- | The phone number associated with an address that a Snow device is to be
    -- delivered to.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The postal code in an address that a Snow device is to be delivered to.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | This field is no longer used and the value is ignored.
    prefectureOrDistrict :: Prelude.Maybe Prelude.Text,
    -- | The state or province in an address that a Snow device is to be
    -- delivered to.
    stateOrProvince :: Prelude.Maybe Prelude.Text,
    -- | The first line in a street address that a Snow device is to be delivered
    -- to.
    street1 :: Prelude.Maybe Prelude.Text,
    -- | The second line in a street address that a Snow device is to be
    -- delivered to.
    street2 :: Prelude.Maybe Prelude.Text,
    -- | The third line in a street address that a Snow device is to be delivered
    -- to.
    street3 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Address' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressId', 'address_addressId' - The unique ID for an address.
--
-- 'city', 'address_city' - The city in an address that a Snow device is to be delivered to.
--
-- 'company', 'address_company' - The name of the company to receive a Snow device at an address.
--
-- 'country', 'address_country' - The country in an address that a Snow device is to be delivered to.
--
-- 'isRestricted', 'address_isRestricted' - If the address you are creating is a primary address, then set this
-- option to true. This field is not supported in most regions.
--
-- 'landmark', 'address_landmark' - This field is no longer used and the value is ignored.
--
-- 'name', 'address_name' - The name of a person to receive a Snow device at an address.
--
-- 'phoneNumber', 'address_phoneNumber' - The phone number associated with an address that a Snow device is to be
-- delivered to.
--
-- 'postalCode', 'address_postalCode' - The postal code in an address that a Snow device is to be delivered to.
--
-- 'prefectureOrDistrict', 'address_prefectureOrDistrict' - This field is no longer used and the value is ignored.
--
-- 'stateOrProvince', 'address_stateOrProvince' - The state or province in an address that a Snow device is to be
-- delivered to.
--
-- 'street1', 'address_street1' - The first line in a street address that a Snow device is to be delivered
-- to.
--
-- 'street2', 'address_street2' - The second line in a street address that a Snow device is to be
-- delivered to.
--
-- 'street3', 'address_street3' - The third line in a street address that a Snow device is to be delivered
-- to.
newAddress ::
  Address
newAddress =
  Address'
    { addressId = Prelude.Nothing,
      city = Prelude.Nothing,
      company = Prelude.Nothing,
      country = Prelude.Nothing,
      isRestricted = Prelude.Nothing,
      landmark = Prelude.Nothing,
      name = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      prefectureOrDistrict = Prelude.Nothing,
      stateOrProvince = Prelude.Nothing,
      street1 = Prelude.Nothing,
      street2 = Prelude.Nothing,
      street3 = Prelude.Nothing
    }

-- | The unique ID for an address.
address_addressId :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_addressId = Lens.lens (\Address' {addressId} -> addressId) (\s@Address' {} a -> s {addressId = a} :: Address)

-- | The city in an address that a Snow device is to be delivered to.
address_city :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address)

-- | The name of the company to receive a Snow device at an address.
address_company :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_company = Lens.lens (\Address' {company} -> company) (\s@Address' {} a -> s {company = a} :: Address)

-- | The country in an address that a Snow device is to be delivered to.
address_country :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_country = Lens.lens (\Address' {country} -> country) (\s@Address' {} a -> s {country = a} :: Address)

-- | If the address you are creating is a primary address, then set this
-- option to true. This field is not supported in most regions.
address_isRestricted :: Lens.Lens' Address (Prelude.Maybe Prelude.Bool)
address_isRestricted = Lens.lens (\Address' {isRestricted} -> isRestricted) (\s@Address' {} a -> s {isRestricted = a} :: Address)

-- | This field is no longer used and the value is ignored.
address_landmark :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_landmark = Lens.lens (\Address' {landmark} -> landmark) (\s@Address' {} a -> s {landmark = a} :: Address)

-- | The name of a person to receive a Snow device at an address.
address_name :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_name = Lens.lens (\Address' {name} -> name) (\s@Address' {} a -> s {name = a} :: Address)

-- | The phone number associated with an address that a Snow device is to be
-- delivered to.
address_phoneNumber :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_phoneNumber = Lens.lens (\Address' {phoneNumber} -> phoneNumber) (\s@Address' {} a -> s {phoneNumber = a} :: Address)

-- | The postal code in an address that a Snow device is to be delivered to.
address_postalCode :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address)

-- | This field is no longer used and the value is ignored.
address_prefectureOrDistrict :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_prefectureOrDistrict = Lens.lens (\Address' {prefectureOrDistrict} -> prefectureOrDistrict) (\s@Address' {} a -> s {prefectureOrDistrict = a} :: Address)

-- | The state or province in an address that a Snow device is to be
-- delivered to.
address_stateOrProvince :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_stateOrProvince = Lens.lens (\Address' {stateOrProvince} -> stateOrProvince) (\s@Address' {} a -> s {stateOrProvince = a} :: Address)

-- | The first line in a street address that a Snow device is to be delivered
-- to.
address_street1 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_street1 = Lens.lens (\Address' {street1} -> street1) (\s@Address' {} a -> s {street1 = a} :: Address)

-- | The second line in a street address that a Snow device is to be
-- delivered to.
address_street2 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_street2 = Lens.lens (\Address' {street2} -> street2) (\s@Address' {} a -> s {street2 = a} :: Address)

-- | The third line in a street address that a Snow device is to be delivered
-- to.
address_street3 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_street3 = Lens.lens (\Address' {street3} -> street3) (\s@Address' {} a -> s {street3 = a} :: Address)

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "AddressId")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "Company")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "IsRestricted")
            Prelude.<*> (x Data..:? "Landmark")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "PostalCode")
            Prelude.<*> (x Data..:? "PrefectureOrDistrict")
            Prelude.<*> (x Data..:? "StateOrProvince")
            Prelude.<*> (x Data..:? "Street1")
            Prelude.<*> (x Data..:? "Street2")
            Prelude.<*> (x Data..:? "Street3")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt `Prelude.hashWithSalt` addressId
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` company
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` isRestricted
      `Prelude.hashWithSalt` landmark
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` prefectureOrDistrict
      `Prelude.hashWithSalt` stateOrProvince
      `Prelude.hashWithSalt` street1
      `Prelude.hashWithSalt` street2
      `Prelude.hashWithSalt` street3

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf addressId
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf company
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf isRestricted
      `Prelude.seq` Prelude.rnf landmark
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf prefectureOrDistrict
      `Prelude.seq` Prelude.rnf stateOrProvince
      `Prelude.seq` Prelude.rnf street1
      `Prelude.seq` Prelude.rnf street2
      `Prelude.seq` Prelude.rnf street3

instance Data.ToJSON Address where
  toJSON Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddressId" Data..=) Prelude.<$> addressId,
            ("City" Data..=) Prelude.<$> city,
            ("Company" Data..=) Prelude.<$> company,
            ("Country" Data..=) Prelude.<$> country,
            ("IsRestricted" Data..=) Prelude.<$> isRestricted,
            ("Landmark" Data..=) Prelude.<$> landmark,
            ("Name" Data..=) Prelude.<$> name,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("PostalCode" Data..=) Prelude.<$> postalCode,
            ("PrefectureOrDistrict" Data..=)
              Prelude.<$> prefectureOrDistrict,
            ("StateOrProvince" Data..=)
              Prelude.<$> stateOrProvince,
            ("Street1" Data..=) Prelude.<$> street1,
            ("Street2" Data..=) Prelude.<$> street2,
            ("Street3" Data..=) Prelude.<$> street3
          ]
      )
