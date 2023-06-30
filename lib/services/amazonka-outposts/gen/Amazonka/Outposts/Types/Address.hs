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
-- Module      : Amazonka.Outposts.Types.Address
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Address where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an address.
--
-- /See:/ 'newAddress' smart constructor.
data Address = Address'
  { -- | The second line of the address.
    addressLine2 :: Prelude.Maybe Prelude.Text,
    -- | The third line of the address.
    addressLine3 :: Prelude.Maybe Prelude.Text,
    -- | The name of the contact.
    contactName :: Prelude.Maybe Prelude.Text,
    -- | The phone number of the contact.
    contactPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The district or county for the address.
    districtOrCounty :: Prelude.Maybe Prelude.Text,
    -- | The municipality for the address.
    municipality :: Prelude.Maybe Prelude.Text,
    -- | The first line of the address.
    addressLine1 :: Prelude.Text,
    -- | The city for the address.
    city :: Prelude.Text,
    -- | The state for the address.
    stateOrRegion :: Prelude.Text,
    -- | The postal code for the address.
    postalCode :: Prelude.Text,
    -- | The ISO-3166 two-letter country code for the address.
    countryCode :: Prelude.Text
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
-- 'addressLine2', 'address_addressLine2' - The second line of the address.
--
-- 'addressLine3', 'address_addressLine3' - The third line of the address.
--
-- 'contactName', 'address_contactName' - The name of the contact.
--
-- 'contactPhoneNumber', 'address_contactPhoneNumber' - The phone number of the contact.
--
-- 'districtOrCounty', 'address_districtOrCounty' - The district or county for the address.
--
-- 'municipality', 'address_municipality' - The municipality for the address.
--
-- 'addressLine1', 'address_addressLine1' - The first line of the address.
--
-- 'city', 'address_city' - The city for the address.
--
-- 'stateOrRegion', 'address_stateOrRegion' - The state for the address.
--
-- 'postalCode', 'address_postalCode' - The postal code for the address.
--
-- 'countryCode', 'address_countryCode' - The ISO-3166 two-letter country code for the address.
newAddress ::
  -- | 'addressLine1'
  Prelude.Text ->
  -- | 'city'
  Prelude.Text ->
  -- | 'stateOrRegion'
  Prelude.Text ->
  -- | 'postalCode'
  Prelude.Text ->
  -- | 'countryCode'
  Prelude.Text ->
  Address
newAddress
  pAddressLine1_
  pCity_
  pStateOrRegion_
  pPostalCode_
  pCountryCode_ =
    Address'
      { addressLine2 = Prelude.Nothing,
        addressLine3 = Prelude.Nothing,
        contactName = Prelude.Nothing,
        contactPhoneNumber = Prelude.Nothing,
        districtOrCounty = Prelude.Nothing,
        municipality = Prelude.Nothing,
        addressLine1 = pAddressLine1_,
        city = pCity_,
        stateOrRegion = pStateOrRegion_,
        postalCode = pPostalCode_,
        countryCode = pCountryCode_
      }

-- | The second line of the address.
address_addressLine2 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_addressLine2 = Lens.lens (\Address' {addressLine2} -> addressLine2) (\s@Address' {} a -> s {addressLine2 = a} :: Address)

-- | The third line of the address.
address_addressLine3 :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_addressLine3 = Lens.lens (\Address' {addressLine3} -> addressLine3) (\s@Address' {} a -> s {addressLine3 = a} :: Address)

-- | The name of the contact.
address_contactName :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_contactName = Lens.lens (\Address' {contactName} -> contactName) (\s@Address' {} a -> s {contactName = a} :: Address)

-- | The phone number of the contact.
address_contactPhoneNumber :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_contactPhoneNumber = Lens.lens (\Address' {contactPhoneNumber} -> contactPhoneNumber) (\s@Address' {} a -> s {contactPhoneNumber = a} :: Address)

-- | The district or county for the address.
address_districtOrCounty :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_districtOrCounty = Lens.lens (\Address' {districtOrCounty} -> districtOrCounty) (\s@Address' {} a -> s {districtOrCounty = a} :: Address)

-- | The municipality for the address.
address_municipality :: Lens.Lens' Address (Prelude.Maybe Prelude.Text)
address_municipality = Lens.lens (\Address' {municipality} -> municipality) (\s@Address' {} a -> s {municipality = a} :: Address)

-- | The first line of the address.
address_addressLine1 :: Lens.Lens' Address Prelude.Text
address_addressLine1 = Lens.lens (\Address' {addressLine1} -> addressLine1) (\s@Address' {} a -> s {addressLine1 = a} :: Address)

-- | The city for the address.
address_city :: Lens.Lens' Address Prelude.Text
address_city = Lens.lens (\Address' {city} -> city) (\s@Address' {} a -> s {city = a} :: Address)

-- | The state for the address.
address_stateOrRegion :: Lens.Lens' Address Prelude.Text
address_stateOrRegion = Lens.lens (\Address' {stateOrRegion} -> stateOrRegion) (\s@Address' {} a -> s {stateOrRegion = a} :: Address)

-- | The postal code for the address.
address_postalCode :: Lens.Lens' Address Prelude.Text
address_postalCode = Lens.lens (\Address' {postalCode} -> postalCode) (\s@Address' {} a -> s {postalCode = a} :: Address)

-- | The ISO-3166 two-letter country code for the address.
address_countryCode :: Lens.Lens' Address Prelude.Text
address_countryCode = Lens.lens (\Address' {countryCode} -> countryCode) (\s@Address' {} a -> s {countryCode = a} :: Address)

instance Data.FromJSON Address where
  parseJSON =
    Data.withObject
      "Address"
      ( \x ->
          Address'
            Prelude.<$> (x Data..:? "AddressLine2")
            Prelude.<*> (x Data..:? "AddressLine3")
            Prelude.<*> (x Data..:? "ContactName")
            Prelude.<*> (x Data..:? "ContactPhoneNumber")
            Prelude.<*> (x Data..:? "DistrictOrCounty")
            Prelude.<*> (x Data..:? "Municipality")
            Prelude.<*> (x Data..: "AddressLine1")
            Prelude.<*> (x Data..: "City")
            Prelude.<*> (x Data..: "StateOrRegion")
            Prelude.<*> (x Data..: "PostalCode")
            Prelude.<*> (x Data..: "CountryCode")
      )

instance Prelude.Hashable Address where
  hashWithSalt _salt Address' {..} =
    _salt
      `Prelude.hashWithSalt` addressLine2
      `Prelude.hashWithSalt` addressLine3
      `Prelude.hashWithSalt` contactName
      `Prelude.hashWithSalt` contactPhoneNumber
      `Prelude.hashWithSalt` districtOrCounty
      `Prelude.hashWithSalt` municipality
      `Prelude.hashWithSalt` addressLine1
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` stateOrRegion
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` countryCode

instance Prelude.NFData Address where
  rnf Address' {..} =
    Prelude.rnf addressLine2
      `Prelude.seq` Prelude.rnf addressLine3
      `Prelude.seq` Prelude.rnf contactName
      `Prelude.seq` Prelude.rnf contactPhoneNumber
      `Prelude.seq` Prelude.rnf districtOrCounty
      `Prelude.seq` Prelude.rnf municipality
      `Prelude.seq` Prelude.rnf addressLine1
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf stateOrRegion
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf countryCode

instance Data.ToJSON Address where
  toJSON Address' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddressLine2" Data..=) Prelude.<$> addressLine2,
            ("AddressLine3" Data..=) Prelude.<$> addressLine3,
            ("ContactName" Data..=) Prelude.<$> contactName,
            ("ContactPhoneNumber" Data..=)
              Prelude.<$> contactPhoneNumber,
            ("DistrictOrCounty" Data..=)
              Prelude.<$> districtOrCounty,
            ("Municipality" Data..=) Prelude.<$> municipality,
            Prelude.Just ("AddressLine1" Data..= addressLine1),
            Prelude.Just ("City" Data..= city),
            Prelude.Just ("StateOrRegion" Data..= stateOrRegion),
            Prelude.Just ("PostalCode" Data..= postalCode),
            Prelude.Just ("CountryCode" Data..= countryCode)
          ]
      )
