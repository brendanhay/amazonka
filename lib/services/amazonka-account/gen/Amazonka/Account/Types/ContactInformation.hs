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
-- Module      : Amazonka.Account.Types.ContactInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.ContactInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
--
-- /See:/ 'newContactInformation' smart constructor.
data ContactInformation = ContactInformation'
  { -- | The second line of the primary contact address, if any.
    addressLine2 :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The third line of the primary contact address, if any.
    addressLine3 :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the company associated with the primary contact information,
    -- if any.
    companyName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The district or county of the primary contact address, if any.
    districtOrCounty :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state or region of the primary contact address. This field is
    -- required in selected countries.
    stateOrRegion :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The URL of the website associated with the primary contact information,
    -- if any.
    websiteUrl :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The first line of the primary contact address.
    addressLine1 :: Data.Sensitive Prelude.Text,
    -- | The city of the primary contact address.
    city :: Data.Sensitive Prelude.Text,
    -- | The ISO-3166 two-letter country code for the primary contact address.
    countryCode :: Data.Sensitive Prelude.Text,
    -- | The full name of the primary contact address.
    fullName :: Data.Sensitive Prelude.Text,
    -- | The phone number of the primary contact information. The number will be
    -- validated and, in some countries, checked for activation.
    phoneNumber :: Data.Sensitive Prelude.Text,
    -- | The postal code of the primary contact address.
    postalCode :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressLine2', 'contactInformation_addressLine2' - The second line of the primary contact address, if any.
--
-- 'addressLine3', 'contactInformation_addressLine3' - The third line of the primary contact address, if any.
--
-- 'companyName', 'contactInformation_companyName' - The name of the company associated with the primary contact information,
-- if any.
--
-- 'districtOrCounty', 'contactInformation_districtOrCounty' - The district or county of the primary contact address, if any.
--
-- 'stateOrRegion', 'contactInformation_stateOrRegion' - The state or region of the primary contact address. This field is
-- required in selected countries.
--
-- 'websiteUrl', 'contactInformation_websiteUrl' - The URL of the website associated with the primary contact information,
-- if any.
--
-- 'addressLine1', 'contactInformation_addressLine1' - The first line of the primary contact address.
--
-- 'city', 'contactInformation_city' - The city of the primary contact address.
--
-- 'countryCode', 'contactInformation_countryCode' - The ISO-3166 two-letter country code for the primary contact address.
--
-- 'fullName', 'contactInformation_fullName' - The full name of the primary contact address.
--
-- 'phoneNumber', 'contactInformation_phoneNumber' - The phone number of the primary contact information. The number will be
-- validated and, in some countries, checked for activation.
--
-- 'postalCode', 'contactInformation_postalCode' - The postal code of the primary contact address.
newContactInformation ::
  -- | 'addressLine1'
  Prelude.Text ->
  -- | 'city'
  Prelude.Text ->
  -- | 'countryCode'
  Prelude.Text ->
  -- | 'fullName'
  Prelude.Text ->
  -- | 'phoneNumber'
  Prelude.Text ->
  -- | 'postalCode'
  Prelude.Text ->
  ContactInformation
newContactInformation
  pAddressLine1_
  pCity_
  pCountryCode_
  pFullName_
  pPhoneNumber_
  pPostalCode_ =
    ContactInformation'
      { addressLine2 = Prelude.Nothing,
        addressLine3 = Prelude.Nothing,
        companyName = Prelude.Nothing,
        districtOrCounty = Prelude.Nothing,
        stateOrRegion = Prelude.Nothing,
        websiteUrl = Prelude.Nothing,
        addressLine1 = Data._Sensitive Lens.# pAddressLine1_,
        city = Data._Sensitive Lens.# pCity_,
        countryCode = Data._Sensitive Lens.# pCountryCode_,
        fullName = Data._Sensitive Lens.# pFullName_,
        phoneNumber = Data._Sensitive Lens.# pPhoneNumber_,
        postalCode = Data._Sensitive Lens.# pPostalCode_
      }

-- | The second line of the primary contact address, if any.
contactInformation_addressLine2 :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_addressLine2 = Lens.lens (\ContactInformation' {addressLine2} -> addressLine2) (\s@ContactInformation' {} a -> s {addressLine2 = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The third line of the primary contact address, if any.
contactInformation_addressLine3 :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_addressLine3 = Lens.lens (\ContactInformation' {addressLine3} -> addressLine3) (\s@ContactInformation' {} a -> s {addressLine3 = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the company associated with the primary contact information,
-- if any.
contactInformation_companyName :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_companyName = Lens.lens (\ContactInformation' {companyName} -> companyName) (\s@ContactInformation' {} a -> s {companyName = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The district or county of the primary contact address, if any.
contactInformation_districtOrCounty :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_districtOrCounty = Lens.lens (\ContactInformation' {districtOrCounty} -> districtOrCounty) (\s@ContactInformation' {} a -> s {districtOrCounty = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The state or region of the primary contact address. This field is
-- required in selected countries.
contactInformation_stateOrRegion :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_stateOrRegion = Lens.lens (\ContactInformation' {stateOrRegion} -> stateOrRegion) (\s@ContactInformation' {} a -> s {stateOrRegion = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The URL of the website associated with the primary contact information,
-- if any.
contactInformation_websiteUrl :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_websiteUrl = Lens.lens (\ContactInformation' {websiteUrl} -> websiteUrl) (\s@ContactInformation' {} a -> s {websiteUrl = a} :: ContactInformation) Prelude.. Lens.mapping Data._Sensitive

-- | The first line of the primary contact address.
contactInformation_addressLine1 :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_addressLine1 = Lens.lens (\ContactInformation' {addressLine1} -> addressLine1) (\s@ContactInformation' {} a -> s {addressLine1 = a} :: ContactInformation) Prelude.. Data._Sensitive

-- | The city of the primary contact address.
contactInformation_city :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_city = Lens.lens (\ContactInformation' {city} -> city) (\s@ContactInformation' {} a -> s {city = a} :: ContactInformation) Prelude.. Data._Sensitive

-- | The ISO-3166 two-letter country code for the primary contact address.
contactInformation_countryCode :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_countryCode = Lens.lens (\ContactInformation' {countryCode} -> countryCode) (\s@ContactInformation' {} a -> s {countryCode = a} :: ContactInformation) Prelude.. Data._Sensitive

-- | The full name of the primary contact address.
contactInformation_fullName :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_fullName = Lens.lens (\ContactInformation' {fullName} -> fullName) (\s@ContactInformation' {} a -> s {fullName = a} :: ContactInformation) Prelude.. Data._Sensitive

-- | The phone number of the primary contact information. The number will be
-- validated and, in some countries, checked for activation.
contactInformation_phoneNumber :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_phoneNumber = Lens.lens (\ContactInformation' {phoneNumber} -> phoneNumber) (\s@ContactInformation' {} a -> s {phoneNumber = a} :: ContactInformation) Prelude.. Data._Sensitive

-- | The postal code of the primary contact address.
contactInformation_postalCode :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_postalCode = Lens.lens (\ContactInformation' {postalCode} -> postalCode) (\s@ContactInformation' {} a -> s {postalCode = a} :: ContactInformation) Prelude.. Data._Sensitive

instance Data.FromJSON ContactInformation where
  parseJSON =
    Data.withObject
      "ContactInformation"
      ( \x ->
          ContactInformation'
            Prelude.<$> (x Data..:? "AddressLine2")
            Prelude.<*> (x Data..:? "AddressLine3")
            Prelude.<*> (x Data..:? "CompanyName")
            Prelude.<*> (x Data..:? "DistrictOrCounty")
            Prelude.<*> (x Data..:? "StateOrRegion")
            Prelude.<*> (x Data..:? "WebsiteUrl")
            Prelude.<*> (x Data..: "AddressLine1")
            Prelude.<*> (x Data..: "City")
            Prelude.<*> (x Data..: "CountryCode")
            Prelude.<*> (x Data..: "FullName")
            Prelude.<*> (x Data..: "PhoneNumber")
            Prelude.<*> (x Data..: "PostalCode")
      )

instance Prelude.Hashable ContactInformation where
  hashWithSalt _salt ContactInformation' {..} =
    _salt `Prelude.hashWithSalt` addressLine2
      `Prelude.hashWithSalt` addressLine3
      `Prelude.hashWithSalt` companyName
      `Prelude.hashWithSalt` districtOrCounty
      `Prelude.hashWithSalt` stateOrRegion
      `Prelude.hashWithSalt` websiteUrl
      `Prelude.hashWithSalt` addressLine1
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` fullName
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` postalCode

instance Prelude.NFData ContactInformation where
  rnf ContactInformation' {..} =
    Prelude.rnf addressLine2
      `Prelude.seq` Prelude.rnf addressLine3
      `Prelude.seq` Prelude.rnf companyName
      `Prelude.seq` Prelude.rnf districtOrCounty
      `Prelude.seq` Prelude.rnf stateOrRegion
      `Prelude.seq` Prelude.rnf websiteUrl
      `Prelude.seq` Prelude.rnf addressLine1
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf fullName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf postalCode

instance Data.ToJSON ContactInformation where
  toJSON ContactInformation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AddressLine2" Data..=) Prelude.<$> addressLine2,
            ("AddressLine3" Data..=) Prelude.<$> addressLine3,
            ("CompanyName" Data..=) Prelude.<$> companyName,
            ("DistrictOrCounty" Data..=)
              Prelude.<$> districtOrCounty,
            ("StateOrRegion" Data..=) Prelude.<$> stateOrRegion,
            ("WebsiteUrl" Data..=) Prelude.<$> websiteUrl,
            Prelude.Just ("AddressLine1" Data..= addressLine1),
            Prelude.Just ("City" Data..= city),
            Prelude.Just ("CountryCode" Data..= countryCode),
            Prelude.Just ("FullName" Data..= fullName),
            Prelude.Just ("PhoneNumber" Data..= phoneNumber),
            Prelude.Just ("PostalCode" Data..= postalCode)
          ]
      )
