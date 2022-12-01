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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.ContactInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of the primary contact information associated with
-- an Amazon Web Services account.
--
-- /See:/ 'newContactInformation' smart constructor.
data ContactInformation = ContactInformation'
  { -- | The name of the company associated with the primary contact information,
    -- if any.
    companyName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The second line of the primary contact address, if any.
    addressLine2 :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The district or county of the primary contact address, if any.
    districtOrCounty :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The state or region of the primary contact address. This field is
    -- required in selected countries.
    stateOrRegion :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The third line of the primary contact address, if any.
    addressLine3 :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The URL of the website associated with the primary contact information,
    -- if any.
    websiteUrl :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The first line of the primary contact address.
    addressLine1 :: Core.Sensitive Prelude.Text,
    -- | The city of the primary contact address.
    city :: Core.Sensitive Prelude.Text,
    -- | The ISO-3166 two-letter country code for the primary contact address.
    countryCode :: Core.Sensitive Prelude.Text,
    -- | The full name of the primary contact address.
    fullName :: Core.Sensitive Prelude.Text,
    -- | The phone number of the primary contact information. The number will be
    -- validated and, in some countries, checked for activation.
    phoneNumber :: Core.Sensitive Prelude.Text,
    -- | The postal code of the primary contact address.
    postalCode :: Core.Sensitive Prelude.Text
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
-- 'companyName', 'contactInformation_companyName' - The name of the company associated with the primary contact information,
-- if any.
--
-- 'addressLine2', 'contactInformation_addressLine2' - The second line of the primary contact address, if any.
--
-- 'districtOrCounty', 'contactInformation_districtOrCounty' - The district or county of the primary contact address, if any.
--
-- 'stateOrRegion', 'contactInformation_stateOrRegion' - The state or region of the primary contact address. This field is
-- required in selected countries.
--
-- 'addressLine3', 'contactInformation_addressLine3' - The third line of the primary contact address, if any.
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
      { companyName = Prelude.Nothing,
        addressLine2 = Prelude.Nothing,
        districtOrCounty = Prelude.Nothing,
        stateOrRegion = Prelude.Nothing,
        addressLine3 = Prelude.Nothing,
        websiteUrl = Prelude.Nothing,
        addressLine1 = Core._Sensitive Lens.# pAddressLine1_,
        city = Core._Sensitive Lens.# pCity_,
        countryCode = Core._Sensitive Lens.# pCountryCode_,
        fullName = Core._Sensitive Lens.# pFullName_,
        phoneNumber = Core._Sensitive Lens.# pPhoneNumber_,
        postalCode = Core._Sensitive Lens.# pPostalCode_
      }

-- | The name of the company associated with the primary contact information,
-- if any.
contactInformation_companyName :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_companyName = Lens.lens (\ContactInformation' {companyName} -> companyName) (\s@ContactInformation' {} a -> s {companyName = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The second line of the primary contact address, if any.
contactInformation_addressLine2 :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_addressLine2 = Lens.lens (\ContactInformation' {addressLine2} -> addressLine2) (\s@ContactInformation' {} a -> s {addressLine2 = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The district or county of the primary contact address, if any.
contactInformation_districtOrCounty :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_districtOrCounty = Lens.lens (\ContactInformation' {districtOrCounty} -> districtOrCounty) (\s@ContactInformation' {} a -> s {districtOrCounty = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The state or region of the primary contact address. This field is
-- required in selected countries.
contactInformation_stateOrRegion :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_stateOrRegion = Lens.lens (\ContactInformation' {stateOrRegion} -> stateOrRegion) (\s@ContactInformation' {} a -> s {stateOrRegion = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The third line of the primary contact address, if any.
contactInformation_addressLine3 :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_addressLine3 = Lens.lens (\ContactInformation' {addressLine3} -> addressLine3) (\s@ContactInformation' {} a -> s {addressLine3 = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The URL of the website associated with the primary contact information,
-- if any.
contactInformation_websiteUrl :: Lens.Lens' ContactInformation (Prelude.Maybe Prelude.Text)
contactInformation_websiteUrl = Lens.lens (\ContactInformation' {websiteUrl} -> websiteUrl) (\s@ContactInformation' {} a -> s {websiteUrl = a} :: ContactInformation) Prelude.. Lens.mapping Core._Sensitive

-- | The first line of the primary contact address.
contactInformation_addressLine1 :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_addressLine1 = Lens.lens (\ContactInformation' {addressLine1} -> addressLine1) (\s@ContactInformation' {} a -> s {addressLine1 = a} :: ContactInformation) Prelude.. Core._Sensitive

-- | The city of the primary contact address.
contactInformation_city :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_city = Lens.lens (\ContactInformation' {city} -> city) (\s@ContactInformation' {} a -> s {city = a} :: ContactInformation) Prelude.. Core._Sensitive

-- | The ISO-3166 two-letter country code for the primary contact address.
contactInformation_countryCode :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_countryCode = Lens.lens (\ContactInformation' {countryCode} -> countryCode) (\s@ContactInformation' {} a -> s {countryCode = a} :: ContactInformation) Prelude.. Core._Sensitive

-- | The full name of the primary contact address.
contactInformation_fullName :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_fullName = Lens.lens (\ContactInformation' {fullName} -> fullName) (\s@ContactInformation' {} a -> s {fullName = a} :: ContactInformation) Prelude.. Core._Sensitive

-- | The phone number of the primary contact information. The number will be
-- validated and, in some countries, checked for activation.
contactInformation_phoneNumber :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_phoneNumber = Lens.lens (\ContactInformation' {phoneNumber} -> phoneNumber) (\s@ContactInformation' {} a -> s {phoneNumber = a} :: ContactInformation) Prelude.. Core._Sensitive

-- | The postal code of the primary contact address.
contactInformation_postalCode :: Lens.Lens' ContactInformation Prelude.Text
contactInformation_postalCode = Lens.lens (\ContactInformation' {postalCode} -> postalCode) (\s@ContactInformation' {} a -> s {postalCode = a} :: ContactInformation) Prelude.. Core._Sensitive

instance Core.FromJSON ContactInformation where
  parseJSON =
    Core.withObject
      "ContactInformation"
      ( \x ->
          ContactInformation'
            Prelude.<$> (x Core..:? "CompanyName")
            Prelude.<*> (x Core..:? "AddressLine2")
            Prelude.<*> (x Core..:? "DistrictOrCounty")
            Prelude.<*> (x Core..:? "StateOrRegion")
            Prelude.<*> (x Core..:? "AddressLine3")
            Prelude.<*> (x Core..:? "WebsiteUrl")
            Prelude.<*> (x Core..: "AddressLine1")
            Prelude.<*> (x Core..: "City")
            Prelude.<*> (x Core..: "CountryCode")
            Prelude.<*> (x Core..: "FullName")
            Prelude.<*> (x Core..: "PhoneNumber")
            Prelude.<*> (x Core..: "PostalCode")
      )

instance Prelude.Hashable ContactInformation where
  hashWithSalt _salt ContactInformation' {..} =
    _salt `Prelude.hashWithSalt` companyName
      `Prelude.hashWithSalt` addressLine2
      `Prelude.hashWithSalt` districtOrCounty
      `Prelude.hashWithSalt` stateOrRegion
      `Prelude.hashWithSalt` addressLine3
      `Prelude.hashWithSalt` websiteUrl
      `Prelude.hashWithSalt` addressLine1
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` fullName
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` postalCode

instance Prelude.NFData ContactInformation where
  rnf ContactInformation' {..} =
    Prelude.rnf companyName
      `Prelude.seq` Prelude.rnf addressLine2
      `Prelude.seq` Prelude.rnf districtOrCounty
      `Prelude.seq` Prelude.rnf stateOrRegion
      `Prelude.seq` Prelude.rnf addressLine3
      `Prelude.seq` Prelude.rnf websiteUrl
      `Prelude.seq` Prelude.rnf addressLine1
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf fullName
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf postalCode

instance Core.ToJSON ContactInformation where
  toJSON ContactInformation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CompanyName" Core..=) Prelude.<$> companyName,
            ("AddressLine2" Core..=) Prelude.<$> addressLine2,
            ("DistrictOrCounty" Core..=)
              Prelude.<$> districtOrCounty,
            ("StateOrRegion" Core..=) Prelude.<$> stateOrRegion,
            ("AddressLine3" Core..=) Prelude.<$> addressLine3,
            ("WebsiteUrl" Core..=) Prelude.<$> websiteUrl,
            Prelude.Just ("AddressLine1" Core..= addressLine1),
            Prelude.Just ("City" Core..= city),
            Prelude.Just ("CountryCode" Core..= countryCode),
            Prelude.Just ("FullName" Core..= fullName),
            Prelude.Just ("PhoneNumber" Core..= phoneNumber),
            Prelude.Just ("PostalCode" Core..= postalCode)
          ]
      )
