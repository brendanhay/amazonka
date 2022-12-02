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
-- Module      : Amazonka.Route53Domains.Types.ContactDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Domains.Types.ContactDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Domains.Types.ContactType
import Amazonka.Route53Domains.Types.CountryCode
import Amazonka.Route53Domains.Types.ExtraParam

-- | ContactDetail includes the following elements.
--
-- /See:/ 'newContactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { -- | The zip or postal code of the contact\'s address.
    zipCode :: Prelude.Maybe Prelude.Text,
    -- | First name of contact.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | Email address of the contact.
    email :: Prelude.Maybe Prelude.Text,
    -- | A list of name-value pairs for parameters required by certain top-level
    -- domains.
    extraParams :: Prelude.Maybe [ExtraParam],
    -- | Second line of contact\'s address, if any.
    addressLine2 :: Prelude.Maybe Prelude.Text,
    -- | Name of the organization for contact types other than @PERSON@.
    organizationName :: Prelude.Maybe Prelude.Text,
    -- | Code for the country of the contact\'s address.
    countryCode :: Prelude.Maybe CountryCode,
    -- | The state or province of the contact\'s city.
    state :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the contact is a person, company, association, or
    -- public organization. Note the following:
    --
    -- -   If you specify a value other than @PERSON@, you must also specify a
    --     value for @OrganizationName@.
    --
    -- -   For some TLDs, the privacy protection available depends on the value
    --     that you specify for @Contact Type@. For the privacy protection
    --     settings for your TLD, see
    --     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
    --     in the /Amazon Route 53 Developer Guide/
    --
    -- -   For .es domains, the value of @ContactType@ must be @PERSON@ for all
    --     three contacts.
    contactType :: Prelude.Maybe ContactType,
    -- | Last name of contact.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | First line of the contact\'s address.
    addressLine1 :: Prelude.Maybe Prelude.Text,
    -- | The city of the contact\'s address.
    city :: Prelude.Maybe Prelude.Text,
    -- | Fax number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    fax :: Prelude.Maybe Prelude.Text,
    -- | The phone number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code>]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    phoneNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'zipCode', 'contactDetail_zipCode' - The zip or postal code of the contact\'s address.
--
-- 'firstName', 'contactDetail_firstName' - First name of contact.
--
-- 'email', 'contactDetail_email' - Email address of the contact.
--
-- 'extraParams', 'contactDetail_extraParams' - A list of name-value pairs for parameters required by certain top-level
-- domains.
--
-- 'addressLine2', 'contactDetail_addressLine2' - Second line of contact\'s address, if any.
--
-- 'organizationName', 'contactDetail_organizationName' - Name of the organization for contact types other than @PERSON@.
--
-- 'countryCode', 'contactDetail_countryCode' - Code for the country of the contact\'s address.
--
-- 'state', 'contactDetail_state' - The state or province of the contact\'s city.
--
-- 'contactType', 'contactDetail_contactType' - Indicates whether the contact is a person, company, association, or
-- public organization. Note the following:
--
-- -   If you specify a value other than @PERSON@, you must also specify a
--     value for @OrganizationName@.
--
-- -   For some TLDs, the privacy protection available depends on the value
--     that you specify for @Contact Type@. For the privacy protection
--     settings for your TLD, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/
--
-- -   For .es domains, the value of @ContactType@ must be @PERSON@ for all
--     three contacts.
--
-- 'lastName', 'contactDetail_lastName' - Last name of contact.
--
-- 'addressLine1', 'contactDetail_addressLine1' - First line of the contact\'s address.
--
-- 'city', 'contactDetail_city' - The city of the contact\'s address.
--
-- 'fax', 'contactDetail_fax' - Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- 'phoneNumber', 'contactDetail_phoneNumber' - The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
newContactDetail ::
  ContactDetail
newContactDetail =
  ContactDetail'
    { zipCode = Prelude.Nothing,
      firstName = Prelude.Nothing,
      email = Prelude.Nothing,
      extraParams = Prelude.Nothing,
      addressLine2 = Prelude.Nothing,
      organizationName = Prelude.Nothing,
      countryCode = Prelude.Nothing,
      state = Prelude.Nothing,
      contactType = Prelude.Nothing,
      lastName = Prelude.Nothing,
      addressLine1 = Prelude.Nothing,
      city = Prelude.Nothing,
      fax = Prelude.Nothing,
      phoneNumber = Prelude.Nothing
    }

-- | The zip or postal code of the contact\'s address.
contactDetail_zipCode :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_zipCode = Lens.lens (\ContactDetail' {zipCode} -> zipCode) (\s@ContactDetail' {} a -> s {zipCode = a} :: ContactDetail)

-- | First name of contact.
contactDetail_firstName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_firstName = Lens.lens (\ContactDetail' {firstName} -> firstName) (\s@ContactDetail' {} a -> s {firstName = a} :: ContactDetail)

-- | Email address of the contact.
contactDetail_email :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_email = Lens.lens (\ContactDetail' {email} -> email) (\s@ContactDetail' {} a -> s {email = a} :: ContactDetail)

-- | A list of name-value pairs for parameters required by certain top-level
-- domains.
contactDetail_extraParams :: Lens.Lens' ContactDetail (Prelude.Maybe [ExtraParam])
contactDetail_extraParams = Lens.lens (\ContactDetail' {extraParams} -> extraParams) (\s@ContactDetail' {} a -> s {extraParams = a} :: ContactDetail) Prelude.. Lens.mapping Lens.coerced

-- | Second line of contact\'s address, if any.
contactDetail_addressLine2 :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_addressLine2 = Lens.lens (\ContactDetail' {addressLine2} -> addressLine2) (\s@ContactDetail' {} a -> s {addressLine2 = a} :: ContactDetail)

-- | Name of the organization for contact types other than @PERSON@.
contactDetail_organizationName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_organizationName = Lens.lens (\ContactDetail' {organizationName} -> organizationName) (\s@ContactDetail' {} a -> s {organizationName = a} :: ContactDetail)

-- | Code for the country of the contact\'s address.
contactDetail_countryCode :: Lens.Lens' ContactDetail (Prelude.Maybe CountryCode)
contactDetail_countryCode = Lens.lens (\ContactDetail' {countryCode} -> countryCode) (\s@ContactDetail' {} a -> s {countryCode = a} :: ContactDetail)

-- | The state or province of the contact\'s city.
contactDetail_state :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_state = Lens.lens (\ContactDetail' {state} -> state) (\s@ContactDetail' {} a -> s {state = a} :: ContactDetail)

-- | Indicates whether the contact is a person, company, association, or
-- public organization. Note the following:
--
-- -   If you specify a value other than @PERSON@, you must also specify a
--     value for @OrganizationName@.
--
-- -   For some TLDs, the privacy protection available depends on the value
--     that you specify for @Contact Type@. For the privacy protection
--     settings for your TLD, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains that You Can Register with Amazon Route 53>
--     in the /Amazon Route 53 Developer Guide/
--
-- -   For .es domains, the value of @ContactType@ must be @PERSON@ for all
--     three contacts.
contactDetail_contactType :: Lens.Lens' ContactDetail (Prelude.Maybe ContactType)
contactDetail_contactType = Lens.lens (\ContactDetail' {contactType} -> contactType) (\s@ContactDetail' {} a -> s {contactType = a} :: ContactDetail)

-- | Last name of contact.
contactDetail_lastName :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_lastName = Lens.lens (\ContactDetail' {lastName} -> lastName) (\s@ContactDetail' {} a -> s {lastName = a} :: ContactDetail)

-- | First line of the contact\'s address.
contactDetail_addressLine1 :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_addressLine1 = Lens.lens (\ContactDetail' {addressLine1} -> addressLine1) (\s@ContactDetail' {} a -> s {addressLine1 = a} :: ContactDetail)

-- | The city of the contact\'s address.
contactDetail_city :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_city = Lens.lens (\ContactDetail' {city} -> city) (\s@ContactDetail' {} a -> s {city = a} :: ContactDetail)

-- | Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_fax :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_fax = Lens.lens (\ContactDetail' {fax} -> fax) (\s@ContactDetail' {} a -> s {fax = a} :: ContactDetail)

-- | The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_phoneNumber :: Lens.Lens' ContactDetail (Prelude.Maybe Prelude.Text)
contactDetail_phoneNumber = Lens.lens (\ContactDetail' {phoneNumber} -> phoneNumber) (\s@ContactDetail' {} a -> s {phoneNumber = a} :: ContactDetail)

instance Data.FromJSON ContactDetail where
  parseJSON =
    Data.withObject
      "ContactDetail"
      ( \x ->
          ContactDetail'
            Prelude.<$> (x Data..:? "ZipCode")
            Prelude.<*> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "ExtraParams" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "AddressLine2")
            Prelude.<*> (x Data..:? "OrganizationName")
            Prelude.<*> (x Data..:? "CountryCode")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "ContactType")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "AddressLine1")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "Fax")
            Prelude.<*> (x Data..:? "PhoneNumber")
      )

instance Prelude.Hashable ContactDetail where
  hashWithSalt _salt ContactDetail' {..} =
    _salt `Prelude.hashWithSalt` zipCode
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` extraParams
      `Prelude.hashWithSalt` addressLine2
      `Prelude.hashWithSalt` organizationName
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` contactType
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` addressLine1
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` fax
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData ContactDetail where
  rnf ContactDetail' {..} =
    Prelude.rnf zipCode
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf extraParams
      `Prelude.seq` Prelude.rnf addressLine2
      `Prelude.seq` Prelude.rnf organizationName
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf contactType
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf addressLine1
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf fax
      `Prelude.seq` Prelude.rnf phoneNumber

instance Data.ToJSON ContactDetail where
  toJSON ContactDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ZipCode" Data..=) Prelude.<$> zipCode,
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("Email" Data..=) Prelude.<$> email,
            ("ExtraParams" Data..=) Prelude.<$> extraParams,
            ("AddressLine2" Data..=) Prelude.<$> addressLine2,
            ("OrganizationName" Data..=)
              Prelude.<$> organizationName,
            ("CountryCode" Data..=) Prelude.<$> countryCode,
            ("State" Data..=) Prelude.<$> state,
            ("ContactType" Data..=) Prelude.<$> contactType,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("AddressLine1" Data..=) Prelude.<$> addressLine1,
            ("City" Data..=) Prelude.<$> city,
            ("Fax" Data..=) Prelude.<$> fax,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber
          ]
      )
