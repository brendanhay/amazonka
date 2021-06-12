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
-- Module      : Network.AWS.Route53Domains.Types.ContactDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.ContactDetail where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53Domains.Types.ContactType
import Network.AWS.Route53Domains.Types.CountryCode
import Network.AWS.Route53Domains.Types.ExtraParam

-- | ContactDetail includes the following elements.
--
-- /See:/ 'newContactDetail' smart constructor.
data ContactDetail = ContactDetail'
  { -- | The phone number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code>]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    phoneNumber :: Core.Maybe Core.Text,
    -- | Name of the organization for contact types other than @PERSON@.
    organizationName :: Core.Maybe Core.Text,
    -- | First line of the contact\'s address.
    addressLine1 :: Core.Maybe Core.Text,
    -- | A list of name-value pairs for parameters required by certain top-level
    -- domains.
    extraParams :: Core.Maybe [ExtraParam],
    -- | The zip or postal code of the contact\'s address.
    zipCode :: Core.Maybe Core.Text,
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
    -- -   For .es domains, if you specify @PERSON@, you must specify
    --     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
    contactType :: Core.Maybe ContactType,
    -- | The city of the contact\'s address.
    city :: Core.Maybe Core.Text,
    -- | The state or province of the contact\'s city.
    state :: Core.Maybe Core.Text,
    -- | Fax number of the contact.
    --
    -- Constraints: Phone number must be specified in the format \"+[country
    -- dialing code].[number including any area code]\". For example, a US
    -- phone number might appear as @\"+1.1234567890\"@.
    fax :: Core.Maybe Core.Text,
    -- | Email address of the contact.
    email :: Core.Maybe Core.Text,
    -- | Code for the country of the contact\'s address.
    countryCode :: Core.Maybe CountryCode,
    -- | First name of contact.
    firstName :: Core.Maybe Core.Text,
    -- | Last name of contact.
    lastName :: Core.Maybe Core.Text,
    -- | Second line of contact\'s address, if any.
    addressLine2 :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContactDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'contactDetail_phoneNumber' - The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- 'organizationName', 'contactDetail_organizationName' - Name of the organization for contact types other than @PERSON@.
--
-- 'addressLine1', 'contactDetail_addressLine1' - First line of the contact\'s address.
--
-- 'extraParams', 'contactDetail_extraParams' - A list of name-value pairs for parameters required by certain top-level
-- domains.
--
-- 'zipCode', 'contactDetail_zipCode' - The zip or postal code of the contact\'s address.
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
-- -   For .es domains, if you specify @PERSON@, you must specify
--     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
--
-- 'city', 'contactDetail_city' - The city of the contact\'s address.
--
-- 'state', 'contactDetail_state' - The state or province of the contact\'s city.
--
-- 'fax', 'contactDetail_fax' - Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- 'email', 'contactDetail_email' - Email address of the contact.
--
-- 'countryCode', 'contactDetail_countryCode' - Code for the country of the contact\'s address.
--
-- 'firstName', 'contactDetail_firstName' - First name of contact.
--
-- 'lastName', 'contactDetail_lastName' - Last name of contact.
--
-- 'addressLine2', 'contactDetail_addressLine2' - Second line of contact\'s address, if any.
newContactDetail ::
  ContactDetail
newContactDetail =
  ContactDetail'
    { phoneNumber = Core.Nothing,
      organizationName = Core.Nothing,
      addressLine1 = Core.Nothing,
      extraParams = Core.Nothing,
      zipCode = Core.Nothing,
      contactType = Core.Nothing,
      city = Core.Nothing,
      state = Core.Nothing,
      fax = Core.Nothing,
      email = Core.Nothing,
      countryCode = Core.Nothing,
      firstName = Core.Nothing,
      lastName = Core.Nothing,
      addressLine2 = Core.Nothing
    }

-- | The phone number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_phoneNumber :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_phoneNumber = Lens.lens (\ContactDetail' {phoneNumber} -> phoneNumber) (\s@ContactDetail' {} a -> s {phoneNumber = a} :: ContactDetail)

-- | Name of the organization for contact types other than @PERSON@.
contactDetail_organizationName :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_organizationName = Lens.lens (\ContactDetail' {organizationName} -> organizationName) (\s@ContactDetail' {} a -> s {organizationName = a} :: ContactDetail)

-- | First line of the contact\'s address.
contactDetail_addressLine1 :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_addressLine1 = Lens.lens (\ContactDetail' {addressLine1} -> addressLine1) (\s@ContactDetail' {} a -> s {addressLine1 = a} :: ContactDetail)

-- | A list of name-value pairs for parameters required by certain top-level
-- domains.
contactDetail_extraParams :: Lens.Lens' ContactDetail (Core.Maybe [ExtraParam])
contactDetail_extraParams = Lens.lens (\ContactDetail' {extraParams} -> extraParams) (\s@ContactDetail' {} a -> s {extraParams = a} :: ContactDetail) Core.. Lens.mapping Lens._Coerce

-- | The zip or postal code of the contact\'s address.
contactDetail_zipCode :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_zipCode = Lens.lens (\ContactDetail' {zipCode} -> zipCode) (\s@ContactDetail' {} a -> s {zipCode = a} :: ContactDetail)

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
-- -   For .es domains, if you specify @PERSON@, you must specify
--     @INDIVIDUAL@ for the value of @ES_LEGAL_FORM@.
contactDetail_contactType :: Lens.Lens' ContactDetail (Core.Maybe ContactType)
contactDetail_contactType = Lens.lens (\ContactDetail' {contactType} -> contactType) (\s@ContactDetail' {} a -> s {contactType = a} :: ContactDetail)

-- | The city of the contact\'s address.
contactDetail_city :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_city = Lens.lens (\ContactDetail' {city} -> city) (\s@ContactDetail' {} a -> s {city = a} :: ContactDetail)

-- | The state or province of the contact\'s city.
contactDetail_state :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_state = Lens.lens (\ContactDetail' {state} -> state) (\s@ContactDetail' {} a -> s {state = a} :: ContactDetail)

-- | Fax number of the contact.
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
contactDetail_fax :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_fax = Lens.lens (\ContactDetail' {fax} -> fax) (\s@ContactDetail' {} a -> s {fax = a} :: ContactDetail)

-- | Email address of the contact.
contactDetail_email :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_email = Lens.lens (\ContactDetail' {email} -> email) (\s@ContactDetail' {} a -> s {email = a} :: ContactDetail)

-- | Code for the country of the contact\'s address.
contactDetail_countryCode :: Lens.Lens' ContactDetail (Core.Maybe CountryCode)
contactDetail_countryCode = Lens.lens (\ContactDetail' {countryCode} -> countryCode) (\s@ContactDetail' {} a -> s {countryCode = a} :: ContactDetail)

-- | First name of contact.
contactDetail_firstName :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_firstName = Lens.lens (\ContactDetail' {firstName} -> firstName) (\s@ContactDetail' {} a -> s {firstName = a} :: ContactDetail)

-- | Last name of contact.
contactDetail_lastName :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_lastName = Lens.lens (\ContactDetail' {lastName} -> lastName) (\s@ContactDetail' {} a -> s {lastName = a} :: ContactDetail)

-- | Second line of contact\'s address, if any.
contactDetail_addressLine2 :: Lens.Lens' ContactDetail (Core.Maybe Core.Text)
contactDetail_addressLine2 = Lens.lens (\ContactDetail' {addressLine2} -> addressLine2) (\s@ContactDetail' {} a -> s {addressLine2 = a} :: ContactDetail)

instance Core.FromJSON ContactDetail where
  parseJSON =
    Core.withObject
      "ContactDetail"
      ( \x ->
          ContactDetail'
            Core.<$> (x Core..:? "PhoneNumber")
            Core.<*> (x Core..:? "OrganizationName")
            Core.<*> (x Core..:? "AddressLine1")
            Core.<*> (x Core..:? "ExtraParams" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ZipCode")
            Core.<*> (x Core..:? "ContactType")
            Core.<*> (x Core..:? "City")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "Fax")
            Core.<*> (x Core..:? "Email")
            Core.<*> (x Core..:? "CountryCode")
            Core.<*> (x Core..:? "FirstName")
            Core.<*> (x Core..:? "LastName")
            Core.<*> (x Core..:? "AddressLine2")
      )

instance Core.Hashable ContactDetail

instance Core.NFData ContactDetail

instance Core.ToJSON ContactDetail where
  toJSON ContactDetail' {..} =
    Core.object
      ( Core.catMaybes
          [ ("PhoneNumber" Core..=) Core.<$> phoneNumber,
            ("OrganizationName" Core..=)
              Core.<$> organizationName,
            ("AddressLine1" Core..=) Core.<$> addressLine1,
            ("ExtraParams" Core..=) Core.<$> extraParams,
            ("ZipCode" Core..=) Core.<$> zipCode,
            ("ContactType" Core..=) Core.<$> contactType,
            ("City" Core..=) Core.<$> city,
            ("State" Core..=) Core.<$> state,
            ("Fax" Core..=) Core.<$> fax,
            ("Email" Core..=) Core.<$> email,
            ("CountryCode" Core..=) Core.<$> countryCode,
            ("FirstName" Core..=) Core.<$> firstName,
            ("LastName" Core..=) Core.<$> lastName,
            ("AddressLine2" Core..=) Core.<$> addressLine2
          ]
      )
