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
-- Module      : Amazonka.CustomerProfiles.Types.FieldSourceProfileIds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.FieldSourceProfileIds where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A duplicate customer profile that is to be merged into a main profile.
--
-- /See:/ 'newFieldSourceProfileIds' smart constructor.
data FieldSourceProfileIds = FieldSourceProfileIds'
  { -- | A unique identifier for the account number field to be merged.
    accountNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the additional information field to be merged.
    additionalInformation :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the party type field to be merged.
    address :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the attributes field to be merged.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique identifier for the billing type field to be merged.
    billingAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the birthdate field to be merged.
    birthDate :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the party type field to be merged.
    businessEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the business name field to be merged.
    businessName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the business phone number field to be merged.
    businessPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the email address field to be merged.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the first name field to be merged.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the gender field to be merged.
    gender :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the home phone number field to be merged.
    homePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the last name field to be merged.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the mailing address field to be merged.
    mailingAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the middle name field to be merged.
    middleName :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the mobile phone number field to be merged.
    mobilePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the party type field to be merged.
    partyType :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the personal email address field to be merged.
    personalEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number field to be merged.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the shipping address field to be merged.
    shippingAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldSourceProfileIds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountNumber', 'fieldSourceProfileIds_accountNumber' - A unique identifier for the account number field to be merged.
--
-- 'additionalInformation', 'fieldSourceProfileIds_additionalInformation' - A unique identifier for the additional information field to be merged.
--
-- 'address', 'fieldSourceProfileIds_address' - A unique identifier for the party type field to be merged.
--
-- 'attributes', 'fieldSourceProfileIds_attributes' - A unique identifier for the attributes field to be merged.
--
-- 'billingAddress', 'fieldSourceProfileIds_billingAddress' - A unique identifier for the billing type field to be merged.
--
-- 'birthDate', 'fieldSourceProfileIds_birthDate' - A unique identifier for the birthdate field to be merged.
--
-- 'businessEmailAddress', 'fieldSourceProfileIds_businessEmailAddress' - A unique identifier for the party type field to be merged.
--
-- 'businessName', 'fieldSourceProfileIds_businessName' - A unique identifier for the business name field to be merged.
--
-- 'businessPhoneNumber', 'fieldSourceProfileIds_businessPhoneNumber' - A unique identifier for the business phone number field to be merged.
--
-- 'emailAddress', 'fieldSourceProfileIds_emailAddress' - A unique identifier for the email address field to be merged.
--
-- 'firstName', 'fieldSourceProfileIds_firstName' - A unique identifier for the first name field to be merged.
--
-- 'gender', 'fieldSourceProfileIds_gender' - A unique identifier for the gender field to be merged.
--
-- 'homePhoneNumber', 'fieldSourceProfileIds_homePhoneNumber' - A unique identifier for the home phone number field to be merged.
--
-- 'lastName', 'fieldSourceProfileIds_lastName' - A unique identifier for the last name field to be merged.
--
-- 'mailingAddress', 'fieldSourceProfileIds_mailingAddress' - A unique identifier for the mailing address field to be merged.
--
-- 'middleName', 'fieldSourceProfileIds_middleName' - A unique identifier for the middle name field to be merged.
--
-- 'mobilePhoneNumber', 'fieldSourceProfileIds_mobilePhoneNumber' - A unique identifier for the mobile phone number field to be merged.
--
-- 'partyType', 'fieldSourceProfileIds_partyType' - A unique identifier for the party type field to be merged.
--
-- 'personalEmailAddress', 'fieldSourceProfileIds_personalEmailAddress' - A unique identifier for the personal email address field to be merged.
--
-- 'phoneNumber', 'fieldSourceProfileIds_phoneNumber' - A unique identifier for the phone number field to be merged.
--
-- 'shippingAddress', 'fieldSourceProfileIds_shippingAddress' - A unique identifier for the shipping address field to be merged.
newFieldSourceProfileIds ::
  FieldSourceProfileIds
newFieldSourceProfileIds =
  FieldSourceProfileIds'
    { accountNumber =
        Prelude.Nothing,
      additionalInformation = Prelude.Nothing,
      address = Prelude.Nothing,
      attributes = Prelude.Nothing,
      billingAddress = Prelude.Nothing,
      birthDate = Prelude.Nothing,
      businessEmailAddress = Prelude.Nothing,
      businessName = Prelude.Nothing,
      businessPhoneNumber = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      firstName = Prelude.Nothing,
      gender = Prelude.Nothing,
      homePhoneNumber = Prelude.Nothing,
      lastName = Prelude.Nothing,
      mailingAddress = Prelude.Nothing,
      middleName = Prelude.Nothing,
      mobilePhoneNumber = Prelude.Nothing,
      partyType = Prelude.Nothing,
      personalEmailAddress = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      shippingAddress = Prelude.Nothing
    }

-- | A unique identifier for the account number field to be merged.
fieldSourceProfileIds_accountNumber :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_accountNumber = Lens.lens (\FieldSourceProfileIds' {accountNumber} -> accountNumber) (\s@FieldSourceProfileIds' {} a -> s {accountNumber = a} :: FieldSourceProfileIds)

-- | A unique identifier for the additional information field to be merged.
fieldSourceProfileIds_additionalInformation :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_additionalInformation = Lens.lens (\FieldSourceProfileIds' {additionalInformation} -> additionalInformation) (\s@FieldSourceProfileIds' {} a -> s {additionalInformation = a} :: FieldSourceProfileIds)

-- | A unique identifier for the party type field to be merged.
fieldSourceProfileIds_address :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_address = Lens.lens (\FieldSourceProfileIds' {address} -> address) (\s@FieldSourceProfileIds' {} a -> s {address = a} :: FieldSourceProfileIds)

-- | A unique identifier for the attributes field to be merged.
fieldSourceProfileIds_attributes :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
fieldSourceProfileIds_attributes = Lens.lens (\FieldSourceProfileIds' {attributes} -> attributes) (\s@FieldSourceProfileIds' {} a -> s {attributes = a} :: FieldSourceProfileIds) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier for the billing type field to be merged.
fieldSourceProfileIds_billingAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_billingAddress = Lens.lens (\FieldSourceProfileIds' {billingAddress} -> billingAddress) (\s@FieldSourceProfileIds' {} a -> s {billingAddress = a} :: FieldSourceProfileIds)

-- | A unique identifier for the birthdate field to be merged.
fieldSourceProfileIds_birthDate :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_birthDate = Lens.lens (\FieldSourceProfileIds' {birthDate} -> birthDate) (\s@FieldSourceProfileIds' {} a -> s {birthDate = a} :: FieldSourceProfileIds)

-- | A unique identifier for the party type field to be merged.
fieldSourceProfileIds_businessEmailAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_businessEmailAddress = Lens.lens (\FieldSourceProfileIds' {businessEmailAddress} -> businessEmailAddress) (\s@FieldSourceProfileIds' {} a -> s {businessEmailAddress = a} :: FieldSourceProfileIds)

-- | A unique identifier for the business name field to be merged.
fieldSourceProfileIds_businessName :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_businessName = Lens.lens (\FieldSourceProfileIds' {businessName} -> businessName) (\s@FieldSourceProfileIds' {} a -> s {businessName = a} :: FieldSourceProfileIds)

-- | A unique identifier for the business phone number field to be merged.
fieldSourceProfileIds_businessPhoneNumber :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_businessPhoneNumber = Lens.lens (\FieldSourceProfileIds' {businessPhoneNumber} -> businessPhoneNumber) (\s@FieldSourceProfileIds' {} a -> s {businessPhoneNumber = a} :: FieldSourceProfileIds)

-- | A unique identifier for the email address field to be merged.
fieldSourceProfileIds_emailAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_emailAddress = Lens.lens (\FieldSourceProfileIds' {emailAddress} -> emailAddress) (\s@FieldSourceProfileIds' {} a -> s {emailAddress = a} :: FieldSourceProfileIds)

-- | A unique identifier for the first name field to be merged.
fieldSourceProfileIds_firstName :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_firstName = Lens.lens (\FieldSourceProfileIds' {firstName} -> firstName) (\s@FieldSourceProfileIds' {} a -> s {firstName = a} :: FieldSourceProfileIds)

-- | A unique identifier for the gender field to be merged.
fieldSourceProfileIds_gender :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_gender = Lens.lens (\FieldSourceProfileIds' {gender} -> gender) (\s@FieldSourceProfileIds' {} a -> s {gender = a} :: FieldSourceProfileIds)

-- | A unique identifier for the home phone number field to be merged.
fieldSourceProfileIds_homePhoneNumber :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_homePhoneNumber = Lens.lens (\FieldSourceProfileIds' {homePhoneNumber} -> homePhoneNumber) (\s@FieldSourceProfileIds' {} a -> s {homePhoneNumber = a} :: FieldSourceProfileIds)

-- | A unique identifier for the last name field to be merged.
fieldSourceProfileIds_lastName :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_lastName = Lens.lens (\FieldSourceProfileIds' {lastName} -> lastName) (\s@FieldSourceProfileIds' {} a -> s {lastName = a} :: FieldSourceProfileIds)

-- | A unique identifier for the mailing address field to be merged.
fieldSourceProfileIds_mailingAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_mailingAddress = Lens.lens (\FieldSourceProfileIds' {mailingAddress} -> mailingAddress) (\s@FieldSourceProfileIds' {} a -> s {mailingAddress = a} :: FieldSourceProfileIds)

-- | A unique identifier for the middle name field to be merged.
fieldSourceProfileIds_middleName :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_middleName = Lens.lens (\FieldSourceProfileIds' {middleName} -> middleName) (\s@FieldSourceProfileIds' {} a -> s {middleName = a} :: FieldSourceProfileIds)

-- | A unique identifier for the mobile phone number field to be merged.
fieldSourceProfileIds_mobilePhoneNumber :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_mobilePhoneNumber = Lens.lens (\FieldSourceProfileIds' {mobilePhoneNumber} -> mobilePhoneNumber) (\s@FieldSourceProfileIds' {} a -> s {mobilePhoneNumber = a} :: FieldSourceProfileIds)

-- | A unique identifier for the party type field to be merged.
fieldSourceProfileIds_partyType :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_partyType = Lens.lens (\FieldSourceProfileIds' {partyType} -> partyType) (\s@FieldSourceProfileIds' {} a -> s {partyType = a} :: FieldSourceProfileIds)

-- | A unique identifier for the personal email address field to be merged.
fieldSourceProfileIds_personalEmailAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_personalEmailAddress = Lens.lens (\FieldSourceProfileIds' {personalEmailAddress} -> personalEmailAddress) (\s@FieldSourceProfileIds' {} a -> s {personalEmailAddress = a} :: FieldSourceProfileIds)

-- | A unique identifier for the phone number field to be merged.
fieldSourceProfileIds_phoneNumber :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_phoneNumber = Lens.lens (\FieldSourceProfileIds' {phoneNumber} -> phoneNumber) (\s@FieldSourceProfileIds' {} a -> s {phoneNumber = a} :: FieldSourceProfileIds)

-- | A unique identifier for the shipping address field to be merged.
fieldSourceProfileIds_shippingAddress :: Lens.Lens' FieldSourceProfileIds (Prelude.Maybe Prelude.Text)
fieldSourceProfileIds_shippingAddress = Lens.lens (\FieldSourceProfileIds' {shippingAddress} -> shippingAddress) (\s@FieldSourceProfileIds' {} a -> s {shippingAddress = a} :: FieldSourceProfileIds)

instance Prelude.Hashable FieldSourceProfileIds where
  hashWithSalt _salt FieldSourceProfileIds' {..} =
    _salt `Prelude.hashWithSalt` accountNumber
      `Prelude.hashWithSalt` additionalInformation
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` billingAddress
      `Prelude.hashWithSalt` birthDate
      `Prelude.hashWithSalt` businessEmailAddress
      `Prelude.hashWithSalt` businessName
      `Prelude.hashWithSalt` businessPhoneNumber
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` gender
      `Prelude.hashWithSalt` homePhoneNumber
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` mailingAddress
      `Prelude.hashWithSalt` middleName
      `Prelude.hashWithSalt` mobilePhoneNumber
      `Prelude.hashWithSalt` partyType
      `Prelude.hashWithSalt` personalEmailAddress
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` shippingAddress

instance Prelude.NFData FieldSourceProfileIds where
  rnf FieldSourceProfileIds' {..} =
    Prelude.rnf accountNumber
      `Prelude.seq` Prelude.rnf additionalInformation
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf billingAddress
      `Prelude.seq` Prelude.rnf birthDate
      `Prelude.seq` Prelude.rnf businessEmailAddress
      `Prelude.seq` Prelude.rnf businessName
      `Prelude.seq` Prelude.rnf businessPhoneNumber
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf gender
      `Prelude.seq` Prelude.rnf homePhoneNumber
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf mailingAddress
      `Prelude.seq` Prelude.rnf middleName
      `Prelude.seq` Prelude.rnf mobilePhoneNumber
      `Prelude.seq` Prelude.rnf partyType
      `Prelude.seq` Prelude.rnf personalEmailAddress
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf shippingAddress

instance Data.ToJSON FieldSourceProfileIds where
  toJSON FieldSourceProfileIds' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountNumber" Data..=) Prelude.<$> accountNumber,
            ("AdditionalInformation" Data..=)
              Prelude.<$> additionalInformation,
            ("Address" Data..=) Prelude.<$> address,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("BillingAddress" Data..=)
              Prelude.<$> billingAddress,
            ("BirthDate" Data..=) Prelude.<$> birthDate,
            ("BusinessEmailAddress" Data..=)
              Prelude.<$> businessEmailAddress,
            ("BusinessName" Data..=) Prelude.<$> businessName,
            ("BusinessPhoneNumber" Data..=)
              Prelude.<$> businessPhoneNumber,
            ("EmailAddress" Data..=) Prelude.<$> emailAddress,
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("Gender" Data..=) Prelude.<$> gender,
            ("HomePhoneNumber" Data..=)
              Prelude.<$> homePhoneNumber,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("MailingAddress" Data..=)
              Prelude.<$> mailingAddress,
            ("MiddleName" Data..=) Prelude.<$> middleName,
            ("MobilePhoneNumber" Data..=)
              Prelude.<$> mobilePhoneNumber,
            ("PartyType" Data..=) Prelude.<$> partyType,
            ("PersonalEmailAddress" Data..=)
              Prelude.<$> personalEmailAddress,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("ShippingAddress" Data..=)
              Prelude.<$> shippingAddress
          ]
      )
