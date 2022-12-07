{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CustomerProfiles.CreateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a standard profile.
--
-- A standard profile represents the following attributes for a customer
-- profile in a domain.
module Amazonka.CustomerProfiles.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_homePhoneNumber,
    createProfile_mailingAddress,
    createProfile_shippingAddress,
    createProfile_firstName,
    createProfile_businessPhoneNumber,
    createProfile_businessEmailAddress,
    createProfile_businessName,
    createProfile_personalEmailAddress,
    createProfile_billingAddress,
    createProfile_lastName,
    createProfile_birthDate,
    createProfile_address,
    createProfile_partyType,
    createProfile_gender,
    createProfile_mobilePhoneNumber,
    createProfile_middleName,
    createProfile_attributes,
    createProfile_phoneNumber,
    createProfile_additionalInformation,
    createProfile_emailAddress,
    createProfile_accountNumber,
    createProfile_domainName,

    -- * Destructuring the Response
    CreateProfileResponse (..),
    newCreateProfileResponse,

    -- * Response Lenses
    createProfileResponse_httpStatus,
    createProfileResponse_profileId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The customer’s home phone number.
    homePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s mailing address.
    mailingAddress :: Prelude.Maybe Address,
    -- | The customer’s shipping address.
    shippingAddress :: Prelude.Maybe Address,
    -- | The customer’s first name.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s business phone number.
    businessPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s business email address.
    businessEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the customer’s business.
    businessName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s personal email address.
    personalEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer’s billing address.
    billingAddress :: Prelude.Maybe Address,
    -- | The customer’s last name.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s birth date.
    birthDate :: Prelude.Maybe Prelude.Text,
    -- | A generic address associated with the customer that is not mailing,
    -- shipping, or billing.
    address :: Prelude.Maybe Address,
    -- | The type of profile used to describe the customer.
    partyType :: Prelude.Maybe PartyType,
    -- | The gender with which the customer identifies.
    gender :: Prelude.Maybe Gender,
    -- | The customer’s mobile phone number.
    mobilePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s middle name.
    middleName :: Prelude.Maybe Prelude.Text,
    -- | A key value pair of attributes of a customer profile.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The customer’s phone number, which has not been specified as a mobile,
    -- home, or business number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | Any additional information relevant to the customer’s profile.
    additionalInformation :: Prelude.Maybe Prelude.Text,
    -- | The customer’s email address, which has not been specified as a personal
    -- or business address.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | A unique account number that you have given to the customer.
    accountNumber :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homePhoneNumber', 'createProfile_homePhoneNumber' - The customer’s home phone number.
--
-- 'mailingAddress', 'createProfile_mailingAddress' - The customer’s mailing address.
--
-- 'shippingAddress', 'createProfile_shippingAddress' - The customer’s shipping address.
--
-- 'firstName', 'createProfile_firstName' - The customer’s first name.
--
-- 'businessPhoneNumber', 'createProfile_businessPhoneNumber' - The customer’s business phone number.
--
-- 'businessEmailAddress', 'createProfile_businessEmailAddress' - The customer’s business email address.
--
-- 'businessName', 'createProfile_businessName' - The name of the customer’s business.
--
-- 'personalEmailAddress', 'createProfile_personalEmailAddress' - The customer’s personal email address.
--
-- 'billingAddress', 'createProfile_billingAddress' - The customer’s billing address.
--
-- 'lastName', 'createProfile_lastName' - The customer’s last name.
--
-- 'birthDate', 'createProfile_birthDate' - The customer’s birth date.
--
-- 'address', 'createProfile_address' - A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- 'partyType', 'createProfile_partyType' - The type of profile used to describe the customer.
--
-- 'gender', 'createProfile_gender' - The gender with which the customer identifies.
--
-- 'mobilePhoneNumber', 'createProfile_mobilePhoneNumber' - The customer’s mobile phone number.
--
-- 'middleName', 'createProfile_middleName' - The customer’s middle name.
--
-- 'attributes', 'createProfile_attributes' - A key value pair of attributes of a customer profile.
--
-- 'phoneNumber', 'createProfile_phoneNumber' - The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
--
-- 'additionalInformation', 'createProfile_additionalInformation' - Any additional information relevant to the customer’s profile.
--
-- 'emailAddress', 'createProfile_emailAddress' - The customer’s email address, which has not been specified as a personal
-- or business address.
--
-- 'accountNumber', 'createProfile_accountNumber' - A unique account number that you have given to the customer.
--
-- 'domainName', 'createProfile_domainName' - The unique name of the domain.
newCreateProfile ::
  -- | 'domainName'
  Prelude.Text ->
  CreateProfile
newCreateProfile pDomainName_ =
  CreateProfile'
    { homePhoneNumber = Prelude.Nothing,
      mailingAddress = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      firstName = Prelude.Nothing,
      businessPhoneNumber = Prelude.Nothing,
      businessEmailAddress = Prelude.Nothing,
      businessName = Prelude.Nothing,
      personalEmailAddress = Prelude.Nothing,
      billingAddress = Prelude.Nothing,
      lastName = Prelude.Nothing,
      birthDate = Prelude.Nothing,
      address = Prelude.Nothing,
      partyType = Prelude.Nothing,
      gender = Prelude.Nothing,
      mobilePhoneNumber = Prelude.Nothing,
      middleName = Prelude.Nothing,
      attributes = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      additionalInformation = Prelude.Nothing,
      emailAddress = Prelude.Nothing,
      accountNumber = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The customer’s home phone number.
createProfile_homePhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_homePhoneNumber = Lens.lens (\CreateProfile' {homePhoneNumber} -> homePhoneNumber) (\s@CreateProfile' {} a -> s {homePhoneNumber = a} :: CreateProfile)

-- | The customer’s mailing address.
createProfile_mailingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_mailingAddress = Lens.lens (\CreateProfile' {mailingAddress} -> mailingAddress) (\s@CreateProfile' {} a -> s {mailingAddress = a} :: CreateProfile)

-- | The customer’s shipping address.
createProfile_shippingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_shippingAddress = Lens.lens (\CreateProfile' {shippingAddress} -> shippingAddress) (\s@CreateProfile' {} a -> s {shippingAddress = a} :: CreateProfile)

-- | The customer’s first name.
createProfile_firstName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_firstName = Lens.lens (\CreateProfile' {firstName} -> firstName) (\s@CreateProfile' {} a -> s {firstName = a} :: CreateProfile)

-- | The customer’s business phone number.
createProfile_businessPhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessPhoneNumber = Lens.lens (\CreateProfile' {businessPhoneNumber} -> businessPhoneNumber) (\s@CreateProfile' {} a -> s {businessPhoneNumber = a} :: CreateProfile)

-- | The customer’s business email address.
createProfile_businessEmailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessEmailAddress = Lens.lens (\CreateProfile' {businessEmailAddress} -> businessEmailAddress) (\s@CreateProfile' {} a -> s {businessEmailAddress = a} :: CreateProfile)

-- | The name of the customer’s business.
createProfile_businessName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessName = Lens.lens (\CreateProfile' {businessName} -> businessName) (\s@CreateProfile' {} a -> s {businessName = a} :: CreateProfile)

-- | The customer’s personal email address.
createProfile_personalEmailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_personalEmailAddress = Lens.lens (\CreateProfile' {personalEmailAddress} -> personalEmailAddress) (\s@CreateProfile' {} a -> s {personalEmailAddress = a} :: CreateProfile)

-- | The customer’s billing address.
createProfile_billingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_billingAddress = Lens.lens (\CreateProfile' {billingAddress} -> billingAddress) (\s@CreateProfile' {} a -> s {billingAddress = a} :: CreateProfile)

-- | The customer’s last name.
createProfile_lastName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_lastName = Lens.lens (\CreateProfile' {lastName} -> lastName) (\s@CreateProfile' {} a -> s {lastName = a} :: CreateProfile)

-- | The customer’s birth date.
createProfile_birthDate :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_birthDate = Lens.lens (\CreateProfile' {birthDate} -> birthDate) (\s@CreateProfile' {} a -> s {birthDate = a} :: CreateProfile)

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
createProfile_address :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_address = Lens.lens (\CreateProfile' {address} -> address) (\s@CreateProfile' {} a -> s {address = a} :: CreateProfile)

-- | The type of profile used to describe the customer.
createProfile_partyType :: Lens.Lens' CreateProfile (Prelude.Maybe PartyType)
createProfile_partyType = Lens.lens (\CreateProfile' {partyType} -> partyType) (\s@CreateProfile' {} a -> s {partyType = a} :: CreateProfile)

-- | The gender with which the customer identifies.
createProfile_gender :: Lens.Lens' CreateProfile (Prelude.Maybe Gender)
createProfile_gender = Lens.lens (\CreateProfile' {gender} -> gender) (\s@CreateProfile' {} a -> s {gender = a} :: CreateProfile)

-- | The customer’s mobile phone number.
createProfile_mobilePhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_mobilePhoneNumber = Lens.lens (\CreateProfile' {mobilePhoneNumber} -> mobilePhoneNumber) (\s@CreateProfile' {} a -> s {mobilePhoneNumber = a} :: CreateProfile)

-- | The customer’s middle name.
createProfile_middleName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_middleName = Lens.lens (\CreateProfile' {middleName} -> middleName) (\s@CreateProfile' {} a -> s {middleName = a} :: CreateProfile)

-- | A key value pair of attributes of a customer profile.
createProfile_attributes :: Lens.Lens' CreateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProfile_attributes = Lens.lens (\CreateProfile' {attributes} -> attributes) (\s@CreateProfile' {} a -> s {attributes = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
createProfile_phoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_phoneNumber = Lens.lens (\CreateProfile' {phoneNumber} -> phoneNumber) (\s@CreateProfile' {} a -> s {phoneNumber = a} :: CreateProfile)

-- | Any additional information relevant to the customer’s profile.
createProfile_additionalInformation :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_additionalInformation = Lens.lens (\CreateProfile' {additionalInformation} -> additionalInformation) (\s@CreateProfile' {} a -> s {additionalInformation = a} :: CreateProfile)

-- | The customer’s email address, which has not been specified as a personal
-- or business address.
createProfile_emailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_emailAddress = Lens.lens (\CreateProfile' {emailAddress} -> emailAddress) (\s@CreateProfile' {} a -> s {emailAddress = a} :: CreateProfile)

-- | A unique account number that you have given to the customer.
createProfile_accountNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_accountNumber = Lens.lens (\CreateProfile' {accountNumber} -> accountNumber) (\s@CreateProfile' {} a -> s {accountNumber = a} :: CreateProfile)

-- | The unique name of the domain.
createProfile_domainName :: Lens.Lens' CreateProfile Prelude.Text
createProfile_domainName = Lens.lens (\CreateProfile' {domainName} -> domainName) (\s@CreateProfile' {} a -> s {domainName = a} :: CreateProfile)

instance Core.AWSRequest CreateProfile where
  type
    AWSResponse CreateProfile =
      CreateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProfileId")
      )

instance Prelude.Hashable CreateProfile where
  hashWithSalt _salt CreateProfile' {..} =
    _salt `Prelude.hashWithSalt` homePhoneNumber
      `Prelude.hashWithSalt` mailingAddress
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` firstName
      `Prelude.hashWithSalt` businessPhoneNumber
      `Prelude.hashWithSalt` businessEmailAddress
      `Prelude.hashWithSalt` businessName
      `Prelude.hashWithSalt` personalEmailAddress
      `Prelude.hashWithSalt` billingAddress
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` birthDate
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` partyType
      `Prelude.hashWithSalt` gender
      `Prelude.hashWithSalt` mobilePhoneNumber
      `Prelude.hashWithSalt` middleName
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` additionalInformation
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` accountNumber
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
    Prelude.rnf homePhoneNumber
      `Prelude.seq` Prelude.rnf mailingAddress
      `Prelude.seq` Prelude.rnf shippingAddress
      `Prelude.seq` Prelude.rnf firstName
      `Prelude.seq` Prelude.rnf businessPhoneNumber
      `Prelude.seq` Prelude.rnf businessEmailAddress
      `Prelude.seq` Prelude.rnf businessName
      `Prelude.seq` Prelude.rnf personalEmailAddress
      `Prelude.seq` Prelude.rnf billingAddress
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf birthDate
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf partyType
      `Prelude.seq` Prelude.rnf gender
      `Prelude.seq` Prelude.rnf mobilePhoneNumber
      `Prelude.seq` Prelude.rnf middleName
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf
        additionalInformation
      `Prelude.seq` Prelude.rnf emailAddress
      `Prelude.seq` Prelude.rnf accountNumber
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HomePhoneNumber" Data..=)
              Prelude.<$> homePhoneNumber,
            ("MailingAddress" Data..=)
              Prelude.<$> mailingAddress,
            ("ShippingAddress" Data..=)
              Prelude.<$> shippingAddress,
            ("FirstName" Data..=) Prelude.<$> firstName,
            ("BusinessPhoneNumber" Data..=)
              Prelude.<$> businessPhoneNumber,
            ("BusinessEmailAddress" Data..=)
              Prelude.<$> businessEmailAddress,
            ("BusinessName" Data..=) Prelude.<$> businessName,
            ("PersonalEmailAddress" Data..=)
              Prelude.<$> personalEmailAddress,
            ("BillingAddress" Data..=)
              Prelude.<$> billingAddress,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("BirthDate" Data..=) Prelude.<$> birthDate,
            ("Address" Data..=) Prelude.<$> address,
            ("PartyType" Data..=) Prelude.<$> partyType,
            ("Gender" Data..=) Prelude.<$> gender,
            ("MobilePhoneNumber" Data..=)
              Prelude.<$> mobilePhoneNumber,
            ("MiddleName" Data..=) Prelude.<$> middleName,
            ("Attributes" Data..=) Prelude.<$> attributes,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("AdditionalInformation" Data..=)
              Prelude.<$> additionalInformation,
            ("EmailAddress" Data..=) Prelude.<$> emailAddress,
            ("AccountNumber" Data..=) Prelude.<$> accountNumber
          ]
      )

instance Data.ToPath CreateProfile where
  toPath CreateProfile' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/profiles"]

instance Data.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createProfileResponse_httpStatus' - The response's http status code.
--
-- 'profileId', 'createProfileResponse_profileId' - The unique identifier of a customer profile.
newCreateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profileId'
  Prelude.Text ->
  CreateProfileResponse
newCreateProfileResponse pHttpStatus_ pProfileId_ =
  CreateProfileResponse'
    { httpStatus = pHttpStatus_,
      profileId = pProfileId_
    }

-- | The response's http status code.
createProfileResponse_httpStatus :: Lens.Lens' CreateProfileResponse Prelude.Int
createProfileResponse_httpStatus = Lens.lens (\CreateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateProfileResponse' {} a -> s {httpStatus = a} :: CreateProfileResponse)

-- | The unique identifier of a customer profile.
createProfileResponse_profileId :: Lens.Lens' CreateProfileResponse Prelude.Text
createProfileResponse_profileId = Lens.lens (\CreateProfileResponse' {profileId} -> profileId) (\s@CreateProfileResponse' {} a -> s {profileId = a} :: CreateProfileResponse)

instance Prelude.NFData CreateProfileResponse where
  rnf CreateProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profileId
