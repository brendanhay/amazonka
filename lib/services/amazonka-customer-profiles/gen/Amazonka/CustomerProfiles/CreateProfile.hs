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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createProfile_accountNumber,
    createProfile_additionalInformation,
    createProfile_address,
    createProfile_attributes,
    createProfile_billingAddress,
    createProfile_birthDate,
    createProfile_businessEmailAddress,
    createProfile_businessName,
    createProfile_businessPhoneNumber,
    createProfile_emailAddress,
    createProfile_firstName,
    createProfile_gender,
    createProfile_genderString,
    createProfile_homePhoneNumber,
    createProfile_lastName,
    createProfile_mailingAddress,
    createProfile_middleName,
    createProfile_mobilePhoneNumber,
    createProfile_partyType,
    createProfile_partyTypeString,
    createProfile_personalEmailAddress,
    createProfile_phoneNumber,
    createProfile_shippingAddress,
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
  { -- | A unique account number that you have given to the customer.
    accountNumber :: Prelude.Maybe Prelude.Text,
    -- | Any additional information relevant to the customer’s profile.
    additionalInformation :: Prelude.Maybe Prelude.Text,
    -- | A generic address associated with the customer that is not mailing,
    -- shipping, or billing.
    address :: Prelude.Maybe Address,
    -- | A key value pair of attributes of a customer profile.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The customer’s billing address.
    billingAddress :: Prelude.Maybe Address,
    -- | The customer’s birth date.
    birthDate :: Prelude.Maybe Prelude.Text,
    -- | The customer’s business email address.
    businessEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the customer’s business.
    businessName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s business phone number.
    businessPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s email address, which has not been specified as a personal
    -- or business address.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer’s first name.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | The gender with which the customer identifies.
    gender :: Prelude.Maybe Gender,
    -- | An alternative to @Gender@ which accepts any string as input.
    genderString :: Prelude.Maybe Prelude.Text,
    -- | The customer’s home phone number.
    homePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s last name.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s mailing address.
    mailingAddress :: Prelude.Maybe Address,
    -- | The customer’s middle name.
    middleName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s mobile phone number.
    mobilePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The type of profile used to describe the customer.
    partyType :: Prelude.Maybe PartyType,
    -- | An alternative to @PartyType@ which accepts any string as input.
    partyTypeString :: Prelude.Maybe Prelude.Text,
    -- | The customer’s personal email address.
    personalEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer’s phone number, which has not been specified as a mobile,
    -- home, or business number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s shipping address.
    shippingAddress :: Prelude.Maybe Address,
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
-- 'accountNumber', 'createProfile_accountNumber' - A unique account number that you have given to the customer.
--
-- 'additionalInformation', 'createProfile_additionalInformation' - Any additional information relevant to the customer’s profile.
--
-- 'address', 'createProfile_address' - A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- 'attributes', 'createProfile_attributes' - A key value pair of attributes of a customer profile.
--
-- 'billingAddress', 'createProfile_billingAddress' - The customer’s billing address.
--
-- 'birthDate', 'createProfile_birthDate' - The customer’s birth date.
--
-- 'businessEmailAddress', 'createProfile_businessEmailAddress' - The customer’s business email address.
--
-- 'businessName', 'createProfile_businessName' - The name of the customer’s business.
--
-- 'businessPhoneNumber', 'createProfile_businessPhoneNumber' - The customer’s business phone number.
--
-- 'emailAddress', 'createProfile_emailAddress' - The customer’s email address, which has not been specified as a personal
-- or business address.
--
-- 'firstName', 'createProfile_firstName' - The customer’s first name.
--
-- 'gender', 'createProfile_gender' - The gender with which the customer identifies.
--
-- 'genderString', 'createProfile_genderString' - An alternative to @Gender@ which accepts any string as input.
--
-- 'homePhoneNumber', 'createProfile_homePhoneNumber' - The customer’s home phone number.
--
-- 'lastName', 'createProfile_lastName' - The customer’s last name.
--
-- 'mailingAddress', 'createProfile_mailingAddress' - The customer’s mailing address.
--
-- 'middleName', 'createProfile_middleName' - The customer’s middle name.
--
-- 'mobilePhoneNumber', 'createProfile_mobilePhoneNumber' - The customer’s mobile phone number.
--
-- 'partyType', 'createProfile_partyType' - The type of profile used to describe the customer.
--
-- 'partyTypeString', 'createProfile_partyTypeString' - An alternative to @PartyType@ which accepts any string as input.
--
-- 'personalEmailAddress', 'createProfile_personalEmailAddress' - The customer’s personal email address.
--
-- 'phoneNumber', 'createProfile_phoneNumber' - The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
--
-- 'shippingAddress', 'createProfile_shippingAddress' - The customer’s shipping address.
--
-- 'domainName', 'createProfile_domainName' - The unique name of the domain.
newCreateProfile ::
  -- | 'domainName'
  Prelude.Text ->
  CreateProfile
newCreateProfile pDomainName_ =
  CreateProfile'
    { accountNumber = Prelude.Nothing,
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
      genderString = Prelude.Nothing,
      homePhoneNumber = Prelude.Nothing,
      lastName = Prelude.Nothing,
      mailingAddress = Prelude.Nothing,
      middleName = Prelude.Nothing,
      mobilePhoneNumber = Prelude.Nothing,
      partyType = Prelude.Nothing,
      partyTypeString = Prelude.Nothing,
      personalEmailAddress = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | A unique account number that you have given to the customer.
createProfile_accountNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_accountNumber = Lens.lens (\CreateProfile' {accountNumber} -> accountNumber) (\s@CreateProfile' {} a -> s {accountNumber = a} :: CreateProfile)

-- | Any additional information relevant to the customer’s profile.
createProfile_additionalInformation :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_additionalInformation = Lens.lens (\CreateProfile' {additionalInformation} -> additionalInformation) (\s@CreateProfile' {} a -> s {additionalInformation = a} :: CreateProfile)

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
createProfile_address :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_address = Lens.lens (\CreateProfile' {address} -> address) (\s@CreateProfile' {} a -> s {address = a} :: CreateProfile)

-- | A key value pair of attributes of a customer profile.
createProfile_attributes :: Lens.Lens' CreateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createProfile_attributes = Lens.lens (\CreateProfile' {attributes} -> attributes) (\s@CreateProfile' {} a -> s {attributes = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The customer’s billing address.
createProfile_billingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_billingAddress = Lens.lens (\CreateProfile' {billingAddress} -> billingAddress) (\s@CreateProfile' {} a -> s {billingAddress = a} :: CreateProfile)

-- | The customer’s birth date.
createProfile_birthDate :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_birthDate = Lens.lens (\CreateProfile' {birthDate} -> birthDate) (\s@CreateProfile' {} a -> s {birthDate = a} :: CreateProfile)

-- | The customer’s business email address.
createProfile_businessEmailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessEmailAddress = Lens.lens (\CreateProfile' {businessEmailAddress} -> businessEmailAddress) (\s@CreateProfile' {} a -> s {businessEmailAddress = a} :: CreateProfile)

-- | The name of the customer’s business.
createProfile_businessName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessName = Lens.lens (\CreateProfile' {businessName} -> businessName) (\s@CreateProfile' {} a -> s {businessName = a} :: CreateProfile)

-- | The customer’s business phone number.
createProfile_businessPhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_businessPhoneNumber = Lens.lens (\CreateProfile' {businessPhoneNumber} -> businessPhoneNumber) (\s@CreateProfile' {} a -> s {businessPhoneNumber = a} :: CreateProfile)

-- | The customer’s email address, which has not been specified as a personal
-- or business address.
createProfile_emailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_emailAddress = Lens.lens (\CreateProfile' {emailAddress} -> emailAddress) (\s@CreateProfile' {} a -> s {emailAddress = a} :: CreateProfile)

-- | The customer’s first name.
createProfile_firstName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_firstName = Lens.lens (\CreateProfile' {firstName} -> firstName) (\s@CreateProfile' {} a -> s {firstName = a} :: CreateProfile)

-- | The gender with which the customer identifies.
createProfile_gender :: Lens.Lens' CreateProfile (Prelude.Maybe Gender)
createProfile_gender = Lens.lens (\CreateProfile' {gender} -> gender) (\s@CreateProfile' {} a -> s {gender = a} :: CreateProfile)

-- | An alternative to @Gender@ which accepts any string as input.
createProfile_genderString :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_genderString = Lens.lens (\CreateProfile' {genderString} -> genderString) (\s@CreateProfile' {} a -> s {genderString = a} :: CreateProfile)

-- | The customer’s home phone number.
createProfile_homePhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_homePhoneNumber = Lens.lens (\CreateProfile' {homePhoneNumber} -> homePhoneNumber) (\s@CreateProfile' {} a -> s {homePhoneNumber = a} :: CreateProfile)

-- | The customer’s last name.
createProfile_lastName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_lastName = Lens.lens (\CreateProfile' {lastName} -> lastName) (\s@CreateProfile' {} a -> s {lastName = a} :: CreateProfile)

-- | The customer’s mailing address.
createProfile_mailingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_mailingAddress = Lens.lens (\CreateProfile' {mailingAddress} -> mailingAddress) (\s@CreateProfile' {} a -> s {mailingAddress = a} :: CreateProfile)

-- | The customer’s middle name.
createProfile_middleName :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_middleName = Lens.lens (\CreateProfile' {middleName} -> middleName) (\s@CreateProfile' {} a -> s {middleName = a} :: CreateProfile)

-- | The customer’s mobile phone number.
createProfile_mobilePhoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_mobilePhoneNumber = Lens.lens (\CreateProfile' {mobilePhoneNumber} -> mobilePhoneNumber) (\s@CreateProfile' {} a -> s {mobilePhoneNumber = a} :: CreateProfile)

-- | The type of profile used to describe the customer.
createProfile_partyType :: Lens.Lens' CreateProfile (Prelude.Maybe PartyType)
createProfile_partyType = Lens.lens (\CreateProfile' {partyType} -> partyType) (\s@CreateProfile' {} a -> s {partyType = a} :: CreateProfile)

-- | An alternative to @PartyType@ which accepts any string as input.
createProfile_partyTypeString :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_partyTypeString = Lens.lens (\CreateProfile' {partyTypeString} -> partyTypeString) (\s@CreateProfile' {} a -> s {partyTypeString = a} :: CreateProfile)

-- | The customer’s personal email address.
createProfile_personalEmailAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_personalEmailAddress = Lens.lens (\CreateProfile' {personalEmailAddress} -> personalEmailAddress) (\s@CreateProfile' {} a -> s {personalEmailAddress = a} :: CreateProfile)

-- | The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
createProfile_phoneNumber :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_phoneNumber = Lens.lens (\CreateProfile' {phoneNumber} -> phoneNumber) (\s@CreateProfile' {} a -> s {phoneNumber = a} :: CreateProfile)

-- | The customer’s shipping address.
createProfile_shippingAddress :: Lens.Lens' CreateProfile (Prelude.Maybe Address)
createProfile_shippingAddress = Lens.lens (\CreateProfile' {shippingAddress} -> shippingAddress) (\s@CreateProfile' {} a -> s {shippingAddress = a} :: CreateProfile)

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
    _salt
      `Prelude.hashWithSalt` accountNumber
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
      `Prelude.hashWithSalt` genderString
      `Prelude.hashWithSalt` homePhoneNumber
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` mailingAddress
      `Prelude.hashWithSalt` middleName
      `Prelude.hashWithSalt` mobilePhoneNumber
      `Prelude.hashWithSalt` partyType
      `Prelude.hashWithSalt` partyTypeString
      `Prelude.hashWithSalt` personalEmailAddress
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` shippingAddress
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
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
      `Prelude.seq` Prelude.rnf genderString
      `Prelude.seq` Prelude.rnf homePhoneNumber
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf mailingAddress
      `Prelude.seq` Prelude.rnf middleName
      `Prelude.seq` Prelude.rnf mobilePhoneNumber
      `Prelude.seq` Prelude.rnf partyType
      `Prelude.seq` Prelude.rnf partyTypeString
      `Prelude.seq` Prelude.rnf
        personalEmailAddress
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf
        shippingAddress
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
            ("GenderString" Data..=) Prelude.<$> genderString,
            ("HomePhoneNumber" Data..=)
              Prelude.<$> homePhoneNumber,
            ("LastName" Data..=) Prelude.<$> lastName,
            ("MailingAddress" Data..=)
              Prelude.<$> mailingAddress,
            ("MiddleName" Data..=) Prelude.<$> middleName,
            ("MobilePhoneNumber" Data..=)
              Prelude.<$> mobilePhoneNumber,
            ("PartyType" Data..=) Prelude.<$> partyType,
            ("PartyTypeString" Data..=)
              Prelude.<$> partyTypeString,
            ("PersonalEmailAddress" Data..=)
              Prelude.<$> personalEmailAddress,
            ("PhoneNumber" Data..=) Prelude.<$> phoneNumber,
            ("ShippingAddress" Data..=)
              Prelude.<$> shippingAddress
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
