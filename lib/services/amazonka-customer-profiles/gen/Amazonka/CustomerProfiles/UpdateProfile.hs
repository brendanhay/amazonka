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
-- Module      : Amazonka.CustomerProfiles.UpdateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of a profile. The ProfileId is required for
-- updating a customer profile.
--
-- When calling the UpdateProfile API, specifying an empty string value
-- means that any existing value will be removed. Not specifying a string
-- value means that any value already there will be kept.
module Amazonka.CustomerProfiles.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_accountNumber,
    updateProfile_additionalInformation,
    updateProfile_address,
    updateProfile_attributes,
    updateProfile_billingAddress,
    updateProfile_birthDate,
    updateProfile_businessEmailAddress,
    updateProfile_businessName,
    updateProfile_businessPhoneNumber,
    updateProfile_emailAddress,
    updateProfile_firstName,
    updateProfile_gender,
    updateProfile_homePhoneNumber,
    updateProfile_lastName,
    updateProfile_mailingAddress,
    updateProfile_middleName,
    updateProfile_mobilePhoneNumber,
    updateProfile_partyType,
    updateProfile_personalEmailAddress,
    updateProfile_phoneNumber,
    updateProfile_shippingAddress,
    updateProfile_domainName,
    updateProfile_profileId,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_httpStatus,
    updateProfileResponse_profileId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | A unique account number that you have given to the customer.
    accountNumber :: Prelude.Maybe Prelude.Text,
    -- | Any additional information relevant to the customer’s profile.
    additionalInformation :: Prelude.Maybe Prelude.Text,
    -- | A generic address associated with the customer that is not mailing,
    -- shipping, or billing.
    address :: Prelude.Maybe UpdateAddress,
    -- | A key value pair of attributes of a customer profile.
    attributes :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The customer’s billing address.
    billingAddress :: Prelude.Maybe UpdateAddress,
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
    -- | The customer’s home phone number.
    homePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s last name.
    lastName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s mailing address.
    mailingAddress :: Prelude.Maybe UpdateAddress,
    -- | The customer’s middle name.
    middleName :: Prelude.Maybe Prelude.Text,
    -- | The customer’s mobile phone number.
    mobilePhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The type of profile used to describe the customer.
    partyType :: Prelude.Maybe PartyType,
    -- | The customer’s personal email address.
    personalEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer’s phone number, which has not been specified as a mobile,
    -- home, or business number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s shipping address.
    shippingAddress :: Prelude.Maybe UpdateAddress,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountNumber', 'updateProfile_accountNumber' - A unique account number that you have given to the customer.
--
-- 'additionalInformation', 'updateProfile_additionalInformation' - Any additional information relevant to the customer’s profile.
--
-- 'address', 'updateProfile_address' - A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- 'attributes', 'updateProfile_attributes' - A key value pair of attributes of a customer profile.
--
-- 'billingAddress', 'updateProfile_billingAddress' - The customer’s billing address.
--
-- 'birthDate', 'updateProfile_birthDate' - The customer’s birth date.
--
-- 'businessEmailAddress', 'updateProfile_businessEmailAddress' - The customer’s business email address.
--
-- 'businessName', 'updateProfile_businessName' - The name of the customer’s business.
--
-- 'businessPhoneNumber', 'updateProfile_businessPhoneNumber' - The customer’s business phone number.
--
-- 'emailAddress', 'updateProfile_emailAddress' - The customer’s email address, which has not been specified as a personal
-- or business address.
--
-- 'firstName', 'updateProfile_firstName' - The customer’s first name.
--
-- 'gender', 'updateProfile_gender' - The gender with which the customer identifies.
--
-- 'homePhoneNumber', 'updateProfile_homePhoneNumber' - The customer’s home phone number.
--
-- 'lastName', 'updateProfile_lastName' - The customer’s last name.
--
-- 'mailingAddress', 'updateProfile_mailingAddress' - The customer’s mailing address.
--
-- 'middleName', 'updateProfile_middleName' - The customer’s middle name.
--
-- 'mobilePhoneNumber', 'updateProfile_mobilePhoneNumber' - The customer’s mobile phone number.
--
-- 'partyType', 'updateProfile_partyType' - The type of profile used to describe the customer.
--
-- 'personalEmailAddress', 'updateProfile_personalEmailAddress' - The customer’s personal email address.
--
-- 'phoneNumber', 'updateProfile_phoneNumber' - The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
--
-- 'shippingAddress', 'updateProfile_shippingAddress' - The customer’s shipping address.
--
-- 'domainName', 'updateProfile_domainName' - The unique name of the domain.
--
-- 'profileId', 'updateProfile_profileId' - The unique identifier of a customer profile.
newUpdateProfile ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'profileId'
  Prelude.Text ->
  UpdateProfile
newUpdateProfile pDomainName_ pProfileId_ =
  UpdateProfile'
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
      homePhoneNumber = Prelude.Nothing,
      lastName = Prelude.Nothing,
      mailingAddress = Prelude.Nothing,
      middleName = Prelude.Nothing,
      mobilePhoneNumber = Prelude.Nothing,
      partyType = Prelude.Nothing,
      personalEmailAddress = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      shippingAddress = Prelude.Nothing,
      domainName = pDomainName_,
      profileId = pProfileId_
    }

-- | A unique account number that you have given to the customer.
updateProfile_accountNumber :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_accountNumber = Lens.lens (\UpdateProfile' {accountNumber} -> accountNumber) (\s@UpdateProfile' {} a -> s {accountNumber = a} :: UpdateProfile)

-- | Any additional information relevant to the customer’s profile.
updateProfile_additionalInformation :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_additionalInformation = Lens.lens (\UpdateProfile' {additionalInformation} -> additionalInformation) (\s@UpdateProfile' {} a -> s {additionalInformation = a} :: UpdateProfile)

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
updateProfile_address :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateAddress)
updateProfile_address = Lens.lens (\UpdateProfile' {address} -> address) (\s@UpdateProfile' {} a -> s {address = a} :: UpdateProfile)

-- | A key value pair of attributes of a customer profile.
updateProfile_attributes :: Lens.Lens' UpdateProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateProfile_attributes = Lens.lens (\UpdateProfile' {attributes} -> attributes) (\s@UpdateProfile' {} a -> s {attributes = a} :: UpdateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The customer’s billing address.
updateProfile_billingAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateAddress)
updateProfile_billingAddress = Lens.lens (\UpdateProfile' {billingAddress} -> billingAddress) (\s@UpdateProfile' {} a -> s {billingAddress = a} :: UpdateProfile)

-- | The customer’s birth date.
updateProfile_birthDate :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_birthDate = Lens.lens (\UpdateProfile' {birthDate} -> birthDate) (\s@UpdateProfile' {} a -> s {birthDate = a} :: UpdateProfile)

-- | The customer’s business email address.
updateProfile_businessEmailAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_businessEmailAddress = Lens.lens (\UpdateProfile' {businessEmailAddress} -> businessEmailAddress) (\s@UpdateProfile' {} a -> s {businessEmailAddress = a} :: UpdateProfile)

-- | The name of the customer’s business.
updateProfile_businessName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_businessName = Lens.lens (\UpdateProfile' {businessName} -> businessName) (\s@UpdateProfile' {} a -> s {businessName = a} :: UpdateProfile)

-- | The customer’s business phone number.
updateProfile_businessPhoneNumber :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_businessPhoneNumber = Lens.lens (\UpdateProfile' {businessPhoneNumber} -> businessPhoneNumber) (\s@UpdateProfile' {} a -> s {businessPhoneNumber = a} :: UpdateProfile)

-- | The customer’s email address, which has not been specified as a personal
-- or business address.
updateProfile_emailAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_emailAddress = Lens.lens (\UpdateProfile' {emailAddress} -> emailAddress) (\s@UpdateProfile' {} a -> s {emailAddress = a} :: UpdateProfile)

-- | The customer’s first name.
updateProfile_firstName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_firstName = Lens.lens (\UpdateProfile' {firstName} -> firstName) (\s@UpdateProfile' {} a -> s {firstName = a} :: UpdateProfile)

-- | The gender with which the customer identifies.
updateProfile_gender :: Lens.Lens' UpdateProfile (Prelude.Maybe Gender)
updateProfile_gender = Lens.lens (\UpdateProfile' {gender} -> gender) (\s@UpdateProfile' {} a -> s {gender = a} :: UpdateProfile)

-- | The customer’s home phone number.
updateProfile_homePhoneNumber :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_homePhoneNumber = Lens.lens (\UpdateProfile' {homePhoneNumber} -> homePhoneNumber) (\s@UpdateProfile' {} a -> s {homePhoneNumber = a} :: UpdateProfile)

-- | The customer’s last name.
updateProfile_lastName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_lastName = Lens.lens (\UpdateProfile' {lastName} -> lastName) (\s@UpdateProfile' {} a -> s {lastName = a} :: UpdateProfile)

-- | The customer’s mailing address.
updateProfile_mailingAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateAddress)
updateProfile_mailingAddress = Lens.lens (\UpdateProfile' {mailingAddress} -> mailingAddress) (\s@UpdateProfile' {} a -> s {mailingAddress = a} :: UpdateProfile)

-- | The customer’s middle name.
updateProfile_middleName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_middleName = Lens.lens (\UpdateProfile' {middleName} -> middleName) (\s@UpdateProfile' {} a -> s {middleName = a} :: UpdateProfile)

-- | The customer’s mobile phone number.
updateProfile_mobilePhoneNumber :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_mobilePhoneNumber = Lens.lens (\UpdateProfile' {mobilePhoneNumber} -> mobilePhoneNumber) (\s@UpdateProfile' {} a -> s {mobilePhoneNumber = a} :: UpdateProfile)

-- | The type of profile used to describe the customer.
updateProfile_partyType :: Lens.Lens' UpdateProfile (Prelude.Maybe PartyType)
updateProfile_partyType = Lens.lens (\UpdateProfile' {partyType} -> partyType) (\s@UpdateProfile' {} a -> s {partyType = a} :: UpdateProfile)

-- | The customer’s personal email address.
updateProfile_personalEmailAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_personalEmailAddress = Lens.lens (\UpdateProfile' {personalEmailAddress} -> personalEmailAddress) (\s@UpdateProfile' {} a -> s {personalEmailAddress = a} :: UpdateProfile)

-- | The customer’s phone number, which has not been specified as a mobile,
-- home, or business number.
updateProfile_phoneNumber :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_phoneNumber = Lens.lens (\UpdateProfile' {phoneNumber} -> phoneNumber) (\s@UpdateProfile' {} a -> s {phoneNumber = a} :: UpdateProfile)

-- | The customer’s shipping address.
updateProfile_shippingAddress :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateAddress)
updateProfile_shippingAddress = Lens.lens (\UpdateProfile' {shippingAddress} -> shippingAddress) (\s@UpdateProfile' {} a -> s {shippingAddress = a} :: UpdateProfile)

-- | The unique name of the domain.
updateProfile_domainName :: Lens.Lens' UpdateProfile Prelude.Text
updateProfile_domainName = Lens.lens (\UpdateProfile' {domainName} -> domainName) (\s@UpdateProfile' {} a -> s {domainName = a} :: UpdateProfile)

-- | The unique identifier of a customer profile.
updateProfile_profileId :: Lens.Lens' UpdateProfile Prelude.Text
updateProfile_profileId = Lens.lens (\UpdateProfile' {profileId} -> profileId) (\s@UpdateProfile' {} a -> s {profileId = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ProfileId")
      )

instance Prelude.Hashable UpdateProfile where
  hashWithSalt _salt UpdateProfile' {..} =
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
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` profileId

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
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
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf profileId

instance Data.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
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
              Prelude.<$> shippingAddress,
            Prelude.Just ("ProfileId" Data..= profileId)
          ]
      )

instance Data.ToPath UpdateProfile where
  toPath UpdateProfile' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/profiles"]

instance Data.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProfileResponse_httpStatus' - The response's http status code.
--
-- 'profileId', 'updateProfileResponse_profileId' - The unique identifier of a customer profile.
newUpdateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'profileId'
  Prelude.Text ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ pProfileId_ =
  UpdateProfileResponse'
    { httpStatus = pHttpStatus_,
      profileId = pProfileId_
    }

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Prelude.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

-- | The unique identifier of a customer profile.
updateProfileResponse_profileId :: Lens.Lens' UpdateProfileResponse Prelude.Text
updateProfileResponse_profileId = Lens.lens (\UpdateProfileResponse' {profileId} -> profileId) (\s@UpdateProfileResponse' {} a -> s {profileId = a} :: UpdateProfileResponse)

instance Prelude.NFData UpdateProfileResponse where
  rnf UpdateProfileResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profileId
