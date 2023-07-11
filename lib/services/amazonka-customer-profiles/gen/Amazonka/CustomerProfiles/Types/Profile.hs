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
-- Module      : Amazonka.CustomerProfiles.Types.Profile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.Profile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.Address
import Amazonka.CustomerProfiles.Types.FoundByKeyValue
import Amazonka.CustomerProfiles.Types.Gender
import Amazonka.CustomerProfiles.Types.PartyType
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The standard profile of a customer.
--
-- /See:/ 'newProfile' smart constructor.
data Profile = Profile'
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
    -- | The customer’s home phone number.
    businessPhoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The customer’s email address, which has not been specified as a personal
    -- or business address.
    emailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer’s first name.
    firstName :: Prelude.Maybe Prelude.Text,
    -- | A list of items used to find a profile returned in a
    -- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
    -- response. An item is a key-value(s) pair that matches an attribute in
    -- the profile.
    --
    -- If the optional @AdditionalSearchKeys@ parameter was included in the
    -- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
    -- request, the @FoundByItems@ list should be interpreted based on the
    -- @LogicalOperator@ used in the request:
    --
    -- -   @AND@ - The profile included in the response matched all of the
    --     search keys specified in the request. The @FoundByItems@ will
    --     include all of the key-value(s) pairs that were specified in the
    --     request (as this is a requirement of @AND@ search logic).
    --
    -- -   @OR@ - The profile included in the response matched at least one of
    --     the search keys specified in the request. The @FoundByItems@ will
    --     include each of the key-value(s) pairs that the profile was found
    --     by.
    --
    -- The @OR@ relationship is the default behavior if the @LogicalOperator@
    -- parameter is not included in the
    -- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
    -- request.
    foundByItems :: Prelude.Maybe (Prelude.NonEmpty FoundByKeyValue),
    -- | The gender with which the customer identifies.
    gender :: Prelude.Maybe Gender,
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
    -- | The customer’s personal email address.
    personalEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The customer\'s phone number, which has not been specified as a mobile,
    -- home, or business number.
    phoneNumber :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of a customer profile.
    profileId :: Prelude.Maybe Prelude.Text,
    -- | The customer’s shipping address.
    shippingAddress :: Prelude.Maybe Address
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Profile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountNumber', 'profile_accountNumber' - A unique account number that you have given to the customer.
--
-- 'additionalInformation', 'profile_additionalInformation' - Any additional information relevant to the customer’s profile.
--
-- 'address', 'profile_address' - A generic address associated with the customer that is not mailing,
-- shipping, or billing.
--
-- 'attributes', 'profile_attributes' - A key value pair of attributes of a customer profile.
--
-- 'billingAddress', 'profile_billingAddress' - The customer’s billing address.
--
-- 'birthDate', 'profile_birthDate' - The customer’s birth date.
--
-- 'businessEmailAddress', 'profile_businessEmailAddress' - The customer’s business email address.
--
-- 'businessName', 'profile_businessName' - The name of the customer’s business.
--
-- 'businessPhoneNumber', 'profile_businessPhoneNumber' - The customer’s home phone number.
--
-- 'emailAddress', 'profile_emailAddress' - The customer’s email address, which has not been specified as a personal
-- or business address.
--
-- 'firstName', 'profile_firstName' - The customer’s first name.
--
-- 'foundByItems', 'profile_foundByItems' - A list of items used to find a profile returned in a
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- response. An item is a key-value(s) pair that matches an attribute in
-- the profile.
--
-- If the optional @AdditionalSearchKeys@ parameter was included in the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- request, the @FoundByItems@ list should be interpreted based on the
-- @LogicalOperator@ used in the request:
--
-- -   @AND@ - The profile included in the response matched all of the
--     search keys specified in the request. The @FoundByItems@ will
--     include all of the key-value(s) pairs that were specified in the
--     request (as this is a requirement of @AND@ search logic).
--
-- -   @OR@ - The profile included in the response matched at least one of
--     the search keys specified in the request. The @FoundByItems@ will
--     include each of the key-value(s) pairs that the profile was found
--     by.
--
-- The @OR@ relationship is the default behavior if the @LogicalOperator@
-- parameter is not included in the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- request.
--
-- 'gender', 'profile_gender' - The gender with which the customer identifies.
--
-- 'homePhoneNumber', 'profile_homePhoneNumber' - The customer’s home phone number.
--
-- 'lastName', 'profile_lastName' - The customer’s last name.
--
-- 'mailingAddress', 'profile_mailingAddress' - The customer’s mailing address.
--
-- 'middleName', 'profile_middleName' - The customer’s middle name.
--
-- 'mobilePhoneNumber', 'profile_mobilePhoneNumber' - The customer’s mobile phone number.
--
-- 'partyType', 'profile_partyType' - The type of profile used to describe the customer.
--
-- 'personalEmailAddress', 'profile_personalEmailAddress' - The customer’s personal email address.
--
-- 'phoneNumber', 'profile_phoneNumber' - The customer\'s phone number, which has not been specified as a mobile,
-- home, or business number.
--
-- 'profileId', 'profile_profileId' - The unique identifier of a customer profile.
--
-- 'shippingAddress', 'profile_shippingAddress' - The customer’s shipping address.
newProfile ::
  Profile
newProfile =
  Profile'
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
      foundByItems = Prelude.Nothing,
      gender = Prelude.Nothing,
      homePhoneNumber = Prelude.Nothing,
      lastName = Prelude.Nothing,
      mailingAddress = Prelude.Nothing,
      middleName = Prelude.Nothing,
      mobilePhoneNumber = Prelude.Nothing,
      partyType = Prelude.Nothing,
      personalEmailAddress = Prelude.Nothing,
      phoneNumber = Prelude.Nothing,
      profileId = Prelude.Nothing,
      shippingAddress = Prelude.Nothing
    }

-- | A unique account number that you have given to the customer.
profile_accountNumber :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_accountNumber = Lens.lens (\Profile' {accountNumber} -> accountNumber) (\s@Profile' {} a -> s {accountNumber = a} :: Profile)

-- | Any additional information relevant to the customer’s profile.
profile_additionalInformation :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_additionalInformation = Lens.lens (\Profile' {additionalInformation} -> additionalInformation) (\s@Profile' {} a -> s {additionalInformation = a} :: Profile)

-- | A generic address associated with the customer that is not mailing,
-- shipping, or billing.
profile_address :: Lens.Lens' Profile (Prelude.Maybe Address)
profile_address = Lens.lens (\Profile' {address} -> address) (\s@Profile' {} a -> s {address = a} :: Profile)

-- | A key value pair of attributes of a customer profile.
profile_attributes :: Lens.Lens' Profile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
profile_attributes = Lens.lens (\Profile' {attributes} -> attributes) (\s@Profile' {} a -> s {attributes = a} :: Profile) Prelude.. Lens.mapping Lens.coerced

-- | The customer’s billing address.
profile_billingAddress :: Lens.Lens' Profile (Prelude.Maybe Address)
profile_billingAddress = Lens.lens (\Profile' {billingAddress} -> billingAddress) (\s@Profile' {} a -> s {billingAddress = a} :: Profile)

-- | The customer’s birth date.
profile_birthDate :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_birthDate = Lens.lens (\Profile' {birthDate} -> birthDate) (\s@Profile' {} a -> s {birthDate = a} :: Profile)

-- | The customer’s business email address.
profile_businessEmailAddress :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_businessEmailAddress = Lens.lens (\Profile' {businessEmailAddress} -> businessEmailAddress) (\s@Profile' {} a -> s {businessEmailAddress = a} :: Profile)

-- | The name of the customer’s business.
profile_businessName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_businessName = Lens.lens (\Profile' {businessName} -> businessName) (\s@Profile' {} a -> s {businessName = a} :: Profile)

-- | The customer’s home phone number.
profile_businessPhoneNumber :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_businessPhoneNumber = Lens.lens (\Profile' {businessPhoneNumber} -> businessPhoneNumber) (\s@Profile' {} a -> s {businessPhoneNumber = a} :: Profile)

-- | The customer’s email address, which has not been specified as a personal
-- or business address.
profile_emailAddress :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_emailAddress = Lens.lens (\Profile' {emailAddress} -> emailAddress) (\s@Profile' {} a -> s {emailAddress = a} :: Profile)

-- | The customer’s first name.
profile_firstName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_firstName = Lens.lens (\Profile' {firstName} -> firstName) (\s@Profile' {} a -> s {firstName = a} :: Profile)

-- | A list of items used to find a profile returned in a
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- response. An item is a key-value(s) pair that matches an attribute in
-- the profile.
--
-- If the optional @AdditionalSearchKeys@ parameter was included in the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- request, the @FoundByItems@ list should be interpreted based on the
-- @LogicalOperator@ used in the request:
--
-- -   @AND@ - The profile included in the response matched all of the
--     search keys specified in the request. The @FoundByItems@ will
--     include all of the key-value(s) pairs that were specified in the
--     request (as this is a requirement of @AND@ search logic).
--
-- -   @OR@ - The profile included in the response matched at least one of
--     the search keys specified in the request. The @FoundByItems@ will
--     include each of the key-value(s) pairs that the profile was found
--     by.
--
-- The @OR@ relationship is the default behavior if the @LogicalOperator@
-- parameter is not included in the
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_SearchProfiles.html SearchProfiles>
-- request.
profile_foundByItems :: Lens.Lens' Profile (Prelude.Maybe (Prelude.NonEmpty FoundByKeyValue))
profile_foundByItems = Lens.lens (\Profile' {foundByItems} -> foundByItems) (\s@Profile' {} a -> s {foundByItems = a} :: Profile) Prelude.. Lens.mapping Lens.coerced

-- | The gender with which the customer identifies.
profile_gender :: Lens.Lens' Profile (Prelude.Maybe Gender)
profile_gender = Lens.lens (\Profile' {gender} -> gender) (\s@Profile' {} a -> s {gender = a} :: Profile)

-- | The customer’s home phone number.
profile_homePhoneNumber :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_homePhoneNumber = Lens.lens (\Profile' {homePhoneNumber} -> homePhoneNumber) (\s@Profile' {} a -> s {homePhoneNumber = a} :: Profile)

-- | The customer’s last name.
profile_lastName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_lastName = Lens.lens (\Profile' {lastName} -> lastName) (\s@Profile' {} a -> s {lastName = a} :: Profile)

-- | The customer’s mailing address.
profile_mailingAddress :: Lens.Lens' Profile (Prelude.Maybe Address)
profile_mailingAddress = Lens.lens (\Profile' {mailingAddress} -> mailingAddress) (\s@Profile' {} a -> s {mailingAddress = a} :: Profile)

-- | The customer’s middle name.
profile_middleName :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_middleName = Lens.lens (\Profile' {middleName} -> middleName) (\s@Profile' {} a -> s {middleName = a} :: Profile)

-- | The customer’s mobile phone number.
profile_mobilePhoneNumber :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_mobilePhoneNumber = Lens.lens (\Profile' {mobilePhoneNumber} -> mobilePhoneNumber) (\s@Profile' {} a -> s {mobilePhoneNumber = a} :: Profile)

-- | The type of profile used to describe the customer.
profile_partyType :: Lens.Lens' Profile (Prelude.Maybe PartyType)
profile_partyType = Lens.lens (\Profile' {partyType} -> partyType) (\s@Profile' {} a -> s {partyType = a} :: Profile)

-- | The customer’s personal email address.
profile_personalEmailAddress :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_personalEmailAddress = Lens.lens (\Profile' {personalEmailAddress} -> personalEmailAddress) (\s@Profile' {} a -> s {personalEmailAddress = a} :: Profile)

-- | The customer\'s phone number, which has not been specified as a mobile,
-- home, or business number.
profile_phoneNumber :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_phoneNumber = Lens.lens (\Profile' {phoneNumber} -> phoneNumber) (\s@Profile' {} a -> s {phoneNumber = a} :: Profile)

-- | The unique identifier of a customer profile.
profile_profileId :: Lens.Lens' Profile (Prelude.Maybe Prelude.Text)
profile_profileId = Lens.lens (\Profile' {profileId} -> profileId) (\s@Profile' {} a -> s {profileId = a} :: Profile)

-- | The customer’s shipping address.
profile_shippingAddress :: Lens.Lens' Profile (Prelude.Maybe Address)
profile_shippingAddress = Lens.lens (\Profile' {shippingAddress} -> shippingAddress) (\s@Profile' {} a -> s {shippingAddress = a} :: Profile)

instance Data.FromJSON Profile where
  parseJSON =
    Data.withObject
      "Profile"
      ( \x ->
          Profile'
            Prelude.<$> (x Data..:? "AccountNumber")
            Prelude.<*> (x Data..:? "AdditionalInformation")
            Prelude.<*> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "Attributes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "BillingAddress")
            Prelude.<*> (x Data..:? "BirthDate")
            Prelude.<*> (x Data..:? "BusinessEmailAddress")
            Prelude.<*> (x Data..:? "BusinessName")
            Prelude.<*> (x Data..:? "BusinessPhoneNumber")
            Prelude.<*> (x Data..:? "EmailAddress")
            Prelude.<*> (x Data..:? "FirstName")
            Prelude.<*> (x Data..:? "FoundByItems")
            Prelude.<*> (x Data..:? "Gender")
            Prelude.<*> (x Data..:? "HomePhoneNumber")
            Prelude.<*> (x Data..:? "LastName")
            Prelude.<*> (x Data..:? "MailingAddress")
            Prelude.<*> (x Data..:? "MiddleName")
            Prelude.<*> (x Data..:? "MobilePhoneNumber")
            Prelude.<*> (x Data..:? "PartyType")
            Prelude.<*> (x Data..:? "PersonalEmailAddress")
            Prelude.<*> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "ProfileId")
            Prelude.<*> (x Data..:? "ShippingAddress")
      )

instance Prelude.Hashable Profile where
  hashWithSalt _salt Profile' {..} =
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
      `Prelude.hashWithSalt` foundByItems
      `Prelude.hashWithSalt` gender
      `Prelude.hashWithSalt` homePhoneNumber
      `Prelude.hashWithSalt` lastName
      `Prelude.hashWithSalt` mailingAddress
      `Prelude.hashWithSalt` middleName
      `Prelude.hashWithSalt` mobilePhoneNumber
      `Prelude.hashWithSalt` partyType
      `Prelude.hashWithSalt` personalEmailAddress
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` shippingAddress

instance Prelude.NFData Profile where
  rnf Profile' {..} =
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
      `Prelude.seq` Prelude.rnf foundByItems
      `Prelude.seq` Prelude.rnf gender
      `Prelude.seq` Prelude.rnf homePhoneNumber
      `Prelude.seq` Prelude.rnf lastName
      `Prelude.seq` Prelude.rnf mailingAddress
      `Prelude.seq` Prelude.rnf middleName
      `Prelude.seq` Prelude.rnf mobilePhoneNumber
      `Prelude.seq` Prelude.rnf partyType
      `Prelude.seq` Prelude.rnf
        personalEmailAddress
      `Prelude.seq` Prelude.rnf phoneNumber
      `Prelude.seq` Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf
        shippingAddress
