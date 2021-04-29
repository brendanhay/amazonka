{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Organizations.Types.CreateAccountStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountStatus where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountState
import qualified Network.AWS.Prelude as Prelude

-- | Contains the status about a CreateAccount or CreateGovCloudAccount
-- request to create an AWS account or an AWS GovCloud (US) account in an
-- organization.
--
-- /See:/ 'newCreateAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { -- | If the account was created successfully, the unique identifier (ID) of
    -- the new account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the request was made for the account creation.
    requestedTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The account name given to the account when it was created.
    accountName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | If the account was created successfully, the unique identifier (ID) of
    -- the new account in the AWS GovCloud (US) Region.
    govCloudAccountId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the account was created and the request
    -- completed.
    completedTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier (ID) that references this request. You get this
    -- value from the response of the initial CreateAccount request to create
    -- the account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
    -- request ID string requires \"car-\" followed by from 8 to 32 lowercase
    -- letters or digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of the request.
    state :: Prelude.Maybe CreateAccountState,
    -- | If the request failed, a description of the reason for the failure.
    --
    -- -   ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you
    --     have reached the limit on the number of accounts in your
    --     organization.
    --
    -- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
    --     with the same information.
    --
    -- -   EMAIL_ALREADY_EXISTS: The account could not be created because
    --     another AWS account with that email address already exists.
    --
    -- -   FAILED_BUSINESS_VALIDATION: The AWS account that owns your
    --     organization failed to receive business license validation.
    --
    -- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud
    --     (US) Region could not be created because this Region already
    --     includes an account with that email address.
    --
    -- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The AWS account that owns your
    --     organization can\'t complete business license validation because it
    --     doesn\'t have valid identity data.
    --
    -- -   INVALID_ADDRESS: The account could not be created because the
    --     address you provided is not valid.
    --
    -- -   INVALID_EMAIL: The account could not be created because the email
    --     address you provided is not valid.
    --
    -- -   INTERNAL_FAILURE: The account could not be created because of an
    --     internal failure. Try again later. If the problem persists, contact
    --     Customer Support.
    --
    -- -   MISSING_BUSINESS_VALIDATION: The AWS account that owns your
    --     organization has not received Business Validation.
    --
    -- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
    --     account with a valid payment method, such as a credit card.
    --
    -- -   PENDING_BUSINESS_VALIDATION: The AWS account that owns your
    --     organization is still in the process of completing business license
    --     validation.
    --
    -- -   UNKNOWN_BUSINESS_VALIDATION: The AWS account that owns your
    --     organization has an unknown issue with business license validation.
    failureReason :: Prelude.Maybe CreateAccountFailureReason
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'createAccountStatus_accountId' - If the account was created successfully, the unique identifier (ID) of
-- the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
--
-- 'requestedTimestamp', 'createAccountStatus_requestedTimestamp' - The date and time that the request was made for the account creation.
--
-- 'accountName', 'createAccountStatus_accountName' - The account name given to the account when it was created.
--
-- 'govCloudAccountId', 'createAccountStatus_govCloudAccountId' - If the account was created successfully, the unique identifier (ID) of
-- the new account in the AWS GovCloud (US) Region.
--
-- 'completedTimestamp', 'createAccountStatus_completedTimestamp' - The date and time that the account was created and the request
-- completed.
--
-- 'id', 'createAccountStatus_id' - The unique identifier (ID) that references this request. You get this
-- value from the response of the initial CreateAccount request to create
-- the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
--
-- 'state', 'createAccountStatus_state' - The status of the request.
--
-- 'failureReason', 'createAccountStatus_failureReason' - If the request failed, a description of the reason for the failure.
--
-- -   ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you
--     have reached the limit on the number of accounts in your
--     organization.
--
-- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
--     with the same information.
--
-- -   EMAIL_ALREADY_EXISTS: The account could not be created because
--     another AWS account with that email address already exists.
--
-- -   FAILED_BUSINESS_VALIDATION: The AWS account that owns your
--     organization failed to receive business license validation.
--
-- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud
--     (US) Region could not be created because this Region already
--     includes an account with that email address.
--
-- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The AWS account that owns your
--     organization can\'t complete business license validation because it
--     doesn\'t have valid identity data.
--
-- -   INVALID_ADDRESS: The account could not be created because the
--     address you provided is not valid.
--
-- -   INVALID_EMAIL: The account could not be created because the email
--     address you provided is not valid.
--
-- -   INTERNAL_FAILURE: The account could not be created because of an
--     internal failure. Try again later. If the problem persists, contact
--     Customer Support.
--
-- -   MISSING_BUSINESS_VALIDATION: The AWS account that owns your
--     organization has not received Business Validation.
--
-- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
--     account with a valid payment method, such as a credit card.
--
-- -   PENDING_BUSINESS_VALIDATION: The AWS account that owns your
--     organization is still in the process of completing business license
--     validation.
--
-- -   UNKNOWN_BUSINESS_VALIDATION: The AWS account that owns your
--     organization has an unknown issue with business license validation.
newCreateAccountStatus ::
  CreateAccountStatus
newCreateAccountStatus =
  CreateAccountStatus'
    { accountId = Prelude.Nothing,
      requestedTimestamp = Prelude.Nothing,
      accountName = Prelude.Nothing,
      govCloudAccountId = Prelude.Nothing,
      completedTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      state = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | If the account was created successfully, the unique identifier (ID) of
-- the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
createAccountStatus_accountId :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_accountId = Lens.lens (\CreateAccountStatus' {accountId} -> accountId) (\s@CreateAccountStatus' {} a -> s {accountId = a} :: CreateAccountStatus)

-- | The date and time that the request was made for the account creation.
createAccountStatus_requestedTimestamp :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.UTCTime)
createAccountStatus_requestedTimestamp = Lens.lens (\CreateAccountStatus' {requestedTimestamp} -> requestedTimestamp) (\s@CreateAccountStatus' {} a -> s {requestedTimestamp = a} :: CreateAccountStatus) Prelude.. Lens.mapping Prelude._Time

-- | The account name given to the account when it was created.
createAccountStatus_accountName :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_accountName = Lens.lens (\CreateAccountStatus' {accountName} -> accountName) (\s@CreateAccountStatus' {} a -> s {accountName = a} :: CreateAccountStatus) Prelude.. Lens.mapping Prelude._Sensitive

-- | If the account was created successfully, the unique identifier (ID) of
-- the new account in the AWS GovCloud (US) Region.
createAccountStatus_govCloudAccountId :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_govCloudAccountId = Lens.lens (\CreateAccountStatus' {govCloudAccountId} -> govCloudAccountId) (\s@CreateAccountStatus' {} a -> s {govCloudAccountId = a} :: CreateAccountStatus)

-- | The date and time that the account was created and the request
-- completed.
createAccountStatus_completedTimestamp :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.UTCTime)
createAccountStatus_completedTimestamp = Lens.lens (\CreateAccountStatus' {completedTimestamp} -> completedTimestamp) (\s@CreateAccountStatus' {} a -> s {completedTimestamp = a} :: CreateAccountStatus) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier (ID) that references this request. You get this
-- value from the response of the initial CreateAccount request to create
-- the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
createAccountStatus_id :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_id = Lens.lens (\CreateAccountStatus' {id} -> id) (\s@CreateAccountStatus' {} a -> s {id = a} :: CreateAccountStatus)

-- | The status of the request.
createAccountStatus_state :: Lens.Lens' CreateAccountStatus (Prelude.Maybe CreateAccountState)
createAccountStatus_state = Lens.lens (\CreateAccountStatus' {state} -> state) (\s@CreateAccountStatus' {} a -> s {state = a} :: CreateAccountStatus)

-- | If the request failed, a description of the reason for the failure.
--
-- -   ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you
--     have reached the limit on the number of accounts in your
--     organization.
--
-- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
--     with the same information.
--
-- -   EMAIL_ALREADY_EXISTS: The account could not be created because
--     another AWS account with that email address already exists.
--
-- -   FAILED_BUSINESS_VALIDATION: The AWS account that owns your
--     organization failed to receive business license validation.
--
-- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud
--     (US) Region could not be created because this Region already
--     includes an account with that email address.
--
-- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The AWS account that owns your
--     organization can\'t complete business license validation because it
--     doesn\'t have valid identity data.
--
-- -   INVALID_ADDRESS: The account could not be created because the
--     address you provided is not valid.
--
-- -   INVALID_EMAIL: The account could not be created because the email
--     address you provided is not valid.
--
-- -   INTERNAL_FAILURE: The account could not be created because of an
--     internal failure. Try again later. If the problem persists, contact
--     Customer Support.
--
-- -   MISSING_BUSINESS_VALIDATION: The AWS account that owns your
--     organization has not received Business Validation.
--
-- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
--     account with a valid payment method, such as a credit card.
--
-- -   PENDING_BUSINESS_VALIDATION: The AWS account that owns your
--     organization is still in the process of completing business license
--     validation.
--
-- -   UNKNOWN_BUSINESS_VALIDATION: The AWS account that owns your
--     organization has an unknown issue with business license validation.
createAccountStatus_failureReason :: Lens.Lens' CreateAccountStatus (Prelude.Maybe CreateAccountFailureReason)
createAccountStatus_failureReason = Lens.lens (\CreateAccountStatus' {failureReason} -> failureReason) (\s@CreateAccountStatus' {} a -> s {failureReason = a} :: CreateAccountStatus)

instance Prelude.FromJSON CreateAccountStatus where
  parseJSON =
    Prelude.withObject
      "CreateAccountStatus"
      ( \x ->
          CreateAccountStatus'
            Prelude.<$> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "RequestedTimestamp")
            Prelude.<*> (x Prelude..:? "AccountName")
            Prelude.<*> (x Prelude..:? "GovCloudAccountId")
            Prelude.<*> (x Prelude..:? "CompletedTimestamp")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "FailureReason")
      )

instance Prelude.Hashable CreateAccountStatus

instance Prelude.NFData CreateAccountStatus
