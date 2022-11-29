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
-- Module      : Amazonka.Organizations.Types.CreateAccountStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.CreateAccountStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Organizations.Types.CreateAccountFailureReason
import Amazonka.Organizations.Types.CreateAccountState
import qualified Amazonka.Prelude as Prelude

-- | Contains the status about a CreateAccount or CreateGovCloudAccount
-- request to create an Amazon Web Services account or an Amazon Web
-- Services GovCloud (US) account in an organization.
--
-- /See:/ 'newCreateAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { -- | The date and time that the request was made for the account creation.
    requestedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | If the account was created successfully, the unique identifier (ID) of
    -- the new account in the Amazon Web Services GovCloud (US) Region.
    govCloudAccountId :: Prelude.Maybe Prelude.Text,
    -- | The status of the asynchronous request to create an Amazon Web Services
    -- account.
    state :: Prelude.Maybe CreateAccountState,
    -- | The unique identifier (ID) that references this request. You get this
    -- value from the response of the initial CreateAccount request to create
    -- the account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
    -- request ID string requires \"car-\" followed by from 8 to 32 lowercase
    -- letters or digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | If the account was created successfully, the unique identifier (ID) of
    -- the new account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The account name given to the account when it was created.
    accountName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The date and time that the account was created and the request
    -- completed.
    completedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | If the request failed, a description of the reason for the failure.
    --
    -- -   ACCOUNT_LIMIT_EXCEEDED: The account couldn\'t be created because you
    --     reached the limit on the number of accounts in your organization.
    --
    -- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
    --     with the same information.
    --
    -- -   EMAIL_ALREADY_EXISTS: The account could not be created because
    --     another Amazon Web Services account with that email address already
    --     exists.
    --
    -- -   FAILED_BUSINESS_VALIDATION: The Amazon Web Services account that
    --     owns your organization failed to receive business license
    --     validation.
    --
    -- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the Amazon Web
    --     Services GovCloud (US) Region could not be created because this
    --     Region already includes an account with that email address.
    --
    -- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The Amazon Web Services
    --     account that owns your organization can\'t complete business license
    --     validation because it doesn\'t have valid identity data.
    --
    -- -   INVALID_ADDRESS: The account could not be created because the
    --     address you provided is not valid.
    --
    -- -   INVALID_EMAIL: The account could not be created because the email
    --     address you provided is not valid.
    --
    -- -   INVALID_PAYMENT_INSTRUMENT: The Amazon Web Services account that
    --     owns your organization does not have a supported payment method
    --     associated with the account. Amazon Web Services does not support
    --     cards issued by financial institutions in Russia or Belarus. For
    --     more information, see
    --     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-general.html Managing your Amazon Web Services payments>.
    --
    -- -   INTERNAL_FAILURE: The account could not be created because of an
    --     internal failure. Try again later. If the problem persists, contact
    --     Amazon Web Services Customer Support.
    --
    -- -   MISSING_BUSINESS_VALIDATION: The Amazon Web Services account that
    --     owns your organization has not received Business Validation.
    --
    -- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
    --     account with a valid payment method, such as a credit card.
    --
    -- -   PENDING_BUSINESS_VALIDATION: The Amazon Web Services account that
    --     owns your organization is still in the process of completing
    --     business license validation.
    --
    -- -   UNKNOWN_BUSINESS_VALIDATION: The Amazon Web Services account that
    --     owns your organization has an unknown issue with business license
    --     validation.
    failureReason :: Prelude.Maybe CreateAccountFailureReason
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestedTimestamp', 'createAccountStatus_requestedTimestamp' - The date and time that the request was made for the account creation.
--
-- 'govCloudAccountId', 'createAccountStatus_govCloudAccountId' - If the account was created successfully, the unique identifier (ID) of
-- the new account in the Amazon Web Services GovCloud (US) Region.
--
-- 'state', 'createAccountStatus_state' - The status of the asynchronous request to create an Amazon Web Services
-- account.
--
-- 'id', 'createAccountStatus_id' - The unique identifier (ID) that references this request. You get this
-- value from the response of the initial CreateAccount request to create
-- the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
--
-- 'accountId', 'createAccountStatus_accountId' - If the account was created successfully, the unique identifier (ID) of
-- the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
--
-- 'accountName', 'createAccountStatus_accountName' - The account name given to the account when it was created.
--
-- 'completedTimestamp', 'createAccountStatus_completedTimestamp' - The date and time that the account was created and the request
-- completed.
--
-- 'failureReason', 'createAccountStatus_failureReason' - If the request failed, a description of the reason for the failure.
--
-- -   ACCOUNT_LIMIT_EXCEEDED: The account couldn\'t be created because you
--     reached the limit on the number of accounts in your organization.
--
-- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
--     with the same information.
--
-- -   EMAIL_ALREADY_EXISTS: The account could not be created because
--     another Amazon Web Services account with that email address already
--     exists.
--
-- -   FAILED_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization failed to receive business license
--     validation.
--
-- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the Amazon Web
--     Services GovCloud (US) Region could not be created because this
--     Region already includes an account with that email address.
--
-- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The Amazon Web Services
--     account that owns your organization can\'t complete business license
--     validation because it doesn\'t have valid identity data.
--
-- -   INVALID_ADDRESS: The account could not be created because the
--     address you provided is not valid.
--
-- -   INVALID_EMAIL: The account could not be created because the email
--     address you provided is not valid.
--
-- -   INVALID_PAYMENT_INSTRUMENT: The Amazon Web Services account that
--     owns your organization does not have a supported payment method
--     associated with the account. Amazon Web Services does not support
--     cards issued by financial institutions in Russia or Belarus. For
--     more information, see
--     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-general.html Managing your Amazon Web Services payments>.
--
-- -   INTERNAL_FAILURE: The account could not be created because of an
--     internal failure. Try again later. If the problem persists, contact
--     Amazon Web Services Customer Support.
--
-- -   MISSING_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization has not received Business Validation.
--
-- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
--     account with a valid payment method, such as a credit card.
--
-- -   PENDING_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization is still in the process of completing
--     business license validation.
--
-- -   UNKNOWN_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization has an unknown issue with business license
--     validation.
newCreateAccountStatus ::
  CreateAccountStatus
newCreateAccountStatus =
  CreateAccountStatus'
    { requestedTimestamp =
        Prelude.Nothing,
      govCloudAccountId = Prelude.Nothing,
      state = Prelude.Nothing,
      id = Prelude.Nothing,
      accountId = Prelude.Nothing,
      accountName = Prelude.Nothing,
      completedTimestamp = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The date and time that the request was made for the account creation.
createAccountStatus_requestedTimestamp :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.UTCTime)
createAccountStatus_requestedTimestamp = Lens.lens (\CreateAccountStatus' {requestedTimestamp} -> requestedTimestamp) (\s@CreateAccountStatus' {} a -> s {requestedTimestamp = a} :: CreateAccountStatus) Prelude.. Lens.mapping Core._Time

-- | If the account was created successfully, the unique identifier (ID) of
-- the new account in the Amazon Web Services GovCloud (US) Region.
createAccountStatus_govCloudAccountId :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_govCloudAccountId = Lens.lens (\CreateAccountStatus' {govCloudAccountId} -> govCloudAccountId) (\s@CreateAccountStatus' {} a -> s {govCloudAccountId = a} :: CreateAccountStatus)

-- | The status of the asynchronous request to create an Amazon Web Services
-- account.
createAccountStatus_state :: Lens.Lens' CreateAccountStatus (Prelude.Maybe CreateAccountState)
createAccountStatus_state = Lens.lens (\CreateAccountStatus' {state} -> state) (\s@CreateAccountStatus' {} a -> s {state = a} :: CreateAccountStatus)

-- | The unique identifier (ID) that references this request. You get this
-- value from the response of the initial CreateAccount request to create
-- the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account
-- request ID string requires \"car-\" followed by from 8 to 32 lowercase
-- letters or digits.
createAccountStatus_id :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_id = Lens.lens (\CreateAccountStatus' {id} -> id) (\s@CreateAccountStatus' {} a -> s {id = a} :: CreateAccountStatus)

-- | If the account was created successfully, the unique identifier (ID) of
-- the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
createAccountStatus_accountId :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_accountId = Lens.lens (\CreateAccountStatus' {accountId} -> accountId) (\s@CreateAccountStatus' {} a -> s {accountId = a} :: CreateAccountStatus)

-- | The account name given to the account when it was created.
createAccountStatus_accountName :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.Text)
createAccountStatus_accountName = Lens.lens (\CreateAccountStatus' {accountName} -> accountName) (\s@CreateAccountStatus' {} a -> s {accountName = a} :: CreateAccountStatus) Prelude.. Lens.mapping Core._Sensitive

-- | The date and time that the account was created and the request
-- completed.
createAccountStatus_completedTimestamp :: Lens.Lens' CreateAccountStatus (Prelude.Maybe Prelude.UTCTime)
createAccountStatus_completedTimestamp = Lens.lens (\CreateAccountStatus' {completedTimestamp} -> completedTimestamp) (\s@CreateAccountStatus' {} a -> s {completedTimestamp = a} :: CreateAccountStatus) Prelude.. Lens.mapping Core._Time

-- | If the request failed, a description of the reason for the failure.
--
-- -   ACCOUNT_LIMIT_EXCEEDED: The account couldn\'t be created because you
--     reached the limit on the number of accounts in your organization.
--
-- -   CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request
--     with the same information.
--
-- -   EMAIL_ALREADY_EXISTS: The account could not be created because
--     another Amazon Web Services account with that email address already
--     exists.
--
-- -   FAILED_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization failed to receive business license
--     validation.
--
-- -   GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the Amazon Web
--     Services GovCloud (US) Region could not be created because this
--     Region already includes an account with that email address.
--
-- -   IDENTITY_INVALID_BUSINESS_VALIDATION: The Amazon Web Services
--     account that owns your organization can\'t complete business license
--     validation because it doesn\'t have valid identity data.
--
-- -   INVALID_ADDRESS: The account could not be created because the
--     address you provided is not valid.
--
-- -   INVALID_EMAIL: The account could not be created because the email
--     address you provided is not valid.
--
-- -   INVALID_PAYMENT_INSTRUMENT: The Amazon Web Services account that
--     owns your organization does not have a supported payment method
--     associated with the account. Amazon Web Services does not support
--     cards issued by financial institutions in Russia or Belarus. For
--     more information, see
--     <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/manage-general.html Managing your Amazon Web Services payments>.
--
-- -   INTERNAL_FAILURE: The account could not be created because of an
--     internal failure. Try again later. If the problem persists, contact
--     Amazon Web Services Customer Support.
--
-- -   MISSING_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization has not received Business Validation.
--
-- -   MISSING_PAYMENT_INSTRUMENT: You must configure the management
--     account with a valid payment method, such as a credit card.
--
-- -   PENDING_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization is still in the process of completing
--     business license validation.
--
-- -   UNKNOWN_BUSINESS_VALIDATION: The Amazon Web Services account that
--     owns your organization has an unknown issue with business license
--     validation.
createAccountStatus_failureReason :: Lens.Lens' CreateAccountStatus (Prelude.Maybe CreateAccountFailureReason)
createAccountStatus_failureReason = Lens.lens (\CreateAccountStatus' {failureReason} -> failureReason) (\s@CreateAccountStatus' {} a -> s {failureReason = a} :: CreateAccountStatus)

instance Core.FromJSON CreateAccountStatus where
  parseJSON =
    Core.withObject
      "CreateAccountStatus"
      ( \x ->
          CreateAccountStatus'
            Prelude.<$> (x Core..:? "RequestedTimestamp")
            Prelude.<*> (x Core..:? "GovCloudAccountId")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "AccountName")
            Prelude.<*> (x Core..:? "CompletedTimestamp")
            Prelude.<*> (x Core..:? "FailureReason")
      )

instance Prelude.Hashable CreateAccountStatus where
  hashWithSalt _salt CreateAccountStatus' {..} =
    _salt `Prelude.hashWithSalt` requestedTimestamp
      `Prelude.hashWithSalt` govCloudAccountId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` accountName
      `Prelude.hashWithSalt` completedTimestamp
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData CreateAccountStatus where
  rnf CreateAccountStatus' {..} =
    Prelude.rnf requestedTimestamp
      `Prelude.seq` Prelude.rnf govCloudAccountId
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf accountName
      `Prelude.seq` Prelude.rnf completedTimestamp
      `Prelude.seq` Prelude.rnf failureReason
