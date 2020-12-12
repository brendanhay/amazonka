{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountStatus
  ( CreateAccountStatus (..),

    -- * Smart constructor
    mkCreateAccountStatus,

    -- * Lenses
    casFailureReason,
    casState,
    casCompletedTimestamp,
    casAccountName,
    casAccountId,
    casId,
    casGovCloudAccountId,
    casRequestedTimestamp,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountState
import qualified Network.AWS.Prelude as Lude

-- | Contains the status about a 'CreateAccount' or 'CreateGovCloudAccount' request to create an AWS account or an AWS GovCloud (US) account in an organization.
--
-- /See:/ 'mkCreateAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { failureReason ::
      Lude.Maybe CreateAccountFailureReason,
    state :: Lude.Maybe CreateAccountState,
    completedTimestamp :: Lude.Maybe Lude.Timestamp,
    accountName ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    accountId :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    govCloudAccountId :: Lude.Maybe Lude.Text,
    requestedTimestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAccountStatus' with the minimum fields required to make a request.
--
-- * 'accountId' - If the account was created successfully, the unique identifier (ID) of the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
-- * 'accountName' - The account name given to the account when it was created.
-- * 'completedTimestamp' - The date and time that the account was created and the request completed.
-- * 'failureReason' - If the request failed, a description of the reason for the failure.
--
--
--     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.
--
--
--     * CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request with the same information.
--
--
--     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.
--
--
--     * GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud (US) Region could not be created because this Region already includes an account with that email address.
--
--
--     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.
--
--
--     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.
--
--
--     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.
--
--
--     * MISSING_BUSINESS_VALIDATION: The AWS account that owns your organization has not received Business Validation.
--
--
--     * MISSING_PAYMENT_INSTRUMENT: You must configure the management account with a valid payment method, such as a credit card.
--
--
-- * 'govCloudAccountId' - If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
-- * 'id' - The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
-- * 'requestedTimestamp' - The date and time that the request was made for the account creation.
-- * 'state' - The status of the request.
mkCreateAccountStatus ::
  CreateAccountStatus
mkCreateAccountStatus =
  CreateAccountStatus'
    { failureReason = Lude.Nothing,
      state = Lude.Nothing,
      completedTimestamp = Lude.Nothing,
      accountName = Lude.Nothing,
      accountId = Lude.Nothing,
      id = Lude.Nothing,
      govCloudAccountId = Lude.Nothing,
      requestedTimestamp = Lude.Nothing
    }

-- | If the request failed, a description of the reason for the failure.
--
--
--     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.
--
--
--     * CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request with the same information.
--
--
--     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.
--
--
--     * GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud (US) Region could not be created because this Region already includes an account with that email address.
--
--
--     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.
--
--
--     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.
--
--
--     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.
--
--
--     * MISSING_BUSINESS_VALIDATION: The AWS account that owns your organization has not received Business Validation.
--
--
--     * MISSING_PAYMENT_INSTRUMENT: You must configure the management account with a valid payment method, such as a credit card.
--
--
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casFailureReason :: Lens.Lens' CreateAccountStatus (Lude.Maybe CreateAccountFailureReason)
casFailureReason = Lens.lens (failureReason :: CreateAccountStatus -> Lude.Maybe CreateAccountFailureReason) (\s a -> s {failureReason = a} :: CreateAccountStatus)
{-# DEPRECATED casFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The status of the request.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casState :: Lens.Lens' CreateAccountStatus (Lude.Maybe CreateAccountState)
casState = Lens.lens (state :: CreateAccountStatus -> Lude.Maybe CreateAccountState) (\s a -> s {state = a} :: CreateAccountStatus)
{-# DEPRECATED casState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The date and time that the account was created and the request completed.
--
-- /Note:/ Consider using 'completedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casCompletedTimestamp :: Lens.Lens' CreateAccountStatus (Lude.Maybe Lude.Timestamp)
casCompletedTimestamp = Lens.lens (completedTimestamp :: CreateAccountStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedTimestamp = a} :: CreateAccountStatus)
{-# DEPRECATED casCompletedTimestamp "Use generic-lens or generic-optics with 'completedTimestamp' instead." #-}

-- | The account name given to the account when it was created.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAccountName :: Lens.Lens' CreateAccountStatus (Lude.Maybe (Lude.Sensitive Lude.Text))
casAccountName = Lens.lens (accountName :: CreateAccountStatus -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {accountName = a} :: CreateAccountStatus)
{-# DEPRECATED casAccountName "Use generic-lens or generic-optics with 'accountName' instead." #-}

-- | If the account was created successfully, the unique identifier (ID) of the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAccountId :: Lens.Lens' CreateAccountStatus (Lude.Maybe Lude.Text)
casAccountId = Lens.lens (accountId :: CreateAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: CreateAccountStatus)
{-# DEPRECATED casAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casId :: Lens.Lens' CreateAccountStatus (Lude.Maybe Lude.Text)
casId = Lens.lens (id :: CreateAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: CreateAccountStatus)
{-# DEPRECATED casId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
--
-- /Note:/ Consider using 'govCloudAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casGovCloudAccountId :: Lens.Lens' CreateAccountStatus (Lude.Maybe Lude.Text)
casGovCloudAccountId = Lens.lens (govCloudAccountId :: CreateAccountStatus -> Lude.Maybe Lude.Text) (\s a -> s {govCloudAccountId = a} :: CreateAccountStatus)
{-# DEPRECATED casGovCloudAccountId "Use generic-lens or generic-optics with 'govCloudAccountId' instead." #-}

-- | The date and time that the request was made for the account creation.
--
-- /Note:/ Consider using 'requestedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casRequestedTimestamp :: Lens.Lens' CreateAccountStatus (Lude.Maybe Lude.Timestamp)
casRequestedTimestamp = Lens.lens (requestedTimestamp :: CreateAccountStatus -> Lude.Maybe Lude.Timestamp) (\s a -> s {requestedTimestamp = a} :: CreateAccountStatus)
{-# DEPRECATED casRequestedTimestamp "Use generic-lens or generic-optics with 'requestedTimestamp' instead." #-}

instance Lude.FromJSON CreateAccountStatus where
  parseJSON =
    Lude.withObject
      "CreateAccountStatus"
      ( \x ->
          CreateAccountStatus'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CompletedTimestamp")
            Lude.<*> (x Lude..:? "AccountName")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "GovCloudAccountId")
            Lude.<*> (x Lude..:? "RequestedTimestamp")
      )
