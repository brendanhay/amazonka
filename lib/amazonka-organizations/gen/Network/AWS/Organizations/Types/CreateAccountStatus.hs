{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Organizations.Types.CreateAccountStatus
  ( CreateAccountStatus (..)
  -- * Smart constructor
  , mkCreateAccountStatus
  -- * Lenses
  , casAccountId
  , casAccountName
  , casCompletedTimestamp
  , casFailureReason
  , casGovCloudAccountId
  , casId
  , casRequestedTimestamp
  , casState
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types.AccountId as Types
import qualified Network.AWS.Organizations.Types.AccountName as Types
import qualified Network.AWS.Organizations.Types.CreateAccountFailureReason as Types
import qualified Network.AWS.Organizations.Types.CreateAccountRequestId as Types
import qualified Network.AWS.Organizations.Types.CreateAccountState as Types
import qualified Network.AWS.Prelude as Core

-- | Contains the status about a 'CreateAccount' or 'CreateGovCloudAccount' request to create an AWS account or an AWS GovCloud (US) account in an organization.
--
-- /See:/ 'mkCreateAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ If the account was created successfully, the unique identifier (ID) of the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
  , accountName :: Core.Maybe Types.AccountName
    -- ^ The account name given to the account when it was created.
  , completedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the account was created and the request completed.
  , failureReason :: Core.Maybe Types.CreateAccountFailureReason
    -- ^ If the request failed, a description of the reason for the failure.
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
  , govCloudAccountId :: Core.Maybe Types.AccountId
    -- ^ If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
  , id :: Core.Maybe Types.CreateAccountRequestId
    -- ^ The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
  , requestedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the request was made for the account creation.
  , state :: Core.Maybe Types.CreateAccountState
    -- ^ The status of the request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateAccountStatus' value with any optional fields omitted.
mkCreateAccountStatus
    :: CreateAccountStatus
mkCreateAccountStatus
  = CreateAccountStatus'{accountId = Core.Nothing,
                         accountName = Core.Nothing, completedTimestamp = Core.Nothing,
                         failureReason = Core.Nothing, govCloudAccountId = Core.Nothing,
                         id = Core.Nothing, requestedTimestamp = Core.Nothing,
                         state = Core.Nothing}

-- | If the account was created successfully, the unique identifier (ID) of the new account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAccountId :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.AccountId)
casAccountId = Lens.field @"accountId"
{-# INLINEABLE casAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The account name given to the account when it was created.
--
-- /Note:/ Consider using 'accountName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casAccountName :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.AccountName)
casAccountName = Lens.field @"accountName"
{-# INLINEABLE casAccountName #-}
{-# DEPRECATED accountName "Use generic-lens or generic-optics with 'accountName' instead"  #-}

-- | The date and time that the account was created and the request completed.
--
-- /Note:/ Consider using 'completedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casCompletedTimestamp :: Lens.Lens' CreateAccountStatus (Core.Maybe Core.NominalDiffTime)
casCompletedTimestamp = Lens.field @"completedTimestamp"
{-# INLINEABLE casCompletedTimestamp #-}
{-# DEPRECATED completedTimestamp "Use generic-lens or generic-optics with 'completedTimestamp' instead"  #-}

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
casFailureReason :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.CreateAccountFailureReason)
casFailureReason = Lens.field @"failureReason"
{-# INLINEABLE casFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
--
-- /Note:/ Consider using 'govCloudAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casGovCloudAccountId :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.AccountId)
casGovCloudAccountId = Lens.field @"govCloudAccountId"
{-# INLINEABLE casGovCloudAccountId #-}
{-# DEPRECATED govCloudAccountId "Use generic-lens or generic-optics with 'govCloudAccountId' instead"  #-}

-- | The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casId :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.CreateAccountRequestId)
casId = Lens.field @"id"
{-# INLINEABLE casId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The date and time that the request was made for the account creation.
--
-- /Note:/ Consider using 'requestedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casRequestedTimestamp :: Lens.Lens' CreateAccountStatus (Core.Maybe Core.NominalDiffTime)
casRequestedTimestamp = Lens.field @"requestedTimestamp"
{-# INLINEABLE casRequestedTimestamp #-}
{-# DEPRECATED requestedTimestamp "Use generic-lens or generic-optics with 'requestedTimestamp' instead"  #-}

-- | The status of the request.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
casState :: Lens.Lens' CreateAccountStatus (Core.Maybe Types.CreateAccountState)
casState = Lens.field @"state"
{-# INLINEABLE casState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromJSON CreateAccountStatus where
        parseJSON
          = Core.withObject "CreateAccountStatus" Core.$
              \ x ->
                CreateAccountStatus' Core.<$>
                  (x Core..:? "AccountId") Core.<*> x Core..:? "AccountName" Core.<*>
                    x Core..:? "CompletedTimestamp"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "GovCloudAccountId"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "RequestedTimestamp"
                    Core.<*> x Core..:? "State"
