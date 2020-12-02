{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountStatus where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.CreateAccountFailureReason
import Network.AWS.Organizations.Types.CreateAccountState
import Network.AWS.Prelude

-- | Contains the status about a 'CreateAccount' or 'CreateGovCloudAccount' request to create an AWS account or an AWS GovCloud (US) account in an organization.
--
--
--
-- /See:/ 'createAccountStatus' smart constructor.
data CreateAccountStatus = CreateAccountStatus'
  { _casFailureReason ::
      !(Maybe CreateAccountFailureReason),
    _casState :: !(Maybe CreateAccountState),
    _casCompletedTimestamp :: !(Maybe POSIX),
    _casAccountName :: !(Maybe (Sensitive Text)),
    _casAccountId :: !(Maybe Text),
    _casId :: !(Maybe Text),
    _casGovCloudAccountId :: !(Maybe Text),
    _casRequestedTimestamp :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateAccountStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'casFailureReason' - If the request failed, a description of the reason for the failure.     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.     * CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request with the same information.     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.     * GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud (US) Region could not be created because this Region already includes an account with that email address.     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.     * MISSING_BUSINESS_VALIDATION: The AWS account that owns your organization has not received Business Validation.     * MISSING_PAYMENT_INSTRUMENT: You must configure the management account with a valid payment method, such as a credit card.
--
-- * 'casState' - The status of the request.
--
-- * 'casCompletedTimestamp' - The date and time that the account was created and the request completed.
--
-- * 'casAccountName' - The account name given to the account when it was created.
--
-- * 'casAccountId' - If the account was created successfully, the unique identifier (ID) of the new account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
--
-- * 'casId' - The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account. The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- * 'casGovCloudAccountId' - If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
--
-- * 'casRequestedTimestamp' - The date and time that the request was made for the account creation.
createAccountStatus ::
  CreateAccountStatus
createAccountStatus =
  CreateAccountStatus'
    { _casFailureReason = Nothing,
      _casState = Nothing,
      _casCompletedTimestamp = Nothing,
      _casAccountName = Nothing,
      _casAccountId = Nothing,
      _casId = Nothing,
      _casGovCloudAccountId = Nothing,
      _casRequestedTimestamp = Nothing
    }

-- | If the request failed, a description of the reason for the failure.     * ACCOUNT_LIMIT_EXCEEDED: The account could not be created because you have reached the limit on the number of accounts in your organization.     * CONCURRENT_ACCOUNT_MODIFICATION: You already submitted a request with the same information.     * EMAIL_ALREADY_EXISTS: The account could not be created because another AWS account with that email address already exists.     * GOVCLOUD_ACCOUNT_ALREADY_EXISTS: The account in the AWS GovCloud (US) Region could not be created because this Region already includes an account with that email address.     * INVALID_ADDRESS: The account could not be created because the address you provided is not valid.     * INVALID_EMAIL: The account could not be created because the email address you provided is not valid.     * INTERNAL_FAILURE: The account could not be created because of an internal failure. Try again later. If the problem persists, contact Customer Support.     * MISSING_BUSINESS_VALIDATION: The AWS account that owns your organization has not received Business Validation.     * MISSING_PAYMENT_INSTRUMENT: You must configure the management account with a valid payment method, such as a credit card.
casFailureReason :: Lens' CreateAccountStatus (Maybe CreateAccountFailureReason)
casFailureReason = lens _casFailureReason (\s a -> s {_casFailureReason = a})

-- | The status of the request.
casState :: Lens' CreateAccountStatus (Maybe CreateAccountState)
casState = lens _casState (\s a -> s {_casState = a})

-- | The date and time that the account was created and the request completed.
casCompletedTimestamp :: Lens' CreateAccountStatus (Maybe UTCTime)
casCompletedTimestamp = lens _casCompletedTimestamp (\s a -> s {_casCompletedTimestamp = a}) . mapping _Time

-- | The account name given to the account when it was created.
casAccountName :: Lens' CreateAccountStatus (Maybe Text)
casAccountName = lens _casAccountName (\s a -> s {_casAccountName = a}) . mapping _Sensitive

-- | If the account was created successfully, the unique identifier (ID) of the new account. The <http://wikipedia.org/wiki/regex regex pattern> for an account ID string requires exactly 12 digits.
casAccountId :: Lens' CreateAccountStatus (Maybe Text)
casAccountId = lens _casAccountId (\s a -> s {_casAccountId = a})

-- | The unique identifier (ID) that references this request. You get this value from the response of the initial 'CreateAccount' request to create the account. The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
casId :: Lens' CreateAccountStatus (Maybe Text)
casId = lens _casId (\s a -> s {_casId = a})

-- | If the account was created successfully, the unique identifier (ID) of the new account in the AWS GovCloud (US) Region.
casGovCloudAccountId :: Lens' CreateAccountStatus (Maybe Text)
casGovCloudAccountId = lens _casGovCloudAccountId (\s a -> s {_casGovCloudAccountId = a})

-- | The date and time that the request was made for the account creation.
casRequestedTimestamp :: Lens' CreateAccountStatus (Maybe UTCTime)
casRequestedTimestamp = lens _casRequestedTimestamp (\s a -> s {_casRequestedTimestamp = a}) . mapping _Time

instance FromJSON CreateAccountStatus where
  parseJSON =
    withObject
      "CreateAccountStatus"
      ( \x ->
          CreateAccountStatus'
            <$> (x .:? "FailureReason")
            <*> (x .:? "State")
            <*> (x .:? "CompletedTimestamp")
            <*> (x .:? "AccountName")
            <*> (x .:? "AccountId")
            <*> (x .:? "Id")
            <*> (x .:? "GovCloudAccountId")
            <*> (x .:? "RequestedTimestamp")
      )

instance Hashable CreateAccountStatus

instance NFData CreateAccountStatus
