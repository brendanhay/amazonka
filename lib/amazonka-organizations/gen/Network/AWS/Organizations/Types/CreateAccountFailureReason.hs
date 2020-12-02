{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.CreateAccountFailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.CreateAccountFailureReason where

import Network.AWS.Prelude

data CreateAccountFailureReason
  = AccountLimitExceeded
  | ConcurrentAccountModification
  | EmailAlreadyExists
  | GovcloudAccountAlreadyExists
  | InternalFailure
  | InvalidAddress
  | InvalidEmail
  | MissingBusinessValidation
  | MissingPaymentInstrument
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText CreateAccountFailureReason where
  parser =
    takeLowerText >>= \case
      "account_limit_exceeded" -> pure AccountLimitExceeded
      "concurrent_account_modification" -> pure ConcurrentAccountModification
      "email_already_exists" -> pure EmailAlreadyExists
      "govcloud_account_already_exists" -> pure GovcloudAccountAlreadyExists
      "internal_failure" -> pure InternalFailure
      "invalid_address" -> pure InvalidAddress
      "invalid_email" -> pure InvalidEmail
      "missing_business_validation" -> pure MissingBusinessValidation
      "missing_payment_instrument" -> pure MissingPaymentInstrument
      e ->
        fromTextError $
          "Failure parsing CreateAccountFailureReason from value: '" <> e
            <> "'. Accepted values: account_limit_exceeded, concurrent_account_modification, email_already_exists, govcloud_account_already_exists, internal_failure, invalid_address, invalid_email, missing_business_validation, missing_payment_instrument"

instance ToText CreateAccountFailureReason where
  toText = \case
    AccountLimitExceeded -> "ACCOUNT_LIMIT_EXCEEDED"
    ConcurrentAccountModification -> "CONCURRENT_ACCOUNT_MODIFICATION"
    EmailAlreadyExists -> "EMAIL_ALREADY_EXISTS"
    GovcloudAccountAlreadyExists -> "GOVCLOUD_ACCOUNT_ALREADY_EXISTS"
    InternalFailure -> "INTERNAL_FAILURE"
    InvalidAddress -> "INVALID_ADDRESS"
    InvalidEmail -> "INVALID_EMAIL"
    MissingBusinessValidation -> "MISSING_BUSINESS_VALIDATION"
    MissingPaymentInstrument -> "MISSING_PAYMENT_INSTRUMENT"

instance Hashable CreateAccountFailureReason

instance NFData CreateAccountFailureReason

instance ToByteString CreateAccountFailureReason

instance ToQuery CreateAccountFailureReason

instance ToHeader CreateAccountFailureReason

instance FromJSON CreateAccountFailureReason where
  parseJSON = parseJSONText "CreateAccountFailureReason"
