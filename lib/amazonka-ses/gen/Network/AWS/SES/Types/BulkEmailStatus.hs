{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailStatus where

import Network.AWS.Prelude

data BulkEmailStatus
  = AccountDailyQuotaExceeded
  | AccountSendingPaused
  | AccountSuspended
  | AccountThrottled
  | ConfigurationSetDoesNotExist
  | ConfigurationSetSendingPaused
  | Failed
  | InvalidParameterValue
  | InvalidSendingPoolName
  | MailFromDomainNotVerified
  | MessageRejected
  | Success
  | TemplateDoesNotExist
  | TransientFailure
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

instance FromText BulkEmailStatus where
  parser =
    takeLowerText >>= \case
      "accountdailyquotaexceeded" -> pure AccountDailyQuotaExceeded
      "accountsendingpaused" -> pure AccountSendingPaused
      "accountsuspended" -> pure AccountSuspended
      "accountthrottled" -> pure AccountThrottled
      "configurationsetdoesnotexist" -> pure ConfigurationSetDoesNotExist
      "configurationsetsendingpaused" -> pure ConfigurationSetSendingPaused
      "failed" -> pure Failed
      "invalidparametervalue" -> pure InvalidParameterValue
      "invalidsendingpoolname" -> pure InvalidSendingPoolName
      "mailfromdomainnotverified" -> pure MailFromDomainNotVerified
      "messagerejected" -> pure MessageRejected
      "success" -> pure Success
      "templatedoesnotexist" -> pure TemplateDoesNotExist
      "transientfailure" -> pure TransientFailure
      e ->
        fromTextError $
          "Failure parsing BulkEmailStatus from value: '" <> e
            <> "'. Accepted values: accountdailyquotaexceeded, accountsendingpaused, accountsuspended, accountthrottled, configurationsetdoesnotexist, configurationsetsendingpaused, failed, invalidparametervalue, invalidsendingpoolname, mailfromdomainnotverified, messagerejected, success, templatedoesnotexist, transientfailure"

instance ToText BulkEmailStatus where
  toText = \case
    AccountDailyQuotaExceeded -> "AccountDailyQuotaExceeded"
    AccountSendingPaused -> "AccountSendingPaused"
    AccountSuspended -> "AccountSuspended"
    AccountThrottled -> "AccountThrottled"
    ConfigurationSetDoesNotExist -> "ConfigurationSetDoesNotExist"
    ConfigurationSetSendingPaused -> "ConfigurationSetSendingPaused"
    Failed -> "Failed"
    InvalidParameterValue -> "InvalidParameterValue"
    InvalidSendingPoolName -> "InvalidSendingPoolName"
    MailFromDomainNotVerified -> "MailFromDomainNotVerified"
    MessageRejected -> "MessageRejected"
    Success -> "Success"
    TemplateDoesNotExist -> "TemplateDoesNotExist"
    TransientFailure -> "TransientFailure"

instance Hashable BulkEmailStatus

instance NFData BulkEmailStatus

instance ToByteString BulkEmailStatus

instance ToQuery BulkEmailStatus

instance ToHeader BulkEmailStatus

instance FromXML BulkEmailStatus where
  parseXML = parseXMLText "BulkEmailStatus"
