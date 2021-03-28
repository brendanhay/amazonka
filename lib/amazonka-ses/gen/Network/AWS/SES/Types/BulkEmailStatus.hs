{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.BulkEmailStatus
  ( BulkEmailStatus
    ( BulkEmailStatus'
    , BulkEmailStatusSuccess
    , BulkEmailStatusMessageRejected
    , BulkEmailStatusMailFromDomainNotVerified
    , BulkEmailStatusConfigurationSetDoesNotExist
    , BulkEmailStatusTemplateDoesNotExist
    , BulkEmailStatusAccountSuspended
    , BulkEmailStatusAccountThrottled
    , BulkEmailStatusAccountDailyQuotaExceeded
    , BulkEmailStatusInvalidSendingPoolName
    , BulkEmailStatusAccountSendingPaused
    , BulkEmailStatusConfigurationSetSendingPaused
    , BulkEmailStatusInvalidParameterValue
    , BulkEmailStatusTransientFailure
    , BulkEmailStatusFailed
    , fromBulkEmailStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BulkEmailStatus = BulkEmailStatus'{fromBulkEmailStatus ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern BulkEmailStatusSuccess :: BulkEmailStatus
pattern BulkEmailStatusSuccess = BulkEmailStatus' "Success"

pattern BulkEmailStatusMessageRejected :: BulkEmailStatus
pattern BulkEmailStatusMessageRejected = BulkEmailStatus' "MessageRejected"

pattern BulkEmailStatusMailFromDomainNotVerified :: BulkEmailStatus
pattern BulkEmailStatusMailFromDomainNotVerified = BulkEmailStatus' "MailFromDomainNotVerified"

pattern BulkEmailStatusConfigurationSetDoesNotExist :: BulkEmailStatus
pattern BulkEmailStatusConfigurationSetDoesNotExist = BulkEmailStatus' "ConfigurationSetDoesNotExist"

pattern BulkEmailStatusTemplateDoesNotExist :: BulkEmailStatus
pattern BulkEmailStatusTemplateDoesNotExist = BulkEmailStatus' "TemplateDoesNotExist"

pattern BulkEmailStatusAccountSuspended :: BulkEmailStatus
pattern BulkEmailStatusAccountSuspended = BulkEmailStatus' "AccountSuspended"

pattern BulkEmailStatusAccountThrottled :: BulkEmailStatus
pattern BulkEmailStatusAccountThrottled = BulkEmailStatus' "AccountThrottled"

pattern BulkEmailStatusAccountDailyQuotaExceeded :: BulkEmailStatus
pattern BulkEmailStatusAccountDailyQuotaExceeded = BulkEmailStatus' "AccountDailyQuotaExceeded"

pattern BulkEmailStatusInvalidSendingPoolName :: BulkEmailStatus
pattern BulkEmailStatusInvalidSendingPoolName = BulkEmailStatus' "InvalidSendingPoolName"

pattern BulkEmailStatusAccountSendingPaused :: BulkEmailStatus
pattern BulkEmailStatusAccountSendingPaused = BulkEmailStatus' "AccountSendingPaused"

pattern BulkEmailStatusConfigurationSetSendingPaused :: BulkEmailStatus
pattern BulkEmailStatusConfigurationSetSendingPaused = BulkEmailStatus' "ConfigurationSetSendingPaused"

pattern BulkEmailStatusInvalidParameterValue :: BulkEmailStatus
pattern BulkEmailStatusInvalidParameterValue = BulkEmailStatus' "InvalidParameterValue"

pattern BulkEmailStatusTransientFailure :: BulkEmailStatus
pattern BulkEmailStatusTransientFailure = BulkEmailStatus' "TransientFailure"

pattern BulkEmailStatusFailed :: BulkEmailStatus
pattern BulkEmailStatusFailed = BulkEmailStatus' "Failed"

{-# COMPLETE 
  BulkEmailStatusSuccess,

  BulkEmailStatusMessageRejected,

  BulkEmailStatusMailFromDomainNotVerified,

  BulkEmailStatusConfigurationSetDoesNotExist,

  BulkEmailStatusTemplateDoesNotExist,

  BulkEmailStatusAccountSuspended,

  BulkEmailStatusAccountThrottled,

  BulkEmailStatusAccountDailyQuotaExceeded,

  BulkEmailStatusInvalidSendingPoolName,

  BulkEmailStatusAccountSendingPaused,

  BulkEmailStatusConfigurationSetSendingPaused,

  BulkEmailStatusInvalidParameterValue,

  BulkEmailStatusTransientFailure,

  BulkEmailStatusFailed,
  BulkEmailStatus'
  #-}
