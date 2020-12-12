{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BulkEmailStatus
  ( BulkEmailStatus
      ( BulkEmailStatus',
        AccountDailyQuotaExceeded,
        AccountSendingPaused,
        AccountSuspended,
        AccountThrottled,
        ConfigurationSetDoesNotExist,
        ConfigurationSetSendingPaused,
        Failed,
        InvalidParameterValue,
        InvalidSendingPoolName,
        MailFromDomainNotVerified,
        MessageRejected,
        Success,
        TemplateDoesNotExist,
        TransientFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BulkEmailStatus = BulkEmailStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern AccountDailyQuotaExceeded :: BulkEmailStatus
pattern AccountDailyQuotaExceeded = BulkEmailStatus' "AccountDailyQuotaExceeded"

pattern AccountSendingPaused :: BulkEmailStatus
pattern AccountSendingPaused = BulkEmailStatus' "AccountSendingPaused"

pattern AccountSuspended :: BulkEmailStatus
pattern AccountSuspended = BulkEmailStatus' "AccountSuspended"

pattern AccountThrottled :: BulkEmailStatus
pattern AccountThrottled = BulkEmailStatus' "AccountThrottled"

pattern ConfigurationSetDoesNotExist :: BulkEmailStatus
pattern ConfigurationSetDoesNotExist = BulkEmailStatus' "ConfigurationSetDoesNotExist"

pattern ConfigurationSetSendingPaused :: BulkEmailStatus
pattern ConfigurationSetSendingPaused = BulkEmailStatus' "ConfigurationSetSendingPaused"

pattern Failed :: BulkEmailStatus
pattern Failed = BulkEmailStatus' "Failed"

pattern InvalidParameterValue :: BulkEmailStatus
pattern InvalidParameterValue = BulkEmailStatus' "InvalidParameterValue"

pattern InvalidSendingPoolName :: BulkEmailStatus
pattern InvalidSendingPoolName = BulkEmailStatus' "InvalidSendingPoolName"

pattern MailFromDomainNotVerified :: BulkEmailStatus
pattern MailFromDomainNotVerified = BulkEmailStatus' "MailFromDomainNotVerified"

pattern MessageRejected :: BulkEmailStatus
pattern MessageRejected = BulkEmailStatus' "MessageRejected"

pattern Success :: BulkEmailStatus
pattern Success = BulkEmailStatus' "Success"

pattern TemplateDoesNotExist :: BulkEmailStatus
pattern TemplateDoesNotExist = BulkEmailStatus' "TemplateDoesNotExist"

pattern TransientFailure :: BulkEmailStatus
pattern TransientFailure = BulkEmailStatus' "TransientFailure"

{-# COMPLETE
  AccountDailyQuotaExceeded,
  AccountSendingPaused,
  AccountSuspended,
  AccountThrottled,
  ConfigurationSetDoesNotExist,
  ConfigurationSetSendingPaused,
  Failed,
  InvalidParameterValue,
  InvalidSendingPoolName,
  MailFromDomainNotVerified,
  MessageRejected,
  Success,
  TemplateDoesNotExist,
  TransientFailure,
  BulkEmailStatus'
  #-}
