{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.BulkEmailStatus
  ( BulkEmailStatus
      ( ..,
        BulkEmailStatus_AccountDailyQuotaExceeded,
        BulkEmailStatus_AccountSendingPaused,
        BulkEmailStatus_AccountSuspended,
        BulkEmailStatus_AccountThrottled,
        BulkEmailStatus_ConfigurationSetDoesNotExist,
        BulkEmailStatus_ConfigurationSetSendingPaused,
        BulkEmailStatus_Failed,
        BulkEmailStatus_InvalidParameterValue,
        BulkEmailStatus_InvalidSendingPoolName,
        BulkEmailStatus_MailFromDomainNotVerified,
        BulkEmailStatus_MessageRejected,
        BulkEmailStatus_Success,
        BulkEmailStatus_TemplateDoesNotExist,
        BulkEmailStatus_TransientFailure
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BulkEmailStatus = BulkEmailStatus'
  { fromBulkEmailStatus ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern BulkEmailStatus_AccountDailyQuotaExceeded :: BulkEmailStatus
pattern BulkEmailStatus_AccountDailyQuotaExceeded = BulkEmailStatus' "AccountDailyQuotaExceeded"

pattern BulkEmailStatus_AccountSendingPaused :: BulkEmailStatus
pattern BulkEmailStatus_AccountSendingPaused = BulkEmailStatus' "AccountSendingPaused"

pattern BulkEmailStatus_AccountSuspended :: BulkEmailStatus
pattern BulkEmailStatus_AccountSuspended = BulkEmailStatus' "AccountSuspended"

pattern BulkEmailStatus_AccountThrottled :: BulkEmailStatus
pattern BulkEmailStatus_AccountThrottled = BulkEmailStatus' "AccountThrottled"

pattern BulkEmailStatus_ConfigurationSetDoesNotExist :: BulkEmailStatus
pattern BulkEmailStatus_ConfigurationSetDoesNotExist = BulkEmailStatus' "ConfigurationSetDoesNotExist"

pattern BulkEmailStatus_ConfigurationSetSendingPaused :: BulkEmailStatus
pattern BulkEmailStatus_ConfigurationSetSendingPaused = BulkEmailStatus' "ConfigurationSetSendingPaused"

pattern BulkEmailStatus_Failed :: BulkEmailStatus
pattern BulkEmailStatus_Failed = BulkEmailStatus' "Failed"

pattern BulkEmailStatus_InvalidParameterValue :: BulkEmailStatus
pattern BulkEmailStatus_InvalidParameterValue = BulkEmailStatus' "InvalidParameterValue"

pattern BulkEmailStatus_InvalidSendingPoolName :: BulkEmailStatus
pattern BulkEmailStatus_InvalidSendingPoolName = BulkEmailStatus' "InvalidSendingPoolName"

pattern BulkEmailStatus_MailFromDomainNotVerified :: BulkEmailStatus
pattern BulkEmailStatus_MailFromDomainNotVerified = BulkEmailStatus' "MailFromDomainNotVerified"

pattern BulkEmailStatus_MessageRejected :: BulkEmailStatus
pattern BulkEmailStatus_MessageRejected = BulkEmailStatus' "MessageRejected"

pattern BulkEmailStatus_Success :: BulkEmailStatus
pattern BulkEmailStatus_Success = BulkEmailStatus' "Success"

pattern BulkEmailStatus_TemplateDoesNotExist :: BulkEmailStatus
pattern BulkEmailStatus_TemplateDoesNotExist = BulkEmailStatus' "TemplateDoesNotExist"

pattern BulkEmailStatus_TransientFailure :: BulkEmailStatus
pattern BulkEmailStatus_TransientFailure = BulkEmailStatus' "TransientFailure"

{-# COMPLETE
  BulkEmailStatus_AccountDailyQuotaExceeded,
  BulkEmailStatus_AccountSendingPaused,
  BulkEmailStatus_AccountSuspended,
  BulkEmailStatus_AccountThrottled,
  BulkEmailStatus_ConfigurationSetDoesNotExist,
  BulkEmailStatus_ConfigurationSetSendingPaused,
  BulkEmailStatus_Failed,
  BulkEmailStatus_InvalidParameterValue,
  BulkEmailStatus_InvalidSendingPoolName,
  BulkEmailStatus_MailFromDomainNotVerified,
  BulkEmailStatus_MessageRejected,
  BulkEmailStatus_Success,
  BulkEmailStatus_TemplateDoesNotExist,
  BulkEmailStatus_TransientFailure,
  BulkEmailStatus'
  #-}
