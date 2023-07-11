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
-- Module      : Amazonka.SESV2.Types.BulkEmailStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.BulkEmailStatus
  ( BulkEmailStatus
      ( ..,
        BulkEmailStatus_ACCOUNT_DAILY_QUOTA_EXCEEDED,
        BulkEmailStatus_ACCOUNT_SENDING_PAUSED,
        BulkEmailStatus_ACCOUNT_SUSPENDED,
        BulkEmailStatus_ACCOUNT_THROTTLED,
        BulkEmailStatus_CONFIGURATION_SET_NOT_FOUND,
        BulkEmailStatus_CONFIGURATION_SET_SENDING_PAUSED,
        BulkEmailStatus_FAILED,
        BulkEmailStatus_INVALID_PARAMETER,
        BulkEmailStatus_INVALID_SENDING_POOL_NAME,
        BulkEmailStatus_MAIL_FROM_DOMAIN_NOT_VERIFIED,
        BulkEmailStatus_MESSAGE_REJECTED,
        BulkEmailStatus_SUCCESS,
        BulkEmailStatus_TEMPLATE_NOT_FOUND,
        BulkEmailStatus_TRANSIENT_FAILURE
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

pattern BulkEmailStatus_ACCOUNT_DAILY_QUOTA_EXCEEDED :: BulkEmailStatus
pattern BulkEmailStatus_ACCOUNT_DAILY_QUOTA_EXCEEDED = BulkEmailStatus' "ACCOUNT_DAILY_QUOTA_EXCEEDED"

pattern BulkEmailStatus_ACCOUNT_SENDING_PAUSED :: BulkEmailStatus
pattern BulkEmailStatus_ACCOUNT_SENDING_PAUSED = BulkEmailStatus' "ACCOUNT_SENDING_PAUSED"

pattern BulkEmailStatus_ACCOUNT_SUSPENDED :: BulkEmailStatus
pattern BulkEmailStatus_ACCOUNT_SUSPENDED = BulkEmailStatus' "ACCOUNT_SUSPENDED"

pattern BulkEmailStatus_ACCOUNT_THROTTLED :: BulkEmailStatus
pattern BulkEmailStatus_ACCOUNT_THROTTLED = BulkEmailStatus' "ACCOUNT_THROTTLED"

pattern BulkEmailStatus_CONFIGURATION_SET_NOT_FOUND :: BulkEmailStatus
pattern BulkEmailStatus_CONFIGURATION_SET_NOT_FOUND = BulkEmailStatus' "CONFIGURATION_SET_NOT_FOUND"

pattern BulkEmailStatus_CONFIGURATION_SET_SENDING_PAUSED :: BulkEmailStatus
pattern BulkEmailStatus_CONFIGURATION_SET_SENDING_PAUSED = BulkEmailStatus' "CONFIGURATION_SET_SENDING_PAUSED"

pattern BulkEmailStatus_FAILED :: BulkEmailStatus
pattern BulkEmailStatus_FAILED = BulkEmailStatus' "FAILED"

pattern BulkEmailStatus_INVALID_PARAMETER :: BulkEmailStatus
pattern BulkEmailStatus_INVALID_PARAMETER = BulkEmailStatus' "INVALID_PARAMETER"

pattern BulkEmailStatus_INVALID_SENDING_POOL_NAME :: BulkEmailStatus
pattern BulkEmailStatus_INVALID_SENDING_POOL_NAME = BulkEmailStatus' "INVALID_SENDING_POOL_NAME"

pattern BulkEmailStatus_MAIL_FROM_DOMAIN_NOT_VERIFIED :: BulkEmailStatus
pattern BulkEmailStatus_MAIL_FROM_DOMAIN_NOT_VERIFIED = BulkEmailStatus' "MAIL_FROM_DOMAIN_NOT_VERIFIED"

pattern BulkEmailStatus_MESSAGE_REJECTED :: BulkEmailStatus
pattern BulkEmailStatus_MESSAGE_REJECTED = BulkEmailStatus' "MESSAGE_REJECTED"

pattern BulkEmailStatus_SUCCESS :: BulkEmailStatus
pattern BulkEmailStatus_SUCCESS = BulkEmailStatus' "SUCCESS"

pattern BulkEmailStatus_TEMPLATE_NOT_FOUND :: BulkEmailStatus
pattern BulkEmailStatus_TEMPLATE_NOT_FOUND = BulkEmailStatus' "TEMPLATE_NOT_FOUND"

pattern BulkEmailStatus_TRANSIENT_FAILURE :: BulkEmailStatus
pattern BulkEmailStatus_TRANSIENT_FAILURE = BulkEmailStatus' "TRANSIENT_FAILURE"

{-# COMPLETE
  BulkEmailStatus_ACCOUNT_DAILY_QUOTA_EXCEEDED,
  BulkEmailStatus_ACCOUNT_SENDING_PAUSED,
  BulkEmailStatus_ACCOUNT_SUSPENDED,
  BulkEmailStatus_ACCOUNT_THROTTLED,
  BulkEmailStatus_CONFIGURATION_SET_NOT_FOUND,
  BulkEmailStatus_CONFIGURATION_SET_SENDING_PAUSED,
  BulkEmailStatus_FAILED,
  BulkEmailStatus_INVALID_PARAMETER,
  BulkEmailStatus_INVALID_SENDING_POOL_NAME,
  BulkEmailStatus_MAIL_FROM_DOMAIN_NOT_VERIFIED,
  BulkEmailStatus_MESSAGE_REJECTED,
  BulkEmailStatus_SUCCESS,
  BulkEmailStatus_TEMPLATE_NOT_FOUND,
  BulkEmailStatus_TRANSIENT_FAILURE,
  BulkEmailStatus'
  #-}
