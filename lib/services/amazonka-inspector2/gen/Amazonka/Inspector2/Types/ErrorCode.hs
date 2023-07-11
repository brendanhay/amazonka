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
-- Module      : Amazonka.Inspector2.Types.ErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_ACCESS_DENIED,
        ErrorCode_ACCOUNT_IS_ISOLATED,
        ErrorCode_ALREADY_ENABLED,
        ErrorCode_DISABLE_IN_PROGRESS,
        ErrorCode_DISASSOCIATE_ALL_MEMBERS,
        ErrorCode_ENABLE_IN_PROGRESS,
        ErrorCode_EVENTBRIDGE_THROTTLED,
        ErrorCode_EVENTBRIDGE_UNAVAILABLE,
        ErrorCode_INTERNAL_ERROR,
        ErrorCode_RESOURCE_NOT_FOUND,
        ErrorCode_RESOURCE_SCAN_NOT_DISABLED,
        ErrorCode_SSM_THROTTLED,
        ErrorCode_SSM_UNAVAILABLE,
        ErrorCode_SUSPEND_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ErrorCode = ErrorCode'
  { fromErrorCode ::
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

pattern ErrorCode_ACCESS_DENIED :: ErrorCode
pattern ErrorCode_ACCESS_DENIED = ErrorCode' "ACCESS_DENIED"

pattern ErrorCode_ACCOUNT_IS_ISOLATED :: ErrorCode
pattern ErrorCode_ACCOUNT_IS_ISOLATED = ErrorCode' "ACCOUNT_IS_ISOLATED"

pattern ErrorCode_ALREADY_ENABLED :: ErrorCode
pattern ErrorCode_ALREADY_ENABLED = ErrorCode' "ALREADY_ENABLED"

pattern ErrorCode_DISABLE_IN_PROGRESS :: ErrorCode
pattern ErrorCode_DISABLE_IN_PROGRESS = ErrorCode' "DISABLE_IN_PROGRESS"

pattern ErrorCode_DISASSOCIATE_ALL_MEMBERS :: ErrorCode
pattern ErrorCode_DISASSOCIATE_ALL_MEMBERS = ErrorCode' "DISASSOCIATE_ALL_MEMBERS"

pattern ErrorCode_ENABLE_IN_PROGRESS :: ErrorCode
pattern ErrorCode_ENABLE_IN_PROGRESS = ErrorCode' "ENABLE_IN_PROGRESS"

pattern ErrorCode_EVENTBRIDGE_THROTTLED :: ErrorCode
pattern ErrorCode_EVENTBRIDGE_THROTTLED = ErrorCode' "EVENTBRIDGE_THROTTLED"

pattern ErrorCode_EVENTBRIDGE_UNAVAILABLE :: ErrorCode
pattern ErrorCode_EVENTBRIDGE_UNAVAILABLE = ErrorCode' "EVENTBRIDGE_UNAVAILABLE"

pattern ErrorCode_INTERNAL_ERROR :: ErrorCode
pattern ErrorCode_INTERNAL_ERROR = ErrorCode' "INTERNAL_ERROR"

pattern ErrorCode_RESOURCE_NOT_FOUND :: ErrorCode
pattern ErrorCode_RESOURCE_NOT_FOUND = ErrorCode' "RESOURCE_NOT_FOUND"

pattern ErrorCode_RESOURCE_SCAN_NOT_DISABLED :: ErrorCode
pattern ErrorCode_RESOURCE_SCAN_NOT_DISABLED = ErrorCode' "RESOURCE_SCAN_NOT_DISABLED"

pattern ErrorCode_SSM_THROTTLED :: ErrorCode
pattern ErrorCode_SSM_THROTTLED = ErrorCode' "SSM_THROTTLED"

pattern ErrorCode_SSM_UNAVAILABLE :: ErrorCode
pattern ErrorCode_SSM_UNAVAILABLE = ErrorCode' "SSM_UNAVAILABLE"

pattern ErrorCode_SUSPEND_IN_PROGRESS :: ErrorCode
pattern ErrorCode_SUSPEND_IN_PROGRESS = ErrorCode' "SUSPEND_IN_PROGRESS"

{-# COMPLETE
  ErrorCode_ACCESS_DENIED,
  ErrorCode_ACCOUNT_IS_ISOLATED,
  ErrorCode_ALREADY_ENABLED,
  ErrorCode_DISABLE_IN_PROGRESS,
  ErrorCode_DISASSOCIATE_ALL_MEMBERS,
  ErrorCode_ENABLE_IN_PROGRESS,
  ErrorCode_EVENTBRIDGE_THROTTLED,
  ErrorCode_EVENTBRIDGE_UNAVAILABLE,
  ErrorCode_INTERNAL_ERROR,
  ErrorCode_RESOURCE_NOT_FOUND,
  ErrorCode_RESOURCE_SCAN_NOT_DISABLED,
  ErrorCode_SSM_THROTTLED,
  ErrorCode_SSM_UNAVAILABLE,
  ErrorCode_SUSPEND_IN_PROGRESS,
  ErrorCode'
  #-}
