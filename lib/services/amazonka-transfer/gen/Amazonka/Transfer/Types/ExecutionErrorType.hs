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
-- Module      : Amazonka.Transfer.Types.ExecutionErrorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ExecutionErrorType
  ( ExecutionErrorType
      ( ..,
        ExecutionErrorType_ALREADY_EXISTS,
        ExecutionErrorType_BAD_REQUEST,
        ExecutionErrorType_CUSTOM_STEP_FAILED,
        ExecutionErrorType_INTERNAL_SERVER_ERROR,
        ExecutionErrorType_NOT_FOUND,
        ExecutionErrorType_PERMISSION_DENIED,
        ExecutionErrorType_THROTTLED,
        ExecutionErrorType_TIMEOUT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ExecutionErrorType = ExecutionErrorType'
  { fromExecutionErrorType ::
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

pattern ExecutionErrorType_ALREADY_EXISTS :: ExecutionErrorType
pattern ExecutionErrorType_ALREADY_EXISTS = ExecutionErrorType' "ALREADY_EXISTS"

pattern ExecutionErrorType_BAD_REQUEST :: ExecutionErrorType
pattern ExecutionErrorType_BAD_REQUEST = ExecutionErrorType' "BAD_REQUEST"

pattern ExecutionErrorType_CUSTOM_STEP_FAILED :: ExecutionErrorType
pattern ExecutionErrorType_CUSTOM_STEP_FAILED = ExecutionErrorType' "CUSTOM_STEP_FAILED"

pattern ExecutionErrorType_INTERNAL_SERVER_ERROR :: ExecutionErrorType
pattern ExecutionErrorType_INTERNAL_SERVER_ERROR = ExecutionErrorType' "INTERNAL_SERVER_ERROR"

pattern ExecutionErrorType_NOT_FOUND :: ExecutionErrorType
pattern ExecutionErrorType_NOT_FOUND = ExecutionErrorType' "NOT_FOUND"

pattern ExecutionErrorType_PERMISSION_DENIED :: ExecutionErrorType
pattern ExecutionErrorType_PERMISSION_DENIED = ExecutionErrorType' "PERMISSION_DENIED"

pattern ExecutionErrorType_THROTTLED :: ExecutionErrorType
pattern ExecutionErrorType_THROTTLED = ExecutionErrorType' "THROTTLED"

pattern ExecutionErrorType_TIMEOUT :: ExecutionErrorType
pattern ExecutionErrorType_TIMEOUT = ExecutionErrorType' "TIMEOUT"

{-# COMPLETE
  ExecutionErrorType_ALREADY_EXISTS,
  ExecutionErrorType_BAD_REQUEST,
  ExecutionErrorType_CUSTOM_STEP_FAILED,
  ExecutionErrorType_INTERNAL_SERVER_ERROR,
  ExecutionErrorType_NOT_FOUND,
  ExecutionErrorType_PERMISSION_DENIED,
  ExecutionErrorType_THROTTLED,
  ExecutionErrorType_TIMEOUT,
  ExecutionErrorType'
  #-}
