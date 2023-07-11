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
-- Module      : Amazonka.ServiceQuotas.Types.ErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceQuotas.Types.ErrorCode
  ( ErrorCode
      ( ..,
        ErrorCode_DEPENDENCY_ACCESS_DENIED_ERROR,
        ErrorCode_DEPENDENCY_SERVICE_ERROR,
        ErrorCode_DEPENDENCY_THROTTLING_ERROR,
        ErrorCode_SERVICE_QUOTA_NOT_AVAILABLE_ERROR
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

pattern ErrorCode_DEPENDENCY_ACCESS_DENIED_ERROR :: ErrorCode
pattern ErrorCode_DEPENDENCY_ACCESS_DENIED_ERROR = ErrorCode' "DEPENDENCY_ACCESS_DENIED_ERROR"

pattern ErrorCode_DEPENDENCY_SERVICE_ERROR :: ErrorCode
pattern ErrorCode_DEPENDENCY_SERVICE_ERROR = ErrorCode' "DEPENDENCY_SERVICE_ERROR"

pattern ErrorCode_DEPENDENCY_THROTTLING_ERROR :: ErrorCode
pattern ErrorCode_DEPENDENCY_THROTTLING_ERROR = ErrorCode' "DEPENDENCY_THROTTLING_ERROR"

pattern ErrorCode_SERVICE_QUOTA_NOT_AVAILABLE_ERROR :: ErrorCode
pattern ErrorCode_SERVICE_QUOTA_NOT_AVAILABLE_ERROR = ErrorCode' "SERVICE_QUOTA_NOT_AVAILABLE_ERROR"

{-# COMPLETE
  ErrorCode_DEPENDENCY_ACCESS_DENIED_ERROR,
  ErrorCode_DEPENDENCY_SERVICE_ERROR,
  ErrorCode_DEPENDENCY_THROTTLING_ERROR,
  ErrorCode_SERVICE_QUOTA_NOT_AVAILABLE_ERROR,
  ErrorCode'
  #-}
