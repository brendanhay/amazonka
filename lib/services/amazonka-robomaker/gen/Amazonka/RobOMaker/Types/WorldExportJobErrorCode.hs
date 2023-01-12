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
-- Module      : Amazonka.RobOMaker.Types.WorldExportJobErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldExportJobErrorCode
  ( WorldExportJobErrorCode
      ( ..,
        WorldExportJobErrorCode_AccessDenied,
        WorldExportJobErrorCode_InternalServiceError,
        WorldExportJobErrorCode_InvalidInput,
        WorldExportJobErrorCode_LimitExceeded,
        WorldExportJobErrorCode_RequestThrottled,
        WorldExportJobErrorCode_ResourceNotFound
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorldExportJobErrorCode = WorldExportJobErrorCode'
  { fromWorldExportJobErrorCode ::
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

pattern WorldExportJobErrorCode_AccessDenied :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_AccessDenied = WorldExportJobErrorCode' "AccessDenied"

pattern WorldExportJobErrorCode_InternalServiceError :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_InternalServiceError = WorldExportJobErrorCode' "InternalServiceError"

pattern WorldExportJobErrorCode_InvalidInput :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_InvalidInput = WorldExportJobErrorCode' "InvalidInput"

pattern WorldExportJobErrorCode_LimitExceeded :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_LimitExceeded = WorldExportJobErrorCode' "LimitExceeded"

pattern WorldExportJobErrorCode_RequestThrottled :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_RequestThrottled = WorldExportJobErrorCode' "RequestThrottled"

pattern WorldExportJobErrorCode_ResourceNotFound :: WorldExportJobErrorCode
pattern WorldExportJobErrorCode_ResourceNotFound = WorldExportJobErrorCode' "ResourceNotFound"

{-# COMPLETE
  WorldExportJobErrorCode_AccessDenied,
  WorldExportJobErrorCode_InternalServiceError,
  WorldExportJobErrorCode_InvalidInput,
  WorldExportJobErrorCode_LimitExceeded,
  WorldExportJobErrorCode_RequestThrottled,
  WorldExportJobErrorCode_ResourceNotFound,
  WorldExportJobErrorCode'
  #-}
