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
-- Module      : Amazonka.RobOMaker.Types.WorldGenerationJobErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldGenerationJobErrorCode
  ( WorldGenerationJobErrorCode
      ( ..,
        WorldGenerationJobErrorCode_AllWorldGenerationFailed,
        WorldGenerationJobErrorCode_InternalServiceError,
        WorldGenerationJobErrorCode_InvalidInput,
        WorldGenerationJobErrorCode_LimitExceeded,
        WorldGenerationJobErrorCode_RequestThrottled,
        WorldGenerationJobErrorCode_ResourceNotFound
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WorldGenerationJobErrorCode = WorldGenerationJobErrorCode'
  { fromWorldGenerationJobErrorCode ::
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

pattern WorldGenerationJobErrorCode_AllWorldGenerationFailed :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_AllWorldGenerationFailed = WorldGenerationJobErrorCode' "AllWorldGenerationFailed"

pattern WorldGenerationJobErrorCode_InternalServiceError :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_InternalServiceError = WorldGenerationJobErrorCode' "InternalServiceError"

pattern WorldGenerationJobErrorCode_InvalidInput :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_InvalidInput = WorldGenerationJobErrorCode' "InvalidInput"

pattern WorldGenerationJobErrorCode_LimitExceeded :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_LimitExceeded = WorldGenerationJobErrorCode' "LimitExceeded"

pattern WorldGenerationJobErrorCode_RequestThrottled :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_RequestThrottled = WorldGenerationJobErrorCode' "RequestThrottled"

pattern WorldGenerationJobErrorCode_ResourceNotFound :: WorldGenerationJobErrorCode
pattern WorldGenerationJobErrorCode_ResourceNotFound = WorldGenerationJobErrorCode' "ResourceNotFound"

{-# COMPLETE
  WorldGenerationJobErrorCode_AllWorldGenerationFailed,
  WorldGenerationJobErrorCode_InternalServiceError,
  WorldGenerationJobErrorCode_InvalidInput,
  WorldGenerationJobErrorCode_LimitExceeded,
  WorldGenerationJobErrorCode_RequestThrottled,
  WorldGenerationJobErrorCode_ResourceNotFound,
  WorldGenerationJobErrorCode'
  #-}
