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
-- Module      : Amazonka.Nimble.Types.LaunchProfileValidationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.LaunchProfileValidationState
  ( LaunchProfileValidationState
      ( ..,
        LaunchProfileValidationState_VALIDATION_FAILED,
        LaunchProfileValidationState_VALIDATION_FAILED_INTERNAL_SERVER_ERROR,
        LaunchProfileValidationState_VALIDATION_IN_PROGRESS,
        LaunchProfileValidationState_VALIDATION_NOT_STARTED,
        LaunchProfileValidationState_VALIDATION_SUCCESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LaunchProfileValidationState = LaunchProfileValidationState'
  { fromLaunchProfileValidationState ::
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

pattern LaunchProfileValidationState_VALIDATION_FAILED :: LaunchProfileValidationState
pattern LaunchProfileValidationState_VALIDATION_FAILED = LaunchProfileValidationState' "VALIDATION_FAILED"

pattern LaunchProfileValidationState_VALIDATION_FAILED_INTERNAL_SERVER_ERROR :: LaunchProfileValidationState
pattern LaunchProfileValidationState_VALIDATION_FAILED_INTERNAL_SERVER_ERROR = LaunchProfileValidationState' "VALIDATION_FAILED_INTERNAL_SERVER_ERROR"

pattern LaunchProfileValidationState_VALIDATION_IN_PROGRESS :: LaunchProfileValidationState
pattern LaunchProfileValidationState_VALIDATION_IN_PROGRESS = LaunchProfileValidationState' "VALIDATION_IN_PROGRESS"

pattern LaunchProfileValidationState_VALIDATION_NOT_STARTED :: LaunchProfileValidationState
pattern LaunchProfileValidationState_VALIDATION_NOT_STARTED = LaunchProfileValidationState' "VALIDATION_NOT_STARTED"

pattern LaunchProfileValidationState_VALIDATION_SUCCESS :: LaunchProfileValidationState
pattern LaunchProfileValidationState_VALIDATION_SUCCESS = LaunchProfileValidationState' "VALIDATION_SUCCESS"

{-# COMPLETE
  LaunchProfileValidationState_VALIDATION_FAILED,
  LaunchProfileValidationState_VALIDATION_FAILED_INTERNAL_SERVER_ERROR,
  LaunchProfileValidationState_VALIDATION_IN_PROGRESS,
  LaunchProfileValidationState_VALIDATION_NOT_STARTED,
  LaunchProfileValidationState_VALIDATION_SUCCESS,
  LaunchProfileValidationState'
  #-}
