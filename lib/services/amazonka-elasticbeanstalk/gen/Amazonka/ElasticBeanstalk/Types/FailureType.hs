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
-- Module      : Amazonka.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.FailureType
  ( FailureType
      ( ..,
        FailureType_CancellationFailed,
        FailureType_InternalFailure,
        FailureType_InvalidEnvironmentState,
        FailureType_PermissionsError,
        FailureType_RollbackFailed,
        FailureType_RollbackSuccessful,
        FailureType_UpdateCancelled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailureType = FailureType'
  { fromFailureType ::
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

pattern FailureType_CancellationFailed :: FailureType
pattern FailureType_CancellationFailed = FailureType' "CancellationFailed"

pattern FailureType_InternalFailure :: FailureType
pattern FailureType_InternalFailure = FailureType' "InternalFailure"

pattern FailureType_InvalidEnvironmentState :: FailureType
pattern FailureType_InvalidEnvironmentState = FailureType' "InvalidEnvironmentState"

pattern FailureType_PermissionsError :: FailureType
pattern FailureType_PermissionsError = FailureType' "PermissionsError"

pattern FailureType_RollbackFailed :: FailureType
pattern FailureType_RollbackFailed = FailureType' "RollbackFailed"

pattern FailureType_RollbackSuccessful :: FailureType
pattern FailureType_RollbackSuccessful = FailureType' "RollbackSuccessful"

pattern FailureType_UpdateCancelled :: FailureType
pattern FailureType_UpdateCancelled = FailureType' "UpdateCancelled"

{-# COMPLETE
  FailureType_CancellationFailed,
  FailureType_InternalFailure,
  FailureType_InvalidEnvironmentState,
  FailureType_PermissionsError,
  FailureType_RollbackFailed,
  FailureType_RollbackSuccessful,
  FailureType_UpdateCancelled,
  FailureType'
  #-}
