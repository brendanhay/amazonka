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
-- Module      : Network.AWS.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.FailureType
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

import qualified Network.AWS.Core as Core

newtype FailureType = FailureType'
  { fromFailureType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
