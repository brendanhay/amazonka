-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.FailureType
  ( FailureType
      ( FailureType',
        CancellationFailed,
        InternalFailure,
        InvalidEnvironmentState,
        PermissionsError,
        RollbackFailed,
        RollbackSuccessful,
        UpdateCancelled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FailureType = FailureType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CancellationFailed :: FailureType
pattern CancellationFailed = FailureType' "CancellationFailed"

pattern InternalFailure :: FailureType
pattern InternalFailure = FailureType' "InternalFailure"

pattern InvalidEnvironmentState :: FailureType
pattern InvalidEnvironmentState = FailureType' "InvalidEnvironmentState"

pattern PermissionsError :: FailureType
pattern PermissionsError = FailureType' "PermissionsError"

pattern RollbackFailed :: FailureType
pattern RollbackFailed = FailureType' "RollbackFailed"

pattern RollbackSuccessful :: FailureType
pattern RollbackSuccessful = FailureType' "RollbackSuccessful"

pattern UpdateCancelled :: FailureType
pattern UpdateCancelled = FailureType' "UpdateCancelled"

{-# COMPLETE
  CancellationFailed,
  InternalFailure,
  InvalidEnvironmentState,
  PermissionsError,
  RollbackFailed,
  RollbackSuccessful,
  UpdateCancelled,
  FailureType'
  #-}
