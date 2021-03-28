{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.FailureType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.FailureType
  ( FailureType
    ( FailureType'
    , FailureTypeUpdateCancelled
    , FailureTypeCancellationFailed
    , FailureTypeRollbackFailed
    , FailureTypeRollbackSuccessful
    , FailureTypeInternalFailure
    , FailureTypeInvalidEnvironmentState
    , FailureTypePermissionsError
    , fromFailureType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype FailureType = FailureType'{fromFailureType :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern FailureTypeUpdateCancelled :: FailureType
pattern FailureTypeUpdateCancelled = FailureType' "UpdateCancelled"

pattern FailureTypeCancellationFailed :: FailureType
pattern FailureTypeCancellationFailed = FailureType' "CancellationFailed"

pattern FailureTypeRollbackFailed :: FailureType
pattern FailureTypeRollbackFailed = FailureType' "RollbackFailed"

pattern FailureTypeRollbackSuccessful :: FailureType
pattern FailureTypeRollbackSuccessful = FailureType' "RollbackSuccessful"

pattern FailureTypeInternalFailure :: FailureType
pattern FailureTypeInternalFailure = FailureType' "InternalFailure"

pattern FailureTypeInvalidEnvironmentState :: FailureType
pattern FailureTypeInvalidEnvironmentState = FailureType' "InvalidEnvironmentState"

pattern FailureTypePermissionsError :: FailureType
pattern FailureTypePermissionsError = FailureType' "PermissionsError"

{-# COMPLETE 
  FailureTypeUpdateCancelled,

  FailureTypeCancellationFailed,

  FailureTypeRollbackFailed,

  FailureTypeRollbackSuccessful,

  FailureTypeInternalFailure,

  FailureTypeInvalidEnvironmentState,

  FailureTypePermissionsError,
  FailureType'
  #-}
