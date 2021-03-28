{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceStateChangeReasonCode
  ( InstanceStateChangeReasonCode
    ( InstanceStateChangeReasonCode'
    , InstanceStateChangeReasonCodeInternalError
    , InstanceStateChangeReasonCodeValidationError
    , InstanceStateChangeReasonCodeInstanceFailure
    , InstanceStateChangeReasonCodeBootstrapFailure
    , InstanceStateChangeReasonCodeClusterTerminated
    , fromInstanceStateChangeReasonCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceStateChangeReasonCode = InstanceStateChangeReasonCode'{fromInstanceStateChangeReasonCode
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern InstanceStateChangeReasonCodeInternalError :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCodeInternalError = InstanceStateChangeReasonCode' "INTERNAL_ERROR"

pattern InstanceStateChangeReasonCodeValidationError :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCodeValidationError = InstanceStateChangeReasonCode' "VALIDATION_ERROR"

pattern InstanceStateChangeReasonCodeInstanceFailure :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCodeInstanceFailure = InstanceStateChangeReasonCode' "INSTANCE_FAILURE"

pattern InstanceStateChangeReasonCodeBootstrapFailure :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCodeBootstrapFailure = InstanceStateChangeReasonCode' "BOOTSTRAP_FAILURE"

pattern InstanceStateChangeReasonCodeClusterTerminated :: InstanceStateChangeReasonCode
pattern InstanceStateChangeReasonCodeClusterTerminated = InstanceStateChangeReasonCode' "CLUSTER_TERMINATED"

{-# COMPLETE 
  InstanceStateChangeReasonCodeInternalError,

  InstanceStateChangeReasonCodeValidationError,

  InstanceStateChangeReasonCodeInstanceFailure,

  InstanceStateChangeReasonCodeBootstrapFailure,

  InstanceStateChangeReasonCodeClusterTerminated,
  InstanceStateChangeReasonCode'
  #-}
