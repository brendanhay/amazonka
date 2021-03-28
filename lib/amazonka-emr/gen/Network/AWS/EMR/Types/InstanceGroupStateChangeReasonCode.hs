{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
  ( InstanceGroupStateChangeReasonCode
    ( InstanceGroupStateChangeReasonCode'
    , InstanceGroupStateChangeReasonCodeInternalError
    , InstanceGroupStateChangeReasonCodeValidationError
    , InstanceGroupStateChangeReasonCodeInstanceFailure
    , InstanceGroupStateChangeReasonCodeClusterTerminated
    , fromInstanceGroupStateChangeReasonCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceGroupStateChangeReasonCode = InstanceGroupStateChangeReasonCode'{fromInstanceGroupStateChangeReasonCode
                                                                                 :: Core.Text}
                                               deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                               Core.Show, Core.Generic)
                                               deriving newtype (Core.IsString, Core.Hashable,
                                                                 Core.NFData, Core.ToJSONKey,
                                                                 Core.FromJSONKey, Core.ToJSON,
                                                                 Core.FromJSON, Core.ToXML,
                                                                 Core.FromXML, Core.ToText,
                                                                 Core.FromText, Core.ToByteString,
                                                                 Core.ToQuery, Core.ToHeader)

pattern InstanceGroupStateChangeReasonCodeInternalError :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCodeInternalError = InstanceGroupStateChangeReasonCode' "INTERNAL_ERROR"

pattern InstanceGroupStateChangeReasonCodeValidationError :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCodeValidationError = InstanceGroupStateChangeReasonCode' "VALIDATION_ERROR"

pattern InstanceGroupStateChangeReasonCodeInstanceFailure :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCodeInstanceFailure = InstanceGroupStateChangeReasonCode' "INSTANCE_FAILURE"

pattern InstanceGroupStateChangeReasonCodeClusterTerminated :: InstanceGroupStateChangeReasonCode
pattern InstanceGroupStateChangeReasonCodeClusterTerminated = InstanceGroupStateChangeReasonCode' "CLUSTER_TERMINATED"

{-# COMPLETE 
  InstanceGroupStateChangeReasonCodeInternalError,

  InstanceGroupStateChangeReasonCodeValidationError,

  InstanceGroupStateChangeReasonCodeInstanceFailure,

  InstanceGroupStateChangeReasonCodeClusterTerminated,
  InstanceGroupStateChangeReasonCode'
  #-}
