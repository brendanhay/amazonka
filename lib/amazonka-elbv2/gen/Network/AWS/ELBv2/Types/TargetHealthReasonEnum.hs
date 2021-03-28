{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthReasonEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.TargetHealthReasonEnum
  ( TargetHealthReasonEnum
    ( TargetHealthReasonEnum'
    , TargetHealthReasonEnumElb_RegistrationInProgress
    , TargetHealthReasonEnumElb_InitialHealthChecking
    , TargetHealthReasonEnumTarget_ResponseCodeMismatch
    , TargetHealthReasonEnumTarget_Timeout
    , TargetHealthReasonEnumTarget_FailedHealthChecks
    , TargetHealthReasonEnumTarget_NotRegistered
    , TargetHealthReasonEnumTarget_NotInUse
    , TargetHealthReasonEnumTarget_DeregistrationInProgress
    , TargetHealthReasonEnumTarget_InvalidState
    , TargetHealthReasonEnumTarget_IpUnusable
    , TargetHealthReasonEnumTarget_HealthCheckDisabled
    , TargetHealthReasonEnumElb_InternalError
    , fromTargetHealthReasonEnum
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TargetHealthReasonEnum = TargetHealthReasonEnum'{fromTargetHealthReasonEnum
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern TargetHealthReasonEnumElb_RegistrationInProgress :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumElb_RegistrationInProgress = TargetHealthReasonEnum' "Elb.RegistrationInProgress"

pattern TargetHealthReasonEnumElb_InitialHealthChecking :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumElb_InitialHealthChecking = TargetHealthReasonEnum' "Elb.InitialHealthChecking"

pattern TargetHealthReasonEnumTarget_ResponseCodeMismatch :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_ResponseCodeMismatch = TargetHealthReasonEnum' "Target.ResponseCodeMismatch"

pattern TargetHealthReasonEnumTarget_Timeout :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_Timeout = TargetHealthReasonEnum' "Target.Timeout"

pattern TargetHealthReasonEnumTarget_FailedHealthChecks :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_FailedHealthChecks = TargetHealthReasonEnum' "Target.FailedHealthChecks"

pattern TargetHealthReasonEnumTarget_NotRegistered :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_NotRegistered = TargetHealthReasonEnum' "Target.NotRegistered"

pattern TargetHealthReasonEnumTarget_NotInUse :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_NotInUse = TargetHealthReasonEnum' "Target.NotInUse"

pattern TargetHealthReasonEnumTarget_DeregistrationInProgress :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_DeregistrationInProgress = TargetHealthReasonEnum' "Target.DeregistrationInProgress"

pattern TargetHealthReasonEnumTarget_InvalidState :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_InvalidState = TargetHealthReasonEnum' "Target.InvalidState"

pattern TargetHealthReasonEnumTarget_IpUnusable :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_IpUnusable = TargetHealthReasonEnum' "Target.IpUnusable"

pattern TargetHealthReasonEnumTarget_HealthCheckDisabled :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumTarget_HealthCheckDisabled = TargetHealthReasonEnum' "Target.HealthCheckDisabled"

pattern TargetHealthReasonEnumElb_InternalError :: TargetHealthReasonEnum
pattern TargetHealthReasonEnumElb_InternalError = TargetHealthReasonEnum' "Elb.InternalError"

{-# COMPLETE 
  TargetHealthReasonEnumElb_RegistrationInProgress,

  TargetHealthReasonEnumElb_InitialHealthChecking,

  TargetHealthReasonEnumTarget_ResponseCodeMismatch,

  TargetHealthReasonEnumTarget_Timeout,

  TargetHealthReasonEnumTarget_FailedHealthChecks,

  TargetHealthReasonEnumTarget_NotRegistered,

  TargetHealthReasonEnumTarget_NotInUse,

  TargetHealthReasonEnumTarget_DeregistrationInProgress,

  TargetHealthReasonEnumTarget_InvalidState,

  TargetHealthReasonEnumTarget_IpUnusable,

  TargetHealthReasonEnumTarget_HealthCheckDisabled,

  TargetHealthReasonEnumElb_InternalError,
  TargetHealthReasonEnum'
  #-}
