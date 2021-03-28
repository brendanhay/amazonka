{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceHealthReason
  ( InstanceHealthReason
    ( InstanceHealthReason'
    , InstanceHealthReasonLb_RegistrationInProgress
    , InstanceHealthReasonLb_InitialHealthChecking
    , InstanceHealthReasonLb_InternalError
    , InstanceHealthReasonInstance_ResponseCodeMismatch
    , InstanceHealthReasonInstance_Timeout
    , InstanceHealthReasonInstance_FailedHealthChecks
    , InstanceHealthReasonInstance_NotRegistered
    , InstanceHealthReasonInstance_NotInUse
    , InstanceHealthReasonInstance_DeregistrationInProgress
    , InstanceHealthReasonInstance_InvalidState
    , InstanceHealthReasonInstance_IpUnusable
    , fromInstanceHealthReason
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype InstanceHealthReason = InstanceHealthReason'{fromInstanceHealthReason
                                                     :: Core.Text}
                                 deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                 Core.Generic)
                                 deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                   Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                   Core.FromJSON, Core.ToXML, Core.FromXML,
                                                   Core.ToText, Core.FromText, Core.ToByteString,
                                                   Core.ToQuery, Core.ToHeader)

pattern InstanceHealthReasonLb_RegistrationInProgress :: InstanceHealthReason
pattern InstanceHealthReasonLb_RegistrationInProgress = InstanceHealthReason' "Lb.RegistrationInProgress"

pattern InstanceHealthReasonLb_InitialHealthChecking :: InstanceHealthReason
pattern InstanceHealthReasonLb_InitialHealthChecking = InstanceHealthReason' "Lb.InitialHealthChecking"

pattern InstanceHealthReasonLb_InternalError :: InstanceHealthReason
pattern InstanceHealthReasonLb_InternalError = InstanceHealthReason' "Lb.InternalError"

pattern InstanceHealthReasonInstance_ResponseCodeMismatch :: InstanceHealthReason
pattern InstanceHealthReasonInstance_ResponseCodeMismatch = InstanceHealthReason' "Instance.ResponseCodeMismatch"

pattern InstanceHealthReasonInstance_Timeout :: InstanceHealthReason
pattern InstanceHealthReasonInstance_Timeout = InstanceHealthReason' "Instance.Timeout"

pattern InstanceHealthReasonInstance_FailedHealthChecks :: InstanceHealthReason
pattern InstanceHealthReasonInstance_FailedHealthChecks = InstanceHealthReason' "Instance.FailedHealthChecks"

pattern InstanceHealthReasonInstance_NotRegistered :: InstanceHealthReason
pattern InstanceHealthReasonInstance_NotRegistered = InstanceHealthReason' "Instance.NotRegistered"

pattern InstanceHealthReasonInstance_NotInUse :: InstanceHealthReason
pattern InstanceHealthReasonInstance_NotInUse = InstanceHealthReason' "Instance.NotInUse"

pattern InstanceHealthReasonInstance_DeregistrationInProgress :: InstanceHealthReason
pattern InstanceHealthReasonInstance_DeregistrationInProgress = InstanceHealthReason' "Instance.DeregistrationInProgress"

pattern InstanceHealthReasonInstance_InvalidState :: InstanceHealthReason
pattern InstanceHealthReasonInstance_InvalidState = InstanceHealthReason' "Instance.InvalidState"

pattern InstanceHealthReasonInstance_IpUnusable :: InstanceHealthReason
pattern InstanceHealthReasonInstance_IpUnusable = InstanceHealthReason' "Instance.IpUnusable"

{-# COMPLETE 
  InstanceHealthReasonLb_RegistrationInProgress,

  InstanceHealthReasonLb_InitialHealthChecking,

  InstanceHealthReasonLb_InternalError,

  InstanceHealthReasonInstance_ResponseCodeMismatch,

  InstanceHealthReasonInstance_Timeout,

  InstanceHealthReasonInstance_FailedHealthChecks,

  InstanceHealthReasonInstance_NotRegistered,

  InstanceHealthReasonInstance_NotInUse,

  InstanceHealthReasonInstance_DeregistrationInProgress,

  InstanceHealthReasonInstance_InvalidState,

  InstanceHealthReasonInstance_IpUnusable,
  InstanceHealthReason'
  #-}
