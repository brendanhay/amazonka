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
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthReason
  ( InstanceHealthReason
      ( ..,
        InstanceHealthReason_Instance_DeregistrationInProgress,
        InstanceHealthReason_Instance_FailedHealthChecks,
        InstanceHealthReason_Instance_InvalidState,
        InstanceHealthReason_Instance_IpUnusable,
        InstanceHealthReason_Instance_NotInUse,
        InstanceHealthReason_Instance_NotRegistered,
        InstanceHealthReason_Instance_ResponseCodeMismatch,
        InstanceHealthReason_Instance_Timeout,
        InstanceHealthReason_Lb_InitialHealthChecking,
        InstanceHealthReason_Lb_InternalError,
        InstanceHealthReason_Lb_RegistrationInProgress
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InstanceHealthReason = InstanceHealthReason'
  { fromInstanceHealthReason ::
      Core.Text
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

pattern InstanceHealthReason_Instance_DeregistrationInProgress :: InstanceHealthReason
pattern InstanceHealthReason_Instance_DeregistrationInProgress = InstanceHealthReason' "Instance.DeregistrationInProgress"

pattern InstanceHealthReason_Instance_FailedHealthChecks :: InstanceHealthReason
pattern InstanceHealthReason_Instance_FailedHealthChecks = InstanceHealthReason' "Instance.FailedHealthChecks"

pattern InstanceHealthReason_Instance_InvalidState :: InstanceHealthReason
pattern InstanceHealthReason_Instance_InvalidState = InstanceHealthReason' "Instance.InvalidState"

pattern InstanceHealthReason_Instance_IpUnusable :: InstanceHealthReason
pattern InstanceHealthReason_Instance_IpUnusable = InstanceHealthReason' "Instance.IpUnusable"

pattern InstanceHealthReason_Instance_NotInUse :: InstanceHealthReason
pattern InstanceHealthReason_Instance_NotInUse = InstanceHealthReason' "Instance.NotInUse"

pattern InstanceHealthReason_Instance_NotRegistered :: InstanceHealthReason
pattern InstanceHealthReason_Instance_NotRegistered = InstanceHealthReason' "Instance.NotRegistered"

pattern InstanceHealthReason_Instance_ResponseCodeMismatch :: InstanceHealthReason
pattern InstanceHealthReason_Instance_ResponseCodeMismatch = InstanceHealthReason' "Instance.ResponseCodeMismatch"

pattern InstanceHealthReason_Instance_Timeout :: InstanceHealthReason
pattern InstanceHealthReason_Instance_Timeout = InstanceHealthReason' "Instance.Timeout"

pattern InstanceHealthReason_Lb_InitialHealthChecking :: InstanceHealthReason
pattern InstanceHealthReason_Lb_InitialHealthChecking = InstanceHealthReason' "Lb.InitialHealthChecking"

pattern InstanceHealthReason_Lb_InternalError :: InstanceHealthReason
pattern InstanceHealthReason_Lb_InternalError = InstanceHealthReason' "Lb.InternalError"

pattern InstanceHealthReason_Lb_RegistrationInProgress :: InstanceHealthReason
pattern InstanceHealthReason_Lb_RegistrationInProgress = InstanceHealthReason' "Lb.RegistrationInProgress"

{-# COMPLETE
  InstanceHealthReason_Instance_DeregistrationInProgress,
  InstanceHealthReason_Instance_FailedHealthChecks,
  InstanceHealthReason_Instance_InvalidState,
  InstanceHealthReason_Instance_IpUnusable,
  InstanceHealthReason_Instance_NotInUse,
  InstanceHealthReason_Instance_NotRegistered,
  InstanceHealthReason_Instance_ResponseCodeMismatch,
  InstanceHealthReason_Instance_Timeout,
  InstanceHealthReason_Lb_InitialHealthChecking,
  InstanceHealthReason_Lb_InternalError,
  InstanceHealthReason_Lb_RegistrationInProgress,
  InstanceHealthReason'
  #-}
