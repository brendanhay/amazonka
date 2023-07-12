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
-- Module      : Amazonka.ELBV2.Types.TargetHealthReasonEnum
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetHealthReasonEnum
  ( TargetHealthReasonEnum
      ( ..,
        TargetHealthReasonEnum_Elb_InitialHealthChecking,
        TargetHealthReasonEnum_Elb_InternalError,
        TargetHealthReasonEnum_Elb_RegistrationInProgress,
        TargetHealthReasonEnum_Target_DeregistrationInProgress,
        TargetHealthReasonEnum_Target_FailedHealthChecks,
        TargetHealthReasonEnum_Target_HealthCheckDisabled,
        TargetHealthReasonEnum_Target_InvalidState,
        TargetHealthReasonEnum_Target_IpUnusable,
        TargetHealthReasonEnum_Target_NotInUse,
        TargetHealthReasonEnum_Target_NotRegistered,
        TargetHealthReasonEnum_Target_ResponseCodeMismatch,
        TargetHealthReasonEnum_Target_Timeout
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetHealthReasonEnum = TargetHealthReasonEnum'
  { fromTargetHealthReasonEnum ::
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

pattern TargetHealthReasonEnum_Elb_InitialHealthChecking :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Elb_InitialHealthChecking = TargetHealthReasonEnum' "Elb.InitialHealthChecking"

pattern TargetHealthReasonEnum_Elb_InternalError :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Elb_InternalError = TargetHealthReasonEnum' "Elb.InternalError"

pattern TargetHealthReasonEnum_Elb_RegistrationInProgress :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Elb_RegistrationInProgress = TargetHealthReasonEnum' "Elb.RegistrationInProgress"

pattern TargetHealthReasonEnum_Target_DeregistrationInProgress :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_DeregistrationInProgress = TargetHealthReasonEnum' "Target.DeregistrationInProgress"

pattern TargetHealthReasonEnum_Target_FailedHealthChecks :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_FailedHealthChecks = TargetHealthReasonEnum' "Target.FailedHealthChecks"

pattern TargetHealthReasonEnum_Target_HealthCheckDisabled :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_HealthCheckDisabled = TargetHealthReasonEnum' "Target.HealthCheckDisabled"

pattern TargetHealthReasonEnum_Target_InvalidState :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_InvalidState = TargetHealthReasonEnum' "Target.InvalidState"

pattern TargetHealthReasonEnum_Target_IpUnusable :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_IpUnusable = TargetHealthReasonEnum' "Target.IpUnusable"

pattern TargetHealthReasonEnum_Target_NotInUse :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_NotInUse = TargetHealthReasonEnum' "Target.NotInUse"

pattern TargetHealthReasonEnum_Target_NotRegistered :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_NotRegistered = TargetHealthReasonEnum' "Target.NotRegistered"

pattern TargetHealthReasonEnum_Target_ResponseCodeMismatch :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_ResponseCodeMismatch = TargetHealthReasonEnum' "Target.ResponseCodeMismatch"

pattern TargetHealthReasonEnum_Target_Timeout :: TargetHealthReasonEnum
pattern TargetHealthReasonEnum_Target_Timeout = TargetHealthReasonEnum' "Target.Timeout"

{-# COMPLETE
  TargetHealthReasonEnum_Elb_InitialHealthChecking,
  TargetHealthReasonEnum_Elb_InternalError,
  TargetHealthReasonEnum_Elb_RegistrationInProgress,
  TargetHealthReasonEnum_Target_DeregistrationInProgress,
  TargetHealthReasonEnum_Target_FailedHealthChecks,
  TargetHealthReasonEnum_Target_HealthCheckDisabled,
  TargetHealthReasonEnum_Target_InvalidState,
  TargetHealthReasonEnum_Target_IpUnusable,
  TargetHealthReasonEnum_Target_NotInUse,
  TargetHealthReasonEnum_Target_NotRegistered,
  TargetHealthReasonEnum_Target_ResponseCodeMismatch,
  TargetHealthReasonEnum_Target_Timeout,
  TargetHealthReasonEnum'
  #-}
