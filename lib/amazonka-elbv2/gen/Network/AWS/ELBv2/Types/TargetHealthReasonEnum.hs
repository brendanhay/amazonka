{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetHealthReasonEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetHealthReasonEnum where

import Network.AWS.Prelude

data TargetHealthReasonEnum
  = Elb_InitialHealthChecking
  | Elb_InternalError
  | Elb_RegistrationInProgress
  | Target_DeregistrationInProgress
  | Target_FailedHealthChecks
  | Target_HealthCheckDisabled
  | Target_IPUnusable
  | Target_InvalidState
  | Target_NotInUse
  | Target_NotRegistered
  | Target_ResponseCodeMismatch
  | Target_Timeout
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText TargetHealthReasonEnum where
  parser =
    takeLowerText >>= \case
      "elb.initialhealthchecking" -> pure Elb_InitialHealthChecking
      "elb.internalerror" -> pure Elb_InternalError
      "elb.registrationinprogress" -> pure Elb_RegistrationInProgress
      "target.deregistrationinprogress" -> pure Target_DeregistrationInProgress
      "target.failedhealthchecks" -> pure Target_FailedHealthChecks
      "target.healthcheckdisabled" -> pure Target_HealthCheckDisabled
      "target.ipunusable" -> pure Target_IPUnusable
      "target.invalidstate" -> pure Target_InvalidState
      "target.notinuse" -> pure Target_NotInUse
      "target.notregistered" -> pure Target_NotRegistered
      "target.responsecodemismatch" -> pure Target_ResponseCodeMismatch
      "target.timeout" -> pure Target_Timeout
      e ->
        fromTextError $
          "Failure parsing TargetHealthReasonEnum from value: '" <> e
            <> "'. Accepted values: elb.initialhealthchecking, elb.internalerror, elb.registrationinprogress, target.deregistrationinprogress, target.failedhealthchecks, target.healthcheckdisabled, target.ipunusable, target.invalidstate, target.notinuse, target.notregistered, target.responsecodemismatch, target.timeout"

instance ToText TargetHealthReasonEnum where
  toText = \case
    Elb_InitialHealthChecking -> "Elb.InitialHealthChecking"
    Elb_InternalError -> "Elb.InternalError"
    Elb_RegistrationInProgress -> "Elb.RegistrationInProgress"
    Target_DeregistrationInProgress -> "Target.DeregistrationInProgress"
    Target_FailedHealthChecks -> "Target.FailedHealthChecks"
    Target_HealthCheckDisabled -> "Target.HealthCheckDisabled"
    Target_IPUnusable -> "Target.IpUnusable"
    Target_InvalidState -> "Target.InvalidState"
    Target_NotInUse -> "Target.NotInUse"
    Target_NotRegistered -> "Target.NotRegistered"
    Target_ResponseCodeMismatch -> "Target.ResponseCodeMismatch"
    Target_Timeout -> "Target.Timeout"

instance Hashable TargetHealthReasonEnum

instance NFData TargetHealthReasonEnum

instance ToByteString TargetHealthReasonEnum

instance ToQuery TargetHealthReasonEnum

instance ToHeader TargetHealthReasonEnum

instance FromXML TargetHealthReasonEnum where
  parseXML = parseXMLText "TargetHealthReasonEnum"
