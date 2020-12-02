{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceHealthReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceHealthReason where

import Network.AWS.Prelude

data InstanceHealthReason
  = Instance_DeregistrationInProgress
  | Instance_FailedHealthChecks
  | Instance_IPUnusable
  | Instance_InvalidState
  | Instance_NotInUse
  | Instance_NotRegistered
  | Instance_ResponseCodeMismatch
  | Instance_Timeout
  | Lb_InitialHealthChecking
  | Lb_InternalError
  | Lb_RegistrationInProgress
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

instance FromText InstanceHealthReason where
  parser =
    takeLowerText >>= \case
      "instance.deregistrationinprogress" -> pure Instance_DeregistrationInProgress
      "instance.failedhealthchecks" -> pure Instance_FailedHealthChecks
      "instance.ipunusable" -> pure Instance_IPUnusable
      "instance.invalidstate" -> pure Instance_InvalidState
      "instance.notinuse" -> pure Instance_NotInUse
      "instance.notregistered" -> pure Instance_NotRegistered
      "instance.responsecodemismatch" -> pure Instance_ResponseCodeMismatch
      "instance.timeout" -> pure Instance_Timeout
      "lb.initialhealthchecking" -> pure Lb_InitialHealthChecking
      "lb.internalerror" -> pure Lb_InternalError
      "lb.registrationinprogress" -> pure Lb_RegistrationInProgress
      e ->
        fromTextError $
          "Failure parsing InstanceHealthReason from value: '" <> e
            <> "'. Accepted values: instance.deregistrationinprogress, instance.failedhealthchecks, instance.ipunusable, instance.invalidstate, instance.notinuse, instance.notregistered, instance.responsecodemismatch, instance.timeout, lb.initialhealthchecking, lb.internalerror, lb.registrationinprogress"

instance ToText InstanceHealthReason where
  toText = \case
    Instance_DeregistrationInProgress -> "Instance.DeregistrationInProgress"
    Instance_FailedHealthChecks -> "Instance.FailedHealthChecks"
    Instance_IPUnusable -> "Instance.IpUnusable"
    Instance_InvalidState -> "Instance.InvalidState"
    Instance_NotInUse -> "Instance.NotInUse"
    Instance_NotRegistered -> "Instance.NotRegistered"
    Instance_ResponseCodeMismatch -> "Instance.ResponseCodeMismatch"
    Instance_Timeout -> "Instance.Timeout"
    Lb_InitialHealthChecking -> "Lb.InitialHealthChecking"
    Lb_InternalError -> "Lb.InternalError"
    Lb_RegistrationInProgress -> "Lb.RegistrationInProgress"

instance Hashable InstanceHealthReason

instance NFData InstanceHealthReason

instance ToByteString InstanceHealthReason

instance ToQuery InstanceHealthReason

instance ToHeader InstanceHealthReason

instance FromJSON InstanceHealthReason where
  parseJSON = parseJSONText "InstanceHealthReason"
