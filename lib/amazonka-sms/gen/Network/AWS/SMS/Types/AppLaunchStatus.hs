{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppLaunchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppLaunchStatus where

import Network.AWS.Prelude

data AppLaunchStatus
  = ConfigurationInProgress
  | ConfigurationInvalid
  | DeltaLaunchFailed
  | DeltaLaunchInProgress
  | LaunchFailed
  | LaunchInProgress
  | LaunchPending
  | Launched
  | PartiallyLaunched
  | ReadyForConfiguration
  | ReadyForLaunch
  | TerminateFailed
  | TerminateInProgress
  | Terminated
  | ValidationInProgress
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

instance FromText AppLaunchStatus where
  parser =
    takeLowerText >>= \case
      "configuration_in_progress" -> pure ConfigurationInProgress
      "configuration_invalid" -> pure ConfigurationInvalid
      "delta_launch_failed" -> pure DeltaLaunchFailed
      "delta_launch_in_progress" -> pure DeltaLaunchInProgress
      "launch_failed" -> pure LaunchFailed
      "launch_in_progress" -> pure LaunchInProgress
      "launch_pending" -> pure LaunchPending
      "launched" -> pure Launched
      "partially_launched" -> pure PartiallyLaunched
      "ready_for_configuration" -> pure ReadyForConfiguration
      "ready_for_launch" -> pure ReadyForLaunch
      "terminate_failed" -> pure TerminateFailed
      "terminate_in_progress" -> pure TerminateInProgress
      "terminated" -> pure Terminated
      "validation_in_progress" -> pure ValidationInProgress
      e ->
        fromTextError $
          "Failure parsing AppLaunchStatus from value: '" <> e
            <> "'. Accepted values: configuration_in_progress, configuration_invalid, delta_launch_failed, delta_launch_in_progress, launch_failed, launch_in_progress, launch_pending, launched, partially_launched, ready_for_configuration, ready_for_launch, terminate_failed, terminate_in_progress, terminated, validation_in_progress"

instance ToText AppLaunchStatus where
  toText = \case
    ConfigurationInProgress -> "CONFIGURATION_IN_PROGRESS"
    ConfigurationInvalid -> "CONFIGURATION_INVALID"
    DeltaLaunchFailed -> "DELTA_LAUNCH_FAILED"
    DeltaLaunchInProgress -> "DELTA_LAUNCH_IN_PROGRESS"
    LaunchFailed -> "LAUNCH_FAILED"
    LaunchInProgress -> "LAUNCH_IN_PROGRESS"
    LaunchPending -> "LAUNCH_PENDING"
    Launched -> "LAUNCHED"
    PartiallyLaunched -> "PARTIALLY_LAUNCHED"
    ReadyForConfiguration -> "READY_FOR_CONFIGURATION"
    ReadyForLaunch -> "READY_FOR_LAUNCH"
    TerminateFailed -> "TERMINATE_FAILED"
    TerminateInProgress -> "TERMINATE_IN_PROGRESS"
    Terminated -> "TERMINATED"
    ValidationInProgress -> "VALIDATION_IN_PROGRESS"

instance Hashable AppLaunchStatus

instance NFData AppLaunchStatus

instance ToByteString AppLaunchStatus

instance ToQuery AppLaunchStatus

instance ToHeader AppLaunchStatus

instance FromJSON AppLaunchStatus where
  parseJSON = parseJSONText "AppLaunchStatus"
