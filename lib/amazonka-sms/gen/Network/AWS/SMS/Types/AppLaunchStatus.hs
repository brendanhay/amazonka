-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppLaunchStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppLaunchStatus
  ( AppLaunchStatus
      ( AppLaunchStatus',
        ConfigurationInProgress,
        ConfigurationInvalid,
        DeltaLaunchFailed,
        DeltaLaunchInProgress,
        LaunchFailed,
        LaunchInProgress,
        LaunchPending,
        Launched,
        PartiallyLaunched,
        ReadyForConfiguration,
        ReadyForLaunch,
        TerminateFailed,
        TerminateInProgress,
        Terminated,
        ValidationInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AppLaunchStatus = AppLaunchStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ConfigurationInProgress :: AppLaunchStatus
pattern ConfigurationInProgress = AppLaunchStatus' "CONFIGURATION_IN_PROGRESS"

pattern ConfigurationInvalid :: AppLaunchStatus
pattern ConfigurationInvalid = AppLaunchStatus' "CONFIGURATION_INVALID"

pattern DeltaLaunchFailed :: AppLaunchStatus
pattern DeltaLaunchFailed = AppLaunchStatus' "DELTA_LAUNCH_FAILED"

pattern DeltaLaunchInProgress :: AppLaunchStatus
pattern DeltaLaunchInProgress = AppLaunchStatus' "DELTA_LAUNCH_IN_PROGRESS"

pattern LaunchFailed :: AppLaunchStatus
pattern LaunchFailed = AppLaunchStatus' "LAUNCH_FAILED"

pattern LaunchInProgress :: AppLaunchStatus
pattern LaunchInProgress = AppLaunchStatus' "LAUNCH_IN_PROGRESS"

pattern LaunchPending :: AppLaunchStatus
pattern LaunchPending = AppLaunchStatus' "LAUNCH_PENDING"

pattern Launched :: AppLaunchStatus
pattern Launched = AppLaunchStatus' "LAUNCHED"

pattern PartiallyLaunched :: AppLaunchStatus
pattern PartiallyLaunched = AppLaunchStatus' "PARTIALLY_LAUNCHED"

pattern ReadyForConfiguration :: AppLaunchStatus
pattern ReadyForConfiguration = AppLaunchStatus' "READY_FOR_CONFIGURATION"

pattern ReadyForLaunch :: AppLaunchStatus
pattern ReadyForLaunch = AppLaunchStatus' "READY_FOR_LAUNCH"

pattern TerminateFailed :: AppLaunchStatus
pattern TerminateFailed = AppLaunchStatus' "TERMINATE_FAILED"

pattern TerminateInProgress :: AppLaunchStatus
pattern TerminateInProgress = AppLaunchStatus' "TERMINATE_IN_PROGRESS"

pattern Terminated :: AppLaunchStatus
pattern Terminated = AppLaunchStatus' "TERMINATED"

pattern ValidationInProgress :: AppLaunchStatus
pattern ValidationInProgress = AppLaunchStatus' "VALIDATION_IN_PROGRESS"

{-# COMPLETE
  ConfigurationInProgress,
  ConfigurationInvalid,
  DeltaLaunchFailed,
  DeltaLaunchInProgress,
  LaunchFailed,
  LaunchInProgress,
  LaunchPending,
  Launched,
  PartiallyLaunched,
  ReadyForConfiguration,
  ReadyForLaunch,
  TerminateFailed,
  TerminateInProgress,
  Terminated,
  ValidationInProgress,
  AppLaunchStatus'
  #-}
