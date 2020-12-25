{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        AppLaunchStatusReadyForConfiguration,
        AppLaunchStatusConfigurationInProgress,
        AppLaunchStatusConfigurationInvalid,
        AppLaunchStatusReadyForLaunch,
        AppLaunchStatusValidationInProgress,
        AppLaunchStatusLaunchPending,
        AppLaunchStatusLaunchInProgress,
        AppLaunchStatusLaunched,
        AppLaunchStatusPartiallyLaunched,
        AppLaunchStatusDeltaLaunchInProgress,
        AppLaunchStatusDeltaLaunchFailed,
        AppLaunchStatusLaunchFailed,
        AppLaunchStatusTerminateInProgress,
        AppLaunchStatusTerminateFailed,
        AppLaunchStatusTerminated,
        fromAppLaunchStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AppLaunchStatus = AppLaunchStatus'
  { fromAppLaunchStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AppLaunchStatusReadyForConfiguration :: AppLaunchStatus
pattern AppLaunchStatusReadyForConfiguration = AppLaunchStatus' "READY_FOR_CONFIGURATION"

pattern AppLaunchStatusConfigurationInProgress :: AppLaunchStatus
pattern AppLaunchStatusConfigurationInProgress = AppLaunchStatus' "CONFIGURATION_IN_PROGRESS"

pattern AppLaunchStatusConfigurationInvalid :: AppLaunchStatus
pattern AppLaunchStatusConfigurationInvalid = AppLaunchStatus' "CONFIGURATION_INVALID"

pattern AppLaunchStatusReadyForLaunch :: AppLaunchStatus
pattern AppLaunchStatusReadyForLaunch = AppLaunchStatus' "READY_FOR_LAUNCH"

pattern AppLaunchStatusValidationInProgress :: AppLaunchStatus
pattern AppLaunchStatusValidationInProgress = AppLaunchStatus' "VALIDATION_IN_PROGRESS"

pattern AppLaunchStatusLaunchPending :: AppLaunchStatus
pattern AppLaunchStatusLaunchPending = AppLaunchStatus' "LAUNCH_PENDING"

pattern AppLaunchStatusLaunchInProgress :: AppLaunchStatus
pattern AppLaunchStatusLaunchInProgress = AppLaunchStatus' "LAUNCH_IN_PROGRESS"

pattern AppLaunchStatusLaunched :: AppLaunchStatus
pattern AppLaunchStatusLaunched = AppLaunchStatus' "LAUNCHED"

pattern AppLaunchStatusPartiallyLaunched :: AppLaunchStatus
pattern AppLaunchStatusPartiallyLaunched = AppLaunchStatus' "PARTIALLY_LAUNCHED"

pattern AppLaunchStatusDeltaLaunchInProgress :: AppLaunchStatus
pattern AppLaunchStatusDeltaLaunchInProgress = AppLaunchStatus' "DELTA_LAUNCH_IN_PROGRESS"

pattern AppLaunchStatusDeltaLaunchFailed :: AppLaunchStatus
pattern AppLaunchStatusDeltaLaunchFailed = AppLaunchStatus' "DELTA_LAUNCH_FAILED"

pattern AppLaunchStatusLaunchFailed :: AppLaunchStatus
pattern AppLaunchStatusLaunchFailed = AppLaunchStatus' "LAUNCH_FAILED"

pattern AppLaunchStatusTerminateInProgress :: AppLaunchStatus
pattern AppLaunchStatusTerminateInProgress = AppLaunchStatus' "TERMINATE_IN_PROGRESS"

pattern AppLaunchStatusTerminateFailed :: AppLaunchStatus
pattern AppLaunchStatusTerminateFailed = AppLaunchStatus' "TERMINATE_FAILED"

pattern AppLaunchStatusTerminated :: AppLaunchStatus
pattern AppLaunchStatusTerminated = AppLaunchStatus' "TERMINATED"

{-# COMPLETE
  AppLaunchStatusReadyForConfiguration,
  AppLaunchStatusConfigurationInProgress,
  AppLaunchStatusConfigurationInvalid,
  AppLaunchStatusReadyForLaunch,
  AppLaunchStatusValidationInProgress,
  AppLaunchStatusLaunchPending,
  AppLaunchStatusLaunchInProgress,
  AppLaunchStatusLaunched,
  AppLaunchStatusPartiallyLaunched,
  AppLaunchStatusDeltaLaunchInProgress,
  AppLaunchStatusDeltaLaunchFailed,
  AppLaunchStatusLaunchFailed,
  AppLaunchStatusTerminateInProgress,
  AppLaunchStatusTerminateFailed,
  AppLaunchStatusTerminated,
  AppLaunchStatus'
  #-}
