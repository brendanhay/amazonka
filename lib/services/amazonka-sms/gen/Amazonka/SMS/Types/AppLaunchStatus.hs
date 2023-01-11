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
-- Module      : Amazonka.SMS.Types.AppLaunchStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppLaunchStatus
  ( AppLaunchStatus
      ( ..,
        AppLaunchStatus_CONFIGURATION_INVALID,
        AppLaunchStatus_CONFIGURATION_IN_PROGRESS,
        AppLaunchStatus_DELTA_LAUNCH_FAILED,
        AppLaunchStatus_DELTA_LAUNCH_IN_PROGRESS,
        AppLaunchStatus_LAUNCHED,
        AppLaunchStatus_LAUNCH_FAILED,
        AppLaunchStatus_LAUNCH_IN_PROGRESS,
        AppLaunchStatus_LAUNCH_PENDING,
        AppLaunchStatus_PARTIALLY_LAUNCHED,
        AppLaunchStatus_READY_FOR_CONFIGURATION,
        AppLaunchStatus_READY_FOR_LAUNCH,
        AppLaunchStatus_TERMINATED,
        AppLaunchStatus_TERMINATE_FAILED,
        AppLaunchStatus_TERMINATE_IN_PROGRESS,
        AppLaunchStatus_VALIDATION_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppLaunchStatus = AppLaunchStatus'
  { fromAppLaunchStatus ::
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

pattern AppLaunchStatus_CONFIGURATION_INVALID :: AppLaunchStatus
pattern AppLaunchStatus_CONFIGURATION_INVALID = AppLaunchStatus' "CONFIGURATION_INVALID"

pattern AppLaunchStatus_CONFIGURATION_IN_PROGRESS :: AppLaunchStatus
pattern AppLaunchStatus_CONFIGURATION_IN_PROGRESS = AppLaunchStatus' "CONFIGURATION_IN_PROGRESS"

pattern AppLaunchStatus_DELTA_LAUNCH_FAILED :: AppLaunchStatus
pattern AppLaunchStatus_DELTA_LAUNCH_FAILED = AppLaunchStatus' "DELTA_LAUNCH_FAILED"

pattern AppLaunchStatus_DELTA_LAUNCH_IN_PROGRESS :: AppLaunchStatus
pattern AppLaunchStatus_DELTA_LAUNCH_IN_PROGRESS = AppLaunchStatus' "DELTA_LAUNCH_IN_PROGRESS"

pattern AppLaunchStatus_LAUNCHED :: AppLaunchStatus
pattern AppLaunchStatus_LAUNCHED = AppLaunchStatus' "LAUNCHED"

pattern AppLaunchStatus_LAUNCH_FAILED :: AppLaunchStatus
pattern AppLaunchStatus_LAUNCH_FAILED = AppLaunchStatus' "LAUNCH_FAILED"

pattern AppLaunchStatus_LAUNCH_IN_PROGRESS :: AppLaunchStatus
pattern AppLaunchStatus_LAUNCH_IN_PROGRESS = AppLaunchStatus' "LAUNCH_IN_PROGRESS"

pattern AppLaunchStatus_LAUNCH_PENDING :: AppLaunchStatus
pattern AppLaunchStatus_LAUNCH_PENDING = AppLaunchStatus' "LAUNCH_PENDING"

pattern AppLaunchStatus_PARTIALLY_LAUNCHED :: AppLaunchStatus
pattern AppLaunchStatus_PARTIALLY_LAUNCHED = AppLaunchStatus' "PARTIALLY_LAUNCHED"

pattern AppLaunchStatus_READY_FOR_CONFIGURATION :: AppLaunchStatus
pattern AppLaunchStatus_READY_FOR_CONFIGURATION = AppLaunchStatus' "READY_FOR_CONFIGURATION"

pattern AppLaunchStatus_READY_FOR_LAUNCH :: AppLaunchStatus
pattern AppLaunchStatus_READY_FOR_LAUNCH = AppLaunchStatus' "READY_FOR_LAUNCH"

pattern AppLaunchStatus_TERMINATED :: AppLaunchStatus
pattern AppLaunchStatus_TERMINATED = AppLaunchStatus' "TERMINATED"

pattern AppLaunchStatus_TERMINATE_FAILED :: AppLaunchStatus
pattern AppLaunchStatus_TERMINATE_FAILED = AppLaunchStatus' "TERMINATE_FAILED"

pattern AppLaunchStatus_TERMINATE_IN_PROGRESS :: AppLaunchStatus
pattern AppLaunchStatus_TERMINATE_IN_PROGRESS = AppLaunchStatus' "TERMINATE_IN_PROGRESS"

pattern AppLaunchStatus_VALIDATION_IN_PROGRESS :: AppLaunchStatus
pattern AppLaunchStatus_VALIDATION_IN_PROGRESS = AppLaunchStatus' "VALIDATION_IN_PROGRESS"

{-# COMPLETE
  AppLaunchStatus_CONFIGURATION_INVALID,
  AppLaunchStatus_CONFIGURATION_IN_PROGRESS,
  AppLaunchStatus_DELTA_LAUNCH_FAILED,
  AppLaunchStatus_DELTA_LAUNCH_IN_PROGRESS,
  AppLaunchStatus_LAUNCHED,
  AppLaunchStatus_LAUNCH_FAILED,
  AppLaunchStatus_LAUNCH_IN_PROGRESS,
  AppLaunchStatus_LAUNCH_PENDING,
  AppLaunchStatus_PARTIALLY_LAUNCHED,
  AppLaunchStatus_READY_FOR_CONFIGURATION,
  AppLaunchStatus_READY_FOR_LAUNCH,
  AppLaunchStatus_TERMINATED,
  AppLaunchStatus_TERMINATE_FAILED,
  AppLaunchStatus_TERMINATE_IN_PROGRESS,
  AppLaunchStatus_VALIDATION_IN_PROGRESS,
  AppLaunchStatus'
  #-}
