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
-- Module      : Amazonka.Panorama.Types.DeviceReportedStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceReportedStatus
  ( DeviceReportedStatus
      ( ..,
        DeviceReportedStatus_INSTALL_ERROR,
        DeviceReportedStatus_INSTALL_IN_PROGRESS,
        DeviceReportedStatus_LAUNCHED,
        DeviceReportedStatus_LAUNCH_ERROR,
        DeviceReportedStatus_REMOVAL_FAILED,
        DeviceReportedStatus_REMOVAL_IN_PROGRESS,
        DeviceReportedStatus_RUNNING,
        DeviceReportedStatus_STARTING,
        DeviceReportedStatus_STOPPED,
        DeviceReportedStatus_STOPPING,
        DeviceReportedStatus_STOP_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceReportedStatus = DeviceReportedStatus'
  { fromDeviceReportedStatus ::
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

pattern DeviceReportedStatus_INSTALL_ERROR :: DeviceReportedStatus
pattern DeviceReportedStatus_INSTALL_ERROR = DeviceReportedStatus' "INSTALL_ERROR"

pattern DeviceReportedStatus_INSTALL_IN_PROGRESS :: DeviceReportedStatus
pattern DeviceReportedStatus_INSTALL_IN_PROGRESS = DeviceReportedStatus' "INSTALL_IN_PROGRESS"

pattern DeviceReportedStatus_LAUNCHED :: DeviceReportedStatus
pattern DeviceReportedStatus_LAUNCHED = DeviceReportedStatus' "LAUNCHED"

pattern DeviceReportedStatus_LAUNCH_ERROR :: DeviceReportedStatus
pattern DeviceReportedStatus_LAUNCH_ERROR = DeviceReportedStatus' "LAUNCH_ERROR"

pattern DeviceReportedStatus_REMOVAL_FAILED :: DeviceReportedStatus
pattern DeviceReportedStatus_REMOVAL_FAILED = DeviceReportedStatus' "REMOVAL_FAILED"

pattern DeviceReportedStatus_REMOVAL_IN_PROGRESS :: DeviceReportedStatus
pattern DeviceReportedStatus_REMOVAL_IN_PROGRESS = DeviceReportedStatus' "REMOVAL_IN_PROGRESS"

pattern DeviceReportedStatus_RUNNING :: DeviceReportedStatus
pattern DeviceReportedStatus_RUNNING = DeviceReportedStatus' "RUNNING"

pattern DeviceReportedStatus_STARTING :: DeviceReportedStatus
pattern DeviceReportedStatus_STARTING = DeviceReportedStatus' "STARTING"

pattern DeviceReportedStatus_STOPPED :: DeviceReportedStatus
pattern DeviceReportedStatus_STOPPED = DeviceReportedStatus' "STOPPED"

pattern DeviceReportedStatus_STOPPING :: DeviceReportedStatus
pattern DeviceReportedStatus_STOPPING = DeviceReportedStatus' "STOPPING"

pattern DeviceReportedStatus_STOP_ERROR :: DeviceReportedStatus
pattern DeviceReportedStatus_STOP_ERROR = DeviceReportedStatus' "STOP_ERROR"

{-# COMPLETE
  DeviceReportedStatus_INSTALL_ERROR,
  DeviceReportedStatus_INSTALL_IN_PROGRESS,
  DeviceReportedStatus_LAUNCHED,
  DeviceReportedStatus_LAUNCH_ERROR,
  DeviceReportedStatus_REMOVAL_FAILED,
  DeviceReportedStatus_REMOVAL_IN_PROGRESS,
  DeviceReportedStatus_RUNNING,
  DeviceReportedStatus_STARTING,
  DeviceReportedStatus_STOPPED,
  DeviceReportedStatus_STOPPING,
  DeviceReportedStatus_STOP_ERROR,
  DeviceReportedStatus'
  #-}
