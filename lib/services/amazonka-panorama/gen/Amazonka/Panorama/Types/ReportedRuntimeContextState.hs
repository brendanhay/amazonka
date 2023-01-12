{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Panorama.Types.ReportedRuntimeContextState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.ReportedRuntimeContextState where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.DesiredState
import Amazonka.Panorama.Types.DeviceReportedStatus
import qualified Amazonka.Prelude as Prelude

-- | An application instance\'s state.
--
-- /See:/ 'newReportedRuntimeContextState' smart constructor.
data ReportedRuntimeContextState = ReportedRuntimeContextState'
  { -- | The application\'s desired state.
    desiredState :: DesiredState,
    -- | The application\'s reported status.
    deviceReportedStatus :: DeviceReportedStatus,
    -- | When the device reported the application\'s state.
    deviceReportedTime :: Data.POSIX,
    -- | The device\'s name.
    runtimeContextName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportedRuntimeContextState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'reportedRuntimeContextState_desiredState' - The application\'s desired state.
--
-- 'deviceReportedStatus', 'reportedRuntimeContextState_deviceReportedStatus' - The application\'s reported status.
--
-- 'deviceReportedTime', 'reportedRuntimeContextState_deviceReportedTime' - When the device reported the application\'s state.
--
-- 'runtimeContextName', 'reportedRuntimeContextState_runtimeContextName' - The device\'s name.
newReportedRuntimeContextState ::
  -- | 'desiredState'
  DesiredState ->
  -- | 'deviceReportedStatus'
  DeviceReportedStatus ->
  -- | 'deviceReportedTime'
  Prelude.UTCTime ->
  -- | 'runtimeContextName'
  Prelude.Text ->
  ReportedRuntimeContextState
newReportedRuntimeContextState
  pDesiredState_
  pDeviceReportedStatus_
  pDeviceReportedTime_
  pRuntimeContextName_ =
    ReportedRuntimeContextState'
      { desiredState =
          pDesiredState_,
        deviceReportedStatus = pDeviceReportedStatus_,
        deviceReportedTime =
          Data._Time Lens.# pDeviceReportedTime_,
        runtimeContextName = pRuntimeContextName_
      }

-- | The application\'s desired state.
reportedRuntimeContextState_desiredState :: Lens.Lens' ReportedRuntimeContextState DesiredState
reportedRuntimeContextState_desiredState = Lens.lens (\ReportedRuntimeContextState' {desiredState} -> desiredState) (\s@ReportedRuntimeContextState' {} a -> s {desiredState = a} :: ReportedRuntimeContextState)

-- | The application\'s reported status.
reportedRuntimeContextState_deviceReportedStatus :: Lens.Lens' ReportedRuntimeContextState DeviceReportedStatus
reportedRuntimeContextState_deviceReportedStatus = Lens.lens (\ReportedRuntimeContextState' {deviceReportedStatus} -> deviceReportedStatus) (\s@ReportedRuntimeContextState' {} a -> s {deviceReportedStatus = a} :: ReportedRuntimeContextState)

-- | When the device reported the application\'s state.
reportedRuntimeContextState_deviceReportedTime :: Lens.Lens' ReportedRuntimeContextState Prelude.UTCTime
reportedRuntimeContextState_deviceReportedTime = Lens.lens (\ReportedRuntimeContextState' {deviceReportedTime} -> deviceReportedTime) (\s@ReportedRuntimeContextState' {} a -> s {deviceReportedTime = a} :: ReportedRuntimeContextState) Prelude.. Data._Time

-- | The device\'s name.
reportedRuntimeContextState_runtimeContextName :: Lens.Lens' ReportedRuntimeContextState Prelude.Text
reportedRuntimeContextState_runtimeContextName = Lens.lens (\ReportedRuntimeContextState' {runtimeContextName} -> runtimeContextName) (\s@ReportedRuntimeContextState' {} a -> s {runtimeContextName = a} :: ReportedRuntimeContextState)

instance Data.FromJSON ReportedRuntimeContextState where
  parseJSON =
    Data.withObject
      "ReportedRuntimeContextState"
      ( \x ->
          ReportedRuntimeContextState'
            Prelude.<$> (x Data..: "DesiredState")
            Prelude.<*> (x Data..: "DeviceReportedStatus")
            Prelude.<*> (x Data..: "DeviceReportedTime")
            Prelude.<*> (x Data..: "RuntimeContextName")
      )

instance Prelude.Hashable ReportedRuntimeContextState where
  hashWithSalt _salt ReportedRuntimeContextState' {..} =
    _salt `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` deviceReportedStatus
      `Prelude.hashWithSalt` deviceReportedTime
      `Prelude.hashWithSalt` runtimeContextName

instance Prelude.NFData ReportedRuntimeContextState where
  rnf ReportedRuntimeContextState' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf deviceReportedStatus
      `Prelude.seq` Prelude.rnf deviceReportedTime
      `Prelude.seq` Prelude.rnf runtimeContextName
