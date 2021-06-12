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
-- Module      : Network.AWS.Config.Types.ConfigurationRecorderStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConfigurationRecorderStatus where

import Network.AWS.Config.Types.RecorderStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The current status of the configuration recorder.
--
-- /See:/ 'newConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { -- | The time the recorder was last stopped.
    lastStopTime :: Core.Maybe Core.POSIX,
    -- | The time the recorder was last started.
    lastStartTime :: Core.Maybe Core.POSIX,
    -- | The message indicating that the recording failed due to an error.
    lastErrorMessage :: Core.Maybe Core.Text,
    -- | Specifies whether or not the recorder is currently recording.
    recording :: Core.Maybe Core.Bool,
    -- | The name of the configuration recorder.
    name :: Core.Maybe Core.Text,
    -- | The error code indicating that the recording failed.
    lastErrorCode :: Core.Maybe Core.Text,
    -- | The last (previous) status of the recorder.
    lastStatus :: Core.Maybe RecorderStatus,
    -- | The time when the status was last changed.
    lastStatusChangeTime :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConfigurationRecorderStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastStopTime', 'configurationRecorderStatus_lastStopTime' - The time the recorder was last stopped.
--
-- 'lastStartTime', 'configurationRecorderStatus_lastStartTime' - The time the recorder was last started.
--
-- 'lastErrorMessage', 'configurationRecorderStatus_lastErrorMessage' - The message indicating that the recording failed due to an error.
--
-- 'recording', 'configurationRecorderStatus_recording' - Specifies whether or not the recorder is currently recording.
--
-- 'name', 'configurationRecorderStatus_name' - The name of the configuration recorder.
--
-- 'lastErrorCode', 'configurationRecorderStatus_lastErrorCode' - The error code indicating that the recording failed.
--
-- 'lastStatus', 'configurationRecorderStatus_lastStatus' - The last (previous) status of the recorder.
--
-- 'lastStatusChangeTime', 'configurationRecorderStatus_lastStatusChangeTime' - The time when the status was last changed.
newConfigurationRecorderStatus ::
  ConfigurationRecorderStatus
newConfigurationRecorderStatus =
  ConfigurationRecorderStatus'
    { lastStopTime =
        Core.Nothing,
      lastStartTime = Core.Nothing,
      lastErrorMessage = Core.Nothing,
      recording = Core.Nothing,
      name = Core.Nothing,
      lastErrorCode = Core.Nothing,
      lastStatus = Core.Nothing,
      lastStatusChangeTime = Core.Nothing
    }

-- | The time the recorder was last stopped.
configurationRecorderStatus_lastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.UTCTime)
configurationRecorderStatus_lastStopTime = Lens.lens (\ConfigurationRecorderStatus' {lastStopTime} -> lastStopTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStopTime = a} :: ConfigurationRecorderStatus) Core.. Lens.mapping Core._Time

-- | The time the recorder was last started.
configurationRecorderStatus_lastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.UTCTime)
configurationRecorderStatus_lastStartTime = Lens.lens (\ConfigurationRecorderStatus' {lastStartTime} -> lastStartTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStartTime = a} :: ConfigurationRecorderStatus) Core.. Lens.mapping Core._Time

-- | The message indicating that the recording failed due to an error.
configurationRecorderStatus_lastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
configurationRecorderStatus_lastErrorMessage = Lens.lens (\ConfigurationRecorderStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorMessage = a} :: ConfigurationRecorderStatus)

-- | Specifies whether or not the recorder is currently recording.
configurationRecorderStatus_recording :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Bool)
configurationRecorderStatus_recording = Lens.lens (\ConfigurationRecorderStatus' {recording} -> recording) (\s@ConfigurationRecorderStatus' {} a -> s {recording = a} :: ConfigurationRecorderStatus)

-- | The name of the configuration recorder.
configurationRecorderStatus_name :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
configurationRecorderStatus_name = Lens.lens (\ConfigurationRecorderStatus' {name} -> name) (\s@ConfigurationRecorderStatus' {} a -> s {name = a} :: ConfigurationRecorderStatus)

-- | The error code indicating that the recording failed.
configurationRecorderStatus_lastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.Text)
configurationRecorderStatus_lastErrorCode = Lens.lens (\ConfigurationRecorderStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorCode = a} :: ConfigurationRecorderStatus)

-- | The last (previous) status of the recorder.
configurationRecorderStatus_lastStatus :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe RecorderStatus)
configurationRecorderStatus_lastStatus = Lens.lens (\ConfigurationRecorderStatus' {lastStatus} -> lastStatus) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatus = a} :: ConfigurationRecorderStatus)

-- | The time when the status was last changed.
configurationRecorderStatus_lastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Core.Maybe Core.UTCTime)
configurationRecorderStatus_lastStatusChangeTime = Lens.lens (\ConfigurationRecorderStatus' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatusChangeTime = a} :: ConfigurationRecorderStatus) Core.. Lens.mapping Core._Time

instance Core.FromJSON ConfigurationRecorderStatus where
  parseJSON =
    Core.withObject
      "ConfigurationRecorderStatus"
      ( \x ->
          ConfigurationRecorderStatus'
            Core.<$> (x Core..:? "lastStopTime")
            Core.<*> (x Core..:? "lastStartTime")
            Core.<*> (x Core..:? "lastErrorMessage")
            Core.<*> (x Core..:? "recording")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "lastErrorCode")
            Core.<*> (x Core..:? "lastStatus")
            Core.<*> (x Core..:? "lastStatusChangeTime")
      )

instance Core.Hashable ConfigurationRecorderStatus

instance Core.NFData ConfigurationRecorderStatus
