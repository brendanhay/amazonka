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
-- Module      : Amazonka.Config.Types.ConfigurationRecorderStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigurationRecorderStatus where

import Amazonka.Config.Types.RecorderStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The current status of the configuration recorder.
--
-- /See:/ 'newConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { -- | The error code indicating that the recording failed.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The message indicating that the recording failed due to an error.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The time the recorder was last started.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | The last (previous) status of the recorder.
    lastStatus :: Prelude.Maybe RecorderStatus,
    -- | The time when the status was last changed.
    lastStatusChangeTime :: Prelude.Maybe Data.POSIX,
    -- | The time the recorder was last stopped.
    lastStopTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the configuration recorder.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether or not the recorder is currently recording.
    recording :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationRecorderStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastErrorCode', 'configurationRecorderStatus_lastErrorCode' - The error code indicating that the recording failed.
--
-- 'lastErrorMessage', 'configurationRecorderStatus_lastErrorMessage' - The message indicating that the recording failed due to an error.
--
-- 'lastStartTime', 'configurationRecorderStatus_lastStartTime' - The time the recorder was last started.
--
-- 'lastStatus', 'configurationRecorderStatus_lastStatus' - The last (previous) status of the recorder.
--
-- 'lastStatusChangeTime', 'configurationRecorderStatus_lastStatusChangeTime' - The time when the status was last changed.
--
-- 'lastStopTime', 'configurationRecorderStatus_lastStopTime' - The time the recorder was last stopped.
--
-- 'name', 'configurationRecorderStatus_name' - The name of the configuration recorder.
--
-- 'recording', 'configurationRecorderStatus_recording' - Specifies whether or not the recorder is currently recording.
newConfigurationRecorderStatus ::
  ConfigurationRecorderStatus
newConfigurationRecorderStatus =
  ConfigurationRecorderStatus'
    { lastErrorCode =
        Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastStatusChangeTime = Prelude.Nothing,
      lastStopTime = Prelude.Nothing,
      name = Prelude.Nothing,
      recording = Prelude.Nothing
    }

-- | The error code indicating that the recording failed.
configurationRecorderStatus_lastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_lastErrorCode = Lens.lens (\ConfigurationRecorderStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorCode = a} :: ConfigurationRecorderStatus)

-- | The message indicating that the recording failed due to an error.
configurationRecorderStatus_lastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_lastErrorMessage = Lens.lens (\ConfigurationRecorderStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorMessage = a} :: ConfigurationRecorderStatus)

-- | The time the recorder was last started.
configurationRecorderStatus_lastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStartTime = Lens.lens (\ConfigurationRecorderStatus' {lastStartTime} -> lastStartTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStartTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Data._Time

-- | The last (previous) status of the recorder.
configurationRecorderStatus_lastStatus :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe RecorderStatus)
configurationRecorderStatus_lastStatus = Lens.lens (\ConfigurationRecorderStatus' {lastStatus} -> lastStatus) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatus = a} :: ConfigurationRecorderStatus)

-- | The time when the status was last changed.
configurationRecorderStatus_lastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStatusChangeTime = Lens.lens (\ConfigurationRecorderStatus' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatusChangeTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Data._Time

-- | The time the recorder was last stopped.
configurationRecorderStatus_lastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStopTime = Lens.lens (\ConfigurationRecorderStatus' {lastStopTime} -> lastStopTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStopTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Data._Time

-- | The name of the configuration recorder.
configurationRecorderStatus_name :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_name = Lens.lens (\ConfigurationRecorderStatus' {name} -> name) (\s@ConfigurationRecorderStatus' {} a -> s {name = a} :: ConfigurationRecorderStatus)

-- | Specifies whether or not the recorder is currently recording.
configurationRecorderStatus_recording :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Bool)
configurationRecorderStatus_recording = Lens.lens (\ConfigurationRecorderStatus' {recording} -> recording) (\s@ConfigurationRecorderStatus' {} a -> s {recording = a} :: ConfigurationRecorderStatus)

instance Data.FromJSON ConfigurationRecorderStatus where
  parseJSON =
    Data.withObject
      "ConfigurationRecorderStatus"
      ( \x ->
          ConfigurationRecorderStatus'
            Prelude.<$> (x Data..:? "lastErrorCode")
            Prelude.<*> (x Data..:? "lastErrorMessage")
            Prelude.<*> (x Data..:? "lastStartTime")
            Prelude.<*> (x Data..:? "lastStatus")
            Prelude.<*> (x Data..:? "lastStatusChangeTime")
            Prelude.<*> (x Data..:? "lastStopTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recording")
      )

instance Prelude.Hashable ConfigurationRecorderStatus where
  hashWithSalt _salt ConfigurationRecorderStatus' {..} =
    _salt `Prelude.hashWithSalt` lastErrorCode
      `Prelude.hashWithSalt` lastErrorMessage
      `Prelude.hashWithSalt` lastStartTime
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` lastStatusChangeTime
      `Prelude.hashWithSalt` lastStopTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recording

instance Prelude.NFData ConfigurationRecorderStatus where
  rnf ConfigurationRecorderStatus' {..} =
    Prelude.rnf lastErrorCode
      `Prelude.seq` Prelude.rnf lastErrorMessage
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf lastStatusChangeTime
      `Prelude.seq` Prelude.rnf lastStopTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recording
