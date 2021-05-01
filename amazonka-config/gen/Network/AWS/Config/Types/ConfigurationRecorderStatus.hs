{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The current status of the configuration recorder.
--
-- /See:/ 'newConfigurationRecorderStatus' smart constructor.
data ConfigurationRecorderStatus = ConfigurationRecorderStatus'
  { -- | The time the recorder was last stopped.
    lastStopTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the recorder was last started.
    lastStartTime :: Prelude.Maybe Prelude.POSIX,
    -- | The message indicating that the recording failed due to an error.
    lastErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether or not the recorder is currently recording.
    recording :: Prelude.Maybe Prelude.Bool,
    -- | The name of the configuration recorder.
    name :: Prelude.Maybe Prelude.Text,
    -- | The error code indicating that the recording failed.
    lastErrorCode :: Prelude.Maybe Prelude.Text,
    -- | The last (previous) status of the recorder.
    lastStatus :: Prelude.Maybe RecorderStatus,
    -- | The time when the status was last changed.
    lastStatusChangeTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      lastErrorMessage = Prelude.Nothing,
      recording = Prelude.Nothing,
      name = Prelude.Nothing,
      lastErrorCode = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastStatusChangeTime = Prelude.Nothing
    }

-- | The time the recorder was last stopped.
configurationRecorderStatus_lastStopTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStopTime = Lens.lens (\ConfigurationRecorderStatus' {lastStopTime} -> lastStopTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStopTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Prelude._Time

-- | The time the recorder was last started.
configurationRecorderStatus_lastStartTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStartTime = Lens.lens (\ConfigurationRecorderStatus' {lastStartTime} -> lastStartTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStartTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Prelude._Time

-- | The message indicating that the recording failed due to an error.
configurationRecorderStatus_lastErrorMessage :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_lastErrorMessage = Lens.lens (\ConfigurationRecorderStatus' {lastErrorMessage} -> lastErrorMessage) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorMessage = a} :: ConfigurationRecorderStatus)

-- | Specifies whether or not the recorder is currently recording.
configurationRecorderStatus_recording :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Bool)
configurationRecorderStatus_recording = Lens.lens (\ConfigurationRecorderStatus' {recording} -> recording) (\s@ConfigurationRecorderStatus' {} a -> s {recording = a} :: ConfigurationRecorderStatus)

-- | The name of the configuration recorder.
configurationRecorderStatus_name :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_name = Lens.lens (\ConfigurationRecorderStatus' {name} -> name) (\s@ConfigurationRecorderStatus' {} a -> s {name = a} :: ConfigurationRecorderStatus)

-- | The error code indicating that the recording failed.
configurationRecorderStatus_lastErrorCode :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.Text)
configurationRecorderStatus_lastErrorCode = Lens.lens (\ConfigurationRecorderStatus' {lastErrorCode} -> lastErrorCode) (\s@ConfigurationRecorderStatus' {} a -> s {lastErrorCode = a} :: ConfigurationRecorderStatus)

-- | The last (previous) status of the recorder.
configurationRecorderStatus_lastStatus :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe RecorderStatus)
configurationRecorderStatus_lastStatus = Lens.lens (\ConfigurationRecorderStatus' {lastStatus} -> lastStatus) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatus = a} :: ConfigurationRecorderStatus)

-- | The time when the status was last changed.
configurationRecorderStatus_lastStatusChangeTime :: Lens.Lens' ConfigurationRecorderStatus (Prelude.Maybe Prelude.UTCTime)
configurationRecorderStatus_lastStatusChangeTime = Lens.lens (\ConfigurationRecorderStatus' {lastStatusChangeTime} -> lastStatusChangeTime) (\s@ConfigurationRecorderStatus' {} a -> s {lastStatusChangeTime = a} :: ConfigurationRecorderStatus) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ConfigurationRecorderStatus where
  parseJSON =
    Prelude.withObject
      "ConfigurationRecorderStatus"
      ( \x ->
          ConfigurationRecorderStatus'
            Prelude.<$> (x Prelude..:? "lastStopTime")
            Prelude.<*> (x Prelude..:? "lastStartTime")
            Prelude.<*> (x Prelude..:? "lastErrorMessage")
            Prelude.<*> (x Prelude..:? "recording")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "lastErrorCode")
            Prelude.<*> (x Prelude..:? "lastStatus")
            Prelude.<*> (x Prelude..:? "lastStatusChangeTime")
      )

instance Prelude.Hashable ConfigurationRecorderStatus

instance Prelude.NFData ConfigurationRecorderStatus
