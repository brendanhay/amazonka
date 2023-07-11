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
-- Module      : Amazonka.MediaLive.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FrameCaptureIntervalUnit
import Amazonka.MediaLive.Types.TimecodeBurninSettings
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Settings
--
-- /See:/ 'newFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { -- | The frequency at which to capture frames for inclusion in the output.
    -- May be specified in either seconds or milliseconds, as specified by
    -- captureIntervalUnits.
    captureInterval :: Prelude.Maybe Prelude.Natural,
    -- | Unit for the frame capture interval.
    captureIntervalUnits :: Prelude.Maybe FrameCaptureIntervalUnit,
    -- | Timecode burn-in settings
    timecodeBurninSettings :: Prelude.Maybe TimecodeBurninSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FrameCaptureSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureInterval', 'frameCaptureSettings_captureInterval' - The frequency at which to capture frames for inclusion in the output.
-- May be specified in either seconds or milliseconds, as specified by
-- captureIntervalUnits.
--
-- 'captureIntervalUnits', 'frameCaptureSettings_captureIntervalUnits' - Unit for the frame capture interval.
--
-- 'timecodeBurninSettings', 'frameCaptureSettings_timecodeBurninSettings' - Timecode burn-in settings
newFrameCaptureSettings ::
  FrameCaptureSettings
newFrameCaptureSettings =
  FrameCaptureSettings'
    { captureInterval =
        Prelude.Nothing,
      captureIntervalUnits = Prelude.Nothing,
      timecodeBurninSettings = Prelude.Nothing
    }

-- | The frequency at which to capture frames for inclusion in the output.
-- May be specified in either seconds or milliseconds, as specified by
-- captureIntervalUnits.
frameCaptureSettings_captureInterval :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_captureInterval = Lens.lens (\FrameCaptureSettings' {captureInterval} -> captureInterval) (\s@FrameCaptureSettings' {} a -> s {captureInterval = a} :: FrameCaptureSettings)

-- | Unit for the frame capture interval.
frameCaptureSettings_captureIntervalUnits :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe FrameCaptureIntervalUnit)
frameCaptureSettings_captureIntervalUnits = Lens.lens (\FrameCaptureSettings' {captureIntervalUnits} -> captureIntervalUnits) (\s@FrameCaptureSettings' {} a -> s {captureIntervalUnits = a} :: FrameCaptureSettings)

-- | Timecode burn-in settings
frameCaptureSettings_timecodeBurninSettings :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe TimecodeBurninSettings)
frameCaptureSettings_timecodeBurninSettings = Lens.lens (\FrameCaptureSettings' {timecodeBurninSettings} -> timecodeBurninSettings) (\s@FrameCaptureSettings' {} a -> s {timecodeBurninSettings = a} :: FrameCaptureSettings)

instance Data.FromJSON FrameCaptureSettings where
  parseJSON =
    Data.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Prelude.<$> (x Data..:? "captureInterval")
            Prelude.<*> (x Data..:? "captureIntervalUnits")
            Prelude.<*> (x Data..:? "timecodeBurninSettings")
      )

instance Prelude.Hashable FrameCaptureSettings where
  hashWithSalt _salt FrameCaptureSettings' {..} =
    _salt
      `Prelude.hashWithSalt` captureInterval
      `Prelude.hashWithSalt` captureIntervalUnits
      `Prelude.hashWithSalt` timecodeBurninSettings

instance Prelude.NFData FrameCaptureSettings where
  rnf FrameCaptureSettings' {..} =
    Prelude.rnf captureInterval
      `Prelude.seq` Prelude.rnf captureIntervalUnits
      `Prelude.seq` Prelude.rnf timecodeBurninSettings

instance Data.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("captureInterval" Data..=)
              Prelude.<$> captureInterval,
            ("captureIntervalUnits" Data..=)
              Prelude.<$> captureIntervalUnits,
            ("timecodeBurninSettings" Data..=)
              Prelude.<$> timecodeBurninSettings
          ]
      )
