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
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FrameCaptureIntervalUnit

-- | Frame Capture Settings
--
-- /See:/ 'newFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { -- | The frequency at which to capture frames for inclusion in the output.
    -- May be specified in either seconds or milliseconds, as specified by
    -- captureIntervalUnits.
    captureInterval :: Core.Maybe Core.Natural,
    -- | Unit for the frame capture interval.
    captureIntervalUnits :: Core.Maybe FrameCaptureIntervalUnit
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
newFrameCaptureSettings ::
  FrameCaptureSettings
newFrameCaptureSettings =
  FrameCaptureSettings'
    { captureInterval =
        Core.Nothing,
      captureIntervalUnits = Core.Nothing
    }

-- | The frequency at which to capture frames for inclusion in the output.
-- May be specified in either seconds or milliseconds, as specified by
-- captureIntervalUnits.
frameCaptureSettings_captureInterval :: Lens.Lens' FrameCaptureSettings (Core.Maybe Core.Natural)
frameCaptureSettings_captureInterval = Lens.lens (\FrameCaptureSettings' {captureInterval} -> captureInterval) (\s@FrameCaptureSettings' {} a -> s {captureInterval = a} :: FrameCaptureSettings)

-- | Unit for the frame capture interval.
frameCaptureSettings_captureIntervalUnits :: Lens.Lens' FrameCaptureSettings (Core.Maybe FrameCaptureIntervalUnit)
frameCaptureSettings_captureIntervalUnits = Lens.lens (\FrameCaptureSettings' {captureIntervalUnits} -> captureIntervalUnits) (\s@FrameCaptureSettings' {} a -> s {captureIntervalUnits = a} :: FrameCaptureSettings)

instance Core.FromJSON FrameCaptureSettings where
  parseJSON =
    Core.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Core.<$> (x Core..:? "captureInterval")
            Core.<*> (x Core..:? "captureIntervalUnits")
      )

instance Core.Hashable FrameCaptureSettings

instance Core.NFData FrameCaptureSettings

instance Core.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("captureInterval" Core..=)
              Core.<$> captureInterval,
            ("captureIntervalUnits" Core..=)
              Core.<$> captureIntervalUnits
          ]
      )
