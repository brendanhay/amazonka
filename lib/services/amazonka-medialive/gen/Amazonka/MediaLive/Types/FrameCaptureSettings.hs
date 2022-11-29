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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FrameCaptureSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.FrameCaptureIntervalUnit
import qualified Amazonka.Prelude as Prelude

-- | Frame Capture Settings
--
-- /See:/ 'newFrameCaptureSettings' smart constructor.
data FrameCaptureSettings = FrameCaptureSettings'
  { -- | Unit for the frame capture interval.
    captureIntervalUnits :: Prelude.Maybe FrameCaptureIntervalUnit,
    -- | The frequency at which to capture frames for inclusion in the output.
    -- May be specified in either seconds or milliseconds, as specified by
    -- captureIntervalUnits.
    captureInterval :: Prelude.Maybe Prelude.Natural
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
-- 'captureIntervalUnits', 'frameCaptureSettings_captureIntervalUnits' - Unit for the frame capture interval.
--
-- 'captureInterval', 'frameCaptureSettings_captureInterval' - The frequency at which to capture frames for inclusion in the output.
-- May be specified in either seconds or milliseconds, as specified by
-- captureIntervalUnits.
newFrameCaptureSettings ::
  FrameCaptureSettings
newFrameCaptureSettings =
  FrameCaptureSettings'
    { captureIntervalUnits =
        Prelude.Nothing,
      captureInterval = Prelude.Nothing
    }

-- | Unit for the frame capture interval.
frameCaptureSettings_captureIntervalUnits :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe FrameCaptureIntervalUnit)
frameCaptureSettings_captureIntervalUnits = Lens.lens (\FrameCaptureSettings' {captureIntervalUnits} -> captureIntervalUnits) (\s@FrameCaptureSettings' {} a -> s {captureIntervalUnits = a} :: FrameCaptureSettings)

-- | The frequency at which to capture frames for inclusion in the output.
-- May be specified in either seconds or milliseconds, as specified by
-- captureIntervalUnits.
frameCaptureSettings_captureInterval :: Lens.Lens' FrameCaptureSettings (Prelude.Maybe Prelude.Natural)
frameCaptureSettings_captureInterval = Lens.lens (\FrameCaptureSettings' {captureInterval} -> captureInterval) (\s@FrameCaptureSettings' {} a -> s {captureInterval = a} :: FrameCaptureSettings)

instance Core.FromJSON FrameCaptureSettings where
  parseJSON =
    Core.withObject
      "FrameCaptureSettings"
      ( \x ->
          FrameCaptureSettings'
            Prelude.<$> (x Core..:? "captureIntervalUnits")
            Prelude.<*> (x Core..:? "captureInterval")
      )

instance Prelude.Hashable FrameCaptureSettings where
  hashWithSalt _salt FrameCaptureSettings' {..} =
    _salt `Prelude.hashWithSalt` captureIntervalUnits
      `Prelude.hashWithSalt` captureInterval

instance Prelude.NFData FrameCaptureSettings where
  rnf FrameCaptureSettings' {..} =
    Prelude.rnf captureIntervalUnits
      `Prelude.seq` Prelude.rnf captureInterval

instance Core.ToJSON FrameCaptureSettings where
  toJSON FrameCaptureSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("captureIntervalUnits" Core..=)
              Prelude.<$> captureIntervalUnits,
            ("captureInterval" Core..=)
              Prelude.<$> captureInterval
          ]
      )
