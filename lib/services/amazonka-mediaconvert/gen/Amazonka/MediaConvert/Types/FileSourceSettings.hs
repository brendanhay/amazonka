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
-- Module      : Amazonka.MediaConvert.Types.FileSourceSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.FileSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CaptionSourceFramerate
import Amazonka.MediaConvert.Types.FileSourceConvert608To708
import Amazonka.MediaConvert.Types.FileSourceTimeDeltaUnits
import qualified Amazonka.Prelude as Prelude

-- | If your input captions are SCC, SMI, SRT, STL, TTML, WebVTT, or IMSC 1.1
-- in an xml file, specify the URI of the input caption source file. If
-- your caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
--
-- /See:/ 'newFileSourceSettings' smart constructor.
data FileSourceSettings = FileSourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in
    -- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
    -- MediaConvert includes the captions data in two ways: it passes the 608
    -- data through using the 608 compatibility bytes fields of the 708
    -- wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Prelude.Maybe FileSourceConvert608To708,
    -- | Ignore this setting unless your input captions format is SCC. To have
    -- the service compensate for differing frame rates between your input
    -- captions and input video, specify the frame rate of the captions file.
    -- Specify this value as a fraction. When you work directly in your JSON
    -- job specification, use the settings framerateNumerator and
    -- framerateDenominator. For example, you might specify 24 \/ 1 for 24 fps,
    -- 25 \/ 1 for 25 fps, 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for
    -- 29.97 fps.
    framerate :: Prelude.Maybe CaptionSourceFramerate,
    -- | External caption file used for loading captions. Accepted file
    -- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
    -- \'smi\', \'webvtt\', and \'vtt\'.
    sourceFile :: Prelude.Maybe Prelude.Text,
    -- | Optional. Use this setting when you need to adjust the sync between your
    -- sidecar captions and your video. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/time-delta-use-cases.html.
    -- Enter a positive or negative number to modify the times in the captions
    -- file. For example, type 15 to add 15 seconds to all the times in the
    -- captions file. Type -5 to subtract 5 seconds from the times in the
    -- captions file. You can optionally specify your time delta in
    -- milliseconds instead of seconds. When you do so, set the related
    -- setting, Time delta units (TimeDeltaUnits) to Milliseconds
    -- (MILLISECONDS). Note that, when you specify a time delta for
    -- timecode-based caption sources, such as SCC and STL, and your time delta
    -- isn\'t a multiple of the input frame rate, MediaConvert snaps the
    -- captions to the nearest frame. For example, when your input video frame
    -- rate is 25 fps and you specify 1010ms for time delta, MediaConvert
    -- delays your captions by 1000 ms.
    timeDelta :: Prelude.Maybe Prelude.Int,
    -- | When you use the setting Time delta (TimeDelta) to adjust the sync
    -- between your sidecar captions and your video, use this setting to
    -- specify the units for the delta that you specify. When you don\'t
    -- specify a value for Time delta units (TimeDeltaUnits), MediaConvert uses
    -- seconds by default.
    timeDeltaUnits :: Prelude.Maybe FileSourceTimeDeltaUnits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'convert608To708', 'fileSourceSettings_convert608To708' - Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
--
-- 'framerate', 'fileSourceSettings_framerate' - Ignore this setting unless your input captions format is SCC. To have
-- the service compensate for differing frame rates between your input
-- captions and input video, specify the frame rate of the captions file.
-- Specify this value as a fraction. When you work directly in your JSON
-- job specification, use the settings framerateNumerator and
-- framerateDenominator. For example, you might specify 24 \/ 1 for 24 fps,
-- 25 \/ 1 for 25 fps, 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for
-- 29.97 fps.
--
-- 'sourceFile', 'fileSourceSettings_sourceFile' - External caption file used for loading captions. Accepted file
-- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
-- \'smi\', \'webvtt\', and \'vtt\'.
--
-- 'timeDelta', 'fileSourceSettings_timeDelta' - Optional. Use this setting when you need to adjust the sync between your
-- sidecar captions and your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/time-delta-use-cases.html.
-- Enter a positive or negative number to modify the times in the captions
-- file. For example, type 15 to add 15 seconds to all the times in the
-- captions file. Type -5 to subtract 5 seconds from the times in the
-- captions file. You can optionally specify your time delta in
-- milliseconds instead of seconds. When you do so, set the related
-- setting, Time delta units (TimeDeltaUnits) to Milliseconds
-- (MILLISECONDS). Note that, when you specify a time delta for
-- timecode-based caption sources, such as SCC and STL, and your time delta
-- isn\'t a multiple of the input frame rate, MediaConvert snaps the
-- captions to the nearest frame. For example, when your input video frame
-- rate is 25 fps and you specify 1010ms for time delta, MediaConvert
-- delays your captions by 1000 ms.
--
-- 'timeDeltaUnits', 'fileSourceSettings_timeDeltaUnits' - When you use the setting Time delta (TimeDelta) to adjust the sync
-- between your sidecar captions and your video, use this setting to
-- specify the units for the delta that you specify. When you don\'t
-- specify a value for Time delta units (TimeDeltaUnits), MediaConvert uses
-- seconds by default.
newFileSourceSettings ::
  FileSourceSettings
newFileSourceSettings =
  FileSourceSettings'
    { convert608To708 =
        Prelude.Nothing,
      framerate = Prelude.Nothing,
      sourceFile = Prelude.Nothing,
      timeDelta = Prelude.Nothing,
      timeDeltaUnits = Prelude.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
fileSourceSettings_convert608To708 :: Lens.Lens' FileSourceSettings (Prelude.Maybe FileSourceConvert608To708)
fileSourceSettings_convert608To708 = Lens.lens (\FileSourceSettings' {convert608To708} -> convert608To708) (\s@FileSourceSettings' {} a -> s {convert608To708 = a} :: FileSourceSettings)

-- | Ignore this setting unless your input captions format is SCC. To have
-- the service compensate for differing frame rates between your input
-- captions and input video, specify the frame rate of the captions file.
-- Specify this value as a fraction. When you work directly in your JSON
-- job specification, use the settings framerateNumerator and
-- framerateDenominator. For example, you might specify 24 \/ 1 for 24 fps,
-- 25 \/ 1 for 25 fps, 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for
-- 29.97 fps.
fileSourceSettings_framerate :: Lens.Lens' FileSourceSettings (Prelude.Maybe CaptionSourceFramerate)
fileSourceSettings_framerate = Lens.lens (\FileSourceSettings' {framerate} -> framerate) (\s@FileSourceSettings' {} a -> s {framerate = a} :: FileSourceSettings)

-- | External caption file used for loading captions. Accepted file
-- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
-- \'smi\', \'webvtt\', and \'vtt\'.
fileSourceSettings_sourceFile :: Lens.Lens' FileSourceSettings (Prelude.Maybe Prelude.Text)
fileSourceSettings_sourceFile = Lens.lens (\FileSourceSettings' {sourceFile} -> sourceFile) (\s@FileSourceSettings' {} a -> s {sourceFile = a} :: FileSourceSettings)

-- | Optional. Use this setting when you need to adjust the sync between your
-- sidecar captions and your video. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/time-delta-use-cases.html.
-- Enter a positive or negative number to modify the times in the captions
-- file. For example, type 15 to add 15 seconds to all the times in the
-- captions file. Type -5 to subtract 5 seconds from the times in the
-- captions file. You can optionally specify your time delta in
-- milliseconds instead of seconds. When you do so, set the related
-- setting, Time delta units (TimeDeltaUnits) to Milliseconds
-- (MILLISECONDS). Note that, when you specify a time delta for
-- timecode-based caption sources, such as SCC and STL, and your time delta
-- isn\'t a multiple of the input frame rate, MediaConvert snaps the
-- captions to the nearest frame. For example, when your input video frame
-- rate is 25 fps and you specify 1010ms for time delta, MediaConvert
-- delays your captions by 1000 ms.
fileSourceSettings_timeDelta :: Lens.Lens' FileSourceSettings (Prelude.Maybe Prelude.Int)
fileSourceSettings_timeDelta = Lens.lens (\FileSourceSettings' {timeDelta} -> timeDelta) (\s@FileSourceSettings' {} a -> s {timeDelta = a} :: FileSourceSettings)

-- | When you use the setting Time delta (TimeDelta) to adjust the sync
-- between your sidecar captions and your video, use this setting to
-- specify the units for the delta that you specify. When you don\'t
-- specify a value for Time delta units (TimeDeltaUnits), MediaConvert uses
-- seconds by default.
fileSourceSettings_timeDeltaUnits :: Lens.Lens' FileSourceSettings (Prelude.Maybe FileSourceTimeDeltaUnits)
fileSourceSettings_timeDeltaUnits = Lens.lens (\FileSourceSettings' {timeDeltaUnits} -> timeDeltaUnits) (\s@FileSourceSettings' {} a -> s {timeDeltaUnits = a} :: FileSourceSettings)

instance Data.FromJSON FileSourceSettings where
  parseJSON =
    Data.withObject
      "FileSourceSettings"
      ( \x ->
          FileSourceSettings'
            Prelude.<$> (x Data..:? "convert608To708")
            Prelude.<*> (x Data..:? "framerate")
            Prelude.<*> (x Data..:? "sourceFile")
            Prelude.<*> (x Data..:? "timeDelta")
            Prelude.<*> (x Data..:? "timeDeltaUnits")
      )

instance Prelude.Hashable FileSourceSettings where
  hashWithSalt _salt FileSourceSettings' {..} =
    _salt
      `Prelude.hashWithSalt` convert608To708
      `Prelude.hashWithSalt` framerate
      `Prelude.hashWithSalt` sourceFile
      `Prelude.hashWithSalt` timeDelta
      `Prelude.hashWithSalt` timeDeltaUnits

instance Prelude.NFData FileSourceSettings where
  rnf FileSourceSettings' {..} =
    Prelude.rnf convert608To708
      `Prelude.seq` Prelude.rnf framerate
      `Prelude.seq` Prelude.rnf sourceFile
      `Prelude.seq` Prelude.rnf timeDelta
      `Prelude.seq` Prelude.rnf timeDeltaUnits

instance Data.ToJSON FileSourceSettings where
  toJSON FileSourceSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("convert608To708" Data..=)
              Prelude.<$> convert608To708,
            ("framerate" Data..=) Prelude.<$> framerate,
            ("sourceFile" Data..=) Prelude.<$> sourceFile,
            ("timeDelta" Data..=) Prelude.<$> timeDelta,
            ("timeDeltaUnits" Data..=)
              Prelude.<$> timeDeltaUnits
          ]
      )
