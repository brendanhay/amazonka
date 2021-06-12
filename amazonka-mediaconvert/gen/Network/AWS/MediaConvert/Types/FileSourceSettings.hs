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
-- Module      : Network.AWS.MediaConvert.Types.FileSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileSourceSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
import Network.AWS.MediaConvert.Types.FileSourceConvert608To708

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an
-- xml file, specify the URI of the input caption source file. If your
-- caption source is IMSC in an IMF package, use TrackSourceSettings
-- instead of FileSoureSettings.
--
-- /See:/ 'newFileSourceSettings' smart constructor.
data FileSourceSettings = FileSourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in
    -- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
    -- MediaConvert includes the captions data in two ways: it passes the 608
    -- data through using the 608 compatibility bytes fields of the 708
    -- wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Core.Maybe FileSourceConvert608To708,
    -- | Ignore this setting unless your input captions format is SCC. To have
    -- the service compensate for differing frame rates between your input
    -- captions and input video, specify the frame rate of the captions file.
    -- Specify this value as a fraction, using the settings Framerate numerator
    -- (framerateNumerator) and Framerate denominator (framerateDenominator).
    -- For example, you might specify 24 \/ 1 for 24 fps, 25 \/ 1 for 25 fps,
    -- 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for 29.97 fps.
    framerate :: Core.Maybe CaptionSourceFramerate,
    -- | External caption file used for loading captions. Accepted file
    -- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
    -- and \'smi\'.
    sourceFile :: Core.Maybe Core.Text,
    -- | Specifies a time delta in seconds to offset the captions from the source
    -- file.
    timeDelta :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- Specify this value as a fraction, using the settings Framerate numerator
-- (framerateNumerator) and Framerate denominator (framerateDenominator).
-- For example, you might specify 24 \/ 1 for 24 fps, 25 \/ 1 for 25 fps,
-- 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for 29.97 fps.
--
-- 'sourceFile', 'fileSourceSettings_sourceFile' - External caption file used for loading captions. Accepted file
-- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
-- and \'smi\'.
--
-- 'timeDelta', 'fileSourceSettings_timeDelta' - Specifies a time delta in seconds to offset the captions from the source
-- file.
newFileSourceSettings ::
  FileSourceSettings
newFileSourceSettings =
  FileSourceSettings'
    { convert608To708 = Core.Nothing,
      framerate = Core.Nothing,
      sourceFile = Core.Nothing,
      timeDelta = Core.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in
-- both 608 and 708 format. If you choose Upconvert (UPCONVERT),
-- MediaConvert includes the captions data in two ways: it passes the 608
-- data through using the 608 compatibility bytes fields of the 708
-- wrapper, and it also translates the 608 data into 708.
fileSourceSettings_convert608To708 :: Lens.Lens' FileSourceSettings (Core.Maybe FileSourceConvert608To708)
fileSourceSettings_convert608To708 = Lens.lens (\FileSourceSettings' {convert608To708} -> convert608To708) (\s@FileSourceSettings' {} a -> s {convert608To708 = a} :: FileSourceSettings)

-- | Ignore this setting unless your input captions format is SCC. To have
-- the service compensate for differing frame rates between your input
-- captions and input video, specify the frame rate of the captions file.
-- Specify this value as a fraction, using the settings Framerate numerator
-- (framerateNumerator) and Framerate denominator (framerateDenominator).
-- For example, you might specify 24 \/ 1 for 24 fps, 25 \/ 1 for 25 fps,
-- 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for 29.97 fps.
fileSourceSettings_framerate :: Lens.Lens' FileSourceSettings (Core.Maybe CaptionSourceFramerate)
fileSourceSettings_framerate = Lens.lens (\FileSourceSettings' {framerate} -> framerate) (\s@FileSourceSettings' {} a -> s {framerate = a} :: FileSourceSettings)

-- | External caption file used for loading captions. Accepted file
-- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
-- and \'smi\'.
fileSourceSettings_sourceFile :: Lens.Lens' FileSourceSettings (Core.Maybe Core.Text)
fileSourceSettings_sourceFile = Lens.lens (\FileSourceSettings' {sourceFile} -> sourceFile) (\s@FileSourceSettings' {} a -> s {sourceFile = a} :: FileSourceSettings)

-- | Specifies a time delta in seconds to offset the captions from the source
-- file.
fileSourceSettings_timeDelta :: Lens.Lens' FileSourceSettings (Core.Maybe Core.Int)
fileSourceSettings_timeDelta = Lens.lens (\FileSourceSettings' {timeDelta} -> timeDelta) (\s@FileSourceSettings' {} a -> s {timeDelta = a} :: FileSourceSettings)

instance Core.FromJSON FileSourceSettings where
  parseJSON =
    Core.withObject
      "FileSourceSettings"
      ( \x ->
          FileSourceSettings'
            Core.<$> (x Core..:? "convert608To708")
            Core.<*> (x Core..:? "framerate")
            Core.<*> (x Core..:? "sourceFile")
            Core.<*> (x Core..:? "timeDelta")
      )

instance Core.Hashable FileSourceSettings

instance Core.NFData FileSourceSettings

instance Core.ToJSON FileSourceSettings where
  toJSON FileSourceSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("convert608To708" Core..=)
              Core.<$> convert608To708,
            ("framerate" Core..=) Core.<$> framerate,
            ("sourceFile" Core..=) Core.<$> sourceFile,
            ("timeDelta" Core..=) Core.<$> timeDelta
          ]
      )
