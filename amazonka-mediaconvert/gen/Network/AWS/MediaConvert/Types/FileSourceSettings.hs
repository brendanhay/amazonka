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
-- Module      : Network.AWS.MediaConvert.Types.FileSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileSourceSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
import Network.AWS.MediaConvert.Types.FileSourceConvert608To708
import qualified Network.AWS.Prelude as Prelude

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
    convert608To708 :: Prelude.Maybe FileSourceConvert608To708,
    -- | Ignore this setting unless your input captions format is SCC. To have
    -- the service compensate for differing frame rates between your input
    -- captions and input video, specify the frame rate of the captions file.
    -- Specify this value as a fraction, using the settings Framerate numerator
    -- (framerateNumerator) and Framerate denominator (framerateDenominator).
    -- For example, you might specify 24 \/ 1 for 24 fps, 25 \/ 1 for 25 fps,
    -- 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for 29.97 fps.
    framerate :: Prelude.Maybe CaptionSourceFramerate,
    -- | External caption file used for loading captions. Accepted file
    -- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
    -- and \'smi\'.
    sourceFile :: Prelude.Maybe Prelude.Text,
    -- | Specifies a time delta in seconds to offset the captions from the source
    -- file.
    timeDelta :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { convert608To708 =
        Prelude.Nothing,
      framerate = Prelude.Nothing,
      sourceFile = Prelude.Nothing,
      timeDelta = Prelude.Nothing
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
-- Specify this value as a fraction, using the settings Framerate numerator
-- (framerateNumerator) and Framerate denominator (framerateDenominator).
-- For example, you might specify 24 \/ 1 for 24 fps, 25 \/ 1 for 25 fps,
-- 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for 29.97 fps.
fileSourceSettings_framerate :: Lens.Lens' FileSourceSettings (Prelude.Maybe CaptionSourceFramerate)
fileSourceSettings_framerate = Lens.lens (\FileSourceSettings' {framerate} -> framerate) (\s@FileSourceSettings' {} a -> s {framerate = a} :: FileSourceSettings)

-- | External caption file used for loading captions. Accepted file
-- extensions are \'scc\', \'ttml\', \'dfxp\', \'stl\', \'srt\', \'xml\',
-- and \'smi\'.
fileSourceSettings_sourceFile :: Lens.Lens' FileSourceSettings (Prelude.Maybe Prelude.Text)
fileSourceSettings_sourceFile = Lens.lens (\FileSourceSettings' {sourceFile} -> sourceFile) (\s@FileSourceSettings' {} a -> s {sourceFile = a} :: FileSourceSettings)

-- | Specifies a time delta in seconds to offset the captions from the source
-- file.
fileSourceSettings_timeDelta :: Lens.Lens' FileSourceSettings (Prelude.Maybe Prelude.Int)
fileSourceSettings_timeDelta = Lens.lens (\FileSourceSettings' {timeDelta} -> timeDelta) (\s@FileSourceSettings' {} a -> s {timeDelta = a} :: FileSourceSettings)

instance Prelude.FromJSON FileSourceSettings where
  parseJSON =
    Prelude.withObject
      "FileSourceSettings"
      ( \x ->
          FileSourceSettings'
            Prelude.<$> (x Prelude..:? "convert608To708")
            Prelude.<*> (x Prelude..:? "framerate")
            Prelude.<*> (x Prelude..:? "sourceFile")
            Prelude.<*> (x Prelude..:? "timeDelta")
      )

instance Prelude.Hashable FileSourceSettings

instance Prelude.NFData FileSourceSettings

instance Prelude.ToJSON FileSourceSettings where
  toJSON FileSourceSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("convert608To708" Prelude..=)
              Prelude.<$> convert608To708,
            ("framerate" Prelude..=) Prelude.<$> framerate,
            ("sourceFile" Prelude..=) Prelude.<$> sourceFile,
            ("timeDelta" Prelude..=) Prelude.<$> timeDelta
          ]
      )
