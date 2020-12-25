{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FileSourceSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileSourceSettings
  ( FileSourceSettings (..),

    -- * Smart constructor
    mkFileSourceSettings,

    -- * Lenses
    fssConvert608To708,
    fssFramerate,
    fssSourceFile,
    fssTimeDelta,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CaptionSourceFramerate as Types
import qualified Network.AWS.MediaConvert.Types.FileSourceConvert608To708 as Types
import qualified Network.AWS.Prelude as Core

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /See:/ 'mkFileSourceSettings' smart constructor.
data FileSourceSettings = FileSourceSettings'
  { -- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
    convert608To708 :: Core.Maybe Types.FileSourceConvert608To708,
    -- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
    framerate :: Core.Maybe Types.CaptionSourceFramerate,
    -- | External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', 'xml', and 'smi'.
    sourceFile :: Core.Maybe Core.Text,
    -- | Specifies a time delta in seconds to offset the captions from the source file.
    timeDelta :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FileSourceSettings' value with any optional fields omitted.
mkFileSourceSettings ::
  FileSourceSettings
mkFileSourceSettings =
  FileSourceSettings'
    { convert608To708 = Core.Nothing,
      framerate = Core.Nothing,
      sourceFile = Core.Nothing,
      timeDelta = Core.Nothing
    }

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssConvert608To708 :: Lens.Lens' FileSourceSettings (Core.Maybe Types.FileSourceConvert608To708)
fssConvert608To708 = Lens.field @"convert608To708"
{-# DEPRECATED fssConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssFramerate :: Lens.Lens' FileSourceSettings (Core.Maybe Types.CaptionSourceFramerate)
fssFramerate = Lens.field @"framerate"
{-# DEPRECATED fssFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

-- | External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', 'xml', and 'smi'.
--
-- /Note:/ Consider using 'sourceFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssSourceFile :: Lens.Lens' FileSourceSettings (Core.Maybe Core.Text)
fssSourceFile = Lens.field @"sourceFile"
{-# DEPRECATED fssSourceFile "Use generic-lens or generic-optics with 'sourceFile' instead." #-}

-- | Specifies a time delta in seconds to offset the captions from the source file.
--
-- /Note:/ Consider using 'timeDelta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssTimeDelta :: Lens.Lens' FileSourceSettings (Core.Maybe Core.Int)
fssTimeDelta = Lens.field @"timeDelta"
{-# DEPRECATED fssTimeDelta "Use generic-lens or generic-optics with 'timeDelta' instead." #-}

instance Core.FromJSON FileSourceSettings where
  toJSON FileSourceSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("convert608To708" Core..=) Core.<$> convert608To708,
            ("framerate" Core..=) Core.<$> framerate,
            ("sourceFile" Core..=) Core.<$> sourceFile,
            ("timeDelta" Core..=) Core.<$> timeDelta
          ]
      )

instance Core.FromJSON FileSourceSettings where
  parseJSON =
    Core.withObject "FileSourceSettings" Core.$
      \x ->
        FileSourceSettings'
          Core.<$> (x Core..:? "convert608To708")
          Core.<*> (x Core..:? "framerate")
          Core.<*> (x Core..:? "sourceFile")
          Core.<*> (x Core..:? "timeDelta")
