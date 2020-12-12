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
    fssFramerate,
    fssConvert608To708,
    fssTimeDelta,
    fssSourceFile,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CaptionSourceFramerate
import Network.AWS.MediaConvert.Types.FileSourceConvert608To708
import qualified Network.AWS.Prelude as Lude

-- | If your input captions are SCC, SMI, SRT, STL, TTML, or IMSC 1.1 in an xml file, specify the URI of the input caption source file. If your caption source is IMSC in an IMF package, use TrackSourceSettings instead of FileSoureSettings.
--
-- /See:/ 'mkFileSourceSettings' smart constructor.
data FileSourceSettings = FileSourceSettings'
  { framerate ::
      Lude.Maybe CaptionSourceFramerate,
    convert608To708 ::
      Lude.Maybe FileSourceConvert608To708,
    timeDelta :: Lude.Maybe Lude.Int,
    sourceFile :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FileSourceSettings' with the minimum fields required to make a request.
--
-- * 'convert608To708' - Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
-- * 'framerate' - Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
-- * 'sourceFile' - External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', 'xml', and 'smi'.
-- * 'timeDelta' - Specifies a time delta in seconds to offset the captions from the source file.
mkFileSourceSettings ::
  FileSourceSettings
mkFileSourceSettings =
  FileSourceSettings'
    { framerate = Lude.Nothing,
      convert608To708 = Lude.Nothing,
      timeDelta = Lude.Nothing,
      sourceFile = Lude.Nothing
    }

-- | Ignore this setting unless your input captions format is SCC. To have the service compensate for differing frame rates between your input captions and input video, specify the frame rate of the captions file. Specify this value as a fraction, using the settings Framerate numerator (framerateNumerator) and Framerate denominator (framerateDenominator). For example, you might specify 24 / 1 for 24 fps, 25 / 1 for 25 fps, 24000 / 1001 for 23.976 fps, or 30000 / 1001 for 29.97 fps.
--
-- /Note:/ Consider using 'framerate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssFramerate :: Lens.Lens' FileSourceSettings (Lude.Maybe CaptionSourceFramerate)
fssFramerate = Lens.lens (framerate :: FileSourceSettings -> Lude.Maybe CaptionSourceFramerate) (\s a -> s {framerate = a} :: FileSourceSettings)
{-# DEPRECATED fssFramerate "Use generic-lens or generic-optics with 'framerate' instead." #-}

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
--
-- /Note:/ Consider using 'convert608To708' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssConvert608To708 :: Lens.Lens' FileSourceSettings (Lude.Maybe FileSourceConvert608To708)
fssConvert608To708 = Lens.lens (convert608To708 :: FileSourceSettings -> Lude.Maybe FileSourceConvert608To708) (\s a -> s {convert608To708 = a} :: FileSourceSettings)
{-# DEPRECATED fssConvert608To708 "Use generic-lens or generic-optics with 'convert608To708' instead." #-}

-- | Specifies a time delta in seconds to offset the captions from the source file.
--
-- /Note:/ Consider using 'timeDelta' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssTimeDelta :: Lens.Lens' FileSourceSettings (Lude.Maybe Lude.Int)
fssTimeDelta = Lens.lens (timeDelta :: FileSourceSettings -> Lude.Maybe Lude.Int) (\s a -> s {timeDelta = a} :: FileSourceSettings)
{-# DEPRECATED fssTimeDelta "Use generic-lens or generic-optics with 'timeDelta' instead." #-}

-- | External caption file used for loading captions. Accepted file extensions are 'scc', 'ttml', 'dfxp', 'stl', 'srt', 'xml', and 'smi'.
--
-- /Note:/ Consider using 'sourceFile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fssSourceFile :: Lens.Lens' FileSourceSettings (Lude.Maybe Lude.Text)
fssSourceFile = Lens.lens (sourceFile :: FileSourceSettings -> Lude.Maybe Lude.Text) (\s a -> s {sourceFile = a} :: FileSourceSettings)
{-# DEPRECATED fssSourceFile "Use generic-lens or generic-optics with 'sourceFile' instead." #-}

instance Lude.FromJSON FileSourceSettings where
  parseJSON =
    Lude.withObject
      "FileSourceSettings"
      ( \x ->
          FileSourceSettings'
            Lude.<$> (x Lude..:? "framerate")
            Lude.<*> (x Lude..:? "convert608To708")
            Lude.<*> (x Lude..:? "timeDelta")
            Lude.<*> (x Lude..:? "sourceFile")
      )

instance Lude.ToJSON FileSourceSettings where
  toJSON FileSourceSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("framerate" Lude..=) Lude.<$> framerate,
            ("convert608To708" Lude..=) Lude.<$> convert608To708,
            ("timeDelta" Lude..=) Lude.<$> timeDelta,
            ("sourceFile" Lude..=) Lude.<$> sourceFile
          ]
      )
