-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4Settings
  ( Mp4Settings (..),

    -- * Smart constructor
    mkMp4Settings,

    -- * Lenses
    mssMoovPlacement,
    mssCttsVersion,
    mssFreeSpaceBox,
    mssAudioDuration,
    mssMp4MajorBrand,
    mssCslgAtom,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.Mp4CslgAtom
import Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
import Network.AWS.MediaConvert.Types.Mp4MoovPlacement
import qualified Network.AWS.Prelude as Lude

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- /See:/ 'mkMp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
  { moovPlacement ::
      Lude.Maybe Mp4MoovPlacement,
    cttsVersion :: Lude.Maybe Lude.Natural,
    freeSpaceBox :: Lude.Maybe Mp4FreeSpaceBox,
    audioDuration :: Lude.Maybe CmfcAudioDuration,
    mp4MajorBrand :: Lude.Maybe Lude.Text,
    cslgAtom :: Lude.Maybe Mp4CslgAtom
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Mp4Settings' with the minimum fields required to make a request.
--
-- * 'audioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
-- * 'cslgAtom' - When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
-- * 'cttsVersion' - Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
-- * 'freeSpaceBox' - Inserts a free-space box immediately after the moov box.
-- * 'moovPlacement' - If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
-- * 'mp4MajorBrand' - Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
mkMp4Settings ::
  Mp4Settings
mkMp4Settings =
  Mp4Settings'
    { moovPlacement = Lude.Nothing,
      cttsVersion = Lude.Nothing,
      freeSpaceBox = Lude.Nothing,
      audioDuration = Lude.Nothing,
      mp4MajorBrand = Lude.Nothing,
      cslgAtom = Lude.Nothing
    }

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
--
-- /Note:/ Consider using 'moovPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMoovPlacement :: Lens.Lens' Mp4Settings (Lude.Maybe Mp4MoovPlacement)
mssMoovPlacement = Lens.lens (moovPlacement :: Mp4Settings -> Lude.Maybe Mp4MoovPlacement) (\s a -> s {moovPlacement = a} :: Mp4Settings)
{-# DEPRECATED mssMoovPlacement "Use generic-lens or generic-optics with 'moovPlacement' instead." #-}

-- | Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
--
-- /Note:/ Consider using 'cttsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssCttsVersion :: Lens.Lens' Mp4Settings (Lude.Maybe Lude.Natural)
mssCttsVersion = Lens.lens (cttsVersion :: Mp4Settings -> Lude.Maybe Lude.Natural) (\s a -> s {cttsVersion = a} :: Mp4Settings)
{-# DEPRECATED mssCttsVersion "Use generic-lens or generic-optics with 'cttsVersion' instead." #-}

-- | Inserts a free-space box immediately after the moov box.
--
-- /Note:/ Consider using 'freeSpaceBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssFreeSpaceBox :: Lens.Lens' Mp4Settings (Lude.Maybe Mp4FreeSpaceBox)
mssFreeSpaceBox = Lens.lens (freeSpaceBox :: Mp4Settings -> Lude.Maybe Mp4FreeSpaceBox) (\s a -> s {freeSpaceBox = a} :: Mp4Settings)
{-# DEPRECATED mssFreeSpaceBox "Use generic-lens or generic-optics with 'freeSpaceBox' instead." #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssAudioDuration :: Lens.Lens' Mp4Settings (Lude.Maybe CmfcAudioDuration)
mssAudioDuration = Lens.lens (audioDuration :: Mp4Settings -> Lude.Maybe CmfcAudioDuration) (\s a -> s {audioDuration = a} :: Mp4Settings)
{-# DEPRECATED mssAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
--
-- /Note:/ Consider using 'mp4MajorBrand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssMp4MajorBrand :: Lens.Lens' Mp4Settings (Lude.Maybe Lude.Text)
mssMp4MajorBrand = Lens.lens (mp4MajorBrand :: Mp4Settings -> Lude.Maybe Lude.Text) (\s a -> s {mp4MajorBrand = a} :: Mp4Settings)
{-# DEPRECATED mssMp4MajorBrand "Use generic-lens or generic-optics with 'mp4MajorBrand' instead." #-}

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
--
-- /Note:/ Consider using 'cslgAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mssCslgAtom :: Lens.Lens' Mp4Settings (Lude.Maybe Mp4CslgAtom)
mssCslgAtom = Lens.lens (cslgAtom :: Mp4Settings -> Lude.Maybe Mp4CslgAtom) (\s a -> s {cslgAtom = a} :: Mp4Settings)
{-# DEPRECATED mssCslgAtom "Use generic-lens or generic-optics with 'cslgAtom' instead." #-}

instance Lude.FromJSON Mp4Settings where
  parseJSON =
    Lude.withObject
      "Mp4Settings"
      ( \x ->
          Mp4Settings'
            Lude.<$> (x Lude..:? "moovPlacement")
            Lude.<*> (x Lude..:? "cttsVersion")
            Lude.<*> (x Lude..:? "freeSpaceBox")
            Lude.<*> (x Lude..:? "audioDuration")
            Lude.<*> (x Lude..:? "mp4MajorBrand")
            Lude.<*> (x Lude..:? "cslgAtom")
      )

instance Lude.ToJSON Mp4Settings where
  toJSON Mp4Settings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("moovPlacement" Lude..=) Lude.<$> moovPlacement,
            ("cttsVersion" Lude..=) Lude.<$> cttsVersion,
            ("freeSpaceBox" Lude..=) Lude.<$> freeSpaceBox,
            ("audioDuration" Lude..=) Lude.<$> audioDuration,
            ("mp4MajorBrand" Lude..=) Lude.<$> mp4MajorBrand,
            ("cslgAtom" Lude..=) Lude.<$> cslgAtom
          ]
      )
