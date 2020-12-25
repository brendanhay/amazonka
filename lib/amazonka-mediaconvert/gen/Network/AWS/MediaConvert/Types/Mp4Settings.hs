{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    mAudioDuration,
    mCslgAtom,
    mCttsVersion,
    mFreeSpaceBox,
    mMoovPlacement,
    mMp4MajorBrand,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmfcAudioDuration as Types
import qualified Network.AWS.MediaConvert.Types.Mp4CslgAtom as Types
import qualified Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox as Types
import qualified Network.AWS.MediaConvert.Types.Mp4MoovPlacement as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for MP4 container. You can create audio-only AAC outputs with this container.
--
-- /See:/ 'mkMp4Settings' smart constructor.
data Mp4Settings = Mp4Settings'
  { -- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
    audioDuration :: Core.Maybe Types.CmfcAudioDuration,
    -- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
    cslgAtom :: Core.Maybe Types.Mp4CslgAtom,
    -- | Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
    cttsVersion :: Core.Maybe Core.Natural,
    -- | Inserts a free-space box immediately after the moov box.
    freeSpaceBox :: Core.Maybe Types.Mp4FreeSpaceBox,
    -- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
    moovPlacement :: Core.Maybe Types.Mp4MoovPlacement,
    -- | Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
    mp4MajorBrand :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Mp4Settings' value with any optional fields omitted.
mkMp4Settings ::
  Mp4Settings
mkMp4Settings =
  Mp4Settings'
    { audioDuration = Core.Nothing,
      cslgAtom = Core.Nothing,
      cttsVersion = Core.Nothing,
      freeSpaceBox = Core.Nothing,
      moovPlacement = Core.Nothing,
      mp4MajorBrand = Core.Nothing
    }

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAudioDuration :: Lens.Lens' Mp4Settings (Core.Maybe Types.CmfcAudioDuration)
mAudioDuration = Lens.field @"audioDuration"
{-# DEPRECATED mAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | When enabled, file composition times will start at zero, composition times in the 'ctts' (composition time to sample) box for B-frames will be negative, and a 'cslg' (composition shift least greatest) box will be included per 14496-1 amendment 1. This improves compatibility with Apple players and tools.
--
-- /Note:/ Consider using 'cslgAtom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCslgAtom :: Lens.Lens' Mp4Settings (Core.Maybe Types.Mp4CslgAtom)
mCslgAtom = Lens.field @"cslgAtom"
{-# DEPRECATED mCslgAtom "Use generic-lens or generic-optics with 'cslgAtom' instead." #-}

-- | Ignore this setting unless compliance to the CTTS box version specification matters in your workflow. Specify a value of 1 to set your CTTS box version to 1 and make your output compliant with the specification. When you specify a value of 1, you must also set CSLG atom (cslgAtom) to the value INCLUDE. Keep the default value 0 to set your CTTS box version to 0. This can provide backward compatibility for some players and packagers.
--
-- /Note:/ Consider using 'cttsVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCttsVersion :: Lens.Lens' Mp4Settings (Core.Maybe Core.Natural)
mCttsVersion = Lens.field @"cttsVersion"
{-# DEPRECATED mCttsVersion "Use generic-lens or generic-optics with 'cttsVersion' instead." #-}

-- | Inserts a free-space box immediately after the moov box.
--
-- /Note:/ Consider using 'freeSpaceBox' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mFreeSpaceBox :: Lens.Lens' Mp4Settings (Core.Maybe Types.Mp4FreeSpaceBox)
mFreeSpaceBox = Lens.field @"freeSpaceBox"
{-# DEPRECATED mFreeSpaceBox "Use generic-lens or generic-optics with 'freeSpaceBox' instead." #-}

-- | If set to PROGRESSIVE_DOWNLOAD, the MOOV atom is relocated to the beginning of the archive as required for progressive downloading. Otherwise it is placed normally at the end.
--
-- /Note:/ Consider using 'moovPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMoovPlacement :: Lens.Lens' Mp4Settings (Core.Maybe Types.Mp4MoovPlacement)
mMoovPlacement = Lens.field @"moovPlacement"
{-# DEPRECATED mMoovPlacement "Use generic-lens or generic-optics with 'moovPlacement' instead." #-}

-- | Overrides the "Major Brand" field in the output file. Usually not necessary to specify.
--
-- /Note:/ Consider using 'mp4MajorBrand' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMp4MajorBrand :: Lens.Lens' Mp4Settings (Core.Maybe Core.Text)
mMp4MajorBrand = Lens.field @"mp4MajorBrand"
{-# DEPRECATED mMp4MajorBrand "Use generic-lens or generic-optics with 'mp4MajorBrand' instead." #-}

instance Core.FromJSON Mp4Settings where
  toJSON Mp4Settings {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioDuration" Core..=) Core.<$> audioDuration,
            ("cslgAtom" Core..=) Core.<$> cslgAtom,
            ("cttsVersion" Core..=) Core.<$> cttsVersion,
            ("freeSpaceBox" Core..=) Core.<$> freeSpaceBox,
            ("moovPlacement" Core..=) Core.<$> moovPlacement,
            ("mp4MajorBrand" Core..=) Core.<$> mp4MajorBrand
          ]
      )

instance Core.FromJSON Mp4Settings where
  parseJSON =
    Core.withObject "Mp4Settings" Core.$
      \x ->
        Mp4Settings'
          Core.<$> (x Core..:? "audioDuration")
          Core.<*> (x Core..:? "cslgAtom")
          Core.<*> (x Core..:? "cttsVersion")
          Core.<*> (x Core..:? "freeSpaceBox")
          Core.<*> (x Core..:? "moovPlacement")
          Core.<*> (x Core..:? "mp4MajorBrand")
