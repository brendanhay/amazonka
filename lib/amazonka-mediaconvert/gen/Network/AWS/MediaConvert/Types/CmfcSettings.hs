-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmfcSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmfcSettings
  ( CmfcSettings (..),

    -- * Smart constructor
    mkCmfcSettings,

    -- * Lenses
    csScte35Esam,
    csAudioDuration,
    csScte35Source,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmfcAudioDuration
import Network.AWS.MediaConvert.Types.CmfcScte35Esam
import Network.AWS.MediaConvert.Types.CmfcScte35Source
import qualified Network.AWS.Prelude as Lude

-- | Settings for MP4 segments in CMAF
--
-- /See:/ 'mkCmfcSettings' smart constructor.
data CmfcSettings = CmfcSettings'
  { scte35Esam ::
      Lude.Maybe CmfcScte35Esam,
    audioDuration :: Lude.Maybe CmfcAudioDuration,
    scte35Source :: Lude.Maybe CmfcScte35Source
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmfcSettings' with the minimum fields required to make a request.
--
-- * 'audioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
-- * 'scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
-- * 'scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
mkCmfcSettings ::
  CmfcSettings
mkCmfcSettings =
  CmfcSettings'
    { scte35Esam = Lude.Nothing,
      audioDuration = Lude.Nothing,
      scte35Source = Lude.Nothing
    }

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScte35Esam :: Lens.Lens' CmfcSettings (Lude.Maybe CmfcScte35Esam)
csScte35Esam = Lens.lens (scte35Esam :: CmfcSettings -> Lude.Maybe CmfcScte35Esam) (\s a -> s {scte35Esam = a} :: CmfcSettings)
{-# DEPRECATED csScte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead." #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAudioDuration :: Lens.Lens' CmfcSettings (Lude.Maybe CmfcAudioDuration)
csAudioDuration = Lens.lens (audioDuration :: CmfcSettings -> Lude.Maybe CmfcAudioDuration) (\s a -> s {audioDuration = a} :: CmfcSettings)
{-# DEPRECATED csAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csScte35Source :: Lens.Lens' CmfcSettings (Lude.Maybe CmfcScte35Source)
csScte35Source = Lens.lens (scte35Source :: CmfcSettings -> Lude.Maybe CmfcScte35Source) (\s a -> s {scte35Source = a} :: CmfcSettings)
{-# DEPRECATED csScte35Source "Use generic-lens or generic-optics with 'scte35Source' instead." #-}

instance Lude.FromJSON CmfcSettings where
  parseJSON =
    Lude.withObject
      "CmfcSettings"
      ( \x ->
          CmfcSettings'
            Lude.<$> (x Lude..:? "scte35Esam")
            Lude.<*> (x Lude..:? "audioDuration")
            Lude.<*> (x Lude..:? "scte35Source")
      )

instance Lude.ToJSON CmfcSettings where
  toJSON CmfcSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scte35Esam" Lude..=) Lude.<$> scte35Esam,
            ("audioDuration" Lude..=) Lude.<$> audioDuration,
            ("scte35Source" Lude..=) Lude.<$> scte35Source
          ]
      )
