{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdSettings
  ( MpdSettings (..),

    -- * Smart constructor
    mkMpdSettings,

    -- * Lenses
    mpdScte35Esam,
    mpdAudioDuration,
    mpdScte35Source,
    mpdAccessibilityCaptionHints,
    mpdCaptionContainerType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
import Network.AWS.MediaConvert.Types.MpdAudioDuration
import Network.AWS.MediaConvert.Types.MpdCaptionContainerType
import Network.AWS.MediaConvert.Types.MpdScte35Esam
import Network.AWS.MediaConvert.Types.MpdScte35Source
import qualified Network.AWS.Prelude as Lude

-- | Settings for MP4 segments in DASH
--
-- /See:/ 'mkMpdSettings' smart constructor.
data MpdSettings = MpdSettings'
  { scte35Esam ::
      Lude.Maybe MpdScte35Esam,
    audioDuration :: Lude.Maybe MpdAudioDuration,
    scte35Source :: Lude.Maybe MpdScte35Source,
    accessibilityCaptionHints ::
      Lude.Maybe MpdAccessibilityCaptionHints,
    captionContainerType :: Lude.Maybe MpdCaptionContainerType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MpdSettings' with the minimum fields required to make a request.
--
-- * 'accessibilityCaptionHints' - <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
-- * 'audioDuration' - Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
-- * 'captionContainerType' - Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
-- * 'scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
-- * 'scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
mkMpdSettings ::
  MpdSettings
mkMpdSettings =
  MpdSettings'
    { scte35Esam = Lude.Nothing,
      audioDuration = Lude.Nothing,
      scte35Source = Lude.Nothing,
      accessibilityCaptionHints = Lude.Nothing,
      captionContainerType = Lude.Nothing
    }

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose INSERT to put SCTE-35 markers in this output at the insertion points that you specify in an ESAM XML document. Provide the document in the setting SCC XML (sccXml).
--
-- /Note:/ Consider using 'scte35Esam' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdScte35Esam :: Lens.Lens' MpdSettings (Lude.Maybe MpdScte35Esam)
mpdScte35Esam = Lens.lens (scte35Esam :: MpdSettings -> Lude.Maybe MpdScte35Esam) (\s a -> s {scte35Esam = a} :: MpdSettings)
{-# DEPRECATED mpdScte35Esam "Use generic-lens or generic-optics with 'scte35Esam' instead." #-}

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
--
-- /Note:/ Consider using 'audioDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdAudioDuration :: Lens.Lens' MpdSettings (Lude.Maybe MpdAudioDuration)
mpdAudioDuration = Lens.lens (audioDuration :: MpdSettings -> Lude.Maybe MpdAudioDuration) (\s a -> s {audioDuration = a} :: MpdSettings)
{-# DEPRECATED mpdAudioDuration "Use generic-lens or generic-optics with 'audioDuration' instead." #-}

-- | Ignore this setting unless you have SCTE-35 markers in your input video file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that appear in your input to also appear in this output. Choose None (NONE) if you don't want those SCTE-35 markers in this output.
--
-- /Note:/ Consider using 'scte35Source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdScte35Source :: Lens.Lens' MpdSettings (Lude.Maybe MpdScte35Source)
mpdScte35Source = Lens.lens (scte35Source :: MpdSettings -> Lude.Maybe MpdScte35Source) (\s a -> s {scte35Source = a} :: MpdSettings)
{-# DEPRECATED mpdScte35Source "Use generic-lens or generic-optics with 'scte35Source' instead." #-}

-- | <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
--
-- /Note:/ Consider using 'accessibilityCaptionHints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdAccessibilityCaptionHints :: Lens.Lens' MpdSettings (Lude.Maybe MpdAccessibilityCaptionHints)
mpdAccessibilityCaptionHints = Lens.lens (accessibilityCaptionHints :: MpdSettings -> Lude.Maybe MpdAccessibilityCaptionHints) (\s a -> s {accessibilityCaptionHints = a} :: MpdSettings)
{-# DEPRECATED mpdAccessibilityCaptionHints "Use generic-lens or generic-optics with 'accessibilityCaptionHints' instead." #-}

-- | Use this setting only in DASH output groups that include sidecar TTML or IMSC captions.  You specify sidecar captions in a separate output from your audio and video. Choose Raw (RAW) for captions in a single XML file in a raw container. Choose Fragmented MPEG-4 (FRAGMENTED_MP4) for captions in XML format contained within fragmented MP4 files. This set of fragmented MP4 files is separate from your video and audio fragmented MP4 files.
--
-- /Note:/ Consider using 'captionContainerType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpdCaptionContainerType :: Lens.Lens' MpdSettings (Lude.Maybe MpdCaptionContainerType)
mpdCaptionContainerType = Lens.lens (captionContainerType :: MpdSettings -> Lude.Maybe MpdCaptionContainerType) (\s a -> s {captionContainerType = a} :: MpdSettings)
{-# DEPRECATED mpdCaptionContainerType "Use generic-lens or generic-optics with 'captionContainerType' instead." #-}

instance Lude.FromJSON MpdSettings where
  parseJSON =
    Lude.withObject
      "MpdSettings"
      ( \x ->
          MpdSettings'
            Lude.<$> (x Lude..:? "scte35Esam")
            Lude.<*> (x Lude..:? "audioDuration")
            Lude.<*> (x Lude..:? "scte35Source")
            Lude.<*> (x Lude..:? "accessibilityCaptionHints")
            Lude.<*> (x Lude..:? "captionContainerType")
      )

instance Lude.ToJSON MpdSettings where
  toJSON MpdSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("scte35Esam" Lude..=) Lude.<$> scte35Esam,
            ("audioDuration" Lude..=) Lude.<$> audioDuration,
            ("scte35Source" Lude..=) Lude.<$> scte35Source,
            ("accessibilityCaptionHints" Lude..=)
              Lude.<$> accessibilityCaptionHints,
            ("captionContainerType" Lude..=) Lude.<$> captionContainerType
          ]
      )
