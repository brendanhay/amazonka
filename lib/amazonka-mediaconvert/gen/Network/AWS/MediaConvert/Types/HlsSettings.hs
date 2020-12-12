{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsSettings
  ( HlsSettings (..),

    -- * Smart constructor
    mkHlsSettings,

    -- * Lenses
    hsAudioRenditionSets,
    hsIFrameOnlyManifest,
    hsAudioGroupId,
    hsSegmentModifier,
    hsAudioOnlyContainer,
    hsAudioTrackType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
import Network.AWS.MediaConvert.Types.HlsAudioTrackType
import Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest
import qualified Network.AWS.Prelude as Lude

-- | Settings for HLS output groups
--
-- /See:/ 'mkHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { audioRenditionSets ::
      Lude.Maybe Lude.Text,
    iFrameOnlyManifest :: Lude.Maybe HlsIFrameOnlyManifest,
    audioGroupId :: Lude.Maybe Lude.Text,
    segmentModifier :: Lude.Maybe Lude.Text,
    audioOnlyContainer :: Lude.Maybe HlsAudioOnlyContainer,
    audioTrackType :: Lude.Maybe HlsAudioTrackType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HlsSettings' with the minimum fields required to make a request.
--
-- * 'audioGroupId' - Specifies the group to which the audio Rendition belongs.
-- * 'audioOnlyContainer' - Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
-- * 'audioRenditionSets' - List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
-- * 'audioTrackType' - Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
-- * 'iFrameOnlyManifest' - When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
-- * 'segmentModifier' - Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
mkHlsSettings ::
  HlsSettings
mkHlsSettings =
  HlsSettings'
    { audioRenditionSets = Lude.Nothing,
      iFrameOnlyManifest = Lude.Nothing,
      audioGroupId = Lude.Nothing,
      segmentModifier = Lude.Nothing,
      audioOnlyContainer = Lude.Nothing,
      audioTrackType = Lude.Nothing
    }

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioRenditionSets :: Lens.Lens' HlsSettings (Lude.Maybe Lude.Text)
hsAudioRenditionSets = Lens.lens (audioRenditionSets :: HlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {audioRenditionSets = a} :: HlsSettings)
{-# DEPRECATED hsAudioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead." #-}

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
--
-- /Note:/ Consider using 'iFrameOnlyManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsIFrameOnlyManifest :: Lens.Lens' HlsSettings (Lude.Maybe HlsIFrameOnlyManifest)
hsIFrameOnlyManifest = Lens.lens (iFrameOnlyManifest :: HlsSettings -> Lude.Maybe HlsIFrameOnlyManifest) (\s a -> s {iFrameOnlyManifest = a} :: HlsSettings)
{-# DEPRECATED hsIFrameOnlyManifest "Use generic-lens or generic-optics with 'iFrameOnlyManifest' instead." #-}

-- | Specifies the group to which the audio Rendition belongs.
--
-- /Note:/ Consider using 'audioGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioGroupId :: Lens.Lens' HlsSettings (Lude.Maybe Lude.Text)
hsAudioGroupId = Lens.lens (audioGroupId :: HlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {audioGroupId = a} :: HlsSettings)
{-# DEPRECATED hsAudioGroupId "Use generic-lens or generic-optics with 'audioGroupId' instead." #-}

-- | Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
--
-- /Note:/ Consider using 'segmentModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSegmentModifier :: Lens.Lens' HlsSettings (Lude.Maybe Lude.Text)
hsSegmentModifier = Lens.lens (segmentModifier :: HlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {segmentModifier = a} :: HlsSettings)
{-# DEPRECATED hsSegmentModifier "Use generic-lens or generic-optics with 'segmentModifier' instead." #-}

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
--
-- /Note:/ Consider using 'audioOnlyContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioOnlyContainer :: Lens.Lens' HlsSettings (Lude.Maybe HlsAudioOnlyContainer)
hsAudioOnlyContainer = Lens.lens (audioOnlyContainer :: HlsSettings -> Lude.Maybe HlsAudioOnlyContainer) (\s a -> s {audioOnlyContainer = a} :: HlsSettings)
{-# DEPRECATED hsAudioOnlyContainer "Use generic-lens or generic-optics with 'audioOnlyContainer' instead." #-}

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
--
-- /Note:/ Consider using 'audioTrackType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioTrackType :: Lens.Lens' HlsSettings (Lude.Maybe HlsAudioTrackType)
hsAudioTrackType = Lens.lens (audioTrackType :: HlsSettings -> Lude.Maybe HlsAudioTrackType) (\s a -> s {audioTrackType = a} :: HlsSettings)
{-# DEPRECATED hsAudioTrackType "Use generic-lens or generic-optics with 'audioTrackType' instead." #-}

instance Lude.FromJSON HlsSettings where
  parseJSON =
    Lude.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Lude.<$> (x Lude..:? "audioRenditionSets")
            Lude.<*> (x Lude..:? "iFrameOnlyManifest")
            Lude.<*> (x Lude..:? "audioGroupId")
            Lude.<*> (x Lude..:? "segmentModifier")
            Lude.<*> (x Lude..:? "audioOnlyContainer")
            Lude.<*> (x Lude..:? "audioTrackType")
      )

instance Lude.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioRenditionSets" Lude..=) Lude.<$> audioRenditionSets,
            ("iFrameOnlyManifest" Lude..=) Lude.<$> iFrameOnlyManifest,
            ("audioGroupId" Lude..=) Lude.<$> audioGroupId,
            ("segmentModifier" Lude..=) Lude.<$> segmentModifier,
            ("audioOnlyContainer" Lude..=) Lude.<$> audioOnlyContainer,
            ("audioTrackType" Lude..=) Lude.<$> audioTrackType
          ]
      )
