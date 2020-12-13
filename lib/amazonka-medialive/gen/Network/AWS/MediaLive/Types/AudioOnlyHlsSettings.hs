{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
  ( AudioOnlyHlsSettings (..),

    -- * Smart constructor
    mkAudioOnlyHlsSettings,

    -- * Lenses
    aohsAudioOnlyImage,
    aohsSegmentType,
    aohsAudioGroupId,
    aohsAudioTrackType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType
import Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType
import Network.AWS.MediaLive.Types.InputLocation
import qualified Network.AWS.Prelude as Lude

-- | Audio Only Hls Settings
--
-- /See:/ 'mkAudioOnlyHlsSettings' smart constructor.
data AudioOnlyHlsSettings = AudioOnlyHlsSettings'
  { -- | Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth.
    --
    --
    -- The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
    audioOnlyImage :: Lude.Maybe InputLocation,
    -- | Specifies the segment type.
    segmentType :: Lude.Maybe AudioOnlyHlsSegmentType,
    -- | Specifies the group to which the audio Rendition belongs.
    audioGroupId :: Lude.Maybe Lude.Text,
    -- | Four types of audio-only tracks are supported:
    --
    --
    -- Audio-Only Variant Stream
    -- The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest.
    --
    -- Alternate Audio, Auto Select, Default
    -- Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
    --
    -- Alternate Audio, Auto Select, Not Default
    -- Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES
    --
    -- Alternate Audio, not Auto Select
    -- Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
    audioTrackType :: Lude.Maybe AudioOnlyHlsTrackType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioOnlyHlsSettings' with the minimum fields required to make a request.
--
-- * 'audioOnlyImage' - Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth.
--
--
-- The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
-- * 'segmentType' - Specifies the segment type.
-- * 'audioGroupId' - Specifies the group to which the audio Rendition belongs.
-- * 'audioTrackType' - Four types of audio-only tracks are supported:
--
--
-- Audio-Only Variant Stream
-- The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest.
--
-- Alternate Audio, Auto Select, Default
-- Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
--
-- Alternate Audio, Auto Select, Not Default
-- Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES
--
-- Alternate Audio, not Auto Select
-- Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
mkAudioOnlyHlsSettings ::
  AudioOnlyHlsSettings
mkAudioOnlyHlsSettings =
  AudioOnlyHlsSettings'
    { audioOnlyImage = Lude.Nothing,
      segmentType = Lude.Nothing,
      audioGroupId = Lude.Nothing,
      audioTrackType = Lude.Nothing
    }

-- | Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth.
--
--
-- The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
--
-- /Note:/ Consider using 'audioOnlyImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsAudioOnlyImage :: Lens.Lens' AudioOnlyHlsSettings (Lude.Maybe InputLocation)
aohsAudioOnlyImage = Lens.lens (audioOnlyImage :: AudioOnlyHlsSettings -> Lude.Maybe InputLocation) (\s a -> s {audioOnlyImage = a} :: AudioOnlyHlsSettings)
{-# DEPRECATED aohsAudioOnlyImage "Use generic-lens or generic-optics with 'audioOnlyImage' instead." #-}

-- | Specifies the segment type.
--
-- /Note:/ Consider using 'segmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsSegmentType :: Lens.Lens' AudioOnlyHlsSettings (Lude.Maybe AudioOnlyHlsSegmentType)
aohsSegmentType = Lens.lens (segmentType :: AudioOnlyHlsSettings -> Lude.Maybe AudioOnlyHlsSegmentType) (\s a -> s {segmentType = a} :: AudioOnlyHlsSettings)
{-# DEPRECATED aohsSegmentType "Use generic-lens or generic-optics with 'segmentType' instead." #-}

-- | Specifies the group to which the audio Rendition belongs.
--
-- /Note:/ Consider using 'audioGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsAudioGroupId :: Lens.Lens' AudioOnlyHlsSettings (Lude.Maybe Lude.Text)
aohsAudioGroupId = Lens.lens (audioGroupId :: AudioOnlyHlsSettings -> Lude.Maybe Lude.Text) (\s a -> s {audioGroupId = a} :: AudioOnlyHlsSettings)
{-# DEPRECATED aohsAudioGroupId "Use generic-lens or generic-optics with 'audioGroupId' instead." #-}

-- | Four types of audio-only tracks are supported:
--
--
-- Audio-Only Variant Stream
-- The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest.
--
-- Alternate Audio, Auto Select, Default
-- Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
--
-- Alternate Audio, Auto Select, Not Default
-- Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES
--
-- Alternate Audio, not Auto Select
-- Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
--
-- /Note:/ Consider using 'audioTrackType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsAudioTrackType :: Lens.Lens' AudioOnlyHlsSettings (Lude.Maybe AudioOnlyHlsTrackType)
aohsAudioTrackType = Lens.lens (audioTrackType :: AudioOnlyHlsSettings -> Lude.Maybe AudioOnlyHlsTrackType) (\s a -> s {audioTrackType = a} :: AudioOnlyHlsSettings)
{-# DEPRECATED aohsAudioTrackType "Use generic-lens or generic-optics with 'audioTrackType' instead." #-}

instance Lude.FromJSON AudioOnlyHlsSettings where
  parseJSON =
    Lude.withObject
      "AudioOnlyHlsSettings"
      ( \x ->
          AudioOnlyHlsSettings'
            Lude.<$> (x Lude..:? "audioOnlyImage")
            Lude.<*> (x Lude..:? "segmentType")
            Lude.<*> (x Lude..:? "audioGroupId")
            Lude.<*> (x Lude..:? "audioTrackType")
      )

instance Lude.ToJSON AudioOnlyHlsSettings where
  toJSON AudioOnlyHlsSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("audioOnlyImage" Lude..=) Lude.<$> audioOnlyImage,
            ("segmentType" Lude..=) Lude.<$> segmentType,
            ("audioGroupId" Lude..=) Lude.<$> audioGroupId,
            ("audioTrackType" Lude..=) Lude.<$> audioTrackType
          ]
      )
