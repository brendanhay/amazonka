{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.AudioOnlyHlsSettings
  ( AudioOnlyHlsSettings (..)
  -- * Smart constructor
  , mkAudioOnlyHlsSettings
  -- * Lenses
  , aohsAudioGroupId
  , aohsAudioOnlyImage
  , aohsAudioTrackType
  , aohsSegmentType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types.AudioOnlyHlsSegmentType as Types
import qualified Network.AWS.MediaLive.Types.AudioOnlyHlsTrackType as Types
import qualified Network.AWS.MediaLive.Types.InputLocation as Types
import qualified Network.AWS.Prelude as Core

-- | Audio Only Hls Settings
--
-- /See:/ 'mkAudioOnlyHlsSettings' smart constructor.
data AudioOnlyHlsSettings = AudioOnlyHlsSettings'
  { audioGroupId :: Core.Maybe Core.Text
    -- ^ Specifies the group to which the audio Rendition belongs.
  , audioOnlyImage :: Core.Maybe Types.InputLocation
    -- ^ Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth.
--
--
-- The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
  , audioTrackType :: Core.Maybe Types.AudioOnlyHlsTrackType
    -- ^ Four types of audio-only tracks are supported:
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
  , segmentType :: Core.Maybe Types.AudioOnlyHlsSegmentType
    -- ^ Specifies the segment type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioOnlyHlsSettings' value with any optional fields omitted.
mkAudioOnlyHlsSettings
    :: AudioOnlyHlsSettings
mkAudioOnlyHlsSettings
  = AudioOnlyHlsSettings'{audioGroupId = Core.Nothing,
                          audioOnlyImage = Core.Nothing, audioTrackType = Core.Nothing,
                          segmentType = Core.Nothing}

-- | Specifies the group to which the audio Rendition belongs.
--
-- /Note:/ Consider using 'audioGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsAudioGroupId :: Lens.Lens' AudioOnlyHlsSettings (Core.Maybe Core.Text)
aohsAudioGroupId = Lens.field @"audioGroupId"
{-# INLINEABLE aohsAudioGroupId #-}
{-# DEPRECATED audioGroupId "Use generic-lens or generic-optics with 'audioGroupId' instead"  #-}

-- | Optional. Specifies the .jpg or .png image to use as the cover art for an audio-only output. We recommend a low bit-size file because the image increases the output audio bandwidth.
--
--
-- The image is attached to the audio as an ID3 tag, frame type APIC, picture type 0x10, as per the "ID3 tag version 2.4.0 - Native Frames" standard.
--
-- /Note:/ Consider using 'audioOnlyImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsAudioOnlyImage :: Lens.Lens' AudioOnlyHlsSettings (Core.Maybe Types.InputLocation)
aohsAudioOnlyImage = Lens.field @"audioOnlyImage"
{-# INLINEABLE aohsAudioOnlyImage #-}
{-# DEPRECATED audioOnlyImage "Use generic-lens or generic-optics with 'audioOnlyImage' instead"  #-}

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
aohsAudioTrackType :: Lens.Lens' AudioOnlyHlsSettings (Core.Maybe Types.AudioOnlyHlsTrackType)
aohsAudioTrackType = Lens.field @"audioTrackType"
{-# INLINEABLE aohsAudioTrackType #-}
{-# DEPRECATED audioTrackType "Use generic-lens or generic-optics with 'audioTrackType' instead"  #-}

-- | Specifies the segment type.
--
-- /Note:/ Consider using 'segmentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aohsSegmentType :: Lens.Lens' AudioOnlyHlsSettings (Core.Maybe Types.AudioOnlyHlsSegmentType)
aohsSegmentType = Lens.field @"segmentType"
{-# INLINEABLE aohsSegmentType #-}
{-# DEPRECATED segmentType "Use generic-lens or generic-optics with 'segmentType' instead"  #-}

instance Core.FromJSON AudioOnlyHlsSettings where
        toJSON AudioOnlyHlsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioGroupId" Core..=) Core.<$> audioGroupId,
                  ("audioOnlyImage" Core..=) Core.<$> audioOnlyImage,
                  ("audioTrackType" Core..=) Core.<$> audioTrackType,
                  ("segmentType" Core..=) Core.<$> segmentType])

instance Core.FromJSON AudioOnlyHlsSettings where
        parseJSON
          = Core.withObject "AudioOnlyHlsSettings" Core.$
              \ x ->
                AudioOnlyHlsSettings' Core.<$>
                  (x Core..:? "audioGroupId") Core.<*> x Core..:? "audioOnlyImage"
                    Core.<*> x Core..:? "audioTrackType"
                    Core.<*> x Core..:? "segmentType"
