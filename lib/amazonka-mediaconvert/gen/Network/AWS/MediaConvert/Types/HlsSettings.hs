{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.HlsSettings
  ( HlsSettings (..)
  -- * Smart constructor
  , mkHlsSettings
  -- * Lenses
  , hsAudioGroupId
  , hsAudioOnlyContainer
  , hsAudioRenditionSets
  , hsAudioTrackType
  , hsIFrameOnlyManifest
  , hsSegmentModifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer as Types
import qualified Network.AWS.MediaConvert.Types.HlsAudioTrackType as Types
import qualified Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest as Types
import qualified Network.AWS.Prelude as Core

-- | Settings for HLS output groups
--
-- /See:/ 'mkHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { audioGroupId :: Core.Maybe Core.Text
    -- ^ Specifies the group to which the audio Rendition belongs.
  , audioOnlyContainer :: Core.Maybe Types.HlsAudioOnlyContainer
    -- ^ Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
  , audioRenditionSets :: Core.Maybe Core.Text
    -- ^ List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
  , audioTrackType :: Core.Maybe Types.HlsAudioTrackType
    -- ^ Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
  , iFrameOnlyManifest :: Core.Maybe Types.HlsIFrameOnlyManifest
    -- ^ When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
  , segmentModifier :: Core.Maybe Core.Text
    -- ^ Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HlsSettings' value with any optional fields omitted.
mkHlsSettings
    :: HlsSettings
mkHlsSettings
  = HlsSettings'{audioGroupId = Core.Nothing,
                 audioOnlyContainer = Core.Nothing,
                 audioRenditionSets = Core.Nothing, audioTrackType = Core.Nothing,
                 iFrameOnlyManifest = Core.Nothing, segmentModifier = Core.Nothing}

-- | Specifies the group to which the audio Rendition belongs.
--
-- /Note:/ Consider using 'audioGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioGroupId :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hsAudioGroupId = Lens.field @"audioGroupId"
{-# INLINEABLE hsAudioGroupId #-}
{-# DEPRECATED audioGroupId "Use generic-lens or generic-optics with 'audioGroupId' instead"  #-}

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport Stream (M2TS) to create a file in an MPEG2-TS container. Keep the default value Automatic (AUTOMATIC) to create an audio-only file in a raw container. Regardless of the value that you specify here, if this output has video, the service will place the output into an MPEG2-TS container.
--
-- /Note:/ Consider using 'audioOnlyContainer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioOnlyContainer :: Lens.Lens' HlsSettings (Core.Maybe Types.HlsAudioOnlyContainer)
hsAudioOnlyContainer = Lens.field @"audioOnlyContainer"
{-# INLINEABLE hsAudioOnlyContainer #-}
{-# DEPRECATED audioOnlyContainer "Use generic-lens or generic-optics with 'audioOnlyContainer' instead"  #-}

-- | List all the audio groups that are used with the video output stream. Input all the audio GROUP-IDs that are associated to the video, separate by ','.
--
-- /Note:/ Consider using 'audioRenditionSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioRenditionSets :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hsAudioRenditionSets = Lens.field @"audioRenditionSets"
{-# INLINEABLE hsAudioRenditionSets #-}
{-# DEPRECATED audioRenditionSets "Use generic-lens or generic-optics with 'audioRenditionSets' instead"  #-}

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream The client can play back this audio-only stream instead of video in low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS manifest. Alternate Audio, Auto Select, Default Alternate rendition that the client should try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES Alternate Audio, Auto Select, Not Default Alternate rendition that the client may try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not Auto Select Alternate rendition that the client will not try to play back by default. Represented as an EXT-X-MEDIA in the HLS manifest with DEFAULT=NO, AUTOSELECT=NO
--
-- /Note:/ Consider using 'audioTrackType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsAudioTrackType :: Lens.Lens' HlsSettings (Core.Maybe Types.HlsAudioTrackType)
hsAudioTrackType = Lens.field @"audioTrackType"
{-# INLINEABLE hsAudioTrackType #-}
{-# DEPRECATED audioTrackType "Use generic-lens or generic-optics with 'audioTrackType' instead"  #-}

-- | When set to INCLUDE, writes I-Frame Only Manifest in addition to the HLS manifest
--
-- /Note:/ Consider using 'iFrameOnlyManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsIFrameOnlyManifest :: Lens.Lens' HlsSettings (Core.Maybe Types.HlsIFrameOnlyManifest)
hsIFrameOnlyManifest = Lens.field @"iFrameOnlyManifest"
{-# INLINEABLE hsIFrameOnlyManifest #-}
{-# DEPRECATED iFrameOnlyManifest "Use generic-lens or generic-optics with 'iFrameOnlyManifest' instead"  #-}

-- | Use this setting to add an identifying string to the filename of each segment. The service adds this string between the name modifier and segment index number. You can use format identifiers in the string. For more information, see https://docs.aws.amazon.com/mediaconvert/latest/ug/using-variables-in-your-job-settings.html
--
-- /Note:/ Consider using 'segmentModifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hsSegmentModifier :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hsSegmentModifier = Lens.field @"segmentModifier"
{-# INLINEABLE hsSegmentModifier #-}
{-# DEPRECATED segmentModifier "Use generic-lens or generic-optics with 'segmentModifier' instead"  #-}

instance Core.FromJSON HlsSettings where
        toJSON HlsSettings{..}
          = Core.object
              (Core.catMaybes
                 [("audioGroupId" Core..=) Core.<$> audioGroupId,
                  ("audioOnlyContainer" Core..=) Core.<$> audioOnlyContainer,
                  ("audioRenditionSets" Core..=) Core.<$> audioRenditionSets,
                  ("audioTrackType" Core..=) Core.<$> audioTrackType,
                  ("iFrameOnlyManifest" Core..=) Core.<$> iFrameOnlyManifest,
                  ("segmentModifier" Core..=) Core.<$> segmentModifier])

instance Core.FromJSON HlsSettings where
        parseJSON
          = Core.withObject "HlsSettings" Core.$
              \ x ->
                HlsSettings' Core.<$>
                  (x Core..:? "audioGroupId") Core.<*>
                    x Core..:? "audioOnlyContainer"
                    Core.<*> x Core..:? "audioRenditionSets"
                    Core.<*> x Core..:? "audioTrackType"
                    Core.<*> x Core..:? "iFrameOnlyManifest"
                    Core.<*> x Core..:? "segmentModifier"
