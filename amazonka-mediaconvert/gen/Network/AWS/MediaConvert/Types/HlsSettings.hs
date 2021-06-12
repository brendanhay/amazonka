{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.HlsAudioOnlyContainer
import Network.AWS.MediaConvert.Types.HlsAudioTrackType
import Network.AWS.MediaConvert.Types.HlsIFrameOnlyManifest

-- | Settings for HLS output groups
--
-- /See:/ 'newHlsSettings' smart constructor.
data HlsSettings = HlsSettings'
  { -- | List all the audio groups that are used with the video output stream.
    -- Input all the audio GROUP-IDs that are associated to the video, separate
    -- by \',\'.
    audioRenditionSets :: Core.Maybe Core.Text,
    -- | Choose Include (INCLUDE) to have MediaConvert generate a child manifest
    -- that lists only the I-frames for this rendition, in addition to your
    -- regular manifest for this rendition. You might use this manifest as part
    -- of a workflow that creates preview functions for your video.
    -- MediaConvert adds both the I-frame only child manifest and the regular
    -- child manifest to the parent manifest. When you don\'t need the I-frame
    -- only child manifest, keep the default value Exclude (EXCLUDE).
    iFrameOnlyManifest :: Core.Maybe HlsIFrameOnlyManifest,
    -- | Use this setting to add an identifying string to the filename of each
    -- segment. The service adds this string between the name modifier and
    -- segment index number. You can use format identifiers in the string. For
    -- more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/using-variables-in-your-job-settings.html
    segmentModifier :: Core.Maybe Core.Text,
    -- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport
    -- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
    -- default value Automatic (AUTOMATIC) to create an audio-only file in a
    -- raw container. Regardless of the value that you specify here, if this
    -- output has video, the service will place the output into an MPEG2-TS
    -- container.
    audioOnlyContainer :: Core.Maybe HlsAudioOnlyContainer,
    -- | Specifies the group to which the audio Rendition belongs.
    audioGroupId :: Core.Maybe Core.Text,
    -- | Four types of audio-only tracks are supported: Audio-Only Variant Stream
    -- The client can play back this audio-only stream instead of video in
    -- low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS
    -- manifest. Alternate Audio, Auto Select, Default Alternate rendition that
    -- the client should try to play back by default. Represented as an
    -- EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
    -- Alternate Audio, Auto Select, Not Default Alternate rendition that the
    -- client may try to play back by default. Represented as an EXT-X-MEDIA in
    -- the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not
    -- Auto Select Alternate rendition that the client will not try to play
    -- back by default. Represented as an EXT-X-MEDIA in the HLS manifest with
    -- DEFAULT=NO, AUTOSELECT=NO
    audioTrackType :: Core.Maybe HlsAudioTrackType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HlsSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioRenditionSets', 'hlsSettings_audioRenditionSets' - List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
--
-- 'iFrameOnlyManifest', 'hlsSettings_iFrameOnlyManifest' - Choose Include (INCLUDE) to have MediaConvert generate a child manifest
-- that lists only the I-frames for this rendition, in addition to your
-- regular manifest for this rendition. You might use this manifest as part
-- of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
--
-- 'segmentModifier', 'hlsSettings_segmentModifier' - Use this setting to add an identifying string to the filename of each
-- segment. The service adds this string between the name modifier and
-- segment index number. You can use format identifiers in the string. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/using-variables-in-your-job-settings.html
--
-- 'audioOnlyContainer', 'hlsSettings_audioOnlyContainer' - Use this setting only in audio-only outputs. Choose MPEG-2 Transport
-- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
-- default value Automatic (AUTOMATIC) to create an audio-only file in a
-- raw container. Regardless of the value that you specify here, if this
-- output has video, the service will place the output into an MPEG2-TS
-- container.
--
-- 'audioGroupId', 'hlsSettings_audioGroupId' - Specifies the group to which the audio Rendition belongs.
--
-- 'audioTrackType', 'hlsSettings_audioTrackType' - Four types of audio-only tracks are supported: Audio-Only Variant Stream
-- The client can play back this audio-only stream instead of video in
-- low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS
-- manifest. Alternate Audio, Auto Select, Default Alternate rendition that
-- the client should try to play back by default. Represented as an
-- EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
-- Alternate Audio, Auto Select, Not Default Alternate rendition that the
-- client may try to play back by default. Represented as an EXT-X-MEDIA in
-- the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not
-- Auto Select Alternate rendition that the client will not try to play
-- back by default. Represented as an EXT-X-MEDIA in the HLS manifest with
-- DEFAULT=NO, AUTOSELECT=NO
newHlsSettings ::
  HlsSettings
newHlsSettings =
  HlsSettings'
    { audioRenditionSets = Core.Nothing,
      iFrameOnlyManifest = Core.Nothing,
      segmentModifier = Core.Nothing,
      audioOnlyContainer = Core.Nothing,
      audioGroupId = Core.Nothing,
      audioTrackType = Core.Nothing
    }

-- | List all the audio groups that are used with the video output stream.
-- Input all the audio GROUP-IDs that are associated to the video, separate
-- by \',\'.
hlsSettings_audioRenditionSets :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hlsSettings_audioRenditionSets = Lens.lens (\HlsSettings' {audioRenditionSets} -> audioRenditionSets) (\s@HlsSettings' {} a -> s {audioRenditionSets = a} :: HlsSettings)

-- | Choose Include (INCLUDE) to have MediaConvert generate a child manifest
-- that lists only the I-frames for this rendition, in addition to your
-- regular manifest for this rendition. You might use this manifest as part
-- of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
hlsSettings_iFrameOnlyManifest :: Lens.Lens' HlsSettings (Core.Maybe HlsIFrameOnlyManifest)
hlsSettings_iFrameOnlyManifest = Lens.lens (\HlsSettings' {iFrameOnlyManifest} -> iFrameOnlyManifest) (\s@HlsSettings' {} a -> s {iFrameOnlyManifest = a} :: HlsSettings)

-- | Use this setting to add an identifying string to the filename of each
-- segment. The service adds this string between the name modifier and
-- segment index number. You can use format identifiers in the string. For
-- more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/using-variables-in-your-job-settings.html
hlsSettings_segmentModifier :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hlsSettings_segmentModifier = Lens.lens (\HlsSettings' {segmentModifier} -> segmentModifier) (\s@HlsSettings' {} a -> s {segmentModifier = a} :: HlsSettings)

-- | Use this setting only in audio-only outputs. Choose MPEG-2 Transport
-- Stream (M2TS) to create a file in an MPEG2-TS container. Keep the
-- default value Automatic (AUTOMATIC) to create an audio-only file in a
-- raw container. Regardless of the value that you specify here, if this
-- output has video, the service will place the output into an MPEG2-TS
-- container.
hlsSettings_audioOnlyContainer :: Lens.Lens' HlsSettings (Core.Maybe HlsAudioOnlyContainer)
hlsSettings_audioOnlyContainer = Lens.lens (\HlsSettings' {audioOnlyContainer} -> audioOnlyContainer) (\s@HlsSettings' {} a -> s {audioOnlyContainer = a} :: HlsSettings)

-- | Specifies the group to which the audio Rendition belongs.
hlsSettings_audioGroupId :: Lens.Lens' HlsSettings (Core.Maybe Core.Text)
hlsSettings_audioGroupId = Lens.lens (\HlsSettings' {audioGroupId} -> audioGroupId) (\s@HlsSettings' {} a -> s {audioGroupId = a} :: HlsSettings)

-- | Four types of audio-only tracks are supported: Audio-Only Variant Stream
-- The client can play back this audio-only stream instead of video in
-- low-bandwidth scenarios. Represented as an EXT-X-STREAM-INF in the HLS
-- manifest. Alternate Audio, Auto Select, Default Alternate rendition that
-- the client should try to play back by default. Represented as an
-- EXT-X-MEDIA in the HLS manifest with DEFAULT=YES, AUTOSELECT=YES
-- Alternate Audio, Auto Select, Not Default Alternate rendition that the
-- client may try to play back by default. Represented as an EXT-X-MEDIA in
-- the HLS manifest with DEFAULT=NO, AUTOSELECT=YES Alternate Audio, not
-- Auto Select Alternate rendition that the client will not try to play
-- back by default. Represented as an EXT-X-MEDIA in the HLS manifest with
-- DEFAULT=NO, AUTOSELECT=NO
hlsSettings_audioTrackType :: Lens.Lens' HlsSettings (Core.Maybe HlsAudioTrackType)
hlsSettings_audioTrackType = Lens.lens (\HlsSettings' {audioTrackType} -> audioTrackType) (\s@HlsSettings' {} a -> s {audioTrackType = a} :: HlsSettings)

instance Core.FromJSON HlsSettings where
  parseJSON =
    Core.withObject
      "HlsSettings"
      ( \x ->
          HlsSettings'
            Core.<$> (x Core..:? "audioRenditionSets")
            Core.<*> (x Core..:? "iFrameOnlyManifest")
            Core.<*> (x Core..:? "segmentModifier")
            Core.<*> (x Core..:? "audioOnlyContainer")
            Core.<*> (x Core..:? "audioGroupId")
            Core.<*> (x Core..:? "audioTrackType")
      )

instance Core.Hashable HlsSettings

instance Core.NFData HlsSettings

instance Core.ToJSON HlsSettings where
  toJSON HlsSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("audioRenditionSets" Core..=)
              Core.<$> audioRenditionSets,
            ("iFrameOnlyManifest" Core..=)
              Core.<$> iFrameOnlyManifest,
            ("segmentModifier" Core..=) Core.<$> segmentModifier,
            ("audioOnlyContainer" Core..=)
              Core.<$> audioOnlyContainer,
            ("audioGroupId" Core..=) Core.<$> audioGroupId,
            ("audioTrackType" Core..=) Core.<$> audioTrackType
          ]
      )
