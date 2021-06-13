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
-- Module      : Network.AWS.MediaConvert.Types.CmafGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.CmafAdditionalManifest
import Network.AWS.MediaConvert.Types.CmafClientCache
import Network.AWS.MediaConvert.Types.CmafCodecSpecification
import Network.AWS.MediaConvert.Types.CmafEncryptionSettings
import Network.AWS.MediaConvert.Types.CmafManifestCompression
import Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
import Network.AWS.MediaConvert.Types.CmafMpdProfile
import Network.AWS.MediaConvert.Types.CmafSegmentControl
import Network.AWS.MediaConvert.Types.CmafStreamInfResolution
import Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
import Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
import Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only
-- contain a single video, audio, or caption output.
--
-- /See:/ 'newCmafGroupSettings' smart constructor.
data CmafGroupSettings = CmafGroupSettings'
  { -- | Use this setting to specify the length, in seconds, of each individual
    -- CMAF segment. This value applies to the whole package; that is, to every
    -- output in the output group. Note that segments end on the first keyframe
    -- after this number of seconds, so the actual segment length might be
    -- slightly longer. If you set Segment control (CmafSegmentControl) to
    -- single file, the service puts the content of each output in a single
    -- file that has metadata that marks these segments. If you set it to
    -- segmented files, the service creates multiple files for each output,
    -- each with the content of one segment.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | When set to SINGLE_FILE, a single output file is generated, which is
    -- internally segmented using the Fragment Length and Segment Length. When
    -- set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Prelude.Maybe CmafSegmentControl,
    -- | When set to ENABLED, a DASH MPD manifest will be generated for this
    -- output.
    writeDashManifest :: Prelude.Maybe CmafWriteDASHManifest,
    -- | Length of fragments to generate (in seconds). Fragment length must be
    -- compatible with GOP size and Framerate. Note that fragments will end on
    -- the next keyframe after this number of seconds, so actual fragment
    -- length may be longer. When Emit Single File is checked, the
    -- fragmentation is internal to a single output file and it does not cause
    -- the creation of many output files as in other output types.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Prelude.Maybe CmafManifestCompression,
    -- | A partial URI prefix that will be put in the manifest file at the top
    -- level BaseURL element. Can be used if streams are delivered from a
    -- different URL than the manifest file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
    -- tag of variant manifest.
    streamInfResolution :: Prelude.Maybe CmafStreamInfResolution,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
    -- playlist generation.
    codecSpecification :: Prelude.Maybe CmafCodecSpecification,
    -- | By default, the service creates one top-level .m3u8 HLS manifest and one
    -- top -level .mpd DASH manifest for each CMAF output group in your job.
    -- These default manifests reference every output in the output group. To
    -- create additional top-level manifests that reference a subset of the
    -- outputs in the output group, specify a list of them here. For each
    -- additional manifest that you specify, the service creates one HLS
    -- manifest and one DASH manifest.
    additionalManifests :: Prelude.Maybe [CmafAdditionalManifest],
    -- | Specify whether your DASH profile is on-demand or main. When you choose
    -- Main profile (MAIN_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
    -- you choose On-demand (ON_DEMAND_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
    -- On-demand, you must also set the output group setting Segment control
    -- (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Prelude.Maybe CmafMpdProfile,
    -- | DRM settings.
    encryption :: Prelude.Maybe CmafEncryptionSettings,
    -- | Minimum time of initially buffered media that is needed to ensure smooth
    -- playout.
    minBufferTime :: Prelude.Maybe Prelude.Natural,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Keep this setting at the default value of 0, unless you are
    -- troubleshooting a problem with how devices play back the end of your
    -- video asset. If you know that player devices are hanging on the final
    -- segment of your video because the length of your final segment is too
    -- short, use this setting to specify a minimum final segment length, in
    -- seconds. Choose a value that is greater than or equal to 1 and less than
    -- your segment length. When you specify a value for this setting, the
    -- encoder will combine any final segment that is shorter than the length
    -- that you specify with the previous segment. For example, your segment
    -- length is 3 seconds and your final segment is .5 seconds without a
    -- minimum final segment length; when you set the minimum final segment
    -- length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Prelude.Maybe Prelude.Double,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | When you enable Precise segment duration in DASH manifests
    -- (writeSegmentTimelineInRepresentation), your DASH manifest shows precise
    -- segment durations. The segment duration information appears inside the
    -- SegmentTimeline element, inside SegmentTemplate at the Representation
    -- level. When this feature isn\'t enabled, the segment durations in your
    -- DASH manifest are approximate. The segment duration information appears
    -- in the duration attribute of the SegmentTemplate element.
    writeSegmentTimelineInRepresentation :: Prelude.Maybe CmafWriteSegmentTimelineInRepresentation,
    -- | When set to ENABLED, an Apple HLS manifest will be generated for this
    -- output.
    writeHlsManifest :: Prelude.Maybe CmafWriteHLSManifest,
    -- | Disable this setting only when your workflow requires the
    -- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
    -- (ENABLED) and control caching in your video distribution set up. For
    -- example, use the Cache-Control http header.
    clientCache :: Prelude.Maybe CmafClientCache,
    -- | Indicates whether the output manifest should use floating point values
    -- for segment duration.
    manifestDurationFormat :: Prelude.Maybe CmafManifestDurationFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CmafGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentLength', 'cmafGroupSettings_segmentLength' - Use this setting to specify the length, in seconds, of each individual
-- CMAF segment. This value applies to the whole package; that is, to every
-- output in the output group. Note that segments end on the first keyframe
-- after this number of seconds, so the actual segment length might be
-- slightly longer. If you set Segment control (CmafSegmentControl) to
-- single file, the service puts the content of each output in a single
-- file that has metadata that marks these segments. If you set it to
-- segmented files, the service creates multiple files for each output,
-- each with the content of one segment.
--
-- 'segmentControl', 'cmafGroupSettings_segmentControl' - When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
--
-- 'writeDashManifest', 'cmafGroupSettings_writeDashManifest' - When set to ENABLED, a DASH MPD manifest will be generated for this
-- output.
--
-- 'fragmentLength', 'cmafGroupSettings_fragmentLength' - Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
--
-- 'manifestCompression', 'cmafGroupSettings_manifestCompression' - When set to GZIP, compresses HLS playlist.
--
-- 'baseUrl', 'cmafGroupSettings_baseUrl' - A partial URI prefix that will be put in the manifest file at the top
-- level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
--
-- 'streamInfResolution', 'cmafGroupSettings_streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
--
-- 'codecSpecification', 'cmafGroupSettings_codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
--
-- 'additionalManifests', 'cmafGroupSettings_additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest and one
-- top -level .mpd DASH manifest for each CMAF output group in your job.
-- These default manifests reference every output in the output group. To
-- create additional top-level manifests that reference a subset of the
-- outputs in the output group, specify a list of them here. For each
-- additional manifest that you specify, the service creates one HLS
-- manifest and one DASH manifest.
--
-- 'mpdProfile', 'cmafGroupSettings_mpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
--
-- 'encryption', 'cmafGroupSettings_encryption' - DRM settings.
--
-- 'minBufferTime', 'cmafGroupSettings_minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
--
-- 'destination', 'cmafGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'minFinalSegmentLength', 'cmafGroupSettings_minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are
-- troubleshooting a problem with how devices play back the end of your
-- video asset. If you know that player devices are hanging on the final
-- segment of your video because the length of your final segment is too
-- short, use this setting to specify a minimum final segment length, in
-- seconds. Choose a value that is greater than or equal to 1 and less than
-- your segment length. When you specify a value for this setting, the
-- encoder will combine any final segment that is shorter than the length
-- that you specify with the previous segment. For example, your segment
-- length is 3 seconds and your final segment is .5 seconds without a
-- minimum final segment length; when you set the minimum final segment
-- length to 1, your final segment is 3.5 seconds.
--
-- 'destinationSettings', 'cmafGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'writeSegmentTimelineInRepresentation', 'cmafGroupSettings_writeSegmentTimelineInRepresentation' - When you enable Precise segment duration in DASH manifests
-- (writeSegmentTimelineInRepresentation), your DASH manifest shows precise
-- segment durations. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When this feature isn\'t enabled, the segment durations in your
-- DASH manifest are approximate. The segment duration information appears
-- in the duration attribute of the SegmentTemplate element.
--
-- 'writeHlsManifest', 'cmafGroupSettings_writeHlsManifest' - When set to ENABLED, an Apple HLS manifest will be generated for this
-- output.
--
-- 'clientCache', 'cmafGroupSettings_clientCache' - Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
--
-- 'manifestDurationFormat', 'cmafGroupSettings_manifestDurationFormat' - Indicates whether the output manifest should use floating point values
-- for segment duration.
newCmafGroupSettings ::
  CmafGroupSettings
newCmafGroupSettings =
  CmafGroupSettings'
    { segmentLength = Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      writeDashManifest = Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      manifestCompression = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      streamInfResolution = Prelude.Nothing,
      codecSpecification = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      mpdProfile = Prelude.Nothing,
      encryption = Prelude.Nothing,
      minBufferTime = Prelude.Nothing,
      destination = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      writeSegmentTimelineInRepresentation =
        Prelude.Nothing,
      writeHlsManifest = Prelude.Nothing,
      clientCache = Prelude.Nothing,
      manifestDurationFormat = Prelude.Nothing
    }

-- | Use this setting to specify the length, in seconds, of each individual
-- CMAF segment. This value applies to the whole package; that is, to every
-- output in the output group. Note that segments end on the first keyframe
-- after this number of seconds, so the actual segment length might be
-- slightly longer. If you set Segment control (CmafSegmentControl) to
-- single file, the service puts the content of each output in a single
-- file that has metadata that marks these segments. If you set it to
-- segmented files, the service creates multiple files for each output,
-- each with the content of one segment.
cmafGroupSettings_segmentLength :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Natural)
cmafGroupSettings_segmentLength = Lens.lens (\CmafGroupSettings' {segmentLength} -> segmentLength) (\s@CmafGroupSettings' {} a -> s {segmentLength = a} :: CmafGroupSettings)

-- | When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
cmafGroupSettings_segmentControl :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafSegmentControl)
cmafGroupSettings_segmentControl = Lens.lens (\CmafGroupSettings' {segmentControl} -> segmentControl) (\s@CmafGroupSettings' {} a -> s {segmentControl = a} :: CmafGroupSettings)

-- | When set to ENABLED, a DASH MPD manifest will be generated for this
-- output.
cmafGroupSettings_writeDashManifest :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafWriteDASHManifest)
cmafGroupSettings_writeDashManifest = Lens.lens (\CmafGroupSettings' {writeDashManifest} -> writeDashManifest) (\s@CmafGroupSettings' {} a -> s {writeDashManifest = a} :: CmafGroupSettings)

-- | Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
cmafGroupSettings_fragmentLength :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Natural)
cmafGroupSettings_fragmentLength = Lens.lens (\CmafGroupSettings' {fragmentLength} -> fragmentLength) (\s@CmafGroupSettings' {} a -> s {fragmentLength = a} :: CmafGroupSettings)

-- | When set to GZIP, compresses HLS playlist.
cmafGroupSettings_manifestCompression :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafManifestCompression)
cmafGroupSettings_manifestCompression = Lens.lens (\CmafGroupSettings' {manifestCompression} -> manifestCompression) (\s@CmafGroupSettings' {} a -> s {manifestCompression = a} :: CmafGroupSettings)

-- | A partial URI prefix that will be put in the manifest file at the top
-- level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
cmafGroupSettings_baseUrl :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Text)
cmafGroupSettings_baseUrl = Lens.lens (\CmafGroupSettings' {baseUrl} -> baseUrl) (\s@CmafGroupSettings' {} a -> s {baseUrl = a} :: CmafGroupSettings)

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF
-- tag of variant manifest.
cmafGroupSettings_streamInfResolution :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafStreamInfResolution)
cmafGroupSettings_streamInfResolution = Lens.lens (\CmafGroupSettings' {streamInfResolution} -> streamInfResolution) (\s@CmafGroupSettings' {} a -> s {streamInfResolution = a} :: CmafGroupSettings)

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8
-- playlist generation.
cmafGroupSettings_codecSpecification :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafCodecSpecification)
cmafGroupSettings_codecSpecification = Lens.lens (\CmafGroupSettings' {codecSpecification} -> codecSpecification) (\s@CmafGroupSettings' {} a -> s {codecSpecification = a} :: CmafGroupSettings)

-- | By default, the service creates one top-level .m3u8 HLS manifest and one
-- top -level .mpd DASH manifest for each CMAF output group in your job.
-- These default manifests reference every output in the output group. To
-- create additional top-level manifests that reference a subset of the
-- outputs in the output group, specify a list of them here. For each
-- additional manifest that you specify, the service creates one HLS
-- manifest and one DASH manifest.
cmafGroupSettings_additionalManifests :: Lens.Lens' CmafGroupSettings (Prelude.Maybe [CmafAdditionalManifest])
cmafGroupSettings_additionalManifests = Lens.lens (\CmafGroupSettings' {additionalManifests} -> additionalManifests) (\s@CmafGroupSettings' {} a -> s {additionalManifests = a} :: CmafGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
cmafGroupSettings_mpdProfile :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafMpdProfile)
cmafGroupSettings_mpdProfile = Lens.lens (\CmafGroupSettings' {mpdProfile} -> mpdProfile) (\s@CmafGroupSettings' {} a -> s {mpdProfile = a} :: CmafGroupSettings)

-- | DRM settings.
cmafGroupSettings_encryption :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafEncryptionSettings)
cmafGroupSettings_encryption = Lens.lens (\CmafGroupSettings' {encryption} -> encryption) (\s@CmafGroupSettings' {} a -> s {encryption = a} :: CmafGroupSettings)

-- | Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
cmafGroupSettings_minBufferTime :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Natural)
cmafGroupSettings_minBufferTime = Lens.lens (\CmafGroupSettings' {minBufferTime} -> minBufferTime) (\s@CmafGroupSettings' {} a -> s {minBufferTime = a} :: CmafGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
cmafGroupSettings_destination :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Text)
cmafGroupSettings_destination = Lens.lens (\CmafGroupSettings' {destination} -> destination) (\s@CmafGroupSettings' {} a -> s {destination = a} :: CmafGroupSettings)

-- | Keep this setting at the default value of 0, unless you are
-- troubleshooting a problem with how devices play back the end of your
-- video asset. If you know that player devices are hanging on the final
-- segment of your video because the length of your final segment is too
-- short, use this setting to specify a minimum final segment length, in
-- seconds. Choose a value that is greater than or equal to 1 and less than
-- your segment length. When you specify a value for this setting, the
-- encoder will combine any final segment that is shorter than the length
-- that you specify with the previous segment. For example, your segment
-- length is 3 seconds and your final segment is .5 seconds without a
-- minimum final segment length; when you set the minimum final segment
-- length to 1, your final segment is 3.5 seconds.
cmafGroupSettings_minFinalSegmentLength :: Lens.Lens' CmafGroupSettings (Prelude.Maybe Prelude.Double)
cmafGroupSettings_minFinalSegmentLength = Lens.lens (\CmafGroupSettings' {minFinalSegmentLength} -> minFinalSegmentLength) (\s@CmafGroupSettings' {} a -> s {minFinalSegmentLength = a} :: CmafGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
cmafGroupSettings_destinationSettings :: Lens.Lens' CmafGroupSettings (Prelude.Maybe DestinationSettings)
cmafGroupSettings_destinationSettings = Lens.lens (\CmafGroupSettings' {destinationSettings} -> destinationSettings) (\s@CmafGroupSettings' {} a -> s {destinationSettings = a} :: CmafGroupSettings)

-- | When you enable Precise segment duration in DASH manifests
-- (writeSegmentTimelineInRepresentation), your DASH manifest shows precise
-- segment durations. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When this feature isn\'t enabled, the segment durations in your
-- DASH manifest are approximate. The segment duration information appears
-- in the duration attribute of the SegmentTemplate element.
cmafGroupSettings_writeSegmentTimelineInRepresentation :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafWriteSegmentTimelineInRepresentation)
cmafGroupSettings_writeSegmentTimelineInRepresentation = Lens.lens (\CmafGroupSettings' {writeSegmentTimelineInRepresentation} -> writeSegmentTimelineInRepresentation) (\s@CmafGroupSettings' {} a -> s {writeSegmentTimelineInRepresentation = a} :: CmafGroupSettings)

-- | When set to ENABLED, an Apple HLS manifest will be generated for this
-- output.
cmafGroupSettings_writeHlsManifest :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafWriteHLSManifest)
cmafGroupSettings_writeHlsManifest = Lens.lens (\CmafGroupSettings' {writeHlsManifest} -> writeHlsManifest) (\s@CmafGroupSettings' {} a -> s {writeHlsManifest = a} :: CmafGroupSettings)

-- | Disable this setting only when your workflow requires the
-- #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled
-- (ENABLED) and control caching in your video distribution set up. For
-- example, use the Cache-Control http header.
cmafGroupSettings_clientCache :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafClientCache)
cmafGroupSettings_clientCache = Lens.lens (\CmafGroupSettings' {clientCache} -> clientCache) (\s@CmafGroupSettings' {} a -> s {clientCache = a} :: CmafGroupSettings)

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
cmafGroupSettings_manifestDurationFormat :: Lens.Lens' CmafGroupSettings (Prelude.Maybe CmafManifestDurationFormat)
cmafGroupSettings_manifestDurationFormat = Lens.lens (\CmafGroupSettings' {manifestDurationFormat} -> manifestDurationFormat) (\s@CmafGroupSettings' {} a -> s {manifestDurationFormat = a} :: CmafGroupSettings)

instance Core.FromJSON CmafGroupSettings where
  parseJSON =
    Core.withObject
      "CmafGroupSettings"
      ( \x ->
          CmafGroupSettings'
            Prelude.<$> (x Core..:? "segmentLength")
            Prelude.<*> (x Core..:? "segmentControl")
            Prelude.<*> (x Core..:? "writeDashManifest")
            Prelude.<*> (x Core..:? "fragmentLength")
            Prelude.<*> (x Core..:? "manifestCompression")
            Prelude.<*> (x Core..:? "baseUrl")
            Prelude.<*> (x Core..:? "streamInfResolution")
            Prelude.<*> (x Core..:? "codecSpecification")
            Prelude.<*> ( x Core..:? "additionalManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "mpdProfile")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "minBufferTime")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "minFinalSegmentLength")
            Prelude.<*> (x Core..:? "destinationSettings")
            Prelude.<*> (x Core..:? "writeSegmentTimelineInRepresentation")
            Prelude.<*> (x Core..:? "writeHlsManifest")
            Prelude.<*> (x Core..:? "clientCache")
            Prelude.<*> (x Core..:? "manifestDurationFormat")
      )

instance Prelude.Hashable CmafGroupSettings

instance Prelude.NFData CmafGroupSettings

instance Core.ToJSON CmafGroupSettings where
  toJSON CmafGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentLength" Core..=) Prelude.<$> segmentLength,
            ("segmentControl" Core..=)
              Prelude.<$> segmentControl,
            ("writeDashManifest" Core..=)
              Prelude.<$> writeDashManifest,
            ("fragmentLength" Core..=)
              Prelude.<$> fragmentLength,
            ("manifestCompression" Core..=)
              Prelude.<$> manifestCompression,
            ("baseUrl" Core..=) Prelude.<$> baseUrl,
            ("streamInfResolution" Core..=)
              Prelude.<$> streamInfResolution,
            ("codecSpecification" Core..=)
              Prelude.<$> codecSpecification,
            ("additionalManifests" Core..=)
              Prelude.<$> additionalManifests,
            ("mpdProfile" Core..=) Prelude.<$> mpdProfile,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("minBufferTime" Core..=) Prelude.<$> minBufferTime,
            ("destination" Core..=) Prelude.<$> destination,
            ("minFinalSegmentLength" Core..=)
              Prelude.<$> minFinalSegmentLength,
            ("destinationSettings" Core..=)
              Prelude.<$> destinationSettings,
            ("writeSegmentTimelineInRepresentation" Core..=)
              Prelude.<$> writeSegmentTimelineInRepresentation,
            ("writeHlsManifest" Core..=)
              Prelude.<$> writeHlsManifest,
            ("clientCache" Core..=) Prelude.<$> clientCache,
            ("manifestDurationFormat" Core..=)
              Prelude.<$> manifestDurationFormat
          ]
      )
