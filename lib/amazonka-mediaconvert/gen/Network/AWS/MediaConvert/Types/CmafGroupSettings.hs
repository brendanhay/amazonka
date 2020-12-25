{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafGroupSettings
  ( CmafGroupSettings (..),

    -- * Smart constructor
    mkCmafGroupSettings,

    -- * Lenses
    cgsAdditionalManifests,
    cgsBaseUrl,
    cgsClientCache,
    cgsCodecSpecification,
    cgsDestination,
    cgsDestinationSettings,
    cgsEncryption,
    cgsFragmentLength,
    cgsManifestCompression,
    cgsManifestDurationFormat,
    cgsMinBufferTime,
    cgsMinFinalSegmentLength,
    cgsMpdProfile,
    cgsSegmentControl,
    cgsSegmentLength,
    cgsStreamInfResolution,
    cgsWriteDashManifest,
    cgsWriteHlsManifest,
    cgsWriteSegmentTimelineInRepresentation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.CmafAdditionalManifest as Types
import qualified Network.AWS.MediaConvert.Types.CmafClientCache as Types
import qualified Network.AWS.MediaConvert.Types.CmafCodecSpecification as Types
import qualified Network.AWS.MediaConvert.Types.CmafEncryptionSettings as Types
import qualified Network.AWS.MediaConvert.Types.CmafManifestCompression as Types
import qualified Network.AWS.MediaConvert.Types.CmafManifestDurationFormat as Types
import qualified Network.AWS.MediaConvert.Types.CmafMpdProfile as Types
import qualified Network.AWS.MediaConvert.Types.CmafSegmentControl as Types
import qualified Network.AWS.MediaConvert.Types.CmafStreamInfResolution as Types
import qualified Network.AWS.MediaConvert.Types.CmafWriteDASHManifest as Types
import qualified Network.AWS.MediaConvert.Types.CmafWriteHLSManifest as Types
import qualified Network.AWS.MediaConvert.Types.CmafWriteSegmentTimelineInRepresentation as Types
import qualified Network.AWS.MediaConvert.Types.DestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /See:/ 'mkCmafGroupSettings' smart constructor.
data CmafGroupSettings = CmafGroupSettings'
  { -- | By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
    additionalManifests :: Core.Maybe [Types.CmafAdditionalManifest],
    -- | A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
    baseUrl :: Core.Maybe Core.Text,
    -- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
    clientCache :: Core.Maybe Types.CmafClientCache,
    -- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
    codecSpecification :: Core.Maybe Types.CmafCodecSpecification,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Core.Maybe Types.DestinationSettings,
    -- | DRM settings.
    encryption :: Core.Maybe Types.CmafEncryptionSettings,
    -- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | When set to GZIP, compresses HLS playlist.
    manifestCompression :: Core.Maybe Types.CmafManifestCompression,
    -- | Indicates whether the output manifest should use floating point values for segment duration.
    manifestDurationFormat :: Core.Maybe Types.CmafManifestDurationFormat,
    -- | Minimum time of initially buffered media that is needed to ensure smooth playout.
    minBufferTime :: Core.Maybe Core.Natural,
    -- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Core.Maybe Core.Double,
    -- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Core.Maybe Types.CmafMpdProfile,
    -- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Core.Maybe Types.CmafSegmentControl,
    -- | Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
    segmentLength :: Core.Maybe Core.Natural,
    -- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
    streamInfResolution :: Core.Maybe Types.CmafStreamInfResolution,
    -- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
    writeDashManifest :: Core.Maybe Types.CmafWriteDASHManifest,
    -- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
    writeHlsManifest :: Core.Maybe Types.CmafWriteHLSManifest,
    -- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
    writeSegmentTimelineInRepresentation :: Core.Maybe Types.CmafWriteSegmentTimelineInRepresentation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CmafGroupSettings' value with any optional fields omitted.
mkCmafGroupSettings ::
  CmafGroupSettings
mkCmafGroupSettings =
  CmafGroupSettings'
    { additionalManifests = Core.Nothing,
      baseUrl = Core.Nothing,
      clientCache = Core.Nothing,
      codecSpecification = Core.Nothing,
      destination = Core.Nothing,
      destinationSettings = Core.Nothing,
      encryption = Core.Nothing,
      fragmentLength = Core.Nothing,
      manifestCompression = Core.Nothing,
      manifestDurationFormat = Core.Nothing,
      minBufferTime = Core.Nothing,
      minFinalSegmentLength = Core.Nothing,
      mpdProfile = Core.Nothing,
      segmentControl = Core.Nothing,
      segmentLength = Core.Nothing,
      streamInfResolution = Core.Nothing,
      writeDashManifest = Core.Nothing,
      writeHlsManifest = Core.Nothing,
      writeSegmentTimelineInRepresentation = Core.Nothing
    }

-- | By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsAdditionalManifests :: Lens.Lens' CmafGroupSettings (Core.Maybe [Types.CmafAdditionalManifest])
cgsAdditionalManifests = Lens.field @"additionalManifests"
{-# DEPRECATED cgsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- /Note:/ Consider using 'baseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsBaseUrl :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Text)
cgsBaseUrl = Lens.field @"baseUrl"
{-# DEPRECATED cgsBaseUrl "Use generic-lens or generic-optics with 'baseUrl' instead." #-}

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsClientCache :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafClientCache)
cgsClientCache = Lens.field @"clientCache"
{-# DEPRECATED cgsClientCache "Use generic-lens or generic-optics with 'clientCache' instead." #-}

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- /Note:/ Consider using 'codecSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsCodecSpecification :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafCodecSpecification)
cgsCodecSpecification = Lens.field @"codecSpecification"
{-# DEPRECATED cgsCodecSpecification "Use generic-lens or generic-optics with 'codecSpecification' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsDestination :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Text)
cgsDestination = Lens.field @"destination"
{-# DEPRECATED cgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsDestinationSettings :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.DestinationSettings)
cgsDestinationSettings = Lens.field @"destinationSettings"
{-# DEPRECATED cgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsEncryption :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafEncryptionSettings)
cgsEncryption = Lens.field @"encryption"
{-# DEPRECATED cgsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsFragmentLength :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Natural)
cgsFragmentLength = Lens.field @"fragmentLength"
{-# DEPRECATED cgsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | When set to GZIP, compresses HLS playlist.
--
-- /Note:/ Consider using 'manifestCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsManifestCompression :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafManifestCompression)
cgsManifestCompression = Lens.field @"manifestCompression"
{-# DEPRECATED cgsManifestCompression "Use generic-lens or generic-optics with 'manifestCompression' instead." #-}

-- | Indicates whether the output manifest should use floating point values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsManifestDurationFormat :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafManifestDurationFormat)
cgsManifestDurationFormat = Lens.field @"manifestDurationFormat"
{-# DEPRECATED cgsManifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead." #-}

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- /Note:/ Consider using 'minBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMinBufferTime :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Natural)
cgsMinBufferTime = Lens.field @"minBufferTime"
{-# DEPRECATED cgsMinBufferTime "Use generic-lens or generic-optics with 'minBufferTime' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMinFinalSegmentLength :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Double)
cgsMinFinalSegmentLength = Lens.field @"minFinalSegmentLength"
{-# DEPRECATED cgsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- /Note:/ Consider using 'mpdProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMpdProfile :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafMpdProfile)
cgsMpdProfile = Lens.field @"mpdProfile"
{-# DEPRECATED cgsMpdProfile "Use generic-lens or generic-optics with 'mpdProfile' instead." #-}

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsSegmentControl :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafSegmentControl)
cgsSegmentControl = Lens.field @"segmentControl"
{-# DEPRECATED cgsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsSegmentLength :: Lens.Lens' CmafGroupSettings (Core.Maybe Core.Natural)
cgsSegmentLength = Lens.field @"segmentLength"
{-# DEPRECATED cgsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsStreamInfResolution :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafStreamInfResolution)
cgsStreamInfResolution = Lens.field @"streamInfResolution"
{-# DEPRECATED cgsStreamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead." #-}

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
--
-- /Note:/ Consider using 'writeDashManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteDashManifest :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafWriteDASHManifest)
cgsWriteDashManifest = Lens.field @"writeDashManifest"
{-# DEPRECATED cgsWriteDashManifest "Use generic-lens or generic-optics with 'writeDashManifest' instead." #-}

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
--
-- /Note:/ Consider using 'writeHlsManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteHlsManifest :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafWriteHLSManifest)
cgsWriteHlsManifest = Lens.field @"writeHlsManifest"
{-# DEPRECATED cgsWriteHlsManifest "Use generic-lens or generic-optics with 'writeHlsManifest' instead." #-}

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
--
-- /Note:/ Consider using 'writeSegmentTimelineInRepresentation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteSegmentTimelineInRepresentation :: Lens.Lens' CmafGroupSettings (Core.Maybe Types.CmafWriteSegmentTimelineInRepresentation)
cgsWriteSegmentTimelineInRepresentation = Lens.field @"writeSegmentTimelineInRepresentation"
{-# DEPRECATED cgsWriteSegmentTimelineInRepresentation "Use generic-lens or generic-optics with 'writeSegmentTimelineInRepresentation' instead." #-}

instance Core.FromJSON CmafGroupSettings where
  toJSON CmafGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("additionalManifests" Core..=) Core.<$> additionalManifests,
            ("baseUrl" Core..=) Core.<$> baseUrl,
            ("clientCache" Core..=) Core.<$> clientCache,
            ("codecSpecification" Core..=) Core.<$> codecSpecification,
            ("destination" Core..=) Core.<$> destination,
            ("destinationSettings" Core..=) Core.<$> destinationSettings,
            ("encryption" Core..=) Core.<$> encryption,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("manifestCompression" Core..=) Core.<$> manifestCompression,
            ("manifestDurationFormat" Core..=) Core.<$> manifestDurationFormat,
            ("minBufferTime" Core..=) Core.<$> minBufferTime,
            ("minFinalSegmentLength" Core..=) Core.<$> minFinalSegmentLength,
            ("mpdProfile" Core..=) Core.<$> mpdProfile,
            ("segmentControl" Core..=) Core.<$> segmentControl,
            ("segmentLength" Core..=) Core.<$> segmentLength,
            ("streamInfResolution" Core..=) Core.<$> streamInfResolution,
            ("writeDashManifest" Core..=) Core.<$> writeDashManifest,
            ("writeHlsManifest" Core..=) Core.<$> writeHlsManifest,
            ("writeSegmentTimelineInRepresentation" Core..=)
              Core.<$> writeSegmentTimelineInRepresentation
          ]
      )

instance Core.FromJSON CmafGroupSettings where
  parseJSON =
    Core.withObject "CmafGroupSettings" Core.$
      \x ->
        CmafGroupSettings'
          Core.<$> (x Core..:? "additionalManifests")
          Core.<*> (x Core..:? "baseUrl")
          Core.<*> (x Core..:? "clientCache")
          Core.<*> (x Core..:? "codecSpecification")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "destinationSettings")
          Core.<*> (x Core..:? "encryption")
          Core.<*> (x Core..:? "fragmentLength")
          Core.<*> (x Core..:? "manifestCompression")
          Core.<*> (x Core..:? "manifestDurationFormat")
          Core.<*> (x Core..:? "minBufferTime")
          Core.<*> (x Core..:? "minFinalSegmentLength")
          Core.<*> (x Core..:? "mpdProfile")
          Core.<*> (x Core..:? "segmentControl")
          Core.<*> (x Core..:? "segmentLength")
          Core.<*> (x Core..:? "streamInfResolution")
          Core.<*> (x Core..:? "writeDashManifest")
          Core.<*> (x Core..:? "writeHlsManifest")
          Core.<*> (x Core..:? "writeSegmentTimelineInRepresentation")
