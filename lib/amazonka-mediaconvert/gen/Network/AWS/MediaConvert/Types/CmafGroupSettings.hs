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
    cgsFragmentLength,
    cgsSegmentControl,
    cgsDestination,
    cgsMinBufferTime,
    cgsMpdProfile,
    cgsWriteHlsManifest,
    cgsAdditionalManifests,
    cgsCodecSpecification,
    cgsBaseURL,
    cgsDestinationSettings,
    cgsMinFinalSegmentLength,
    cgsWriteDashManifest,
    cgsEncryption,
    cgsSegmentLength,
    cgsManifestDurationFormat,
    cgsClientCache,
    cgsWriteSegmentTimelineInRepresentation,
    cgsStreamInfResolution,
    cgsManifestCompression,
  )
where

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
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to CMAF_GROUP_SETTINGS. Each output in a CMAF Output Group may only contain a single video, audio, or caption output.
--
-- /See:/ 'mkCmafGroupSettings' smart constructor.
data CmafGroupSettings = CmafGroupSettings'
  { fragmentLength ::
      Lude.Maybe Lude.Natural,
    segmentControl :: Lude.Maybe CmafSegmentControl,
    destination :: Lude.Maybe Lude.Text,
    minBufferTime :: Lude.Maybe Lude.Natural,
    mpdProfile :: Lude.Maybe CmafMpdProfile,
    writeHlsManifest :: Lude.Maybe CmafWriteHLSManifest,
    additionalManifests ::
      Lude.Maybe [CmafAdditionalManifest],
    codecSpecification :: Lude.Maybe CmafCodecSpecification,
    baseURL :: Lude.Maybe Lude.Text,
    destinationSettings :: Lude.Maybe DestinationSettings,
    minFinalSegmentLength :: Lude.Maybe Lude.Double,
    writeDashManifest :: Lude.Maybe CmafWriteDASHManifest,
    encryption :: Lude.Maybe CmafEncryptionSettings,
    segmentLength :: Lude.Maybe Lude.Natural,
    manifestDurationFormat ::
      Lude.Maybe CmafManifestDurationFormat,
    clientCache :: Lude.Maybe CmafClientCache,
    writeSegmentTimelineInRepresentation ::
      Lude.Maybe CmafWriteSegmentTimelineInRepresentation,
    streamInfResolution ::
      Lude.Maybe CmafStreamInfResolution,
    manifestCompression ::
      Lude.Maybe CmafManifestCompression
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CmafGroupSettings' with the minimum fields required to make a request.
--
-- * 'additionalManifests' - By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
-- * 'baseURL' - A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
-- * 'clientCache' - Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
-- * 'codecSpecification' - Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
-- * 'destination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
-- * 'destinationSettings' - Settings associated with the destination. Will vary based on the type of destination
-- * 'encryption' - DRM settings.
-- * 'fragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
-- * 'manifestCompression' - When set to GZIP, compresses HLS playlist.
-- * 'manifestDurationFormat' - Indicates whether the output manifest should use floating point values for segment duration.
-- * 'minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
-- * 'minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
-- * 'mpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
-- * 'segmentControl' - When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
-- * 'segmentLength' - Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
-- * 'streamInfResolution' - Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
-- * 'writeDashManifest' - When set to ENABLED, a DASH MPD manifest will be generated for this output.
-- * 'writeHlsManifest' - When set to ENABLED, an Apple HLS manifest will be generated for this output.
-- * 'writeSegmentTimelineInRepresentation' - When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
mkCmafGroupSettings ::
  CmafGroupSettings
mkCmafGroupSettings =
  CmafGroupSettings'
    { fragmentLength = Lude.Nothing,
      segmentControl = Lude.Nothing,
      destination = Lude.Nothing,
      minBufferTime = Lude.Nothing,
      mpdProfile = Lude.Nothing,
      writeHlsManifest = Lude.Nothing,
      additionalManifests = Lude.Nothing,
      codecSpecification = Lude.Nothing,
      baseURL = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      minFinalSegmentLength = Lude.Nothing,
      writeDashManifest = Lude.Nothing,
      encryption = Lude.Nothing,
      segmentLength = Lude.Nothing,
      manifestDurationFormat = Lude.Nothing,
      clientCache = Lude.Nothing,
      writeSegmentTimelineInRepresentation = Lude.Nothing,
      streamInfResolution = Lude.Nothing,
      manifestCompression = Lude.Nothing
    }

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsFragmentLength :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Natural)
cgsFragmentLength = Lens.lens (fragmentLength :: CmafGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fragmentLength = a} :: CmafGroupSettings)
{-# DEPRECATED cgsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsSegmentControl :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafSegmentControl)
cgsSegmentControl = Lens.lens (segmentControl :: CmafGroupSettings -> Lude.Maybe CmafSegmentControl) (\s a -> s {segmentControl = a} :: CmafGroupSettings)
{-# DEPRECATED cgsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsDestination :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Text)
cgsDestination = Lens.lens (destination :: CmafGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: CmafGroupSettings)
{-# DEPRECATED cgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- /Note:/ Consider using 'minBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMinBufferTime :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Natural)
cgsMinBufferTime = Lens.lens (minBufferTime :: CmafGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minBufferTime = a} :: CmafGroupSettings)
{-# DEPRECATED cgsMinBufferTime "Use generic-lens or generic-optics with 'minBufferTime' instead." #-}

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- /Note:/ Consider using 'mpdProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMpdProfile :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafMpdProfile)
cgsMpdProfile = Lens.lens (mpdProfile :: CmafGroupSettings -> Lude.Maybe CmafMpdProfile) (\s a -> s {mpdProfile = a} :: CmafGroupSettings)
{-# DEPRECATED cgsMpdProfile "Use generic-lens or generic-optics with 'mpdProfile' instead." #-}

-- | When set to ENABLED, an Apple HLS manifest will be generated for this output.
--
-- /Note:/ Consider using 'writeHlsManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteHlsManifest :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafWriteHLSManifest)
cgsWriteHlsManifest = Lens.lens (writeHlsManifest :: CmafGroupSettings -> Lude.Maybe CmafWriteHLSManifest) (\s a -> s {writeHlsManifest = a} :: CmafGroupSettings)
{-# DEPRECATED cgsWriteHlsManifest "Use generic-lens or generic-optics with 'writeHlsManifest' instead." #-}

-- | By default, the service creates one top-level .m3u8 HLS manifest and one top -level .mpd DASH manifest for each CMAF output group in your job. These default manifests reference every output in the output group. To create additional top-level manifests that reference a subset of the outputs in the output group, specify a list of them here. For each additional manifest that you specify, the service creates one HLS manifest and one DASH manifest.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsAdditionalManifests :: Lens.Lens' CmafGroupSettings (Lude.Maybe [CmafAdditionalManifest])
cgsAdditionalManifests = Lens.lens (additionalManifests :: CmafGroupSettings -> Lude.Maybe [CmafAdditionalManifest]) (\s a -> s {additionalManifests = a} :: CmafGroupSettings)
{-# DEPRECATED cgsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | Specification to use (RFC-6381 or the default RFC-4281) during m3u8 playlist generation.
--
-- /Note:/ Consider using 'codecSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsCodecSpecification :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafCodecSpecification)
cgsCodecSpecification = Lens.lens (codecSpecification :: CmafGroupSettings -> Lude.Maybe CmafCodecSpecification) (\s a -> s {codecSpecification = a} :: CmafGroupSettings)
{-# DEPRECATED cgsCodecSpecification "Use generic-lens or generic-optics with 'codecSpecification' instead." #-}

-- | A partial URI prefix that will be put in the manifest file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- /Note:/ Consider using 'baseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsBaseURL :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Text)
cgsBaseURL = Lens.lens (baseURL :: CmafGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURL = a} :: CmafGroupSettings)
{-# DEPRECATED cgsBaseURL "Use generic-lens or generic-optics with 'baseURL' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsDestinationSettings :: Lens.Lens' CmafGroupSettings (Lude.Maybe DestinationSettings)
cgsDestinationSettings = Lens.lens (destinationSettings :: CmafGroupSettings -> Lude.Maybe DestinationSettings) (\s a -> s {destinationSettings = a} :: CmafGroupSettings)
{-# DEPRECATED cgsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsMinFinalSegmentLength :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Double)
cgsMinFinalSegmentLength = Lens.lens (minFinalSegmentLength :: CmafGroupSettings -> Lude.Maybe Lude.Double) (\s a -> s {minFinalSegmentLength = a} :: CmafGroupSettings)
{-# DEPRECATED cgsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | When set to ENABLED, a DASH MPD manifest will be generated for this output.
--
-- /Note:/ Consider using 'writeDashManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteDashManifest :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafWriteDASHManifest)
cgsWriteDashManifest = Lens.lens (writeDashManifest :: CmafGroupSettings -> Lude.Maybe CmafWriteDASHManifest) (\s a -> s {writeDashManifest = a} :: CmafGroupSettings)
{-# DEPRECATED cgsWriteDashManifest "Use generic-lens or generic-optics with 'writeDashManifest' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsEncryption :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafEncryptionSettings)
cgsEncryption = Lens.lens (encryption :: CmafGroupSettings -> Lude.Maybe CmafEncryptionSettings) (\s a -> s {encryption = a} :: CmafGroupSettings)
{-# DEPRECATED cgsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Use this setting to specify the length, in seconds, of each individual CMAF segment. This value applies to the whole package; that is, to every output in the output group. Note that segments end on the first keyframe after this number of seconds, so the actual segment length might be slightly longer. If you set Segment control (CmafSegmentControl) to single file, the service puts the content of each output in a single file that has metadata that marks these segments. If you set it to segmented files, the service creates multiple files for each output, each with the content of one segment.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsSegmentLength :: Lens.Lens' CmafGroupSettings (Lude.Maybe Lude.Natural)
cgsSegmentLength = Lens.lens (segmentLength :: CmafGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentLength = a} :: CmafGroupSettings)
{-# DEPRECATED cgsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | Indicates whether the output manifest should use floating point values for segment duration.
--
-- /Note:/ Consider using 'manifestDurationFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsManifestDurationFormat :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafManifestDurationFormat)
cgsManifestDurationFormat = Lens.lens (manifestDurationFormat :: CmafGroupSettings -> Lude.Maybe CmafManifestDurationFormat) (\s a -> s {manifestDurationFormat = a} :: CmafGroupSettings)
{-# DEPRECATED cgsManifestDurationFormat "Use generic-lens or generic-optics with 'manifestDurationFormat' instead." #-}

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
--
-- /Note:/ Consider using 'clientCache' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsClientCache :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafClientCache)
cgsClientCache = Lens.lens (clientCache :: CmafGroupSettings -> Lude.Maybe CmafClientCache) (\s a -> s {clientCache = a} :: CmafGroupSettings)
{-# DEPRECATED cgsClientCache "Use generic-lens or generic-optics with 'clientCache' instead." #-}

-- | When you enable Precise segment duration in DASH manifests (writeSegmentTimelineInRepresentation), your DASH manifest shows precise segment durations. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When this feature isn't enabled, the segment durations in your DASH manifest are approximate. The segment duration information appears in the duration attribute of the SegmentTemplate element.
--
-- /Note:/ Consider using 'writeSegmentTimelineInRepresentation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsWriteSegmentTimelineInRepresentation :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafWriteSegmentTimelineInRepresentation)
cgsWriteSegmentTimelineInRepresentation = Lens.lens (writeSegmentTimelineInRepresentation :: CmafGroupSettings -> Lude.Maybe CmafWriteSegmentTimelineInRepresentation) (\s a -> s {writeSegmentTimelineInRepresentation = a} :: CmafGroupSettings)
{-# DEPRECATED cgsWriteSegmentTimelineInRepresentation "Use generic-lens or generic-optics with 'writeSegmentTimelineInRepresentation' instead." #-}

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
--
-- /Note:/ Consider using 'streamInfResolution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsStreamInfResolution :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafStreamInfResolution)
cgsStreamInfResolution = Lens.lens (streamInfResolution :: CmafGroupSettings -> Lude.Maybe CmafStreamInfResolution) (\s a -> s {streamInfResolution = a} :: CmafGroupSettings)
{-# DEPRECATED cgsStreamInfResolution "Use generic-lens or generic-optics with 'streamInfResolution' instead." #-}

-- | When set to GZIP, compresses HLS playlist.
--
-- /Note:/ Consider using 'manifestCompression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgsManifestCompression :: Lens.Lens' CmafGroupSettings (Lude.Maybe CmafManifestCompression)
cgsManifestCompression = Lens.lens (manifestCompression :: CmafGroupSettings -> Lude.Maybe CmafManifestCompression) (\s a -> s {manifestCompression = a} :: CmafGroupSettings)
{-# DEPRECATED cgsManifestCompression "Use generic-lens or generic-optics with 'manifestCompression' instead." #-}

instance Lude.FromJSON CmafGroupSettings where
  parseJSON =
    Lude.withObject
      "CmafGroupSettings"
      ( \x ->
          CmafGroupSettings'
            Lude.<$> (x Lude..:? "fragmentLength")
            Lude.<*> (x Lude..:? "segmentControl")
            Lude.<*> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "minBufferTime")
            Lude.<*> (x Lude..:? "mpdProfile")
            Lude.<*> (x Lude..:? "writeHlsManifest")
            Lude.<*> (x Lude..:? "additionalManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "codecSpecification")
            Lude.<*> (x Lude..:? "baseUrl")
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "minFinalSegmentLength")
            Lude.<*> (x Lude..:? "writeDashManifest")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "segmentLength")
            Lude.<*> (x Lude..:? "manifestDurationFormat")
            Lude.<*> (x Lude..:? "clientCache")
            Lude.<*> (x Lude..:? "writeSegmentTimelineInRepresentation")
            Lude.<*> (x Lude..:? "streamInfResolution")
            Lude.<*> (x Lude..:? "manifestCompression")
      )

instance Lude.ToJSON CmafGroupSettings where
  toJSON CmafGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fragmentLength" Lude..=) Lude.<$> fragmentLength,
            ("segmentControl" Lude..=) Lude.<$> segmentControl,
            ("destination" Lude..=) Lude.<$> destination,
            ("minBufferTime" Lude..=) Lude.<$> minBufferTime,
            ("mpdProfile" Lude..=) Lude.<$> mpdProfile,
            ("writeHlsManifest" Lude..=) Lude.<$> writeHlsManifest,
            ("additionalManifests" Lude..=) Lude.<$> additionalManifests,
            ("codecSpecification" Lude..=) Lude.<$> codecSpecification,
            ("baseUrl" Lude..=) Lude.<$> baseURL,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("minFinalSegmentLength" Lude..=) Lude.<$> minFinalSegmentLength,
            ("writeDashManifest" Lude..=) Lude.<$> writeDashManifest,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("segmentLength" Lude..=) Lude.<$> segmentLength,
            ("manifestDurationFormat" Lude..=) Lude.<$> manifestDurationFormat,
            ("clientCache" Lude..=) Lude.<$> clientCache,
            ("writeSegmentTimelineInRepresentation" Lude..=)
              Lude.<$> writeSegmentTimelineInRepresentation,
            ("streamInfResolution" Lude..=) Lude.<$> streamInfResolution,
            ("manifestCompression" Lude..=) Lude.<$> manifestCompression
          ]
      )
