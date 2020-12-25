{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoGroupSettings
  ( DashIsoGroupSettings (..),

    -- * Smart constructor
    mkDashIsoGroupSettings,

    -- * Lenses
    digsAdditionalManifests,
    digsBaseUrl,
    digsDestination,
    digsDestinationSettings,
    digsEncryption,
    digsFragmentLength,
    digsHbbtvCompliance,
    digsMinBufferTime,
    digsMinFinalSegmentLength,
    digsMpdProfile,
    digsSegmentControl,
    digsSegmentLength,
    digsWriteSegmentTimelineInRepresentation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.DashAdditionalManifest as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoMpdProfile as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoSegmentControl as Types
import qualified Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation as Types
import qualified Network.AWS.MediaConvert.Types.DestinationSettings as Types
import qualified Network.AWS.Prelude as Core

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'mkDashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { -- | By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
    additionalManifests :: Core.Maybe [Types.DashAdditionalManifest],
    -- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
    baseUrl :: Core.Maybe Core.Text,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Core.Maybe Types.DestinationSettings,
    -- | DRM settings.
    encryption :: Core.Maybe Types.DashIsoEncryptionSettings,
    -- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | Supports HbbTV specification as indicated
    hbbtvCompliance :: Core.Maybe Types.DashIsoHbbtvCompliance,
    -- | Minimum time of initially buffered media that is needed to ensure smooth playout.
    minBufferTime :: Core.Maybe Core.Natural,
    -- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Core.Maybe Core.Double,
    -- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Core.Maybe Types.DashIsoMpdProfile,
    -- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Core.Maybe Types.DashIsoSegmentControl,
    -- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
    segmentLength :: Core.Maybe Core.Natural,
    -- | If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
    writeSegmentTimelineInRepresentation :: Core.Maybe Types.DashIsoWriteSegmentTimelineInRepresentation
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DashIsoGroupSettings' value with any optional fields omitted.
mkDashIsoGroupSettings ::
  DashIsoGroupSettings
mkDashIsoGroupSettings =
  DashIsoGroupSettings'
    { additionalManifests = Core.Nothing,
      baseUrl = Core.Nothing,
      destination = Core.Nothing,
      destinationSettings = Core.Nothing,
      encryption = Core.Nothing,
      fragmentLength = Core.Nothing,
      hbbtvCompliance = Core.Nothing,
      minBufferTime = Core.Nothing,
      minFinalSegmentLength = Core.Nothing,
      mpdProfile = Core.Nothing,
      segmentControl = Core.Nothing,
      segmentLength = Core.Nothing,
      writeSegmentTimelineInRepresentation = Core.Nothing
    }

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsAdditionalManifests :: Lens.Lens' DashIsoGroupSettings (Core.Maybe [Types.DashAdditionalManifest])
digsAdditionalManifests = Lens.field @"additionalManifests"
{-# DEPRECATED digsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- /Note:/ Consider using 'baseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsBaseUrl :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Text)
digsBaseUrl = Lens.field @"baseUrl"
{-# DEPRECATED digsBaseUrl "Use generic-lens or generic-optics with 'baseUrl' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsDestination :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Text)
digsDestination = Lens.field @"destination"
{-# DEPRECATED digsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsDestinationSettings :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DestinationSettings)
digsDestinationSettings = Lens.field @"destinationSettings"
{-# DEPRECATED digsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsEncryption :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DashIsoEncryptionSettings)
digsEncryption = Lens.field @"encryption"
{-# DEPRECATED digsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsFragmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
digsFragmentLength = Lens.field @"fragmentLength"
{-# DEPRECATED digsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | Supports HbbTV specification as indicated
--
-- /Note:/ Consider using 'hbbtvCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsHbbtvCompliance :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DashIsoHbbtvCompliance)
digsHbbtvCompliance = Lens.field @"hbbtvCompliance"
{-# DEPRECATED digsHbbtvCompliance "Use generic-lens or generic-optics with 'hbbtvCompliance' instead." #-}

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- /Note:/ Consider using 'minBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMinBufferTime :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
digsMinBufferTime = Lens.field @"minBufferTime"
{-# DEPRECATED digsMinBufferTime "Use generic-lens or generic-optics with 'minBufferTime' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMinFinalSegmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Double)
digsMinFinalSegmentLength = Lens.field @"minFinalSegmentLength"
{-# DEPRECATED digsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- /Note:/ Consider using 'mpdProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMpdProfile :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DashIsoMpdProfile)
digsMpdProfile = Lens.field @"mpdProfile"
{-# DEPRECATED digsMpdProfile "Use generic-lens or generic-optics with 'mpdProfile' instead." #-}

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsSegmentControl :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DashIsoSegmentControl)
digsSegmentControl = Lens.field @"segmentControl"
{-# DEPRECATED digsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsSegmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
digsSegmentLength = Lens.field @"segmentLength"
{-# DEPRECATED digsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
--
-- /Note:/ Consider using 'writeSegmentTimelineInRepresentation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsWriteSegmentTimelineInRepresentation :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Types.DashIsoWriteSegmentTimelineInRepresentation)
digsWriteSegmentTimelineInRepresentation = Lens.field @"writeSegmentTimelineInRepresentation"
{-# DEPRECATED digsWriteSegmentTimelineInRepresentation "Use generic-lens or generic-optics with 'writeSegmentTimelineInRepresentation' instead." #-}

instance Core.FromJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("additionalManifests" Core..=) Core.<$> additionalManifests,
            ("baseUrl" Core..=) Core.<$> baseUrl,
            ("destination" Core..=) Core.<$> destination,
            ("destinationSettings" Core..=) Core.<$> destinationSettings,
            ("encryption" Core..=) Core.<$> encryption,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("hbbtvCompliance" Core..=) Core.<$> hbbtvCompliance,
            ("minBufferTime" Core..=) Core.<$> minBufferTime,
            ("minFinalSegmentLength" Core..=) Core.<$> minFinalSegmentLength,
            ("mpdProfile" Core..=) Core.<$> mpdProfile,
            ("segmentControl" Core..=) Core.<$> segmentControl,
            ("segmentLength" Core..=) Core.<$> segmentLength,
            ("writeSegmentTimelineInRepresentation" Core..=)
              Core.<$> writeSegmentTimelineInRepresentation
          ]
      )

instance Core.FromJSON DashIsoGroupSettings where
  parseJSON =
    Core.withObject "DashIsoGroupSettings" Core.$
      \x ->
        DashIsoGroupSettings'
          Core.<$> (x Core..:? "additionalManifests")
          Core.<*> (x Core..:? "baseUrl")
          Core.<*> (x Core..:? "destination")
          Core.<*> (x Core..:? "destinationSettings")
          Core.<*> (x Core..:? "encryption")
          Core.<*> (x Core..:? "fragmentLength")
          Core.<*> (x Core..:? "hbbtvCompliance")
          Core.<*> (x Core..:? "minBufferTime")
          Core.<*> (x Core..:? "minFinalSegmentLength")
          Core.<*> (x Core..:? "mpdProfile")
          Core.<*> (x Core..:? "segmentControl")
          Core.<*> (x Core..:? "segmentLength")
          Core.<*> (x Core..:? "writeSegmentTimelineInRepresentation")
