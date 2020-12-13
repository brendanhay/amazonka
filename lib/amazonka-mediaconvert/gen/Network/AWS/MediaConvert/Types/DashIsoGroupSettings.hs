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
    digsFragmentLength,
    digsSegmentControl,
    digsDestination,
    digsHbbtvCompliance,
    digsMinBufferTime,
    digsMpdProfile,
    digsAdditionalManifests,
    digsBaseURL,
    digsDestinationSettings,
    digsMinFinalSegmentLength,
    digsEncryption,
    digsSegmentLength,
    digsWriteSegmentTimelineInRepresentation,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings
import qualified Network.AWS.Prelude as Lude

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings) to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'mkDashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { -- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
    fragmentLength :: Lude.Maybe Lude.Natural,
    -- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Lude.Maybe DashIsoSegmentControl,
    -- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
    destination :: Lude.Maybe Lude.Text,
    -- | Supports HbbTV specification as indicated
    hbbtvCompliance :: Lude.Maybe DashIsoHbbtvCompliance,
    -- | Minimum time of initially buffered media that is needed to ensure smooth playout.
    minBufferTime :: Lude.Maybe Lude.Natural,
    -- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Lude.Maybe DashIsoMpdProfile,
    -- | By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
    additionalManifests :: Lude.Maybe [DashAdditionalManifest],
    -- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
    baseURL :: Lude.Maybe Lude.Text,
    -- | Settings associated with the destination. Will vary based on the type of destination
    destinationSettings :: Lude.Maybe DestinationSettings,
    -- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
    minFinalSegmentLength :: Lude.Maybe Lude.Double,
    -- | DRM settings.
    encryption :: Lude.Maybe DashIsoEncryptionSettings,
    -- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
    segmentLength :: Lude.Maybe Lude.Natural,
    -- | If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
    writeSegmentTimelineInRepresentation :: Lude.Maybe DashIsoWriteSegmentTimelineInRepresentation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashIsoGroupSettings' with the minimum fields required to make a request.
--
-- * 'fragmentLength' - Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
-- * 'segmentControl' - When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
-- * 'destination' - Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
-- * 'hbbtvCompliance' - Supports HbbTV specification as indicated
-- * 'minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth playout.
-- * 'mpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
-- * 'additionalManifests' - By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
-- * 'baseURL' - A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
-- * 'destinationSettings' - Settings associated with the destination. Will vary based on the type of destination
-- * 'minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
-- * 'encryption' - DRM settings.
-- * 'segmentLength' - Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
-- * 'writeSegmentTimelineInRepresentation' - If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
mkDashIsoGroupSettings ::
  DashIsoGroupSettings
mkDashIsoGroupSettings =
  DashIsoGroupSettings'
    { fragmentLength = Lude.Nothing,
      segmentControl = Lude.Nothing,
      destination = Lude.Nothing,
      hbbtvCompliance = Lude.Nothing,
      minBufferTime = Lude.Nothing,
      mpdProfile = Lude.Nothing,
      additionalManifests = Lude.Nothing,
      baseURL = Lude.Nothing,
      destinationSettings = Lude.Nothing,
      minFinalSegmentLength = Lude.Nothing,
      encryption = Lude.Nothing,
      segmentLength = Lude.Nothing,
      writeSegmentTimelineInRepresentation = Lude.Nothing
    }

-- | Length of fragments to generate (in seconds). Fragment length must be compatible with GOP size and Framerate. Note that fragments will end on the next keyframe after this number of seconds, so actual fragment length may be longer. When Emit Single File is checked, the fragmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'fragmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsFragmentLength :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Natural)
digsFragmentLength = Lens.lens (fragmentLength :: DashIsoGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {fragmentLength = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsFragmentLength "Use generic-lens or generic-optics with 'fragmentLength' instead." #-}

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
--
-- /Note:/ Consider using 'segmentControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsSegmentControl :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DashIsoSegmentControl)
digsSegmentControl = Lens.lens (segmentControl :: DashIsoGroupSettings -> Lude.Maybe DashIsoSegmentControl) (\s a -> s {segmentControl = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsSegmentControl "Use generic-lens or generic-optics with 'segmentControl' instead." #-}

-- | Use Destination (Destination) to specify the S3 output location and the output filename base. Destination accepts format identifiers. If you do not specify the base filename in the URI, the service will use the filename of the input file. If your job has multiple inputs, the service uses the filename of the first input file.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsDestination :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Text)
digsDestination = Lens.lens (destination :: DashIsoGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {destination = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | Supports HbbTV specification as indicated
--
-- /Note:/ Consider using 'hbbtvCompliance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsHbbtvCompliance :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DashIsoHbbtvCompliance)
digsHbbtvCompliance = Lens.lens (hbbtvCompliance :: DashIsoGroupSettings -> Lude.Maybe DashIsoHbbtvCompliance) (\s a -> s {hbbtvCompliance = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsHbbtvCompliance "Use generic-lens or generic-optics with 'hbbtvCompliance' instead." #-}

-- | Minimum time of initially buffered media that is needed to ensure smooth playout.
--
-- /Note:/ Consider using 'minBufferTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMinBufferTime :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Natural)
digsMinBufferTime = Lens.lens (minBufferTime :: DashIsoGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {minBufferTime = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsMinBufferTime "Use generic-lens or generic-optics with 'minBufferTime' instead." #-}

-- | Specify whether your DASH profile is on-demand or main. When you choose Main profile (MAIN_PROFILE), the service signals  urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When you choose On-demand (ON_DEMAND_PROFILE), the service signals urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose On-demand, you must also set the output group setting Segment control (SegmentControl) to Single file (SINGLE_FILE).
--
-- /Note:/ Consider using 'mpdProfile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMpdProfile :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DashIsoMpdProfile)
digsMpdProfile = Lens.lens (mpdProfile :: DashIsoGroupSettings -> Lude.Maybe DashIsoMpdProfile) (\s a -> s {mpdProfile = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsMpdProfile "Use generic-lens or generic-optics with 'mpdProfile' instead." #-}

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO output group in your job. This default manifest references every output in the output group. To create additional DASH manifests that reference a subset of the outputs in the output group, specify a list of them here.
--
-- /Note:/ Consider using 'additionalManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsAdditionalManifests :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe [DashAdditionalManifest])
digsAdditionalManifests = Lens.lens (additionalManifests :: DashIsoGroupSettings -> Lude.Maybe [DashAdditionalManifest]) (\s a -> s {additionalManifests = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsAdditionalManifests "Use generic-lens or generic-optics with 'additionalManifests' instead." #-}

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the top level BaseURL element. Can be used if streams are delivered from a different URL than the manifest file.
--
-- /Note:/ Consider using 'baseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsBaseURL :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Text)
digsBaseURL = Lens.lens (baseURL :: DashIsoGroupSettings -> Lude.Maybe Lude.Text) (\s a -> s {baseURL = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsBaseURL "Use generic-lens or generic-optics with 'baseURL' instead." #-}

-- | Settings associated with the destination. Will vary based on the type of destination
--
-- /Note:/ Consider using 'destinationSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsDestinationSettings :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DestinationSettings)
digsDestinationSettings = Lens.lens (destinationSettings :: DashIsoGroupSettings -> Lude.Maybe DestinationSettings) (\s a -> s {destinationSettings = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsDestinationSettings "Use generic-lens or generic-optics with 'destinationSettings' instead." #-}

-- | Keep this setting at the default value of 0, unless you are troubleshooting a problem with how devices play back the end of your video asset. If you know that player devices are hanging on the final segment of your video because the length of your final segment is too short, use this setting to specify a minimum final segment length, in seconds. Choose a value that is greater than or equal to 1 and less than your segment length. When you specify a value for this setting, the encoder will combine any final segment that is shorter than the length that you specify with the previous segment. For example, your segment length is 3 seconds and your final segment is .5 seconds without a minimum final segment length; when you set the minimum final segment length to 1, your final segment is 3.5 seconds.
--
-- /Note:/ Consider using 'minFinalSegmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsMinFinalSegmentLength :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Double)
digsMinFinalSegmentLength = Lens.lens (minFinalSegmentLength :: DashIsoGroupSettings -> Lude.Maybe Lude.Double) (\s a -> s {minFinalSegmentLength = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsMinFinalSegmentLength "Use generic-lens or generic-optics with 'minFinalSegmentLength' instead." #-}

-- | DRM settings.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsEncryption :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DashIsoEncryptionSettings)
digsEncryption = Lens.lens (encryption :: DashIsoGroupSettings -> Lude.Maybe DashIsoEncryptionSettings) (\s a -> s {encryption = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | Length of mpd segments to create (in seconds). Note that segments will end on the next keyframe after this number of seconds, so actual segment length may be longer. When Emit Single File is checked, the segmentation is internal to a single output file and it does not cause the creation of many output files as in other output types.
--
-- /Note:/ Consider using 'segmentLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsSegmentLength :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe Lude.Natural)
digsSegmentLength = Lens.lens (segmentLength :: DashIsoGroupSettings -> Lude.Maybe Lude.Natural) (\s a -> s {segmentLength = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsSegmentLength "Use generic-lens or generic-optics with 'segmentLength' instead." #-}

-- | If you get an HTTP error in the 400 range when you play back your DASH output, enable this setting and run your transcoding job again. When you enable this setting, the service writes precise segment durations in the DASH manifest. The segment duration information appears inside the SegmentTimeline element, inside SegmentTemplate at the Representation level. When you don't enable this setting, the service writes approximate segment durations in your DASH manifest.
--
-- /Note:/ Consider using 'writeSegmentTimelineInRepresentation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digsWriteSegmentTimelineInRepresentation :: Lens.Lens' DashIsoGroupSettings (Lude.Maybe DashIsoWriteSegmentTimelineInRepresentation)
digsWriteSegmentTimelineInRepresentation = Lens.lens (writeSegmentTimelineInRepresentation :: DashIsoGroupSettings -> Lude.Maybe DashIsoWriteSegmentTimelineInRepresentation) (\s a -> s {writeSegmentTimelineInRepresentation = a} :: DashIsoGroupSettings)
{-# DEPRECATED digsWriteSegmentTimelineInRepresentation "Use generic-lens or generic-optics with 'writeSegmentTimelineInRepresentation' instead." #-}

instance Lude.FromJSON DashIsoGroupSettings where
  parseJSON =
    Lude.withObject
      "DashIsoGroupSettings"
      ( \x ->
          DashIsoGroupSettings'
            Lude.<$> (x Lude..:? "fragmentLength")
            Lude.<*> (x Lude..:? "segmentControl")
            Lude.<*> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "hbbtvCompliance")
            Lude.<*> (x Lude..:? "minBufferTime")
            Lude.<*> (x Lude..:? "mpdProfile")
            Lude.<*> (x Lude..:? "additionalManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "baseUrl")
            Lude.<*> (x Lude..:? "destinationSettings")
            Lude.<*> (x Lude..:? "minFinalSegmentLength")
            Lude.<*> (x Lude..:? "encryption")
            Lude.<*> (x Lude..:? "segmentLength")
            Lude.<*> (x Lude..:? "writeSegmentTimelineInRepresentation")
      )

instance Lude.ToJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fragmentLength" Lude..=) Lude.<$> fragmentLength,
            ("segmentControl" Lude..=) Lude.<$> segmentControl,
            ("destination" Lude..=) Lude.<$> destination,
            ("hbbtvCompliance" Lude..=) Lude.<$> hbbtvCompliance,
            ("minBufferTime" Lude..=) Lude.<$> minBufferTime,
            ("mpdProfile" Lude..=) Lude.<$> mpdProfile,
            ("additionalManifests" Lude..=) Lude.<$> additionalManifests,
            ("baseUrl" Lude..=) Lude.<$> baseURL,
            ("destinationSettings" Lude..=) Lude.<$> destinationSettings,
            ("minFinalSegmentLength" Lude..=) Lude.<$> minFinalSegmentLength,
            ("encryption" Lude..=) Lude.<$> encryption,
            ("segmentLength" Lude..=) Lude.<$> segmentLength,
            ("writeSegmentTimelineInRepresentation" Lude..=)
              Lude.<$> writeSegmentTimelineInRepresentation
          ]
      )
