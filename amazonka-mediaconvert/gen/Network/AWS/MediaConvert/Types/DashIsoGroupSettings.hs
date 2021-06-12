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
-- Module      : Network.AWS.MediaConvert.Types.DashIsoGroupSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoGroupSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DashAdditionalManifest
import Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings

-- | Required when you set (Type) under (OutputGroups)>(OutputGroupSettings)
-- to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'newDashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { -- | Length of mpd segments to create (in seconds). Note that segments will
    -- end on the next keyframe after this number of seconds, so actual segment
    -- length may be longer. When Emit Single File is checked, the segmentation
    -- is internal to a single output file and it does not cause the creation
    -- of many output files as in other output types.
    segmentLength :: Core.Maybe Core.Natural,
    -- | When set to SINGLE_FILE, a single output file is generated, which is
    -- internally segmented using the Fragment Length and Segment Length. When
    -- set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Core.Maybe DashIsoSegmentControl,
    -- | Length of fragments to generate (in seconds). Fragment length must be
    -- compatible with GOP size and Framerate. Note that fragments will end on
    -- the next keyframe after this number of seconds, so actual fragment
    -- length may be longer. When Emit Single File is checked, the
    -- fragmentation is internal to a single output file and it does not cause
    -- the creation of many output files as in other output types.
    fragmentLength :: Core.Maybe Core.Natural,
    -- | A partial URI prefix that will be put in the manifest (.mpd) file at the
    -- top level BaseURL element. Can be used if streams are delivered from a
    -- different URL than the manifest file.
    baseUrl :: Core.Maybe Core.Text,
    -- | By default, the service creates one .mpd DASH manifest for each DASH ISO
    -- output group in your job. This default manifest references every output
    -- in the output group. To create additional DASH manifests that reference
    -- a subset of the outputs in the output group, specify a list of them
    -- here.
    additionalManifests :: Core.Maybe [DashAdditionalManifest],
    -- | Specify whether your DASH profile is on-demand or main. When you choose
    -- Main profile (MAIN_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
    -- you choose On-demand (ON_DEMAND_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
    -- On-demand, you must also set the output group setting Segment control
    -- (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Core.Maybe DashIsoMpdProfile,
    -- | DRM settings.
    encryption :: Core.Maybe DashIsoEncryptionSettings,
    -- | Minimum time of initially buffered media that is needed to ensure smooth
    -- playout.
    minBufferTime :: Core.Maybe Core.Natural,
    -- | Supports HbbTV specification as indicated
    hbbtvCompliance :: Core.Maybe DashIsoHbbtvCompliance,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Core.Maybe Core.Text,
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
    minFinalSegmentLength :: Core.Maybe Core.Double,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Core.Maybe DestinationSettings,
    -- | If you get an HTTP error in the 400 range when you play back your DASH
    -- output, enable this setting and run your transcoding job again. When you
    -- enable this setting, the service writes precise segment durations in the
    -- DASH manifest. The segment duration information appears inside the
    -- SegmentTimeline element, inside SegmentTemplate at the Representation
    -- level. When you don\'t enable this setting, the service writes
    -- approximate segment durations in your DASH manifest.
    writeSegmentTimelineInRepresentation :: Core.Maybe DashIsoWriteSegmentTimelineInRepresentation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DashIsoGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentLength', 'dashIsoGroupSettings_segmentLength' - Length of mpd segments to create (in seconds). Note that segments will
-- end on the next keyframe after this number of seconds, so actual segment
-- length may be longer. When Emit Single File is checked, the segmentation
-- is internal to a single output file and it does not cause the creation
-- of many output files as in other output types.
--
-- 'segmentControl', 'dashIsoGroupSettings_segmentControl' - When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
--
-- 'fragmentLength', 'dashIsoGroupSettings_fragmentLength' - Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
--
-- 'baseUrl', 'dashIsoGroupSettings_baseUrl' - A partial URI prefix that will be put in the manifest (.mpd) file at the
-- top level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
--
-- 'additionalManifests', 'dashIsoGroupSettings_additionalManifests' - By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
--
-- 'mpdProfile', 'dashIsoGroupSettings_mpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
--
-- 'encryption', 'dashIsoGroupSettings_encryption' - DRM settings.
--
-- 'minBufferTime', 'dashIsoGroupSettings_minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
--
-- 'hbbtvCompliance', 'dashIsoGroupSettings_hbbtvCompliance' - Supports HbbTV specification as indicated
--
-- 'destination', 'dashIsoGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'minFinalSegmentLength', 'dashIsoGroupSettings_minFinalSegmentLength' - Keep this setting at the default value of 0, unless you are
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
-- 'destinationSettings', 'dashIsoGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'writeSegmentTimelineInRepresentation', 'dashIsoGroupSettings_writeSegmentTimelineInRepresentation' - If you get an HTTP error in the 400 range when you play back your DASH
-- output, enable this setting and run your transcoding job again. When you
-- enable this setting, the service writes precise segment durations in the
-- DASH manifest. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When you don\'t enable this setting, the service writes
-- approximate segment durations in your DASH manifest.
newDashIsoGroupSettings ::
  DashIsoGroupSettings
newDashIsoGroupSettings =
  DashIsoGroupSettings'
    { segmentLength = Core.Nothing,
      segmentControl = Core.Nothing,
      fragmentLength = Core.Nothing,
      baseUrl = Core.Nothing,
      additionalManifests = Core.Nothing,
      mpdProfile = Core.Nothing,
      encryption = Core.Nothing,
      minBufferTime = Core.Nothing,
      hbbtvCompliance = Core.Nothing,
      destination = Core.Nothing,
      minFinalSegmentLength = Core.Nothing,
      destinationSettings = Core.Nothing,
      writeSegmentTimelineInRepresentation = Core.Nothing
    }

-- | Length of mpd segments to create (in seconds). Note that segments will
-- end on the next keyframe after this number of seconds, so actual segment
-- length may be longer. When Emit Single File is checked, the segmentation
-- is internal to a single output file and it does not cause the creation
-- of many output files as in other output types.
dashIsoGroupSettings_segmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
dashIsoGroupSettings_segmentLength = Lens.lens (\DashIsoGroupSettings' {segmentLength} -> segmentLength) (\s@DashIsoGroupSettings' {} a -> s {segmentLength = a} :: DashIsoGroupSettings)

-- | When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
dashIsoGroupSettings_segmentControl :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DashIsoSegmentControl)
dashIsoGroupSettings_segmentControl = Lens.lens (\DashIsoGroupSettings' {segmentControl} -> segmentControl) (\s@DashIsoGroupSettings' {} a -> s {segmentControl = a} :: DashIsoGroupSettings)

-- | Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
dashIsoGroupSettings_fragmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
dashIsoGroupSettings_fragmentLength = Lens.lens (\DashIsoGroupSettings' {fragmentLength} -> fragmentLength) (\s@DashIsoGroupSettings' {} a -> s {fragmentLength = a} :: DashIsoGroupSettings)

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the
-- top level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
dashIsoGroupSettings_baseUrl :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Text)
dashIsoGroupSettings_baseUrl = Lens.lens (\DashIsoGroupSettings' {baseUrl} -> baseUrl) (\s@DashIsoGroupSettings' {} a -> s {baseUrl = a} :: DashIsoGroupSettings)

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
dashIsoGroupSettings_additionalManifests :: Lens.Lens' DashIsoGroupSettings (Core.Maybe [DashAdditionalManifest])
dashIsoGroupSettings_additionalManifests = Lens.lens (\DashIsoGroupSettings' {additionalManifests} -> additionalManifests) (\s@DashIsoGroupSettings' {} a -> s {additionalManifests = a} :: DashIsoGroupSettings) Core.. Lens.mapping Lens._Coerce

-- | Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
dashIsoGroupSettings_mpdProfile :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DashIsoMpdProfile)
dashIsoGroupSettings_mpdProfile = Lens.lens (\DashIsoGroupSettings' {mpdProfile} -> mpdProfile) (\s@DashIsoGroupSettings' {} a -> s {mpdProfile = a} :: DashIsoGroupSettings)

-- | DRM settings.
dashIsoGroupSettings_encryption :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DashIsoEncryptionSettings)
dashIsoGroupSettings_encryption = Lens.lens (\DashIsoGroupSettings' {encryption} -> encryption) (\s@DashIsoGroupSettings' {} a -> s {encryption = a} :: DashIsoGroupSettings)

-- | Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
dashIsoGroupSettings_minBufferTime :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Natural)
dashIsoGroupSettings_minBufferTime = Lens.lens (\DashIsoGroupSettings' {minBufferTime} -> minBufferTime) (\s@DashIsoGroupSettings' {} a -> s {minBufferTime = a} :: DashIsoGroupSettings)

-- | Supports HbbTV specification as indicated
dashIsoGroupSettings_hbbtvCompliance :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DashIsoHbbtvCompliance)
dashIsoGroupSettings_hbbtvCompliance = Lens.lens (\DashIsoGroupSettings' {hbbtvCompliance} -> hbbtvCompliance) (\s@DashIsoGroupSettings' {} a -> s {hbbtvCompliance = a} :: DashIsoGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
dashIsoGroupSettings_destination :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Text)
dashIsoGroupSettings_destination = Lens.lens (\DashIsoGroupSettings' {destination} -> destination) (\s@DashIsoGroupSettings' {} a -> s {destination = a} :: DashIsoGroupSettings)

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
dashIsoGroupSettings_minFinalSegmentLength :: Lens.Lens' DashIsoGroupSettings (Core.Maybe Core.Double)
dashIsoGroupSettings_minFinalSegmentLength = Lens.lens (\DashIsoGroupSettings' {minFinalSegmentLength} -> minFinalSegmentLength) (\s@DashIsoGroupSettings' {} a -> s {minFinalSegmentLength = a} :: DashIsoGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
dashIsoGroupSettings_destinationSettings :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DestinationSettings)
dashIsoGroupSettings_destinationSettings = Lens.lens (\DashIsoGroupSettings' {destinationSettings} -> destinationSettings) (\s@DashIsoGroupSettings' {} a -> s {destinationSettings = a} :: DashIsoGroupSettings)

-- | If you get an HTTP error in the 400 range when you play back your DASH
-- output, enable this setting and run your transcoding job again. When you
-- enable this setting, the service writes precise segment durations in the
-- DASH manifest. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When you don\'t enable this setting, the service writes
-- approximate segment durations in your DASH manifest.
dashIsoGroupSettings_writeSegmentTimelineInRepresentation :: Lens.Lens' DashIsoGroupSettings (Core.Maybe DashIsoWriteSegmentTimelineInRepresentation)
dashIsoGroupSettings_writeSegmentTimelineInRepresentation = Lens.lens (\DashIsoGroupSettings' {writeSegmentTimelineInRepresentation} -> writeSegmentTimelineInRepresentation) (\s@DashIsoGroupSettings' {} a -> s {writeSegmentTimelineInRepresentation = a} :: DashIsoGroupSettings)

instance Core.FromJSON DashIsoGroupSettings where
  parseJSON =
    Core.withObject
      "DashIsoGroupSettings"
      ( \x ->
          DashIsoGroupSettings'
            Core.<$> (x Core..:? "segmentLength")
            Core.<*> (x Core..:? "segmentControl")
            Core.<*> (x Core..:? "fragmentLength")
            Core.<*> (x Core..:? "baseUrl")
            Core.<*> ( x Core..:? "additionalManifests"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "mpdProfile")
            Core.<*> (x Core..:? "encryption")
            Core.<*> (x Core..:? "minBufferTime")
            Core.<*> (x Core..:? "hbbtvCompliance")
            Core.<*> (x Core..:? "destination")
            Core.<*> (x Core..:? "minFinalSegmentLength")
            Core.<*> (x Core..:? "destinationSettings")
            Core.<*> (x Core..:? "writeSegmentTimelineInRepresentation")
      )

instance Core.Hashable DashIsoGroupSettings

instance Core.NFData DashIsoGroupSettings

instance Core.ToJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("segmentLength" Core..=) Core.<$> segmentLength,
            ("segmentControl" Core..=) Core.<$> segmentControl,
            ("fragmentLength" Core..=) Core.<$> fragmentLength,
            ("baseUrl" Core..=) Core.<$> baseUrl,
            ("additionalManifests" Core..=)
              Core.<$> additionalManifests,
            ("mpdProfile" Core..=) Core.<$> mpdProfile,
            ("encryption" Core..=) Core.<$> encryption,
            ("minBufferTime" Core..=) Core.<$> minBufferTime,
            ("hbbtvCompliance" Core..=) Core.<$> hbbtvCompliance,
            ("destination" Core..=) Core.<$> destination,
            ("minFinalSegmentLength" Core..=)
              Core.<$> minFinalSegmentLength,
            ("destinationSettings" Core..=)
              Core.<$> destinationSettings,
            ("writeSegmentTimelineInRepresentation" Core..=)
              Core.<$> writeSegmentTimelineInRepresentation
          ]
      )
