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
import Network.AWS.MediaConvert.Types.DashIsoGroupAudioChannelConfigSchemeIdUri
import Network.AWS.MediaConvert.Types.DashIsoHbbtvCompliance
import Network.AWS.MediaConvert.Types.DashIsoImageBasedTrickPlay
import Network.AWS.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
import Network.AWS.MediaConvert.Types.DashIsoMpdProfile
import Network.AWS.MediaConvert.Types.DashIsoPtsOffsetHandlingForBFrames
import Network.AWS.MediaConvert.Types.DashIsoSegmentControl
import Network.AWS.MediaConvert.Types.DashIsoSegmentLengthControl
import Network.AWS.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Network.AWS.MediaConvert.Types.DestinationSettings
import qualified Network.AWS.Prelude as Prelude

-- | Settings related to your DASH output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'newDashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { -- | Specify the length, in whole seconds, of each segment. When you don\'t
    -- specify a value, MediaConvert defaults to 30. Related settings: Use
    -- Segment length control (SegmentLengthControl) to specify whether the
    -- encoder enforces this value strictly. Use Segment control
    -- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
    -- segment files or one content file that has metadata to mark the segment
    -- boundaries.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | When set to SINGLE_FILE, a single output file is generated, which is
    -- internally segmented using the Fragment Length and Segment Length. When
    -- set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Prelude.Maybe DashIsoSegmentControl,
    -- | Length of fragments to generate (in seconds). Fragment length must be
    -- compatible with GOP size and Framerate. Note that fragments will end on
    -- the next keyframe after this number of seconds, so actual fragment
    -- length may be longer. When Emit Single File is checked, the
    -- fragmentation is internal to a single output file and it does not cause
    -- the creation of many output files as in other output types.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | A partial URI prefix that will be put in the manifest (.mpd) file at the
    -- top level BaseURL element. Can be used if streams are delivered from a
    -- different URL than the manifest file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Specify how you want MediaConvert to determine the segment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Segment length (SegmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    segmentLengthControl :: Prelude.Maybe DashIsoSegmentLengthControl,
    -- | By default, the service creates one .mpd DASH manifest for each DASH ISO
    -- output group in your job. This default manifest references every output
    -- in the output group. To create additional DASH manifests that reference
    -- a subset of the outputs in the output group, specify a list of them
    -- here.
    additionalManifests :: Prelude.Maybe [DashAdditionalManifest],
    -- | Specify whether MediaConvert generates images for trick play. Keep the
    -- default value, None (NONE), to not generate any images. Choose Thumbnail
    -- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
    -- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
    -- full-resolution images of single frames. MediaConvert adds an entry in
    -- the .mpd manifest for each set of images that you generate. A common
    -- application for these images is Roku trick mode. The thumbnails and
    -- full-frame images that MediaConvert creates with this feature are
    -- compatible with this Roku specification:
    -- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
    imageBasedTrickPlay :: Prelude.Maybe DashIsoImageBasedTrickPlay,
    -- | Specify whether your DASH profile is on-demand or main. When you choose
    -- Main profile (MAIN_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
    -- you choose On-demand (ON_DEMAND_PROFILE), the service signals
    -- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
    -- On-demand, you must also set the output group setting Segment control
    -- (SegmentControl) to Single file (SINGLE_FILE).
    mpdProfile :: Prelude.Maybe DashIsoMpdProfile,
    -- | Use this setting only when your output video stream has B-frames, which
    -- causes the initial presentation time stamp (PTS) to be offset from the
    -- initial decode time stamp (DTS). Specify how MediaConvert handles PTS
    -- when writing time stamps in output DASH manifests. Choose Match initial
    -- PTS (MATCH_INITIAL_PTS) when you want MediaConvert to use the initial
    -- PTS as the first time stamp in the manifest. Choose Zero-based
    -- (ZERO_BASED) to have MediaConvert ignore the initial PTS in the video
    -- stream and instead write the initial time stamp as zero in the manifest.
    -- For outputs that don\'t have B-frames, the time stamps in your DASH
    -- manifests start at zero regardless of your choice here.
    ptsOffsetHandlingForBFrames :: Prelude.Maybe DashIsoPtsOffsetHandlingForBFrames,
    -- | DRM settings.
    encryption :: Prelude.Maybe DashIsoEncryptionSettings,
    -- | Supports HbbTV specification as indicated
    hbbtvCompliance :: Prelude.Maybe DashIsoHbbtvCompliance,
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
    -- | Use this setting only when your audio codec is a Dolby one (AC3, EAC3,
    -- or Atmos) and your downstream workflow requires that your DASH manifest
    -- use the Dolby channel configuration tag, rather than the MPEG one. For
    -- example, you might need to use this to make dynamic ad insertion work.
    -- Specify which audio channel configuration scheme ID URI MediaConvert
    -- writes in your DASH manifest. Keep the default value, MPEG channel
    -- configuration (MPEG_CHANNEL_CONFIGURATION), to have MediaConvert write
    -- this: urn:mpeg:mpegB:cicp:ChannelConfiguration. Choose Dolby channel
    -- configuration (DOLBY_CHANNEL_CONFIGURATION) to have MediaConvert write
    -- this instead: tag:dolby.com,2014:dash:audio_channel_configuration:2011.
    audioChannelConfigSchemeIdUri :: Prelude.Maybe DashIsoGroupAudioChannelConfigSchemeIdUri,
    -- | If you get an HTTP error in the 400 range when you play back your DASH
    -- output, enable this setting and run your transcoding job again. When you
    -- enable this setting, the service writes precise segment durations in the
    -- DASH manifest. The segment duration information appears inside the
    -- SegmentTimeline element, inside SegmentTemplate at the Representation
    -- level. When you don\'t enable this setting, the service writes
    -- approximate segment durations in your DASH manifest.
    writeSegmentTimelineInRepresentation :: Prelude.Maybe DashIsoWriteSegmentTimelineInRepresentation,
    -- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
    -- ADVANCED
    imageBasedTrickPlaySettings :: Prelude.Maybe DashIsoImageBasedTrickPlaySettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashIsoGroupSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentLength', 'dashIsoGroupSettings_segmentLength' - Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 30. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
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
-- 'segmentLengthControl', 'dashIsoGroupSettings_segmentLengthControl' - Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'additionalManifests', 'dashIsoGroupSettings_additionalManifests' - By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
--
-- 'imageBasedTrickPlay', 'dashIsoGroupSettings_imageBasedTrickPlay' - Specify whether MediaConvert generates images for trick play. Keep the
-- default value, None (NONE), to not generate any images. Choose Thumbnail
-- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
-- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
-- full-resolution images of single frames. MediaConvert adds an entry in
-- the .mpd manifest for each set of images that you generate. A common
-- application for these images is Roku trick mode. The thumbnails and
-- full-frame images that MediaConvert creates with this feature are
-- compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
--
-- 'mpdProfile', 'dashIsoGroupSettings_mpdProfile' - Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
--
-- 'ptsOffsetHandlingForBFrames', 'dashIsoGroupSettings_ptsOffsetHandlingForBFrames' - Use this setting only when your output video stream has B-frames, which
-- causes the initial presentation time stamp (PTS) to be offset from the
-- initial decode time stamp (DTS). Specify how MediaConvert handles PTS
-- when writing time stamps in output DASH manifests. Choose Match initial
-- PTS (MATCH_INITIAL_PTS) when you want MediaConvert to use the initial
-- PTS as the first time stamp in the manifest. Choose Zero-based
-- (ZERO_BASED) to have MediaConvert ignore the initial PTS in the video
-- stream and instead write the initial time stamp as zero in the manifest.
-- For outputs that don\'t have B-frames, the time stamps in your DASH
-- manifests start at zero regardless of your choice here.
--
-- 'encryption', 'dashIsoGroupSettings_encryption' - DRM settings.
--
-- 'hbbtvCompliance', 'dashIsoGroupSettings_hbbtvCompliance' - Supports HbbTV specification as indicated
--
-- 'minBufferTime', 'dashIsoGroupSettings_minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
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
-- 'audioChannelConfigSchemeIdUri', 'dashIsoGroupSettings_audioChannelConfigSchemeIdUri' - Use this setting only when your audio codec is a Dolby one (AC3, EAC3,
-- or Atmos) and your downstream workflow requires that your DASH manifest
-- use the Dolby channel configuration tag, rather than the MPEG one. For
-- example, you might need to use this to make dynamic ad insertion work.
-- Specify which audio channel configuration scheme ID URI MediaConvert
-- writes in your DASH manifest. Keep the default value, MPEG channel
-- configuration (MPEG_CHANNEL_CONFIGURATION), to have MediaConvert write
-- this: urn:mpeg:mpegB:cicp:ChannelConfiguration. Choose Dolby channel
-- configuration (DOLBY_CHANNEL_CONFIGURATION) to have MediaConvert write
-- this instead: tag:dolby.com,2014:dash:audio_channel_configuration:2011.
--
-- 'writeSegmentTimelineInRepresentation', 'dashIsoGroupSettings_writeSegmentTimelineInRepresentation' - If you get an HTTP error in the 400 range when you play back your DASH
-- output, enable this setting and run your transcoding job again. When you
-- enable this setting, the service writes precise segment durations in the
-- DASH manifest. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When you don\'t enable this setting, the service writes
-- approximate segment durations in your DASH manifest.
--
-- 'imageBasedTrickPlaySettings', 'dashIsoGroupSettings_imageBasedTrickPlaySettings' - Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
newDashIsoGroupSettings ::
  DashIsoGroupSettings
newDashIsoGroupSettings =
  DashIsoGroupSettings'
    { segmentLength =
        Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      segmentLengthControl = Prelude.Nothing,
      additionalManifests = Prelude.Nothing,
      imageBasedTrickPlay = Prelude.Nothing,
      mpdProfile = Prelude.Nothing,
      ptsOffsetHandlingForBFrames = Prelude.Nothing,
      encryption = Prelude.Nothing,
      hbbtvCompliance = Prelude.Nothing,
      minBufferTime = Prelude.Nothing,
      destination = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      audioChannelConfigSchemeIdUri = Prelude.Nothing,
      writeSegmentTimelineInRepresentation =
        Prelude.Nothing,
      imageBasedTrickPlaySettings = Prelude.Nothing
    }

-- | Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 30. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
dashIsoGroupSettings_segmentLength :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_segmentLength = Lens.lens (\DashIsoGroupSettings' {segmentLength} -> segmentLength) (\s@DashIsoGroupSettings' {} a -> s {segmentLength = a} :: DashIsoGroupSettings)

-- | When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
dashIsoGroupSettings_segmentControl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoSegmentControl)
dashIsoGroupSettings_segmentControl = Lens.lens (\DashIsoGroupSettings' {segmentControl} -> segmentControl) (\s@DashIsoGroupSettings' {} a -> s {segmentControl = a} :: DashIsoGroupSettings)

-- | Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
dashIsoGroupSettings_fragmentLength :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_fragmentLength = Lens.lens (\DashIsoGroupSettings' {fragmentLength} -> fragmentLength) (\s@DashIsoGroupSettings' {} a -> s {fragmentLength = a} :: DashIsoGroupSettings)

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the
-- top level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
dashIsoGroupSettings_baseUrl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Text)
dashIsoGroupSettings_baseUrl = Lens.lens (\DashIsoGroupSettings' {baseUrl} -> baseUrl) (\s@DashIsoGroupSettings' {} a -> s {baseUrl = a} :: DashIsoGroupSettings)

-- | Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
dashIsoGroupSettings_segmentLengthControl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoSegmentLengthControl)
dashIsoGroupSettings_segmentLengthControl = Lens.lens (\DashIsoGroupSettings' {segmentLengthControl} -> segmentLengthControl) (\s@DashIsoGroupSettings' {} a -> s {segmentLengthControl = a} :: DashIsoGroupSettings)

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
dashIsoGroupSettings_additionalManifests :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe [DashAdditionalManifest])
dashIsoGroupSettings_additionalManifests = Lens.lens (\DashIsoGroupSettings' {additionalManifests} -> additionalManifests) (\s@DashIsoGroupSettings' {} a -> s {additionalManifests = a} :: DashIsoGroupSettings) Prelude.. Lens.mapping Lens._Coerce

-- | Specify whether MediaConvert generates images for trick play. Keep the
-- default value, None (NONE), to not generate any images. Choose Thumbnail
-- (THUMBNAIL) to generate tiled thumbnails. Choose Thumbnail and full
-- frame (THUMBNAIL_AND_FULLFRAME) to generate tiled thumbnails and
-- full-resolution images of single frames. MediaConvert adds an entry in
-- the .mpd manifest for each set of images that you generate. A common
-- application for these images is Roku trick mode. The thumbnails and
-- full-frame images that MediaConvert creates with this feature are
-- compatible with this Roku specification:
-- https:\/\/developer.roku.com\/docs\/developer-program\/media-playback\/trick-mode\/hls-and-dash.md
dashIsoGroupSettings_imageBasedTrickPlay :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoImageBasedTrickPlay)
dashIsoGroupSettings_imageBasedTrickPlay = Lens.lens (\DashIsoGroupSettings' {imageBasedTrickPlay} -> imageBasedTrickPlay) (\s@DashIsoGroupSettings' {} a -> s {imageBasedTrickPlay = a} :: DashIsoGroupSettings)

-- | Specify whether your DASH profile is on-demand or main. When you choose
-- Main profile (MAIN_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-main:2011 in your .mpd DASH manifest. When
-- you choose On-demand (ON_DEMAND_PROFILE), the service signals
-- urn:mpeg:dash:profile:isoff-on-demand:2011 in your .mpd. When you choose
-- On-demand, you must also set the output group setting Segment control
-- (SegmentControl) to Single file (SINGLE_FILE).
dashIsoGroupSettings_mpdProfile :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoMpdProfile)
dashIsoGroupSettings_mpdProfile = Lens.lens (\DashIsoGroupSettings' {mpdProfile} -> mpdProfile) (\s@DashIsoGroupSettings' {} a -> s {mpdProfile = a} :: DashIsoGroupSettings)

-- | Use this setting only when your output video stream has B-frames, which
-- causes the initial presentation time stamp (PTS) to be offset from the
-- initial decode time stamp (DTS). Specify how MediaConvert handles PTS
-- when writing time stamps in output DASH manifests. Choose Match initial
-- PTS (MATCH_INITIAL_PTS) when you want MediaConvert to use the initial
-- PTS as the first time stamp in the manifest. Choose Zero-based
-- (ZERO_BASED) to have MediaConvert ignore the initial PTS in the video
-- stream and instead write the initial time stamp as zero in the manifest.
-- For outputs that don\'t have B-frames, the time stamps in your DASH
-- manifests start at zero regardless of your choice here.
dashIsoGroupSettings_ptsOffsetHandlingForBFrames :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoPtsOffsetHandlingForBFrames)
dashIsoGroupSettings_ptsOffsetHandlingForBFrames = Lens.lens (\DashIsoGroupSettings' {ptsOffsetHandlingForBFrames} -> ptsOffsetHandlingForBFrames) (\s@DashIsoGroupSettings' {} a -> s {ptsOffsetHandlingForBFrames = a} :: DashIsoGroupSettings)

-- | DRM settings.
dashIsoGroupSettings_encryption :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoEncryptionSettings)
dashIsoGroupSettings_encryption = Lens.lens (\DashIsoGroupSettings' {encryption} -> encryption) (\s@DashIsoGroupSettings' {} a -> s {encryption = a} :: DashIsoGroupSettings)

-- | Supports HbbTV specification as indicated
dashIsoGroupSettings_hbbtvCompliance :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoHbbtvCompliance)
dashIsoGroupSettings_hbbtvCompliance = Lens.lens (\DashIsoGroupSettings' {hbbtvCompliance} -> hbbtvCompliance) (\s@DashIsoGroupSettings' {} a -> s {hbbtvCompliance = a} :: DashIsoGroupSettings)

-- | Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
dashIsoGroupSettings_minBufferTime :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_minBufferTime = Lens.lens (\DashIsoGroupSettings' {minBufferTime} -> minBufferTime) (\s@DashIsoGroupSettings' {} a -> s {minBufferTime = a} :: DashIsoGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
dashIsoGroupSettings_destination :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Text)
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
dashIsoGroupSettings_minFinalSegmentLength :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Double)
dashIsoGroupSettings_minFinalSegmentLength = Lens.lens (\DashIsoGroupSettings' {minFinalSegmentLength} -> minFinalSegmentLength) (\s@DashIsoGroupSettings' {} a -> s {minFinalSegmentLength = a} :: DashIsoGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
dashIsoGroupSettings_destinationSettings :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DestinationSettings)
dashIsoGroupSettings_destinationSettings = Lens.lens (\DashIsoGroupSettings' {destinationSettings} -> destinationSettings) (\s@DashIsoGroupSettings' {} a -> s {destinationSettings = a} :: DashIsoGroupSettings)

-- | Use this setting only when your audio codec is a Dolby one (AC3, EAC3,
-- or Atmos) and your downstream workflow requires that your DASH manifest
-- use the Dolby channel configuration tag, rather than the MPEG one. For
-- example, you might need to use this to make dynamic ad insertion work.
-- Specify which audio channel configuration scheme ID URI MediaConvert
-- writes in your DASH manifest. Keep the default value, MPEG channel
-- configuration (MPEG_CHANNEL_CONFIGURATION), to have MediaConvert write
-- this: urn:mpeg:mpegB:cicp:ChannelConfiguration. Choose Dolby channel
-- configuration (DOLBY_CHANNEL_CONFIGURATION) to have MediaConvert write
-- this instead: tag:dolby.com,2014:dash:audio_channel_configuration:2011.
dashIsoGroupSettings_audioChannelConfigSchemeIdUri :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoGroupAudioChannelConfigSchemeIdUri)
dashIsoGroupSettings_audioChannelConfigSchemeIdUri = Lens.lens (\DashIsoGroupSettings' {audioChannelConfigSchemeIdUri} -> audioChannelConfigSchemeIdUri) (\s@DashIsoGroupSettings' {} a -> s {audioChannelConfigSchemeIdUri = a} :: DashIsoGroupSettings)

-- | If you get an HTTP error in the 400 range when you play back your DASH
-- output, enable this setting and run your transcoding job again. When you
-- enable this setting, the service writes precise segment durations in the
-- DASH manifest. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When you don\'t enable this setting, the service writes
-- approximate segment durations in your DASH manifest.
dashIsoGroupSettings_writeSegmentTimelineInRepresentation :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoWriteSegmentTimelineInRepresentation)
dashIsoGroupSettings_writeSegmentTimelineInRepresentation = Lens.lens (\DashIsoGroupSettings' {writeSegmentTimelineInRepresentation} -> writeSegmentTimelineInRepresentation) (\s@DashIsoGroupSettings' {} a -> s {writeSegmentTimelineInRepresentation = a} :: DashIsoGroupSettings)

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
dashIsoGroupSettings_imageBasedTrickPlaySettings :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoImageBasedTrickPlaySettings)
dashIsoGroupSettings_imageBasedTrickPlaySettings = Lens.lens (\DashIsoGroupSettings' {imageBasedTrickPlaySettings} -> imageBasedTrickPlaySettings) (\s@DashIsoGroupSettings' {} a -> s {imageBasedTrickPlaySettings = a} :: DashIsoGroupSettings)

instance Core.FromJSON DashIsoGroupSettings where
  parseJSON =
    Core.withObject
      "DashIsoGroupSettings"
      ( \x ->
          DashIsoGroupSettings'
            Prelude.<$> (x Core..:? "segmentLength")
            Prelude.<*> (x Core..:? "segmentControl")
            Prelude.<*> (x Core..:? "fragmentLength")
            Prelude.<*> (x Core..:? "baseUrl")
            Prelude.<*> (x Core..:? "segmentLengthControl")
            Prelude.<*> ( x Core..:? "additionalManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "imageBasedTrickPlay")
            Prelude.<*> (x Core..:? "mpdProfile")
            Prelude.<*> (x Core..:? "ptsOffsetHandlingForBFrames")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "hbbtvCompliance")
            Prelude.<*> (x Core..:? "minBufferTime")
            Prelude.<*> (x Core..:? "destination")
            Prelude.<*> (x Core..:? "minFinalSegmentLength")
            Prelude.<*> (x Core..:? "destinationSettings")
            Prelude.<*> (x Core..:? "audioChannelConfigSchemeIdUri")
            Prelude.<*> (x Core..:? "writeSegmentTimelineInRepresentation")
            Prelude.<*> (x Core..:? "imageBasedTrickPlaySettings")
      )

instance Prelude.Hashable DashIsoGroupSettings

instance Prelude.NFData DashIsoGroupSettings

instance Core.ToJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentLength" Core..=) Prelude.<$> segmentLength,
            ("segmentControl" Core..=)
              Prelude.<$> segmentControl,
            ("fragmentLength" Core..=)
              Prelude.<$> fragmentLength,
            ("baseUrl" Core..=) Prelude.<$> baseUrl,
            ("segmentLengthControl" Core..=)
              Prelude.<$> segmentLengthControl,
            ("additionalManifests" Core..=)
              Prelude.<$> additionalManifests,
            ("imageBasedTrickPlay" Core..=)
              Prelude.<$> imageBasedTrickPlay,
            ("mpdProfile" Core..=) Prelude.<$> mpdProfile,
            ("ptsOffsetHandlingForBFrames" Core..=)
              Prelude.<$> ptsOffsetHandlingForBFrames,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("hbbtvCompliance" Core..=)
              Prelude.<$> hbbtvCompliance,
            ("minBufferTime" Core..=) Prelude.<$> minBufferTime,
            ("destination" Core..=) Prelude.<$> destination,
            ("minFinalSegmentLength" Core..=)
              Prelude.<$> minFinalSegmentLength,
            ("destinationSettings" Core..=)
              Prelude.<$> destinationSettings,
            ("audioChannelConfigSchemeIdUri" Core..=)
              Prelude.<$> audioChannelConfigSchemeIdUri,
            ("writeSegmentTimelineInRepresentation" Core..=)
              Prelude.<$> writeSegmentTimelineInRepresentation,
            ("imageBasedTrickPlaySettings" Core..=)
              Prelude.<$> imageBasedTrickPlaySettings
          ]
      )
