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
-- Module      : Amazonka.MediaConvert.Types.DashIsoGroupSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DashIsoGroupSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.DashAdditionalManifest
import Amazonka.MediaConvert.Types.DashIsoEncryptionSettings
import Amazonka.MediaConvert.Types.DashIsoGroupAudioChannelConfigSchemeIdUri
import Amazonka.MediaConvert.Types.DashIsoHbbtvCompliance
import Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlay
import Amazonka.MediaConvert.Types.DashIsoImageBasedTrickPlaySettings
import Amazonka.MediaConvert.Types.DashIsoMpdManifestBandwidthType
import Amazonka.MediaConvert.Types.DashIsoMpdProfile
import Amazonka.MediaConvert.Types.DashIsoPtsOffsetHandlingForBFrames
import Amazonka.MediaConvert.Types.DashIsoSegmentControl
import Amazonka.MediaConvert.Types.DashIsoSegmentLengthControl
import Amazonka.MediaConvert.Types.DashIsoVideoCompositionOffsets
import Amazonka.MediaConvert.Types.DashIsoWriteSegmentTimelineInRepresentation
import Amazonka.MediaConvert.Types.DashManifestStyle
import Amazonka.MediaConvert.Types.DestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to your DASH output package. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/outputs-file-ABR.html.
-- When you work directly in your JSON job specification, include this
-- object and any required children when you set Type, under
-- OutputGroupSettings, to DASH_ISO_GROUP_SETTINGS.
--
-- /See:/ 'newDashIsoGroupSettings' smart constructor.
data DashIsoGroupSettings = DashIsoGroupSettings'
  { -- | By default, the service creates one .mpd DASH manifest for each DASH ISO
    -- output group in your job. This default manifest references every output
    -- in the output group. To create additional DASH manifests that reference
    -- a subset of the outputs in the output group, specify a list of them
    -- here.
    additionalManifests :: Prelude.Maybe [DashAdditionalManifest],
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
    -- | A partial URI prefix that will be put in the manifest (.mpd) file at the
    -- top level BaseURL element. Can be used if streams are delivered from a
    -- different URL than the manifest file.
    baseUrl :: Prelude.Maybe Prelude.Text,
    -- | Specify how MediaConvert writes SegmentTimeline in your output DASH
    -- manifest. To write a SegmentTimeline in each video Representation: Keep
    -- the default value, Basic. To write a common SegmentTimeline in the video
    -- AdaptationSet: Choose Compact. Note that MediaConvert will still write a
    -- SegmentTimeline in any Representation that does not share a common
    -- timeline. To write a video AdaptationSet for each different output
    -- framerate, and a common SegmentTimeline in each AdaptationSet: Choose
    -- Distinct.
    dashManifestStyle :: Prelude.Maybe DashManifestStyle,
    -- | Use Destination (Destination) to specify the S3 output location and the
    -- output filename base. Destination accepts format identifiers. If you do
    -- not specify the base filename in the URI, the service will use the
    -- filename of the input file. If your job has multiple inputs, the service
    -- uses the filename of the first input file.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Settings associated with the destination. Will vary based on the type of
    -- destination
    destinationSettings :: Prelude.Maybe DestinationSettings,
    -- | DRM settings.
    encryption :: Prelude.Maybe DashIsoEncryptionSettings,
    -- | Length of fragments to generate (in seconds). Fragment length must be
    -- compatible with GOP size and Framerate. Note that fragments will end on
    -- the next keyframe after this number of seconds, so actual fragment
    -- length may be longer. When Emit Single File is checked, the
    -- fragmentation is internal to a single output file and it does not cause
    -- the creation of many output files as in other output types.
    fragmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Supports HbbTV specification as indicated
    hbbtvCompliance :: Prelude.Maybe DashIsoHbbtvCompliance,
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
    -- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
    -- ADVANCED
    imageBasedTrickPlaySettings :: Prelude.Maybe DashIsoImageBasedTrickPlaySettings,
    -- | Minimum time of initially buffered media that is needed to ensure smooth
    -- playout.
    minBufferTime :: Prelude.Maybe Prelude.Natural,
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
    -- | Specify how the value for bandwidth is determined for each video
    -- Representation in your output MPD manifest. We recommend that you choose
    -- a MPD manifest bandwidth type that is compatible with your downstream
    -- player configuration. Max: Use the same value that you specify for Max
    -- bitrate in the video output, in bits per second. Average: Use the
    -- calculated average bitrate of the encoded video output, in bits per
    -- second.
    mpdManifestBandwidthType :: Prelude.Maybe DashIsoMpdManifestBandwidthType,
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
    -- | When set to SINGLE_FILE, a single output file is generated, which is
    -- internally segmented using the Fragment Length and Segment Length. When
    -- set to SEGMENTED_FILES, separate segment files will be created.
    segmentControl :: Prelude.Maybe DashIsoSegmentControl,
    -- | Specify the length, in whole seconds, of each segment. When you don\'t
    -- specify a value, MediaConvert defaults to 30. Related settings: Use
    -- Segment length control (SegmentLengthControl) to specify whether the
    -- encoder enforces this value strictly. Use Segment control
    -- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
    -- segment files or one content file that has metadata to mark the segment
    -- boundaries.
    segmentLength :: Prelude.Maybe Prelude.Natural,
    -- | Specify how you want MediaConvert to determine the segment length.
    -- Choose Exact (EXACT) to have the encoder use the exact length that you
    -- specify with the setting Segment length (SegmentLength). This might
    -- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
    -- the encoder round up the segment lengths to match the next GOP boundary.
    segmentLengthControl :: Prelude.Maybe DashIsoSegmentLengthControl,
    -- | Specify the video sample composition time offset mode in the output fMP4
    -- TRUN box. For wider player compatibility, set Video composition offsets
    -- to Unsigned or leave blank. The earliest presentation time may be
    -- greater than zero, and sample composition time offsets will increment
    -- using unsigned integers. For strict fMP4 video and audio timing, set
    -- Video composition offsets to Signed. The earliest presentation time will
    -- be equal to zero, and sample composition time offsets will increment
    -- using signed integers.
    videoCompositionOffsets :: Prelude.Maybe DashIsoVideoCompositionOffsets,
    -- | If you get an HTTP error in the 400 range when you play back your DASH
    -- output, enable this setting and run your transcoding job again. When you
    -- enable this setting, the service writes precise segment durations in the
    -- DASH manifest. The segment duration information appears inside the
    -- SegmentTimeline element, inside SegmentTemplate at the Representation
    -- level. When you don\'t enable this setting, the service writes
    -- approximate segment durations in your DASH manifest.
    writeSegmentTimelineInRepresentation :: Prelude.Maybe DashIsoWriteSegmentTimelineInRepresentation
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
-- 'additionalManifests', 'dashIsoGroupSettings_additionalManifests' - By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
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
-- 'baseUrl', 'dashIsoGroupSettings_baseUrl' - A partial URI prefix that will be put in the manifest (.mpd) file at the
-- top level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
--
-- 'dashManifestStyle', 'dashIsoGroupSettings_dashManifestStyle' - Specify how MediaConvert writes SegmentTimeline in your output DASH
-- manifest. To write a SegmentTimeline in each video Representation: Keep
-- the default value, Basic. To write a common SegmentTimeline in the video
-- AdaptationSet: Choose Compact. Note that MediaConvert will still write a
-- SegmentTimeline in any Representation that does not share a common
-- timeline. To write a video AdaptationSet for each different output
-- framerate, and a common SegmentTimeline in each AdaptationSet: Choose
-- Distinct.
--
-- 'destination', 'dashIsoGroupSettings_destination' - Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
--
-- 'destinationSettings', 'dashIsoGroupSettings_destinationSettings' - Settings associated with the destination. Will vary based on the type of
-- destination
--
-- 'encryption', 'dashIsoGroupSettings_encryption' - DRM settings.
--
-- 'fragmentLength', 'dashIsoGroupSettings_fragmentLength' - Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
--
-- 'hbbtvCompliance', 'dashIsoGroupSettings_hbbtvCompliance' - Supports HbbTV specification as indicated
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
-- 'imageBasedTrickPlaySettings', 'dashIsoGroupSettings_imageBasedTrickPlaySettings' - Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
--
-- 'minBufferTime', 'dashIsoGroupSettings_minBufferTime' - Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
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
-- 'mpdManifestBandwidthType', 'dashIsoGroupSettings_mpdManifestBandwidthType' - Specify how the value for bandwidth is determined for each video
-- Representation in your output MPD manifest. We recommend that you choose
-- a MPD manifest bandwidth type that is compatible with your downstream
-- player configuration. Max: Use the same value that you specify for Max
-- bitrate in the video output, in bits per second. Average: Use the
-- calculated average bitrate of the encoded video output, in bits per
-- second.
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
-- 'segmentControl', 'dashIsoGroupSettings_segmentControl' - When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
--
-- 'segmentLength', 'dashIsoGroupSettings_segmentLength' - Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 30. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
--
-- 'segmentLengthControl', 'dashIsoGroupSettings_segmentLengthControl' - Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
--
-- 'videoCompositionOffsets', 'dashIsoGroupSettings_videoCompositionOffsets' - Specify the video sample composition time offset mode in the output fMP4
-- TRUN box. For wider player compatibility, set Video composition offsets
-- to Unsigned or leave blank. The earliest presentation time may be
-- greater than zero, and sample composition time offsets will increment
-- using unsigned integers. For strict fMP4 video and audio timing, set
-- Video composition offsets to Signed. The earliest presentation time will
-- be equal to zero, and sample composition time offsets will increment
-- using signed integers.
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
    { additionalManifests =
        Prelude.Nothing,
      audioChannelConfigSchemeIdUri = Prelude.Nothing,
      baseUrl = Prelude.Nothing,
      dashManifestStyle = Prelude.Nothing,
      destination = Prelude.Nothing,
      destinationSettings = Prelude.Nothing,
      encryption = Prelude.Nothing,
      fragmentLength = Prelude.Nothing,
      hbbtvCompliance = Prelude.Nothing,
      imageBasedTrickPlay = Prelude.Nothing,
      imageBasedTrickPlaySettings = Prelude.Nothing,
      minBufferTime = Prelude.Nothing,
      minFinalSegmentLength = Prelude.Nothing,
      mpdManifestBandwidthType = Prelude.Nothing,
      mpdProfile = Prelude.Nothing,
      ptsOffsetHandlingForBFrames = Prelude.Nothing,
      segmentControl = Prelude.Nothing,
      segmentLength = Prelude.Nothing,
      segmentLengthControl = Prelude.Nothing,
      videoCompositionOffsets = Prelude.Nothing,
      writeSegmentTimelineInRepresentation =
        Prelude.Nothing
    }

-- | By default, the service creates one .mpd DASH manifest for each DASH ISO
-- output group in your job. This default manifest references every output
-- in the output group. To create additional DASH manifests that reference
-- a subset of the outputs in the output group, specify a list of them
-- here.
dashIsoGroupSettings_additionalManifests :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe [DashAdditionalManifest])
dashIsoGroupSettings_additionalManifests = Lens.lens (\DashIsoGroupSettings' {additionalManifests} -> additionalManifests) (\s@DashIsoGroupSettings' {} a -> s {additionalManifests = a} :: DashIsoGroupSettings) Prelude.. Lens.mapping Lens.coerced

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

-- | A partial URI prefix that will be put in the manifest (.mpd) file at the
-- top level BaseURL element. Can be used if streams are delivered from a
-- different URL than the manifest file.
dashIsoGroupSettings_baseUrl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Text)
dashIsoGroupSettings_baseUrl = Lens.lens (\DashIsoGroupSettings' {baseUrl} -> baseUrl) (\s@DashIsoGroupSettings' {} a -> s {baseUrl = a} :: DashIsoGroupSettings)

-- | Specify how MediaConvert writes SegmentTimeline in your output DASH
-- manifest. To write a SegmentTimeline in each video Representation: Keep
-- the default value, Basic. To write a common SegmentTimeline in the video
-- AdaptationSet: Choose Compact. Note that MediaConvert will still write a
-- SegmentTimeline in any Representation that does not share a common
-- timeline. To write a video AdaptationSet for each different output
-- framerate, and a common SegmentTimeline in each AdaptationSet: Choose
-- Distinct.
dashIsoGroupSettings_dashManifestStyle :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashManifestStyle)
dashIsoGroupSettings_dashManifestStyle = Lens.lens (\DashIsoGroupSettings' {dashManifestStyle} -> dashManifestStyle) (\s@DashIsoGroupSettings' {} a -> s {dashManifestStyle = a} :: DashIsoGroupSettings)

-- | Use Destination (Destination) to specify the S3 output location and the
-- output filename base. Destination accepts format identifiers. If you do
-- not specify the base filename in the URI, the service will use the
-- filename of the input file. If your job has multiple inputs, the service
-- uses the filename of the first input file.
dashIsoGroupSettings_destination :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Text)
dashIsoGroupSettings_destination = Lens.lens (\DashIsoGroupSettings' {destination} -> destination) (\s@DashIsoGroupSettings' {} a -> s {destination = a} :: DashIsoGroupSettings)

-- | Settings associated with the destination. Will vary based on the type of
-- destination
dashIsoGroupSettings_destinationSettings :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DestinationSettings)
dashIsoGroupSettings_destinationSettings = Lens.lens (\DashIsoGroupSettings' {destinationSettings} -> destinationSettings) (\s@DashIsoGroupSettings' {} a -> s {destinationSettings = a} :: DashIsoGroupSettings)

-- | DRM settings.
dashIsoGroupSettings_encryption :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoEncryptionSettings)
dashIsoGroupSettings_encryption = Lens.lens (\DashIsoGroupSettings' {encryption} -> encryption) (\s@DashIsoGroupSettings' {} a -> s {encryption = a} :: DashIsoGroupSettings)

-- | Length of fragments to generate (in seconds). Fragment length must be
-- compatible with GOP size and Framerate. Note that fragments will end on
-- the next keyframe after this number of seconds, so actual fragment
-- length may be longer. When Emit Single File is checked, the
-- fragmentation is internal to a single output file and it does not cause
-- the creation of many output files as in other output types.
dashIsoGroupSettings_fragmentLength :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_fragmentLength = Lens.lens (\DashIsoGroupSettings' {fragmentLength} -> fragmentLength) (\s@DashIsoGroupSettings' {} a -> s {fragmentLength = a} :: DashIsoGroupSettings)

-- | Supports HbbTV specification as indicated
dashIsoGroupSettings_hbbtvCompliance :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoHbbtvCompliance)
dashIsoGroupSettings_hbbtvCompliance = Lens.lens (\DashIsoGroupSettings' {hbbtvCompliance} -> hbbtvCompliance) (\s@DashIsoGroupSettings' {} a -> s {hbbtvCompliance = a} :: DashIsoGroupSettings)

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

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
dashIsoGroupSettings_imageBasedTrickPlaySettings :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoImageBasedTrickPlaySettings)
dashIsoGroupSettings_imageBasedTrickPlaySettings = Lens.lens (\DashIsoGroupSettings' {imageBasedTrickPlaySettings} -> imageBasedTrickPlaySettings) (\s@DashIsoGroupSettings' {} a -> s {imageBasedTrickPlaySettings = a} :: DashIsoGroupSettings)

-- | Minimum time of initially buffered media that is needed to ensure smooth
-- playout.
dashIsoGroupSettings_minBufferTime :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_minBufferTime = Lens.lens (\DashIsoGroupSettings' {minBufferTime} -> minBufferTime) (\s@DashIsoGroupSettings' {} a -> s {minBufferTime = a} :: DashIsoGroupSettings)

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

-- | Specify how the value for bandwidth is determined for each video
-- Representation in your output MPD manifest. We recommend that you choose
-- a MPD manifest bandwidth type that is compatible with your downstream
-- player configuration. Max: Use the same value that you specify for Max
-- bitrate in the video output, in bits per second. Average: Use the
-- calculated average bitrate of the encoded video output, in bits per
-- second.
dashIsoGroupSettings_mpdManifestBandwidthType :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoMpdManifestBandwidthType)
dashIsoGroupSettings_mpdManifestBandwidthType = Lens.lens (\DashIsoGroupSettings' {mpdManifestBandwidthType} -> mpdManifestBandwidthType) (\s@DashIsoGroupSettings' {} a -> s {mpdManifestBandwidthType = a} :: DashIsoGroupSettings)

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

-- | When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
dashIsoGroupSettings_segmentControl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoSegmentControl)
dashIsoGroupSettings_segmentControl = Lens.lens (\DashIsoGroupSettings' {segmentControl} -> segmentControl) (\s@DashIsoGroupSettings' {} a -> s {segmentControl = a} :: DashIsoGroupSettings)

-- | Specify the length, in whole seconds, of each segment. When you don\'t
-- specify a value, MediaConvert defaults to 30. Related settings: Use
-- Segment length control (SegmentLengthControl) to specify whether the
-- encoder enforces this value strictly. Use Segment control
-- (DashIsoSegmentControl) to specify whether MediaConvert creates separate
-- segment files or one content file that has metadata to mark the segment
-- boundaries.
dashIsoGroupSettings_segmentLength :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe Prelude.Natural)
dashIsoGroupSettings_segmentLength = Lens.lens (\DashIsoGroupSettings' {segmentLength} -> segmentLength) (\s@DashIsoGroupSettings' {} a -> s {segmentLength = a} :: DashIsoGroupSettings)

-- | Specify how you want MediaConvert to determine the segment length.
-- Choose Exact (EXACT) to have the encoder use the exact length that you
-- specify with the setting Segment length (SegmentLength). This might
-- result in extra I-frames. Choose Multiple of GOP (GOP_MULTIPLE) to have
-- the encoder round up the segment lengths to match the next GOP boundary.
dashIsoGroupSettings_segmentLengthControl :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoSegmentLengthControl)
dashIsoGroupSettings_segmentLengthControl = Lens.lens (\DashIsoGroupSettings' {segmentLengthControl} -> segmentLengthControl) (\s@DashIsoGroupSettings' {} a -> s {segmentLengthControl = a} :: DashIsoGroupSettings)

-- | Specify the video sample composition time offset mode in the output fMP4
-- TRUN box. For wider player compatibility, set Video composition offsets
-- to Unsigned or leave blank. The earliest presentation time may be
-- greater than zero, and sample composition time offsets will increment
-- using unsigned integers. For strict fMP4 video and audio timing, set
-- Video composition offsets to Signed. The earliest presentation time will
-- be equal to zero, and sample composition time offsets will increment
-- using signed integers.
dashIsoGroupSettings_videoCompositionOffsets :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoVideoCompositionOffsets)
dashIsoGroupSettings_videoCompositionOffsets = Lens.lens (\DashIsoGroupSettings' {videoCompositionOffsets} -> videoCompositionOffsets) (\s@DashIsoGroupSettings' {} a -> s {videoCompositionOffsets = a} :: DashIsoGroupSettings)

-- | If you get an HTTP error in the 400 range when you play back your DASH
-- output, enable this setting and run your transcoding job again. When you
-- enable this setting, the service writes precise segment durations in the
-- DASH manifest. The segment duration information appears inside the
-- SegmentTimeline element, inside SegmentTemplate at the Representation
-- level. When you don\'t enable this setting, the service writes
-- approximate segment durations in your DASH manifest.
dashIsoGroupSettings_writeSegmentTimelineInRepresentation :: Lens.Lens' DashIsoGroupSettings (Prelude.Maybe DashIsoWriteSegmentTimelineInRepresentation)
dashIsoGroupSettings_writeSegmentTimelineInRepresentation = Lens.lens (\DashIsoGroupSettings' {writeSegmentTimelineInRepresentation} -> writeSegmentTimelineInRepresentation) (\s@DashIsoGroupSettings' {} a -> s {writeSegmentTimelineInRepresentation = a} :: DashIsoGroupSettings)

instance Data.FromJSON DashIsoGroupSettings where
  parseJSON =
    Data.withObject
      "DashIsoGroupSettings"
      ( \x ->
          DashIsoGroupSettings'
            Prelude.<$> ( x
                            Data..:? "additionalManifests"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "audioChannelConfigSchemeIdUri")
            Prelude.<*> (x Data..:? "baseUrl")
            Prelude.<*> (x Data..:? "dashManifestStyle")
            Prelude.<*> (x Data..:? "destination")
            Prelude.<*> (x Data..:? "destinationSettings")
            Prelude.<*> (x Data..:? "encryption")
            Prelude.<*> (x Data..:? "fragmentLength")
            Prelude.<*> (x Data..:? "hbbtvCompliance")
            Prelude.<*> (x Data..:? "imageBasedTrickPlay")
            Prelude.<*> (x Data..:? "imageBasedTrickPlaySettings")
            Prelude.<*> (x Data..:? "minBufferTime")
            Prelude.<*> (x Data..:? "minFinalSegmentLength")
            Prelude.<*> (x Data..:? "mpdManifestBandwidthType")
            Prelude.<*> (x Data..:? "mpdProfile")
            Prelude.<*> (x Data..:? "ptsOffsetHandlingForBFrames")
            Prelude.<*> (x Data..:? "segmentControl")
            Prelude.<*> (x Data..:? "segmentLength")
            Prelude.<*> (x Data..:? "segmentLengthControl")
            Prelude.<*> (x Data..:? "videoCompositionOffsets")
            Prelude.<*> (x Data..:? "writeSegmentTimelineInRepresentation")
      )

instance Prelude.Hashable DashIsoGroupSettings where
  hashWithSalt _salt DashIsoGroupSettings' {..} =
    _salt
      `Prelude.hashWithSalt` additionalManifests
      `Prelude.hashWithSalt` audioChannelConfigSchemeIdUri
      `Prelude.hashWithSalt` baseUrl
      `Prelude.hashWithSalt` dashManifestStyle
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` destinationSettings
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` fragmentLength
      `Prelude.hashWithSalt` hbbtvCompliance
      `Prelude.hashWithSalt` imageBasedTrickPlay
      `Prelude.hashWithSalt` imageBasedTrickPlaySettings
      `Prelude.hashWithSalt` minBufferTime
      `Prelude.hashWithSalt` minFinalSegmentLength
      `Prelude.hashWithSalt` mpdManifestBandwidthType
      `Prelude.hashWithSalt` mpdProfile
      `Prelude.hashWithSalt` ptsOffsetHandlingForBFrames
      `Prelude.hashWithSalt` segmentControl
      `Prelude.hashWithSalt` segmentLength
      `Prelude.hashWithSalt` segmentLengthControl
      `Prelude.hashWithSalt` videoCompositionOffsets
      `Prelude.hashWithSalt` writeSegmentTimelineInRepresentation

instance Prelude.NFData DashIsoGroupSettings where
  rnf DashIsoGroupSettings' {..} =
    Prelude.rnf additionalManifests
      `Prelude.seq` Prelude.rnf audioChannelConfigSchemeIdUri
      `Prelude.seq` Prelude.rnf baseUrl
      `Prelude.seq` Prelude.rnf dashManifestStyle
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf destinationSettings
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf fragmentLength
      `Prelude.seq` Prelude.rnf hbbtvCompliance
      `Prelude.seq` Prelude.rnf imageBasedTrickPlay
      `Prelude.seq` Prelude.rnf imageBasedTrickPlaySettings
      `Prelude.seq` Prelude.rnf minBufferTime
      `Prelude.seq` Prelude.rnf minFinalSegmentLength
      `Prelude.seq` Prelude.rnf mpdManifestBandwidthType
      `Prelude.seq` Prelude.rnf mpdProfile
      `Prelude.seq` Prelude.rnf
        ptsOffsetHandlingForBFrames
      `Prelude.seq` Prelude.rnf segmentControl
      `Prelude.seq` Prelude.rnf segmentLength
      `Prelude.seq` Prelude.rnf segmentLengthControl
      `Prelude.seq` Prelude.rnf
        videoCompositionOffsets
      `Prelude.seq` Prelude.rnf
        writeSegmentTimelineInRepresentation

instance Data.ToJSON DashIsoGroupSettings where
  toJSON DashIsoGroupSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalManifests" Data..=)
              Prelude.<$> additionalManifests,
            ("audioChannelConfigSchemeIdUri" Data..=)
              Prelude.<$> audioChannelConfigSchemeIdUri,
            ("baseUrl" Data..=) Prelude.<$> baseUrl,
            ("dashManifestStyle" Data..=)
              Prelude.<$> dashManifestStyle,
            ("destination" Data..=) Prelude.<$> destination,
            ("destinationSettings" Data..=)
              Prelude.<$> destinationSettings,
            ("encryption" Data..=) Prelude.<$> encryption,
            ("fragmentLength" Data..=)
              Prelude.<$> fragmentLength,
            ("hbbtvCompliance" Data..=)
              Prelude.<$> hbbtvCompliance,
            ("imageBasedTrickPlay" Data..=)
              Prelude.<$> imageBasedTrickPlay,
            ("imageBasedTrickPlaySettings" Data..=)
              Prelude.<$> imageBasedTrickPlaySettings,
            ("minBufferTime" Data..=) Prelude.<$> minBufferTime,
            ("minFinalSegmentLength" Data..=)
              Prelude.<$> minFinalSegmentLength,
            ("mpdManifestBandwidthType" Data..=)
              Prelude.<$> mpdManifestBandwidthType,
            ("mpdProfile" Data..=) Prelude.<$> mpdProfile,
            ("ptsOffsetHandlingForBFrames" Data..=)
              Prelude.<$> ptsOffsetHandlingForBFrames,
            ("segmentControl" Data..=)
              Prelude.<$> segmentControl,
            ("segmentLength" Data..=) Prelude.<$> segmentLength,
            ("segmentLengthControl" Data..=)
              Prelude.<$> segmentLengthControl,
            ("videoCompositionOffsets" Data..=)
              Prelude.<$> videoCompositionOffsets,
            ("writeSegmentTimelineInRepresentation" Data..=)
              Prelude.<$> writeSegmentTimelineInRepresentation
          ]
      )
