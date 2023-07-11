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
-- Module      : Amazonka.MediaConvert.Types.CmfcSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmfcSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.CmfcAudioDuration
import Amazonka.MediaConvert.Types.CmfcAudioTrackType
import Amazonka.MediaConvert.Types.CmfcDescriptiveVideoServiceFlag
import Amazonka.MediaConvert.Types.CmfcIFrameOnlyManifest
import Amazonka.MediaConvert.Types.CmfcKlvMetadata
import Amazonka.MediaConvert.Types.CmfcManifestMetadataSignaling
import Amazonka.MediaConvert.Types.CmfcScte35Esam
import Amazonka.MediaConvert.Types.CmfcScte35Source
import Amazonka.MediaConvert.Types.CmfcTimedMetadata
import Amazonka.MediaConvert.Types.CmfcTimedMetadataBoxVersion
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to the fragmented MP4 container for the segments
-- in your CMAF outputs.
--
-- /See:/ 'newCmfcSettings' smart constructor.
data CmfcSettings = CmfcSettings'
  { -- | Specify this setting only when your output will be consumed by a
    -- downstream repackaging workflow that is sensitive to very small duration
    -- differences between video and audio. For this situation, choose Match
    -- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
    -- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
    -- choose Match video duration, MediaConvert pads the output audio streams
    -- with silence or trims them to ensure that the total duration of each
    -- audio stream is at least as long as the total duration of the video
    -- stream. After padding or trimming, the audio stream duration is no more
    -- than one frame longer than the video stream. MediaConvert applies audio
    -- padding or trimming only to the end of the last segment of the output.
    -- For unsegmented outputs, MediaConvert adds padding only to the end of
    -- the file. When you keep the default value, any minor discrepancies
    -- between audio and video duration will depend on your output audio codec.
    audioDuration :: Prelude.Maybe CmfcAudioDuration,
    -- | Specify the audio rendition group for this audio rendition. Specify up
    -- to one value for each audio output in your output group. This value
    -- appears in your HLS parent manifest in the EXT-X-MEDIA tag of
    -- TYPE=AUDIO, as the value for the GROUP-ID attribute. For example, if you
    -- specify \"audio_aac_1\" for Audio group ID, it appears in your manifest
    -- like this: #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"audio_aac_1\". Related
    -- setting: To associate the rendition group that this audio track belongs
    -- to with a video rendition, include the same value that you provide here
    -- for that video output\'s setting Audio rendition sets
    -- (audioRenditionSets).
    audioGroupId :: Prelude.Maybe Prelude.Text,
    -- | List the audio rendition groups that you want included with this video
    -- rendition. Use a comma-separated list. For example, say you want to
    -- include the audio rendition groups that have the audio group IDs
    -- \"audio_aac_1\" and \"audio_dolby\". Then you would specify this value:
    -- \"audio_aac_1,audio_dolby\". Related setting: The rendition groups that
    -- you include in your comma-separated list should all match values that
    -- you specify in the setting Audio group ID (AudioGroupId) for audio
    -- renditions in the same output group as this video rendition. Default
    -- behavior: If you don\'t specify anything here and for Audio group ID,
    -- MediaConvert puts each audio variant in its own audio rendition group
    -- and associates it with every video variant. Each value in your list
    -- appears in your HLS parent manifest in the EXT-X-STREAM-INF tag as the
    -- value for the AUDIO attribute. To continue the previous example, say
    -- that the file name for the child manifest for your video rendition is
    -- \"amazing_video_1.m3u8\". Then, in your parent manifest, each value will
    -- appear on separate lines, like this:
    -- #EXT-X-STREAM-INF:AUDIO=\"audio_aac_1\"... amazing_video_1.m3u8
    -- #EXT-X-STREAM-INF:AUDIO=\"audio_dolby\"... amazing_video_1.m3u8
    audioRenditionSets :: Prelude.Maybe Prelude.Text,
    -- | Use this setting to control the values that MediaConvert puts in your
    -- HLS parent playlist to control how the client player selects which audio
    -- track to play. The other options for this setting determine the values
    -- that MediaConvert writes for the DEFAULT and AUTOSELECT attributes of
    -- the EXT-X-MEDIA entry for the audio variant. For more information about
    -- these attributes, see the Apple documentation article
    -- https:\/\/developer.apple.com\/documentation\/http_live_streaming\/example_playlists_for_http_live_streaming\/adding_alternate_media_to_a_playlist.
    -- Choose Alternate audio, auto select, default
    -- (ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT) to set DEFAULT=YES and
    -- AUTOSELECT=YES. Choose this value for only one variant in your output
    -- group. Choose Alternate audio, auto select, not default
    -- (ALTERNATE_AUDIO_AUTO_SELECT) to set DEFAULT=NO and AUTOSELECT=YES.
    -- Choose Alternate Audio, Not Auto Select to set DEFAULT=NO and
    -- AUTOSELECT=NO. When you don\'t specify a value for this setting,
    -- MediaConvert defaults to Alternate audio, auto select, default. When
    -- there is more than one variant in your output group, you must explicitly
    -- choose a value for this setting.
    audioTrackType :: Prelude.Maybe CmfcAudioTrackType,
    -- | Specify whether to flag this audio track as descriptive video service
    -- (DVS) in your HLS parent manifest. When you choose Flag (FLAG),
    -- MediaConvert includes the parameter
    -- CHARACTERISTICS=\"public.accessibility.describes-video\" in the
    -- EXT-X-MEDIA entry for this track. When you keep the default choice,
    -- Don\'t flag (DONT_FLAG), MediaConvert leaves this parameter out. The DVS
    -- flag can help with accessibility on Apple devices. For more information,
    -- see the Apple documentation.
    descriptiveVideoServiceFlag :: Prelude.Maybe CmfcDescriptiveVideoServiceFlag,
    -- | Choose Include (INCLUDE) to have MediaConvert generate an HLS child
    -- manifest that lists only the I-frames for this rendition, in addition to
    -- your regular manifest for this rendition. You might use this manifest as
    -- part of a workflow that creates preview functions for your video.
    -- MediaConvert adds both the I-frame only child manifest and the regular
    -- child manifest to the parent manifest. When you don\'t need the I-frame
    -- only child manifest, keep the default value Exclude (EXCLUDE).
    iFrameOnlyManifest :: Prelude.Maybe CmfcIFrameOnlyManifest,
    -- | To include key-length-value metadata in this output: Set KLV metadata
    -- insertion to Passthrough. MediaConvert reads KLV metadata present in
    -- your input and writes each instance to a separate event message box in
    -- the output, according to MISB ST1910.1. To exclude this KLV metadata:
    -- Set KLV metadata insertion to None or leave blank.
    klvMetadata :: Prelude.Maybe CmfcKlvMetadata,
    -- | To add an InbandEventStream element in your output MPD manifest for each
    -- type of event message, set Manifest metadata signaling to Enabled. For
    -- ID3 event messages, the InbandEventStream element schemeIdUri will be
    -- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
    -- event messages, the InbandEventStream element schemeIdUri will be
    -- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
    -- MPD manifest, set Manifest metadata signaling to Disabled.
    manifestMetadataSignaling :: Prelude.Maybe CmfcManifestMetadataSignaling,
    -- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
    -- INSERT to put SCTE-35 markers in this output at the insertion points
    -- that you specify in an ESAM XML document. Provide the document in the
    -- setting SCC XML (sccXml).
    scte35Esam :: Prelude.Maybe CmfcScte35Esam,
    -- | Ignore this setting unless you have SCTE-35 markers in your input video
    -- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
    -- appear in your input to also appear in this output. Choose None (NONE)
    -- if you don\'t want those SCTE-35 markers in this output.
    scte35Source :: Prelude.Maybe CmfcScte35Source,
    -- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
    -- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
    -- metadata inserter (timedMetadataInsertion). MediaConvert writes each
    -- instance of ID3 metadata in a separate Event Message (eMSG) box. To
    -- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
    -- blank.
    timedMetadata :: Prelude.Maybe CmfcTimedMetadata,
    -- | Specify the event message box (eMSG) version for ID3 timed metadata in
    -- your output. For more information, see ISO\/IEC 23009-1:2022 section
    -- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
    -- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
    -- Passthrough.
    timedMetadataBoxVersion :: Prelude.Maybe CmfcTimedMetadataBoxVersion,
    -- | Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
    -- ID3 timed metadata in your output. For more informaiton, see ISO\/IEC
    -- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
    -- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
    -- for ID3 metadata scheme ID URI, you must also set ID3 metadata
    -- (timedMetadata) to Passthrough.
    timedMetadataSchemeIdUri :: Prelude.Maybe Prelude.Text,
    -- | Specify the event message box (eMSG) value for ID3 timed metadata in
    -- your output. For more informaiton, see ISO\/IEC 23009-1:2022 section
    -- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
    -- you must also set ID3 metadata (timedMetadata) to Passthrough.
    timedMetadataValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CmfcSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioDuration', 'cmfcSettings_audioDuration' - Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
--
-- 'audioGroupId', 'cmfcSettings_audioGroupId' - Specify the audio rendition group for this audio rendition. Specify up
-- to one value for each audio output in your output group. This value
-- appears in your HLS parent manifest in the EXT-X-MEDIA tag of
-- TYPE=AUDIO, as the value for the GROUP-ID attribute. For example, if you
-- specify \"audio_aac_1\" for Audio group ID, it appears in your manifest
-- like this: #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"audio_aac_1\". Related
-- setting: To associate the rendition group that this audio track belongs
-- to with a video rendition, include the same value that you provide here
-- for that video output\'s setting Audio rendition sets
-- (audioRenditionSets).
--
-- 'audioRenditionSets', 'cmfcSettings_audioRenditionSets' - List the audio rendition groups that you want included with this video
-- rendition. Use a comma-separated list. For example, say you want to
-- include the audio rendition groups that have the audio group IDs
-- \"audio_aac_1\" and \"audio_dolby\". Then you would specify this value:
-- \"audio_aac_1,audio_dolby\". Related setting: The rendition groups that
-- you include in your comma-separated list should all match values that
-- you specify in the setting Audio group ID (AudioGroupId) for audio
-- renditions in the same output group as this video rendition. Default
-- behavior: If you don\'t specify anything here and for Audio group ID,
-- MediaConvert puts each audio variant in its own audio rendition group
-- and associates it with every video variant. Each value in your list
-- appears in your HLS parent manifest in the EXT-X-STREAM-INF tag as the
-- value for the AUDIO attribute. To continue the previous example, say
-- that the file name for the child manifest for your video rendition is
-- \"amazing_video_1.m3u8\". Then, in your parent manifest, each value will
-- appear on separate lines, like this:
-- #EXT-X-STREAM-INF:AUDIO=\"audio_aac_1\"... amazing_video_1.m3u8
-- #EXT-X-STREAM-INF:AUDIO=\"audio_dolby\"... amazing_video_1.m3u8
--
-- 'audioTrackType', 'cmfcSettings_audioTrackType' - Use this setting to control the values that MediaConvert puts in your
-- HLS parent playlist to control how the client player selects which audio
-- track to play. The other options for this setting determine the values
-- that MediaConvert writes for the DEFAULT and AUTOSELECT attributes of
-- the EXT-X-MEDIA entry for the audio variant. For more information about
-- these attributes, see the Apple documentation article
-- https:\/\/developer.apple.com\/documentation\/http_live_streaming\/example_playlists_for_http_live_streaming\/adding_alternate_media_to_a_playlist.
-- Choose Alternate audio, auto select, default
-- (ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT) to set DEFAULT=YES and
-- AUTOSELECT=YES. Choose this value for only one variant in your output
-- group. Choose Alternate audio, auto select, not default
-- (ALTERNATE_AUDIO_AUTO_SELECT) to set DEFAULT=NO and AUTOSELECT=YES.
-- Choose Alternate Audio, Not Auto Select to set DEFAULT=NO and
-- AUTOSELECT=NO. When you don\'t specify a value for this setting,
-- MediaConvert defaults to Alternate audio, auto select, default. When
-- there is more than one variant in your output group, you must explicitly
-- choose a value for this setting.
--
-- 'descriptiveVideoServiceFlag', 'cmfcSettings_descriptiveVideoServiceFlag' - Specify whether to flag this audio track as descriptive video service
-- (DVS) in your HLS parent manifest. When you choose Flag (FLAG),
-- MediaConvert includes the parameter
-- CHARACTERISTICS=\"public.accessibility.describes-video\" in the
-- EXT-X-MEDIA entry for this track. When you keep the default choice,
-- Don\'t flag (DONT_FLAG), MediaConvert leaves this parameter out. The DVS
-- flag can help with accessibility on Apple devices. For more information,
-- see the Apple documentation.
--
-- 'iFrameOnlyManifest', 'cmfcSettings_iFrameOnlyManifest' - Choose Include (INCLUDE) to have MediaConvert generate an HLS child
-- manifest that lists only the I-frames for this rendition, in addition to
-- your regular manifest for this rendition. You might use this manifest as
-- part of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
--
-- 'klvMetadata', 'cmfcSettings_klvMetadata' - To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
--
-- 'manifestMetadataSignaling', 'cmfcSettings_manifestMetadataSignaling' - To add an InbandEventStream element in your output MPD manifest for each
-- type of event message, set Manifest metadata signaling to Enabled. For
-- ID3 event messages, the InbandEventStream element schemeIdUri will be
-- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
-- event messages, the InbandEventStream element schemeIdUri will be
-- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
-- MPD manifest, set Manifest metadata signaling to Disabled.
--
-- 'scte35Esam', 'cmfcSettings_scte35Esam' - Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
--
-- 'scte35Source', 'cmfcSettings_scte35Source' - Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
--
-- 'timedMetadata', 'cmfcSettings_timedMetadata' - To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
--
-- 'timedMetadataBoxVersion', 'cmfcSettings_timedMetadataBoxVersion' - Specify the event message box (eMSG) version for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
-- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
-- Passthrough.
--
-- 'timedMetadataSchemeIdUri', 'cmfcSettings_timedMetadataSchemeIdUri' - Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
-- ID3 timed metadata in your output. For more informaiton, see ISO\/IEC
-- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
-- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
-- for ID3 metadata scheme ID URI, you must also set ID3 metadata
-- (timedMetadata) to Passthrough.
--
-- 'timedMetadataValue', 'cmfcSettings_timedMetadataValue' - Specify the event message box (eMSG) value for ID3 timed metadata in
-- your output. For more informaiton, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
-- you must also set ID3 metadata (timedMetadata) to Passthrough.
newCmfcSettings ::
  CmfcSettings
newCmfcSettings =
  CmfcSettings'
    { audioDuration = Prelude.Nothing,
      audioGroupId = Prelude.Nothing,
      audioRenditionSets = Prelude.Nothing,
      audioTrackType = Prelude.Nothing,
      descriptiveVideoServiceFlag = Prelude.Nothing,
      iFrameOnlyManifest = Prelude.Nothing,
      klvMetadata = Prelude.Nothing,
      manifestMetadataSignaling = Prelude.Nothing,
      scte35Esam = Prelude.Nothing,
      scte35Source = Prelude.Nothing,
      timedMetadata = Prelude.Nothing,
      timedMetadataBoxVersion = Prelude.Nothing,
      timedMetadataSchemeIdUri = Prelude.Nothing,
      timedMetadataValue = Prelude.Nothing
    }

-- | Specify this setting only when your output will be consumed by a
-- downstream repackaging workflow that is sensitive to very small duration
-- differences between video and audio. For this situation, choose Match
-- video duration (MATCH_VIDEO_DURATION). In all other cases, keep the
-- default value, Default codec duration (DEFAULT_CODEC_DURATION). When you
-- choose Match video duration, MediaConvert pads the output audio streams
-- with silence or trims them to ensure that the total duration of each
-- audio stream is at least as long as the total duration of the video
-- stream. After padding or trimming, the audio stream duration is no more
-- than one frame longer than the video stream. MediaConvert applies audio
-- padding or trimming only to the end of the last segment of the output.
-- For unsegmented outputs, MediaConvert adds padding only to the end of
-- the file. When you keep the default value, any minor discrepancies
-- between audio and video duration will depend on your output audio codec.
cmfcSettings_audioDuration :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcAudioDuration)
cmfcSettings_audioDuration = Lens.lens (\CmfcSettings' {audioDuration} -> audioDuration) (\s@CmfcSettings' {} a -> s {audioDuration = a} :: CmfcSettings)

-- | Specify the audio rendition group for this audio rendition. Specify up
-- to one value for each audio output in your output group. This value
-- appears in your HLS parent manifest in the EXT-X-MEDIA tag of
-- TYPE=AUDIO, as the value for the GROUP-ID attribute. For example, if you
-- specify \"audio_aac_1\" for Audio group ID, it appears in your manifest
-- like this: #EXT-X-MEDIA:TYPE=AUDIO,GROUP-ID=\"audio_aac_1\". Related
-- setting: To associate the rendition group that this audio track belongs
-- to with a video rendition, include the same value that you provide here
-- for that video output\'s setting Audio rendition sets
-- (audioRenditionSets).
cmfcSettings_audioGroupId :: Lens.Lens' CmfcSettings (Prelude.Maybe Prelude.Text)
cmfcSettings_audioGroupId = Lens.lens (\CmfcSettings' {audioGroupId} -> audioGroupId) (\s@CmfcSettings' {} a -> s {audioGroupId = a} :: CmfcSettings)

-- | List the audio rendition groups that you want included with this video
-- rendition. Use a comma-separated list. For example, say you want to
-- include the audio rendition groups that have the audio group IDs
-- \"audio_aac_1\" and \"audio_dolby\". Then you would specify this value:
-- \"audio_aac_1,audio_dolby\". Related setting: The rendition groups that
-- you include in your comma-separated list should all match values that
-- you specify in the setting Audio group ID (AudioGroupId) for audio
-- renditions in the same output group as this video rendition. Default
-- behavior: If you don\'t specify anything here and for Audio group ID,
-- MediaConvert puts each audio variant in its own audio rendition group
-- and associates it with every video variant. Each value in your list
-- appears in your HLS parent manifest in the EXT-X-STREAM-INF tag as the
-- value for the AUDIO attribute. To continue the previous example, say
-- that the file name for the child manifest for your video rendition is
-- \"amazing_video_1.m3u8\". Then, in your parent manifest, each value will
-- appear on separate lines, like this:
-- #EXT-X-STREAM-INF:AUDIO=\"audio_aac_1\"... amazing_video_1.m3u8
-- #EXT-X-STREAM-INF:AUDIO=\"audio_dolby\"... amazing_video_1.m3u8
cmfcSettings_audioRenditionSets :: Lens.Lens' CmfcSettings (Prelude.Maybe Prelude.Text)
cmfcSettings_audioRenditionSets = Lens.lens (\CmfcSettings' {audioRenditionSets} -> audioRenditionSets) (\s@CmfcSettings' {} a -> s {audioRenditionSets = a} :: CmfcSettings)

-- | Use this setting to control the values that MediaConvert puts in your
-- HLS parent playlist to control how the client player selects which audio
-- track to play. The other options for this setting determine the values
-- that MediaConvert writes for the DEFAULT and AUTOSELECT attributes of
-- the EXT-X-MEDIA entry for the audio variant. For more information about
-- these attributes, see the Apple documentation article
-- https:\/\/developer.apple.com\/documentation\/http_live_streaming\/example_playlists_for_http_live_streaming\/adding_alternate_media_to_a_playlist.
-- Choose Alternate audio, auto select, default
-- (ALTERNATE_AUDIO_AUTO_SELECT_DEFAULT) to set DEFAULT=YES and
-- AUTOSELECT=YES. Choose this value for only one variant in your output
-- group. Choose Alternate audio, auto select, not default
-- (ALTERNATE_AUDIO_AUTO_SELECT) to set DEFAULT=NO and AUTOSELECT=YES.
-- Choose Alternate Audio, Not Auto Select to set DEFAULT=NO and
-- AUTOSELECT=NO. When you don\'t specify a value for this setting,
-- MediaConvert defaults to Alternate audio, auto select, default. When
-- there is more than one variant in your output group, you must explicitly
-- choose a value for this setting.
cmfcSettings_audioTrackType :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcAudioTrackType)
cmfcSettings_audioTrackType = Lens.lens (\CmfcSettings' {audioTrackType} -> audioTrackType) (\s@CmfcSettings' {} a -> s {audioTrackType = a} :: CmfcSettings)

-- | Specify whether to flag this audio track as descriptive video service
-- (DVS) in your HLS parent manifest. When you choose Flag (FLAG),
-- MediaConvert includes the parameter
-- CHARACTERISTICS=\"public.accessibility.describes-video\" in the
-- EXT-X-MEDIA entry for this track. When you keep the default choice,
-- Don\'t flag (DONT_FLAG), MediaConvert leaves this parameter out. The DVS
-- flag can help with accessibility on Apple devices. For more information,
-- see the Apple documentation.
cmfcSettings_descriptiveVideoServiceFlag :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcDescriptiveVideoServiceFlag)
cmfcSettings_descriptiveVideoServiceFlag = Lens.lens (\CmfcSettings' {descriptiveVideoServiceFlag} -> descriptiveVideoServiceFlag) (\s@CmfcSettings' {} a -> s {descriptiveVideoServiceFlag = a} :: CmfcSettings)

-- | Choose Include (INCLUDE) to have MediaConvert generate an HLS child
-- manifest that lists only the I-frames for this rendition, in addition to
-- your regular manifest for this rendition. You might use this manifest as
-- part of a workflow that creates preview functions for your video.
-- MediaConvert adds both the I-frame only child manifest and the regular
-- child manifest to the parent manifest. When you don\'t need the I-frame
-- only child manifest, keep the default value Exclude (EXCLUDE).
cmfcSettings_iFrameOnlyManifest :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcIFrameOnlyManifest)
cmfcSettings_iFrameOnlyManifest = Lens.lens (\CmfcSettings' {iFrameOnlyManifest} -> iFrameOnlyManifest) (\s@CmfcSettings' {} a -> s {iFrameOnlyManifest = a} :: CmfcSettings)

-- | To include key-length-value metadata in this output: Set KLV metadata
-- insertion to Passthrough. MediaConvert reads KLV metadata present in
-- your input and writes each instance to a separate event message box in
-- the output, according to MISB ST1910.1. To exclude this KLV metadata:
-- Set KLV metadata insertion to None or leave blank.
cmfcSettings_klvMetadata :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcKlvMetadata)
cmfcSettings_klvMetadata = Lens.lens (\CmfcSettings' {klvMetadata} -> klvMetadata) (\s@CmfcSettings' {} a -> s {klvMetadata = a} :: CmfcSettings)

-- | To add an InbandEventStream element in your output MPD manifest for each
-- type of event message, set Manifest metadata signaling to Enabled. For
-- ID3 event messages, the InbandEventStream element schemeIdUri will be
-- same value that you specify for ID3 metadata scheme ID URI. For SCTE35
-- event messages, the InbandEventStream element schemeIdUri will be
-- \"urn:scte:scte35:2013:bin\". To leave these elements out of your output
-- MPD manifest, set Manifest metadata signaling to Disabled.
cmfcSettings_manifestMetadataSignaling :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcManifestMetadataSignaling)
cmfcSettings_manifestMetadataSignaling = Lens.lens (\CmfcSettings' {manifestMetadataSignaling} -> manifestMetadataSignaling) (\s@CmfcSettings' {} a -> s {manifestMetadataSignaling = a} :: CmfcSettings)

-- | Use this setting only when you specify SCTE-35 markers from ESAM. Choose
-- INSERT to put SCTE-35 markers in this output at the insertion points
-- that you specify in an ESAM XML document. Provide the document in the
-- setting SCC XML (sccXml).
cmfcSettings_scte35Esam :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcScte35Esam)
cmfcSettings_scte35Esam = Lens.lens (\CmfcSettings' {scte35Esam} -> scte35Esam) (\s@CmfcSettings' {} a -> s {scte35Esam = a} :: CmfcSettings)

-- | Ignore this setting unless you have SCTE-35 markers in your input video
-- file. Choose Passthrough (PASSTHROUGH) if you want SCTE-35 markers that
-- appear in your input to also appear in this output. Choose None (NONE)
-- if you don\'t want those SCTE-35 markers in this output.
cmfcSettings_scte35Source :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcScte35Source)
cmfcSettings_scte35Source = Lens.lens (\CmfcSettings' {scte35Source} -> scte35Source) (\s@CmfcSettings' {} a -> s {scte35Source = a} :: CmfcSettings)

-- | To include ID3 metadata in this output: Set ID3 metadata (timedMetadata)
-- to Passthrough (PASSTHROUGH). Specify this ID3 metadata in Custom ID3
-- metadata inserter (timedMetadataInsertion). MediaConvert writes each
-- instance of ID3 metadata in a separate Event Message (eMSG) box. To
-- exclude this ID3 metadata: Set ID3 metadata to None (NONE) or leave
-- blank.
cmfcSettings_timedMetadata :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcTimedMetadata)
cmfcSettings_timedMetadata = Lens.lens (\CmfcSettings' {timedMetadata} -> timedMetadata) (\s@CmfcSettings' {} a -> s {timedMetadata = a} :: CmfcSettings)

-- | Specify the event message box (eMSG) version for ID3 timed metadata in
-- your output. For more information, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.3 Syntax. Leave blank to use the default value Version 0. When
-- you specify Version 1, you must also set ID3 metadata (timedMetadata) to
-- Passthrough.
cmfcSettings_timedMetadataBoxVersion :: Lens.Lens' CmfcSettings (Prelude.Maybe CmfcTimedMetadataBoxVersion)
cmfcSettings_timedMetadataBoxVersion = Lens.lens (\CmfcSettings' {timedMetadataBoxVersion} -> timedMetadataBoxVersion) (\s@CmfcSettings' {} a -> s {timedMetadataBoxVersion = a} :: CmfcSettings)

-- | Specify the event message box (eMSG) scheme ID URI (scheme_id_uri) for
-- ID3 timed metadata in your output. For more informaiton, see ISO\/IEC
-- 23009-1:2022 section 5.10.3.3.4 Semantics. Leave blank to use the
-- default value: https:\/\/aomedia.org\/emsg\/ID3 When you specify a value
-- for ID3 metadata scheme ID URI, you must also set ID3 metadata
-- (timedMetadata) to Passthrough.
cmfcSettings_timedMetadataSchemeIdUri :: Lens.Lens' CmfcSettings (Prelude.Maybe Prelude.Text)
cmfcSettings_timedMetadataSchemeIdUri = Lens.lens (\CmfcSettings' {timedMetadataSchemeIdUri} -> timedMetadataSchemeIdUri) (\s@CmfcSettings' {} a -> s {timedMetadataSchemeIdUri = a} :: CmfcSettings)

-- | Specify the event message box (eMSG) value for ID3 timed metadata in
-- your output. For more informaiton, see ISO\/IEC 23009-1:2022 section
-- 5.10.3.3.4 Semantics. When you specify a value for ID3 Metadata Value,
-- you must also set ID3 metadata (timedMetadata) to Passthrough.
cmfcSettings_timedMetadataValue :: Lens.Lens' CmfcSettings (Prelude.Maybe Prelude.Text)
cmfcSettings_timedMetadataValue = Lens.lens (\CmfcSettings' {timedMetadataValue} -> timedMetadataValue) (\s@CmfcSettings' {} a -> s {timedMetadataValue = a} :: CmfcSettings)

instance Data.FromJSON CmfcSettings where
  parseJSON =
    Data.withObject
      "CmfcSettings"
      ( \x ->
          CmfcSettings'
            Prelude.<$> (x Data..:? "audioDuration")
            Prelude.<*> (x Data..:? "audioGroupId")
            Prelude.<*> (x Data..:? "audioRenditionSets")
            Prelude.<*> (x Data..:? "audioTrackType")
            Prelude.<*> (x Data..:? "descriptiveVideoServiceFlag")
            Prelude.<*> (x Data..:? "iFrameOnlyManifest")
            Prelude.<*> (x Data..:? "klvMetadata")
            Prelude.<*> (x Data..:? "manifestMetadataSignaling")
            Prelude.<*> (x Data..:? "scte35Esam")
            Prelude.<*> (x Data..:? "scte35Source")
            Prelude.<*> (x Data..:? "timedMetadata")
            Prelude.<*> (x Data..:? "timedMetadataBoxVersion")
            Prelude.<*> (x Data..:? "timedMetadataSchemeIdUri")
            Prelude.<*> (x Data..:? "timedMetadataValue")
      )

instance Prelude.Hashable CmfcSettings where
  hashWithSalt _salt CmfcSettings' {..} =
    _salt
      `Prelude.hashWithSalt` audioDuration
      `Prelude.hashWithSalt` audioGroupId
      `Prelude.hashWithSalt` audioRenditionSets
      `Prelude.hashWithSalt` audioTrackType
      `Prelude.hashWithSalt` descriptiveVideoServiceFlag
      `Prelude.hashWithSalt` iFrameOnlyManifest
      `Prelude.hashWithSalt` klvMetadata
      `Prelude.hashWithSalt` manifestMetadataSignaling
      `Prelude.hashWithSalt` scte35Esam
      `Prelude.hashWithSalt` scte35Source
      `Prelude.hashWithSalt` timedMetadata
      `Prelude.hashWithSalt` timedMetadataBoxVersion
      `Prelude.hashWithSalt` timedMetadataSchemeIdUri
      `Prelude.hashWithSalt` timedMetadataValue

instance Prelude.NFData CmfcSettings where
  rnf CmfcSettings' {..} =
    Prelude.rnf audioDuration
      `Prelude.seq` Prelude.rnf audioGroupId
      `Prelude.seq` Prelude.rnf audioRenditionSets
      `Prelude.seq` Prelude.rnf audioTrackType
      `Prelude.seq` Prelude.rnf descriptiveVideoServiceFlag
      `Prelude.seq` Prelude.rnf iFrameOnlyManifest
      `Prelude.seq` Prelude.rnf klvMetadata
      `Prelude.seq` Prelude.rnf manifestMetadataSignaling
      `Prelude.seq` Prelude.rnf scte35Esam
      `Prelude.seq` Prelude.rnf scte35Source
      `Prelude.seq` Prelude.rnf timedMetadata
      `Prelude.seq` Prelude.rnf timedMetadataBoxVersion
      `Prelude.seq` Prelude.rnf timedMetadataSchemeIdUri
      `Prelude.seq` Prelude.rnf timedMetadataValue

instance Data.ToJSON CmfcSettings where
  toJSON CmfcSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioDuration" Data..=) Prelude.<$> audioDuration,
            ("audioGroupId" Data..=) Prelude.<$> audioGroupId,
            ("audioRenditionSets" Data..=)
              Prelude.<$> audioRenditionSets,
            ("audioTrackType" Data..=)
              Prelude.<$> audioTrackType,
            ("descriptiveVideoServiceFlag" Data..=)
              Prelude.<$> descriptiveVideoServiceFlag,
            ("iFrameOnlyManifest" Data..=)
              Prelude.<$> iFrameOnlyManifest,
            ("klvMetadata" Data..=) Prelude.<$> klvMetadata,
            ("manifestMetadataSignaling" Data..=)
              Prelude.<$> manifestMetadataSignaling,
            ("scte35Esam" Data..=) Prelude.<$> scte35Esam,
            ("scte35Source" Data..=) Prelude.<$> scte35Source,
            ("timedMetadata" Data..=) Prelude.<$> timedMetadata,
            ("timedMetadataBoxVersion" Data..=)
              Prelude.<$> timedMetadataBoxVersion,
            ("timedMetadataSchemeIdUri" Data..=)
              Prelude.<$> timedMetadataSchemeIdUri,
            ("timedMetadataValue" Data..=)
              Prelude.<$> timedMetadataValue
          ]
      )
