{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an MPEG Dynamic Adaptive Streaming over HTTP (DASH) URL for
-- the stream. You can then open the URL in a media player to view the
-- stream contents.
--
-- Both the @StreamName@ and the @StreamARN@ parameters are optional, but
-- you must specify either the @StreamName@ or the @StreamARN@ when
-- invoking this API operation.
--
-- An Amazon Kinesis video stream has the following requirements for
-- providing data through MPEG-DASH:
--
-- -   The media must contain h.264 or h.265 encoded video and, optionally,
--     AAC or G.711 encoded audio. Specifically, the codec ID of track 1
--     should be @V_MPEG\/ISO\/AVC@ (for h.264) or V_MPEGH\/ISO\/HEVC (for
--     H.265). Optionally, the codec ID of track 2 should be @A_AAC@ (for
--     AAC) or A_MS\/ACM (for G.711).
--
-- -   Data retention must be greater than 0.
--
-- -   The video track of each fragment must contain codec private data in
--     the Advanced Video Coding (AVC) for H.264 format and HEVC for H.265
--     format. For more information, see
--     <https://www.iso.org/standard/55980.html MPEG-4 specification ISO\/IEC 14496-15>.
--     For information about adapting stream data to a given format, see
--     <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags>.
--
-- -   The audio track (if present) of each fragment must contain codec
--     private data in the AAC format
--     (<https://www.iso.org/standard/43345.html AAC specification ISO\/IEC 13818-7>)
--     or the
--     <http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html MS Wave format>.
--
-- The following procedure shows how to use MPEG-DASH with Kinesis Video
-- Streams:
--
-- 1.  Get an endpoint using
--     <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_GetDataEndpoint.html GetDataEndpoint>,
--     specifying @GET_DASH_STREAMING_SESSION_URL@ for the @APIName@
--     parameter.
--
-- 2.  Retrieve the MPEG-DASH URL using @GetDASHStreamingSessionURL@.
--     Kinesis Video Streams creates an MPEG-DASH streaming session to be
--     used for accessing content in a stream using the MPEG-DASH protocol.
--     @GetDASHStreamingSessionURL@ returns an authenticated URL (that
--     includes an encrypted session token) for the session\'s MPEG-DASH
--     /manifest/ (the root resource needed for streaming with MPEG-DASH).
--
--     Don\'t share or store this token where an unauthorized entity can
--     access it. The token provides access to the content of the stream.
--     Safeguard the token with the same measures that you use with your
--     AWS credentials.
--
--     The media that is made available through the manifest consists only
--     of the requested stream, time range, and format. No other media data
--     (such as frames outside the requested window or alternate bitrates)
--     is made available.
--
-- 3.  Provide the URL (containing the encrypted session token) for the
--     MPEG-DASH manifest to a media player that supports the MPEG-DASH
--     protocol. Kinesis Video Streams makes the initialization fragment
--     and media fragments available through the manifest URL. The
--     initialization fragment contains the codec private data for the
--     stream, and other data needed to set up the video or audio decoder
--     and renderer. The media fragments contain encoded video frames or
--     encoded audio samples.
--
-- 4.  The media player receives the authenticated URL and requests stream
--     metadata and media data normally. When the media player requests
--     data, it calls the following actions:
--
--     -   __GetDASHManifest:__ Retrieves an MPEG DASH manifest, which
--         contains the metadata for the media that you want to playback.
--
--     -   __GetMP4InitFragment:__ Retrieves the MP4 initialization
--         fragment. The media player typically loads the initialization
--         fragment before loading any media fragments. This fragment
--         contains the \"@fytp@\" and \"@moov@\" MP4 atoms, and the child
--         atoms that are needed to initialize the media player decoder.
--
--         The initialization fragment does not correspond to a fragment in
--         a Kinesis video stream. It contains only the codec private data
--         for the stream and respective track, which the media player
--         needs to decode the media frames.
--
--     -   __GetMP4MediaFragment:__ Retrieves MP4 media fragments. These
--         fragments contain the \"@moof@\" and \"@mdat@\" MP4 atoms and
--         their child atoms, containing the encoded fragment\'s media
--         frames and their timestamps.
--
--         After the first media fragment is made available in a streaming
--         session, any fragments that don\'t contain the same codec
--         private data cause an error to be returned when those different
--         media fragments are loaded. Therefore, the codec private data
--         should not change between fragments in a session. This also
--         means that the session fails if the fragments in a stream change
--         from having only video to having both audio and video.
--
--         Data retrieved with this action is billable. See
--         <https://aws.amazon.com/kinesis/video-streams/pricing/ Pricing>
--         for details.
--
-- For restrictions that apply to MPEG-DASH sessions, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/limits.html Kinesis Video Streams Limits>.
--
-- You can monitor the amount of data that the media player consumes by
-- monitoring the @GetMP4MediaFragment.OutgoingBytes@ Amazon CloudWatch
-- metric. For information about using CloudWatch to monitor Kinesis Video
-- Streams, see
-- <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams>.
-- For pricing information, see
-- <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing>
-- and <https://aws.amazon.com/pricing/ AWS Pricing>. Charges for both HLS
-- sessions and outgoing AWS data apply.
--
-- For more information about HLS, see
-- <https://developer.apple.com/streaming/ HTTP Live Streaming> on the
-- <https://developer.apple.com Apple Developer site>.
--
-- If an error is thrown after invoking a Kinesis Video Streams archived
-- media API, in addition to the HTTP status code and the response body, it
-- includes the following pieces of information:
--
-- -   @x-amz-ErrorType@ HTTP header – contains a more specific error type
--     in addition to what the HTTP status code provides.
--
-- -   @x-amz-RequestId@ HTTP header – if you want to report an issue to
--     AWS, the support team can better diagnose the problem if given the
--     Request Id.
--
-- Both the HTTP status code and the ErrorType header can be utilized to
-- make programmatic decisions about whether errors are retry-able and
-- under what conditions, as well as provide information on what actions
-- the client programmer might need to take in order to successfully try
-- again.
--
-- For more information, see the __Errors__ section at the bottom of this
-- topic, as well as
-- <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors>.
module Amazonka.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
  ( -- * Creating a Request
    GetDASHStreamingSessionURL (..),
    newGetDASHStreamingSessionURL,

    -- * Request Lenses
    getDASHStreamingSessionURL_dASHFragmentSelector,
    getDASHStreamingSessionURL_maxManifestFragmentResults,
    getDASHStreamingSessionURL_displayFragmentTimestamp,
    getDASHStreamingSessionURL_displayFragmentNumber,
    getDASHStreamingSessionURL_expires,
    getDASHStreamingSessionURL_streamARN,
    getDASHStreamingSessionURL_streamName,
    getDASHStreamingSessionURL_playbackMode,

    -- * Destructuring the Response
    GetDASHStreamingSessionURLResponse (..),
    newGetDASHStreamingSessionURLResponse,

    -- * Response Lenses
    getDASHStreamingSessionURLResponse_dASHStreamingSessionURL,
    getDASHStreamingSessionURLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisVideoArchivedMedia.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDASHStreamingSessionURL' smart constructor.
data GetDASHStreamingSessionURL = GetDASHStreamingSessionURL'
  { -- | The time range of the requested fragment and the source of the
    -- timestamps.
    --
    -- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
    -- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
    -- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
    -- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
    -- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
    -- be set.
    dASHFragmentSelector :: Prelude.Maybe DASHFragmentSelector,
    -- | The maximum number of fragments that are returned in the MPEG-DASH
    -- manifest.
    --
    -- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
    -- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
    -- oldest fragments are returned, up to this maximum number.
    --
    -- When there are a higher number of fragments available in a live
    -- MPEG-DASH manifest, video players often buffer content before starting
    -- playback. Increasing the buffer size increases the playback latency, but
    -- it decreases the likelihood that rebuffering will occur during playback.
    -- We recommend that a live MPEG-DASH manifest have a minimum of 3
    -- fragments and a maximum of 10 fragments.
    --
    -- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
    -- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
    --
    -- The maximum value of 1,000 fragments corresponds to more than 16 minutes
    -- of video on streams with 1-second fragments, and more than 2 1\/2 hours
    -- of video on streams with 10-second fragments.
    maxManifestFragmentResults :: Prelude.Maybe Prelude.Natural,
    -- | Per the MPEG-DASH specification, the wall-clock time of fragments in the
    -- manifest file can be derived using attributes in the manifest itself.
    -- However, typically, MPEG-DASH compatible media players do not properly
    -- handle gaps in the media timeline. Kinesis Video Streams adjusts the
    -- media timeline in the manifest file to enable playback of media with
    -- discontinuities. Therefore, the wall-clock time derived from the
    -- manifest file may be inaccurate. If DisplayFragmentTimestamp is set to
    -- @ALWAYS@, the accurate fragment timestamp is added to each S element in
    -- the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH
    -- media player is necessary to leverage this custom attribute.
    --
    -- The default value is @NEVER@. When DASHFragmentSelector is
    -- @SERVER_TIMESTAMP@, the timestamps will be the server start timestamps.
    -- Similarly, when DASHFragmentSelector is @PRODUCER_TIMESTAMP@, the
    -- timestamps will be the producer start timestamps.
    displayFragmentTimestamp :: Prelude.Maybe DASHDisplayFragmentTimestamp,
    -- | Fragments are identified in the manifest file based on their sequence
    -- number in the session. If DisplayFragmentNumber is set to @ALWAYS@, the
    -- Kinesis Video Streams fragment number is added to each S element in the
    -- manifest file with the attribute name “kvs:fn”. These fragment numbers
    -- can be used for logging or for use with other APIs (e.g. @GetMedia@ and
    -- @GetMediaForFragmentList@). A custom MPEG-DASH media player is necessary
    -- to leverage these this custom attribute.
    --
    -- The default value is @NEVER@.
    displayFragmentNumber :: Prelude.Maybe DASHDisplayFragmentNumber,
    -- | The time in seconds until the requested session expires. This value can
    -- be between 300 (5 minutes) and 43200 (12 hours).
    --
    -- When a session expires, no new calls to @GetDashManifest@,
    -- @GetMP4InitFragment@, or @GetMP4MediaFragment@ can be made for that
    -- session.
    --
    -- The default is 300 (5 minutes).
    expires :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
    -- MPEG-DASH manifest URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream for which to retrieve the MPEG-DASH manifest URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text,
    -- | Whether to retrieve live, live replay, or archived, on-demand data.
    --
    -- Features of the three types of sessions include the following:
    --
    -- -   __@LIVE@__ : For sessions of this type, the MPEG-DASH manifest is
    --     continually updated with the latest fragments as they become
    --     available. We recommend that the media player retrieve a new
    --     manifest on a one-second interval. When this type of session is
    --     played in a media player, the user interface typically displays a
    --     \"live\" notification, with no scrubber control for choosing the
    --     position in the playback window to display.
    --
    --     In @LIVE@ mode, the newest available fragments are included in an
    --     MPEG-DASH manifest, even if there is a gap between fragments (that
    --     is, if a fragment is missing). A gap like this might cause a media
    --     player to halt or cause a jump in playback. In this mode, fragments
    --     are not added to the MPEG-DASH manifest if they are older than the
    --     newest fragment in the playlist. If the missing fragment becomes
    --     available after a subsequent fragment is added to the manifest, the
    --     older fragment is not added, and the gap is not filled.
    --
    -- -   __@LIVE_REPLAY@__ : For sessions of this type, the MPEG-DASH
    --     manifest is updated similarly to how it is updated for @LIVE@ mode
    --     except that it starts by including fragments from a given start
    --     time. Instead of fragments being added as they are ingested,
    --     fragments are added as the duration of the next fragment elapses.
    --     For example, if the fragments in the session are two seconds long,
    --     then a new fragment is added to the manifest every two seconds. This
    --     mode is useful to be able to start playback from when an event is
    --     detected and continue live streaming media that has not yet been
    --     ingested as of the time of the session creation. This mode is also
    --     useful to stream previously archived media without being limited by
    --     the 1,000 fragment limit in the @ON_DEMAND@ mode.
    --
    -- -   __@ON_DEMAND@__ : For sessions of this type, the MPEG-DASH manifest
    --     contains all the fragments for the session, up to the number that is
    --     specified in @MaxManifestFragmentResults@. The manifest must be
    --     retrieved only once for each session. When this type of session is
    --     played in a media player, the user interface typically displays a
    --     scrubber control for choosing the position in the playback window to
    --     display.
    --
    -- In all playback modes, if @FragmentSelectorType@ is
    -- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
    -- start timestamp, the fragment that has the larger fragment number (that
    -- is, the newer fragment) is included in the MPEG-DASH manifest. The other
    -- fragments are not included. Fragments that have different timestamps but
    -- have overlapping durations are still included in the MPEG-DASH manifest.
    -- This can lead to unexpected behavior in the media player.
    --
    -- The default is @LIVE@.
    playbackMode :: Prelude.Maybe DASHPlaybackMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDASHStreamingSessionURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dASHFragmentSelector', 'getDASHStreamingSessionURL_dASHFragmentSelector' - The time range of the requested fragment and the source of the
-- timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
-- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
-- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
-- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
-- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
-- be set.
--
-- 'maxManifestFragmentResults', 'getDASHStreamingSessionURL_maxManifestFragmentResults' - The maximum number of fragments that are returned in the MPEG-DASH
-- manifest.
--
-- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
-- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
-- oldest fragments are returned, up to this maximum number.
--
-- When there are a higher number of fragments available in a live
-- MPEG-DASH manifest, video players often buffer content before starting
-- playback. Increasing the buffer size increases the playback latency, but
-- it decreases the likelihood that rebuffering will occur during playback.
-- We recommend that a live MPEG-DASH manifest have a minimum of 3
-- fragments and a maximum of 10 fragments.
--
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
-- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
--
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes
-- of video on streams with 1-second fragments, and more than 2 1\/2 hours
-- of video on streams with 10-second fragments.
--
-- 'displayFragmentTimestamp', 'getDASHStreamingSessionURL_displayFragmentTimestamp' - Per the MPEG-DASH specification, the wall-clock time of fragments in the
-- manifest file can be derived using attributes in the manifest itself.
-- However, typically, MPEG-DASH compatible media players do not properly
-- handle gaps in the media timeline. Kinesis Video Streams adjusts the
-- media timeline in the manifest file to enable playback of media with
-- discontinuities. Therefore, the wall-clock time derived from the
-- manifest file may be inaccurate. If DisplayFragmentTimestamp is set to
-- @ALWAYS@, the accurate fragment timestamp is added to each S element in
-- the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH
-- media player is necessary to leverage this custom attribute.
--
-- The default value is @NEVER@. When DASHFragmentSelector is
-- @SERVER_TIMESTAMP@, the timestamps will be the server start timestamps.
-- Similarly, when DASHFragmentSelector is @PRODUCER_TIMESTAMP@, the
-- timestamps will be the producer start timestamps.
--
-- 'displayFragmentNumber', 'getDASHStreamingSessionURL_displayFragmentNumber' - Fragments are identified in the manifest file based on their sequence
-- number in the session. If DisplayFragmentNumber is set to @ALWAYS@, the
-- Kinesis Video Streams fragment number is added to each S element in the
-- manifest file with the attribute name “kvs:fn”. These fragment numbers
-- can be used for logging or for use with other APIs (e.g. @GetMedia@ and
-- @GetMediaForFragmentList@). A custom MPEG-DASH media player is necessary
-- to leverage these this custom attribute.
--
-- The default value is @NEVER@.
--
-- 'expires', 'getDASHStreamingSessionURL_expires' - The time in seconds until the requested session expires. This value can
-- be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetDashManifest@,
-- @GetMP4InitFragment@, or @GetMP4MediaFragment@ can be made for that
-- session.
--
-- The default is 300 (5 minutes).
--
-- 'streamARN', 'getDASHStreamingSessionURL_streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'getDASHStreamingSessionURL_streamName' - The name of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
--
-- 'playbackMode', 'getDASHStreamingSessionURL_playbackMode' - Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
-- -   __@LIVE@__ : For sessions of this type, the MPEG-DASH manifest is
--     continually updated with the latest fragments as they become
--     available. We recommend that the media player retrieve a new
--     manifest on a one-second interval. When this type of session is
--     played in a media player, the user interface typically displays a
--     \"live\" notification, with no scrubber control for choosing the
--     position in the playback window to display.
--
--     In @LIVE@ mode, the newest available fragments are included in an
--     MPEG-DASH manifest, even if there is a gap between fragments (that
--     is, if a fragment is missing). A gap like this might cause a media
--     player to halt or cause a jump in playback. In this mode, fragments
--     are not added to the MPEG-DASH manifest if they are older than the
--     newest fragment in the playlist. If the missing fragment becomes
--     available after a subsequent fragment is added to the manifest, the
--     older fragment is not added, and the gap is not filled.
--
-- -   __@LIVE_REPLAY@__ : For sessions of this type, the MPEG-DASH
--     manifest is updated similarly to how it is updated for @LIVE@ mode
--     except that it starts by including fragments from a given start
--     time. Instead of fragments being added as they are ingested,
--     fragments are added as the duration of the next fragment elapses.
--     For example, if the fragments in the session are two seconds long,
--     then a new fragment is added to the manifest every two seconds. This
--     mode is useful to be able to start playback from when an event is
--     detected and continue live streaming media that has not yet been
--     ingested as of the time of the session creation. This mode is also
--     useful to stream previously archived media without being limited by
--     the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
-- -   __@ON_DEMAND@__ : For sessions of this type, the MPEG-DASH manifest
--     contains all the fragments for the session, up to the number that is
--     specified in @MaxManifestFragmentResults@. The manifest must be
--     retrieved only once for each session. When this type of session is
--     played in a media player, the user interface typically displays a
--     scrubber control for choosing the position in the playback window to
--     display.
--
-- In all playback modes, if @FragmentSelectorType@ is
-- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
-- start timestamp, the fragment that has the larger fragment number (that
-- is, the newer fragment) is included in the MPEG-DASH manifest. The other
-- fragments are not included. Fragments that have different timestamps but
-- have overlapping durations are still included in the MPEG-DASH manifest.
-- This can lead to unexpected behavior in the media player.
--
-- The default is @LIVE@.
newGetDASHStreamingSessionURL ::
  GetDASHStreamingSessionURL
newGetDASHStreamingSessionURL =
  GetDASHStreamingSessionURL'
    { dASHFragmentSelector =
        Prelude.Nothing,
      maxManifestFragmentResults = Prelude.Nothing,
      displayFragmentTimestamp = Prelude.Nothing,
      displayFragmentNumber = Prelude.Nothing,
      expires = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing,
      playbackMode = Prelude.Nothing
    }

-- | The time range of the requested fragment and the source of the
-- timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
-- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
-- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
-- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
-- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
-- be set.
getDASHStreamingSessionURL_dASHFragmentSelector :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe DASHFragmentSelector)
getDASHStreamingSessionURL_dASHFragmentSelector = Lens.lens (\GetDASHStreamingSessionURL' {dASHFragmentSelector} -> dASHFragmentSelector) (\s@GetDASHStreamingSessionURL' {} a -> s {dASHFragmentSelector = a} :: GetDASHStreamingSessionURL)

-- | The maximum number of fragments that are returned in the MPEG-DASH
-- manifest.
--
-- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
-- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
-- oldest fragments are returned, up to this maximum number.
--
-- When there are a higher number of fragments available in a live
-- MPEG-DASH manifest, video players often buffer content before starting
-- playback. Increasing the buffer size increases the playback latency, but
-- it decreases the likelihood that rebuffering will occur during playback.
-- We recommend that a live MPEG-DASH manifest have a minimum of 3
-- fragments and a maximum of 10 fragments.
--
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
-- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
--
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes
-- of video on streams with 1-second fragments, and more than 2 1\/2 hours
-- of video on streams with 10-second fragments.
getDASHStreamingSessionURL_maxManifestFragmentResults :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe Prelude.Natural)
getDASHStreamingSessionURL_maxManifestFragmentResults = Lens.lens (\GetDASHStreamingSessionURL' {maxManifestFragmentResults} -> maxManifestFragmentResults) (\s@GetDASHStreamingSessionURL' {} a -> s {maxManifestFragmentResults = a} :: GetDASHStreamingSessionURL)

-- | Per the MPEG-DASH specification, the wall-clock time of fragments in the
-- manifest file can be derived using attributes in the manifest itself.
-- However, typically, MPEG-DASH compatible media players do not properly
-- handle gaps in the media timeline. Kinesis Video Streams adjusts the
-- media timeline in the manifest file to enable playback of media with
-- discontinuities. Therefore, the wall-clock time derived from the
-- manifest file may be inaccurate. If DisplayFragmentTimestamp is set to
-- @ALWAYS@, the accurate fragment timestamp is added to each S element in
-- the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH
-- media player is necessary to leverage this custom attribute.
--
-- The default value is @NEVER@. When DASHFragmentSelector is
-- @SERVER_TIMESTAMP@, the timestamps will be the server start timestamps.
-- Similarly, when DASHFragmentSelector is @PRODUCER_TIMESTAMP@, the
-- timestamps will be the producer start timestamps.
getDASHStreamingSessionURL_displayFragmentTimestamp :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe DASHDisplayFragmentTimestamp)
getDASHStreamingSessionURL_displayFragmentTimestamp = Lens.lens (\GetDASHStreamingSessionURL' {displayFragmentTimestamp} -> displayFragmentTimestamp) (\s@GetDASHStreamingSessionURL' {} a -> s {displayFragmentTimestamp = a} :: GetDASHStreamingSessionURL)

-- | Fragments are identified in the manifest file based on their sequence
-- number in the session. If DisplayFragmentNumber is set to @ALWAYS@, the
-- Kinesis Video Streams fragment number is added to each S element in the
-- manifest file with the attribute name “kvs:fn”. These fragment numbers
-- can be used for logging or for use with other APIs (e.g. @GetMedia@ and
-- @GetMediaForFragmentList@). A custom MPEG-DASH media player is necessary
-- to leverage these this custom attribute.
--
-- The default value is @NEVER@.
getDASHStreamingSessionURL_displayFragmentNumber :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe DASHDisplayFragmentNumber)
getDASHStreamingSessionURL_displayFragmentNumber = Lens.lens (\GetDASHStreamingSessionURL' {displayFragmentNumber} -> displayFragmentNumber) (\s@GetDASHStreamingSessionURL' {} a -> s {displayFragmentNumber = a} :: GetDASHStreamingSessionURL)

-- | The time in seconds until the requested session expires. This value can
-- be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetDashManifest@,
-- @GetMP4InitFragment@, or @GetMP4MediaFragment@ can be made for that
-- session.
--
-- The default is 300 (5 minutes).
getDASHStreamingSessionURL_expires :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe Prelude.Natural)
getDASHStreamingSessionURL_expires = Lens.lens (\GetDASHStreamingSessionURL' {expires} -> expires) (\s@GetDASHStreamingSessionURL' {} a -> s {expires = a} :: GetDASHStreamingSessionURL)

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
getDASHStreamingSessionURL_streamARN :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe Prelude.Text)
getDASHStreamingSessionURL_streamARN = Lens.lens (\GetDASHStreamingSessionURL' {streamARN} -> streamARN) (\s@GetDASHStreamingSessionURL' {} a -> s {streamARN = a} :: GetDASHStreamingSessionURL)

-- | The name of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
getDASHStreamingSessionURL_streamName :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe Prelude.Text)
getDASHStreamingSessionURL_streamName = Lens.lens (\GetDASHStreamingSessionURL' {streamName} -> streamName) (\s@GetDASHStreamingSessionURL' {} a -> s {streamName = a} :: GetDASHStreamingSessionURL)

-- | Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
-- -   __@LIVE@__ : For sessions of this type, the MPEG-DASH manifest is
--     continually updated with the latest fragments as they become
--     available. We recommend that the media player retrieve a new
--     manifest on a one-second interval. When this type of session is
--     played in a media player, the user interface typically displays a
--     \"live\" notification, with no scrubber control for choosing the
--     position in the playback window to display.
--
--     In @LIVE@ mode, the newest available fragments are included in an
--     MPEG-DASH manifest, even if there is a gap between fragments (that
--     is, if a fragment is missing). A gap like this might cause a media
--     player to halt or cause a jump in playback. In this mode, fragments
--     are not added to the MPEG-DASH manifest if they are older than the
--     newest fragment in the playlist. If the missing fragment becomes
--     available after a subsequent fragment is added to the manifest, the
--     older fragment is not added, and the gap is not filled.
--
-- -   __@LIVE_REPLAY@__ : For sessions of this type, the MPEG-DASH
--     manifest is updated similarly to how it is updated for @LIVE@ mode
--     except that it starts by including fragments from a given start
--     time. Instead of fragments being added as they are ingested,
--     fragments are added as the duration of the next fragment elapses.
--     For example, if the fragments in the session are two seconds long,
--     then a new fragment is added to the manifest every two seconds. This
--     mode is useful to be able to start playback from when an event is
--     detected and continue live streaming media that has not yet been
--     ingested as of the time of the session creation. This mode is also
--     useful to stream previously archived media without being limited by
--     the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
-- -   __@ON_DEMAND@__ : For sessions of this type, the MPEG-DASH manifest
--     contains all the fragments for the session, up to the number that is
--     specified in @MaxManifestFragmentResults@. The manifest must be
--     retrieved only once for each session. When this type of session is
--     played in a media player, the user interface typically displays a
--     scrubber control for choosing the position in the playback window to
--     display.
--
-- In all playback modes, if @FragmentSelectorType@ is
-- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
-- start timestamp, the fragment that has the larger fragment number (that
-- is, the newer fragment) is included in the MPEG-DASH manifest. The other
-- fragments are not included. Fragments that have different timestamps but
-- have overlapping durations are still included in the MPEG-DASH manifest.
-- This can lead to unexpected behavior in the media player.
--
-- The default is @LIVE@.
getDASHStreamingSessionURL_playbackMode :: Lens.Lens' GetDASHStreamingSessionURL (Prelude.Maybe DASHPlaybackMode)
getDASHStreamingSessionURL_playbackMode = Lens.lens (\GetDASHStreamingSessionURL' {playbackMode} -> playbackMode) (\s@GetDASHStreamingSessionURL' {} a -> s {playbackMode = a} :: GetDASHStreamingSessionURL)

instance Core.AWSRequest GetDASHStreamingSessionURL where
  type
    AWSResponse GetDASHStreamingSessionURL =
      GetDASHStreamingSessionURLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDASHStreamingSessionURLResponse'
            Prelude.<$> (x Core..?> "DASHStreamingSessionURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDASHStreamingSessionURL where
  hashWithSalt _salt GetDASHStreamingSessionURL' {..} =
    _salt `Prelude.hashWithSalt` dASHFragmentSelector
      `Prelude.hashWithSalt` maxManifestFragmentResults
      `Prelude.hashWithSalt` displayFragmentTimestamp
      `Prelude.hashWithSalt` displayFragmentNumber
      `Prelude.hashWithSalt` expires
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName
      `Prelude.hashWithSalt` playbackMode

instance Prelude.NFData GetDASHStreamingSessionURL where
  rnf GetDASHStreamingSessionURL' {..} =
    Prelude.rnf dASHFragmentSelector
      `Prelude.seq` Prelude.rnf maxManifestFragmentResults
      `Prelude.seq` Prelude.rnf displayFragmentTimestamp
      `Prelude.seq` Prelude.rnf displayFragmentNumber
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName
      `Prelude.seq` Prelude.rnf playbackMode

instance Core.ToHeaders GetDASHStreamingSessionURL where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetDASHStreamingSessionURL where
  toJSON GetDASHStreamingSessionURL' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DASHFragmentSelector" Core..=)
              Prelude.<$> dASHFragmentSelector,
            ("MaxManifestFragmentResults" Core..=)
              Prelude.<$> maxManifestFragmentResults,
            ("DisplayFragmentTimestamp" Core..=)
              Prelude.<$> displayFragmentTimestamp,
            ("DisplayFragmentNumber" Core..=)
              Prelude.<$> displayFragmentNumber,
            ("Expires" Core..=) Prelude.<$> expires,
            ("StreamARN" Core..=) Prelude.<$> streamARN,
            ("StreamName" Core..=) Prelude.<$> streamName,
            ("PlaybackMode" Core..=) Prelude.<$> playbackMode
          ]
      )

instance Core.ToPath GetDASHStreamingSessionURL where
  toPath = Prelude.const "/getDASHStreamingSessionURL"

instance Core.ToQuery GetDASHStreamingSessionURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDASHStreamingSessionURLResponse' smart constructor.
data GetDASHStreamingSessionURLResponse = GetDASHStreamingSessionURLResponse'
  { -- | The URL (containing the session token) that a media player can use to
    -- retrieve the MPEG-DASH manifest.
    dASHStreamingSessionURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDASHStreamingSessionURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dASHStreamingSessionURL', 'getDASHStreamingSessionURLResponse_dASHStreamingSessionURL' - The URL (containing the session token) that a media player can use to
-- retrieve the MPEG-DASH manifest.
--
-- 'httpStatus', 'getDASHStreamingSessionURLResponse_httpStatus' - The response's http status code.
newGetDASHStreamingSessionURLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDASHStreamingSessionURLResponse
newGetDASHStreamingSessionURLResponse pHttpStatus_ =
  GetDASHStreamingSessionURLResponse'
    { dASHStreamingSessionURL =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL (containing the session token) that a media player can use to
-- retrieve the MPEG-DASH manifest.
getDASHStreamingSessionURLResponse_dASHStreamingSessionURL :: Lens.Lens' GetDASHStreamingSessionURLResponse (Prelude.Maybe Prelude.Text)
getDASHStreamingSessionURLResponse_dASHStreamingSessionURL = Lens.lens (\GetDASHStreamingSessionURLResponse' {dASHStreamingSessionURL} -> dASHStreamingSessionURL) (\s@GetDASHStreamingSessionURLResponse' {} a -> s {dASHStreamingSessionURL = a} :: GetDASHStreamingSessionURLResponse)

-- | The response's http status code.
getDASHStreamingSessionURLResponse_httpStatus :: Lens.Lens' GetDASHStreamingSessionURLResponse Prelude.Int
getDASHStreamingSessionURLResponse_httpStatus = Lens.lens (\GetDASHStreamingSessionURLResponse' {httpStatus} -> httpStatus) (\s@GetDASHStreamingSessionURLResponse' {} a -> s {httpStatus = a} :: GetDASHStreamingSessionURLResponse)

instance
  Prelude.NFData
    GetDASHStreamingSessionURLResponse
  where
  rnf GetDASHStreamingSessionURLResponse' {..} =
    Prelude.rnf dASHStreamingSessionURL
      `Prelude.seq` Prelude.rnf httpStatus
