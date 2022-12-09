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
-- Module      : Amazonka.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an HTTP Live Streaming (HLS) URL for the stream. You can then
-- open the URL in a browser or media player to view the stream contents.
--
-- Both the @StreamName@ and the @StreamARN@ parameters are optional, but
-- you must specify either the @StreamName@ or the @StreamARN@ when
-- invoking this API operation.
--
-- An Amazon Kinesis video stream has the following requirements for
-- providing data through HLS:
--
-- -   The media must contain h.264 or h.265 encoded video and, optionally,
--     AAC encoded audio. Specifically, the codec ID of track 1 should be
--     @V_MPEG\/ISO\/AVC@ (for h.264) or @V_MPEG\/ISO\/HEVC@ (for h.265).
--     Optionally, the codec ID of track 2 should be @A_AAC@.
--
-- -   Data retention must be greater than 0.
--
-- -   The video track of each fragment must contain codec private data in
--     the Advanced Video Coding (AVC) for H.264 format or HEVC for H.265
--     format
--     (<https://www.iso.org/standard/55980.html MPEG-4 specification ISO\/IEC 14496-15>).
--     For information about adapting stream data to a given format, see
--     <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags>.
--
-- -   The audio track (if present) of each fragment must contain codec
--     private data in the AAC format
--     (<https://www.iso.org/standard/43345.html AAC specification ISO\/IEC 13818-7>).
--
-- Kinesis Video Streams HLS sessions contain fragments in the fragmented
-- MPEG-4 form (also called fMP4 or CMAF) or the MPEG-2 form (also called
-- TS chunks, which the HLS specification also supports). For more
-- information about HLS fragment types, see the
-- <https://tools.ietf.org/html/draft-pantos-http-live-streaming-23 HLS specification>.
--
-- The following procedure shows how to use HLS with Kinesis Video Streams:
--
-- 1.  Get an endpoint using
--     <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_GetDataEndpoint.html GetDataEndpoint>,
--     specifying @GET_HLS_STREAMING_SESSION_URL@ for the @APIName@
--     parameter.
--
-- 2.  Retrieve the HLS URL using @GetHLSStreamingSessionURL@. Kinesis
--     Video Streams creates an HLS streaming session to be used for
--     accessing content in a stream using the HLS protocol.
--     @GetHLSStreamingSessionURL@ returns an authenticated URL (that
--     includes an encrypted session token) for the session\'s HLS /master
--     playlist/ (the root resource needed for streaming with HLS).
--
--     Don\'t share or store this token where an unauthorized entity could
--     access it. The token provides access to the content of the stream.
--     Safeguard the token with the same measures that you would use with
--     your AWS credentials.
--
--     The media that is made available through the playlist consists only
--     of the requested stream, time range, and format. No other media data
--     (such as frames outside the requested window or alternate bitrates)
--     is made available.
--
-- 3.  Provide the URL (containing the encrypted session token) for the HLS
--     master playlist to a media player that supports the HLS protocol.
--     Kinesis Video Streams makes the HLS media playlist, initialization
--     fragment, and media fragments available through the master playlist
--     URL. The initialization fragment contains the codec private data for
--     the stream, and other data needed to set up the video or audio
--     decoder and renderer. The media fragments contain H.264-encoded
--     video frames or AAC-encoded audio samples.
--
-- 4.  The media player receives the authenticated URL and requests stream
--     metadata and media data normally. When the media player requests
--     data, it calls the following actions:
--
--     -   __GetHLSMasterPlaylist:__ Retrieves an HLS master playlist,
--         which contains a URL for the @GetHLSMediaPlaylist@ action for
--         each track, and additional metadata for the media player,
--         including estimated bitrate and resolution.
--
--     -   __GetHLSMediaPlaylist:__ Retrieves an HLS media playlist, which
--         contains a URL to access the MP4 initialization fragment with
--         the @GetMP4InitFragment@ action, and URLs to access the MP4
--         media fragments with the @GetMP4MediaFragment@ actions. The HLS
--         media playlist also contains metadata about the stream that the
--         player needs to play it, such as whether the @PlaybackMode@ is
--         @LIVE@ or @ON_DEMAND@. The HLS media playlist is typically
--         static for sessions with a @PlaybackType@ of @ON_DEMAND@. The
--         HLS media playlist is continually updated with new fragments for
--         sessions with a @PlaybackType@ of @LIVE@. There is a distinct
--         HLS media playlist for the video track and the audio track (if
--         applicable) that contains MP4 media URLs for the specific track.
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
--     -   __GetTSFragment:__ Retrieves MPEG TS fragments containing both
--         initialization and media data for all tracks in the stream.
--
--         If the @ContainerFormat@ is @MPEG_TS@, this API is used instead
--         of @GetMP4InitFragment@ and @GetMP4MediaFragment@ to retrieve
--         stream media.
--
--         Data retrieved with this action is billable. For more
--         information, see
--         <https://aws.amazon.com/kinesis/video-streams/pricing/ Kinesis Video Streams pricing>.
--
-- A streaming session URL must not be shared between players. The service
-- might throttle a session if multiple media players are sharing it. For
-- connection limits, see
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
module Amazonka.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
  ( -- * Creating a Request
    GetHLSStreamingSessionURL (..),
    newGetHLSStreamingSessionURL,

    -- * Request Lenses
    getHLSStreamingSessionURL_containerFormat,
    getHLSStreamingSessionURL_discontinuityMode,
    getHLSStreamingSessionURL_displayFragmentTimestamp,
    getHLSStreamingSessionURL_expires,
    getHLSStreamingSessionURL_hLSFragmentSelector,
    getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults,
    getHLSStreamingSessionURL_playbackMode,
    getHLSStreamingSessionURL_streamARN,
    getHLSStreamingSessionURL_streamName,

    -- * Destructuring the Response
    GetHLSStreamingSessionURLResponse (..),
    newGetHLSStreamingSessionURLResponse,

    -- * Response Lenses
    getHLSStreamingSessionURLResponse_hLSStreamingSessionURL,
    getHLSStreamingSessionURLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoArchivedMedia.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHLSStreamingSessionURL' smart constructor.
data GetHLSStreamingSessionURL = GetHLSStreamingSessionURL'
  { -- | Specifies which format should be used for packaging the media.
    -- Specifying the @FRAGMENTED_MP4@ container format packages the media into
    -- MP4 fragments (fMP4 or CMAF). This is the recommended packaging because
    -- there is minimal packaging overhead. The other container format option
    -- is @MPEG_TS@. HLS has supported MPEG TS chunks since it was released and
    -- is sometimes the only supported packaging on older HLS players. MPEG TS
    -- typically has a 5-25 percent packaging overhead. This means MPEG TS
    -- typically requires 5-25 percent more bandwidth and cost than fMP4.
    --
    -- The default is @FRAGMENTED_MP4@.
    containerFormat :: Prelude.Maybe ContainerFormat,
    -- | Specifies when flags marking discontinuities between fragments are added
    -- to the media playlists.
    --
    -- Media players typically build a timeline of media content to play, based
    -- on the timestamps of each fragment. This means that if there is any
    -- overlap or gap between fragments (as is typical if HLSFragmentSelector
    -- is set to @SERVER_TIMESTAMP@), the media player timeline will also have
    -- small gaps between fragments in some places, and will overwrite frames
    -- in other places. Gaps in the media player timeline can cause playback to
    -- stall and overlaps can cause playback to be jittery. When there are
    -- discontinuity flags between fragments, the media player is expected to
    -- reset the timeline, resulting in the next fragment being played
    -- immediately after the previous fragment.
    --
    -- The following modes are supported:
    --
    -- -   @ALWAYS@: a discontinuity marker is placed between every fragment in
    --     the HLS media playlist. It is recommended to use a value of @ALWAYS@
    --     if the fragment timestamps are not accurate.
    --
    -- -   @NEVER@: no discontinuity markers are placed anywhere. It is
    --     recommended to use a value of @NEVER@ to ensure the media player
    --     timeline most accurately maps to the producer timestamps.
    --
    -- -   @ON_DISCONTINUITY@: a discontinuity marker is placed between
    --     fragments that have a gap or overlap of more than 50 milliseconds.
    --     For most playback scenarios, it is recommended to use a value of
    --     @ON_DISCONTINUITY@ so that the media player timeline is only reset
    --     when there is a significant issue with the media timeline (e.g. a
    --     missing fragment).
    --
    -- The default is @ALWAYS@ when HLSFragmentSelector is set to
    -- @SERVER_TIMESTAMP@, and @NEVER@ when it is set to @PRODUCER_TIMESTAMP@.
    discontinuityMode :: Prelude.Maybe HLSDiscontinuityMode,
    -- | Specifies when the fragment start timestamps should be included in the
    -- HLS media playlist. Typically, media players report the playhead
    -- position as a time relative to the start of the first fragment in the
    -- playback session. However, when the start timestamps are included in the
    -- HLS media playlist, some media players might report the current playhead
    -- as an absolute time based on the fragment timestamps. This can be useful
    -- for creating a playback experience that shows viewers the wall-clock
    -- time of the media.
    --
    -- The default is @NEVER@. When HLSFragmentSelector is @SERVER_TIMESTAMP@,
    -- the timestamps will be the server start timestamps. Similarly, when
    -- HLSFragmentSelector is @PRODUCER_TIMESTAMP@, the timestamps will be the
    -- producer start timestamps.
    displayFragmentTimestamp :: Prelude.Maybe HLSDisplayFragmentTimestamp,
    -- | The time in seconds until the requested session expires. This value can
    -- be between 300 (5 minutes) and 43200 (12 hours).
    --
    -- When a session expires, no new calls to @GetHLSMasterPlaylist@,
    -- @GetHLSMediaPlaylist@, @GetMP4InitFragment@, @GetMP4MediaFragment@, or
    -- @GetTSFragment@ can be made for that session.
    --
    -- The default is 300 (5 minutes).
    expires :: Prelude.Maybe Prelude.Natural,
    -- | The time range of the requested fragment and the source of the
    -- timestamps.
    --
    -- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
    -- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
    -- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
    -- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
    -- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
    -- be set.
    hLSFragmentSelector :: Prelude.Maybe HLSFragmentSelector,
    -- | The maximum number of fragments that are returned in the HLS media
    -- playlists.
    --
    -- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
    -- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
    -- oldest fragments are returned, up to this maximum number.
    --
    -- When there are a higher number of fragments available in a live HLS
    -- media playlist, video players often buffer content before starting
    -- playback. Increasing the buffer size increases the playback latency, but
    -- it decreases the likelihood that rebuffering will occur during playback.
    -- We recommend that a live HLS media playlist have a minimum of 3
    -- fragments and a maximum of 10 fragments.
    --
    -- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
    -- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
    --
    -- The maximum value of 5,000 fragments corresponds to more than 80 minutes
    -- of video on streams with 1-second fragments, and more than 13 hours of
    -- video on streams with 10-second fragments.
    maxMediaPlaylistFragmentResults :: Prelude.Maybe Prelude.Natural,
    -- | Whether to retrieve live, live replay, or archived, on-demand data.
    --
    -- Features of the three types of sessions include the following:
    --
    -- -   __@LIVE@__ : For sessions of this type, the HLS media playlist is
    --     continually updated with the latest fragments as they become
    --     available. We recommend that the media player retrieve a new
    --     playlist on a one-second interval. When this type of session is
    --     played in a media player, the user interface typically displays a
    --     \"live\" notification, with no scrubber control for choosing the
    --     position in the playback window to display.
    --
    --     In @LIVE@ mode, the newest available fragments are included in an
    --     HLS media playlist, even if there is a gap between fragments (that
    --     is, if a fragment is missing). A gap like this might cause a media
    --     player to halt or cause a jump in playback. In this mode, fragments
    --     are not added to the HLS media playlist if they are older than the
    --     newest fragment in the playlist. If the missing fragment becomes
    --     available after a subsequent fragment is added to the playlist, the
    --     older fragment is not added, and the gap is not filled.
    --
    -- -   __@LIVE_REPLAY@__ : For sessions of this type, the HLS media
    --     playlist is updated similarly to how it is updated for @LIVE@ mode
    --     except that it starts by including fragments from a given start
    --     time. Instead of fragments being added as they are ingested,
    --     fragments are added as the duration of the next fragment elapses.
    --     For example, if the fragments in the session are two seconds long,
    --     then a new fragment is added to the media playlist every two
    --     seconds. This mode is useful to be able to start playback from when
    --     an event is detected and continue live streaming media that has not
    --     yet been ingested as of the time of the session creation. This mode
    --     is also useful to stream previously archived media without being
    --     limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
    --
    -- -   __@ON_DEMAND@__ : For sessions of this type, the HLS media playlist
    --     contains all the fragments for the session, up to the number that is
    --     specified in @MaxMediaPlaylistFragmentResults@. The playlist must be
    --     retrieved only once for each session. When this type of session is
    --     played in a media player, the user interface typically displays a
    --     scrubber control for choosing the position in the playback window to
    --     display.
    --
    -- In all playback modes, if @FragmentSelectorType@ is
    -- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
    -- start timestamp, the fragment that has the largest fragment number (that
    -- is, the newest fragment) is included in the HLS media playlist. The
    -- other fragments are not included. Fragments that have different
    -- timestamps but have overlapping durations are still included in the HLS
    -- media playlist. This can lead to unexpected behavior in the media
    -- player.
    --
    -- The default is @LIVE@.
    playbackMode :: Prelude.Maybe HLSPlaybackMode,
    -- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
    -- HLS master playlist URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@.
    streamARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the stream for which to retrieve the HLS master playlist
    -- URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@.
    streamName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHLSStreamingSessionURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerFormat', 'getHLSStreamingSessionURL_containerFormat' - Specifies which format should be used for packaging the media.
-- Specifying the @FRAGMENTED_MP4@ container format packages the media into
-- MP4 fragments (fMP4 or CMAF). This is the recommended packaging because
-- there is minimal packaging overhead. The other container format option
-- is @MPEG_TS@. HLS has supported MPEG TS chunks since it was released and
-- is sometimes the only supported packaging on older HLS players. MPEG TS
-- typically has a 5-25 percent packaging overhead. This means MPEG TS
-- typically requires 5-25 percent more bandwidth and cost than fMP4.
--
-- The default is @FRAGMENTED_MP4@.
--
-- 'discontinuityMode', 'getHLSStreamingSessionURL_discontinuityMode' - Specifies when flags marking discontinuities between fragments are added
-- to the media playlists.
--
-- Media players typically build a timeline of media content to play, based
-- on the timestamps of each fragment. This means that if there is any
-- overlap or gap between fragments (as is typical if HLSFragmentSelector
-- is set to @SERVER_TIMESTAMP@), the media player timeline will also have
-- small gaps between fragments in some places, and will overwrite frames
-- in other places. Gaps in the media player timeline can cause playback to
-- stall and overlaps can cause playback to be jittery. When there are
-- discontinuity flags between fragments, the media player is expected to
-- reset the timeline, resulting in the next fragment being played
-- immediately after the previous fragment.
--
-- The following modes are supported:
--
-- -   @ALWAYS@: a discontinuity marker is placed between every fragment in
--     the HLS media playlist. It is recommended to use a value of @ALWAYS@
--     if the fragment timestamps are not accurate.
--
-- -   @NEVER@: no discontinuity markers are placed anywhere. It is
--     recommended to use a value of @NEVER@ to ensure the media player
--     timeline most accurately maps to the producer timestamps.
--
-- -   @ON_DISCONTINUITY@: a discontinuity marker is placed between
--     fragments that have a gap or overlap of more than 50 milliseconds.
--     For most playback scenarios, it is recommended to use a value of
--     @ON_DISCONTINUITY@ so that the media player timeline is only reset
--     when there is a significant issue with the media timeline (e.g. a
--     missing fragment).
--
-- The default is @ALWAYS@ when HLSFragmentSelector is set to
-- @SERVER_TIMESTAMP@, and @NEVER@ when it is set to @PRODUCER_TIMESTAMP@.
--
-- 'displayFragmentTimestamp', 'getHLSStreamingSessionURL_displayFragmentTimestamp' - Specifies when the fragment start timestamps should be included in the
-- HLS media playlist. Typically, media players report the playhead
-- position as a time relative to the start of the first fragment in the
-- playback session. However, when the start timestamps are included in the
-- HLS media playlist, some media players might report the current playhead
-- as an absolute time based on the fragment timestamps. This can be useful
-- for creating a playback experience that shows viewers the wall-clock
-- time of the media.
--
-- The default is @NEVER@. When HLSFragmentSelector is @SERVER_TIMESTAMP@,
-- the timestamps will be the server start timestamps. Similarly, when
-- HLSFragmentSelector is @PRODUCER_TIMESTAMP@, the timestamps will be the
-- producer start timestamps.
--
-- 'expires', 'getHLSStreamingSessionURL_expires' - The time in seconds until the requested session expires. This value can
-- be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetHLSMasterPlaylist@,
-- @GetHLSMediaPlaylist@, @GetMP4InitFragment@, @GetMP4MediaFragment@, or
-- @GetTSFragment@ can be made for that session.
--
-- The default is 300 (5 minutes).
--
-- 'hLSFragmentSelector', 'getHLSStreamingSessionURL_hLSFragmentSelector' - The time range of the requested fragment and the source of the
-- timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
-- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
-- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
-- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
-- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
-- be set.
--
-- 'maxMediaPlaylistFragmentResults', 'getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults' - The maximum number of fragments that are returned in the HLS media
-- playlists.
--
-- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
-- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
-- oldest fragments are returned, up to this maximum number.
--
-- When there are a higher number of fragments available in a live HLS
-- media playlist, video players often buffer content before starting
-- playback. Increasing the buffer size increases the playback latency, but
-- it decreases the likelihood that rebuffering will occur during playback.
-- We recommend that a live HLS media playlist have a minimum of 3
-- fragments and a maximum of 10 fragments.
--
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
-- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
--
-- The maximum value of 5,000 fragments corresponds to more than 80 minutes
-- of video on streams with 1-second fragments, and more than 13 hours of
-- video on streams with 10-second fragments.
--
-- 'playbackMode', 'getHLSStreamingSessionURL_playbackMode' - Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
-- -   __@LIVE@__ : For sessions of this type, the HLS media playlist is
--     continually updated with the latest fragments as they become
--     available. We recommend that the media player retrieve a new
--     playlist on a one-second interval. When this type of session is
--     played in a media player, the user interface typically displays a
--     \"live\" notification, with no scrubber control for choosing the
--     position in the playback window to display.
--
--     In @LIVE@ mode, the newest available fragments are included in an
--     HLS media playlist, even if there is a gap between fragments (that
--     is, if a fragment is missing). A gap like this might cause a media
--     player to halt or cause a jump in playback. In this mode, fragments
--     are not added to the HLS media playlist if they are older than the
--     newest fragment in the playlist. If the missing fragment becomes
--     available after a subsequent fragment is added to the playlist, the
--     older fragment is not added, and the gap is not filled.
--
-- -   __@LIVE_REPLAY@__ : For sessions of this type, the HLS media
--     playlist is updated similarly to how it is updated for @LIVE@ mode
--     except that it starts by including fragments from a given start
--     time. Instead of fragments being added as they are ingested,
--     fragments are added as the duration of the next fragment elapses.
--     For example, if the fragments in the session are two seconds long,
--     then a new fragment is added to the media playlist every two
--     seconds. This mode is useful to be able to start playback from when
--     an event is detected and continue live streaming media that has not
--     yet been ingested as of the time of the session creation. This mode
--     is also useful to stream previously archived media without being
--     limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
-- -   __@ON_DEMAND@__ : For sessions of this type, the HLS media playlist
--     contains all the fragments for the session, up to the number that is
--     specified in @MaxMediaPlaylistFragmentResults@. The playlist must be
--     retrieved only once for each session. When this type of session is
--     played in a media player, the user interface typically displays a
--     scrubber control for choosing the position in the playback window to
--     display.
--
-- In all playback modes, if @FragmentSelectorType@ is
-- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
-- start timestamp, the fragment that has the largest fragment number (that
-- is, the newest fragment) is included in the HLS media playlist. The
-- other fragments are not included. Fragments that have different
-- timestamps but have overlapping durations are still included in the HLS
-- media playlist. This can lead to unexpected behavior in the media
-- player.
--
-- The default is @LIVE@.
--
-- 'streamARN', 'getHLSStreamingSessionURL_streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
--
-- 'streamName', 'getHLSStreamingSessionURL_streamName' - The name of the stream for which to retrieve the HLS master playlist
-- URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
newGetHLSStreamingSessionURL ::
  GetHLSStreamingSessionURL
newGetHLSStreamingSessionURL =
  GetHLSStreamingSessionURL'
    { containerFormat =
        Prelude.Nothing,
      discontinuityMode = Prelude.Nothing,
      displayFragmentTimestamp = Prelude.Nothing,
      expires = Prelude.Nothing,
      hLSFragmentSelector = Prelude.Nothing,
      maxMediaPlaylistFragmentResults =
        Prelude.Nothing,
      playbackMode = Prelude.Nothing,
      streamARN = Prelude.Nothing,
      streamName = Prelude.Nothing
    }

-- | Specifies which format should be used for packaging the media.
-- Specifying the @FRAGMENTED_MP4@ container format packages the media into
-- MP4 fragments (fMP4 or CMAF). This is the recommended packaging because
-- there is minimal packaging overhead. The other container format option
-- is @MPEG_TS@. HLS has supported MPEG TS chunks since it was released and
-- is sometimes the only supported packaging on older HLS players. MPEG TS
-- typically has a 5-25 percent packaging overhead. This means MPEG TS
-- typically requires 5-25 percent more bandwidth and cost than fMP4.
--
-- The default is @FRAGMENTED_MP4@.
getHLSStreamingSessionURL_containerFormat :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe ContainerFormat)
getHLSStreamingSessionURL_containerFormat = Lens.lens (\GetHLSStreamingSessionURL' {containerFormat} -> containerFormat) (\s@GetHLSStreamingSessionURL' {} a -> s {containerFormat = a} :: GetHLSStreamingSessionURL)

-- | Specifies when flags marking discontinuities between fragments are added
-- to the media playlists.
--
-- Media players typically build a timeline of media content to play, based
-- on the timestamps of each fragment. This means that if there is any
-- overlap or gap between fragments (as is typical if HLSFragmentSelector
-- is set to @SERVER_TIMESTAMP@), the media player timeline will also have
-- small gaps between fragments in some places, and will overwrite frames
-- in other places. Gaps in the media player timeline can cause playback to
-- stall and overlaps can cause playback to be jittery. When there are
-- discontinuity flags between fragments, the media player is expected to
-- reset the timeline, resulting in the next fragment being played
-- immediately after the previous fragment.
--
-- The following modes are supported:
--
-- -   @ALWAYS@: a discontinuity marker is placed between every fragment in
--     the HLS media playlist. It is recommended to use a value of @ALWAYS@
--     if the fragment timestamps are not accurate.
--
-- -   @NEVER@: no discontinuity markers are placed anywhere. It is
--     recommended to use a value of @NEVER@ to ensure the media player
--     timeline most accurately maps to the producer timestamps.
--
-- -   @ON_DISCONTINUITY@: a discontinuity marker is placed between
--     fragments that have a gap or overlap of more than 50 milliseconds.
--     For most playback scenarios, it is recommended to use a value of
--     @ON_DISCONTINUITY@ so that the media player timeline is only reset
--     when there is a significant issue with the media timeline (e.g. a
--     missing fragment).
--
-- The default is @ALWAYS@ when HLSFragmentSelector is set to
-- @SERVER_TIMESTAMP@, and @NEVER@ when it is set to @PRODUCER_TIMESTAMP@.
getHLSStreamingSessionURL_discontinuityMode :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe HLSDiscontinuityMode)
getHLSStreamingSessionURL_discontinuityMode = Lens.lens (\GetHLSStreamingSessionURL' {discontinuityMode} -> discontinuityMode) (\s@GetHLSStreamingSessionURL' {} a -> s {discontinuityMode = a} :: GetHLSStreamingSessionURL)

-- | Specifies when the fragment start timestamps should be included in the
-- HLS media playlist. Typically, media players report the playhead
-- position as a time relative to the start of the first fragment in the
-- playback session. However, when the start timestamps are included in the
-- HLS media playlist, some media players might report the current playhead
-- as an absolute time based on the fragment timestamps. This can be useful
-- for creating a playback experience that shows viewers the wall-clock
-- time of the media.
--
-- The default is @NEVER@. When HLSFragmentSelector is @SERVER_TIMESTAMP@,
-- the timestamps will be the server start timestamps. Similarly, when
-- HLSFragmentSelector is @PRODUCER_TIMESTAMP@, the timestamps will be the
-- producer start timestamps.
getHLSStreamingSessionURL_displayFragmentTimestamp :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe HLSDisplayFragmentTimestamp)
getHLSStreamingSessionURL_displayFragmentTimestamp = Lens.lens (\GetHLSStreamingSessionURL' {displayFragmentTimestamp} -> displayFragmentTimestamp) (\s@GetHLSStreamingSessionURL' {} a -> s {displayFragmentTimestamp = a} :: GetHLSStreamingSessionURL)

-- | The time in seconds until the requested session expires. This value can
-- be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetHLSMasterPlaylist@,
-- @GetHLSMediaPlaylist@, @GetMP4InitFragment@, @GetMP4MediaFragment@, or
-- @GetTSFragment@ can be made for that session.
--
-- The default is 300 (5 minutes).
getHLSStreamingSessionURL_expires :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe Prelude.Natural)
getHLSStreamingSessionURL_expires = Lens.lens (\GetHLSStreamingSessionURL' {expires} -> expires) (\s@GetHLSStreamingSessionURL' {} a -> s {expires = a} :: GetHLSStreamingSessionURL)

-- | The time range of the requested fragment and the source of the
-- timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or
-- @LIVE_REPLAY@. This parameter is optional if PlaybackMode is@@ @LIVE@.
-- If @PlaybackMode@ is @LIVE@, the @FragmentSelectorType@ can be set, but
-- the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@
-- or @LIVE_REPLAY@, both @FragmentSelectorType@ and @TimestampRange@ must
-- be set.
getHLSStreamingSessionURL_hLSFragmentSelector :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe HLSFragmentSelector)
getHLSStreamingSessionURL_hLSFragmentSelector = Lens.lens (\GetHLSStreamingSessionURL' {hLSFragmentSelector} -> hLSFragmentSelector) (\s@GetHLSStreamingSessionURL' {} a -> s {hLSFragmentSelector = a} :: GetHLSStreamingSessionURL)

-- | The maximum number of fragments that are returned in the HLS media
-- playlists.
--
-- When the @PlaybackMode@ is @LIVE@, the most recent fragments are
-- returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@, the
-- oldest fragments are returned, up to this maximum number.
--
-- When there are a higher number of fragments available in a live HLS
-- media playlist, video players often buffer content before starting
-- playback. Increasing the buffer size increases the playback latency, but
-- it decreases the likelihood that rebuffering will occur during playback.
-- We recommend that a live HLS media playlist have a minimum of 3
-- fragments and a maximum of 10 fragments.
--
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@,
-- and 1,000 if @PlaybackMode@ is @ON_DEMAND@.
--
-- The maximum value of 5,000 fragments corresponds to more than 80 minutes
-- of video on streams with 1-second fragments, and more than 13 hours of
-- video on streams with 10-second fragments.
getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe Prelude.Natural)
getHLSStreamingSessionURL_maxMediaPlaylistFragmentResults = Lens.lens (\GetHLSStreamingSessionURL' {maxMediaPlaylistFragmentResults} -> maxMediaPlaylistFragmentResults) (\s@GetHLSStreamingSessionURL' {} a -> s {maxMediaPlaylistFragmentResults = a} :: GetHLSStreamingSessionURL)

-- | Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
-- -   __@LIVE@__ : For sessions of this type, the HLS media playlist is
--     continually updated with the latest fragments as they become
--     available. We recommend that the media player retrieve a new
--     playlist on a one-second interval. When this type of session is
--     played in a media player, the user interface typically displays a
--     \"live\" notification, with no scrubber control for choosing the
--     position in the playback window to display.
--
--     In @LIVE@ mode, the newest available fragments are included in an
--     HLS media playlist, even if there is a gap between fragments (that
--     is, if a fragment is missing). A gap like this might cause a media
--     player to halt or cause a jump in playback. In this mode, fragments
--     are not added to the HLS media playlist if they are older than the
--     newest fragment in the playlist. If the missing fragment becomes
--     available after a subsequent fragment is added to the playlist, the
--     older fragment is not added, and the gap is not filled.
--
-- -   __@LIVE_REPLAY@__ : For sessions of this type, the HLS media
--     playlist is updated similarly to how it is updated for @LIVE@ mode
--     except that it starts by including fragments from a given start
--     time. Instead of fragments being added as they are ingested,
--     fragments are added as the duration of the next fragment elapses.
--     For example, if the fragments in the session are two seconds long,
--     then a new fragment is added to the media playlist every two
--     seconds. This mode is useful to be able to start playback from when
--     an event is detected and continue live streaming media that has not
--     yet been ingested as of the time of the session creation. This mode
--     is also useful to stream previously archived media without being
--     limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
-- -   __@ON_DEMAND@__ : For sessions of this type, the HLS media playlist
--     contains all the fragments for the session, up to the number that is
--     specified in @MaxMediaPlaylistFragmentResults@. The playlist must be
--     retrieved only once for each session. When this type of session is
--     played in a media player, the user interface typically displays a
--     scrubber control for choosing the position in the playback window to
--     display.
--
-- In all playback modes, if @FragmentSelectorType@ is
-- @PRODUCER_TIMESTAMP@, and if there are multiple fragments with the same
-- start timestamp, the fragment that has the largest fragment number (that
-- is, the newest fragment) is included in the HLS media playlist. The
-- other fragments are not included. Fragments that have different
-- timestamps but have overlapping durations are still included in the HLS
-- media playlist. This can lead to unexpected behavior in the media
-- player.
--
-- The default is @LIVE@.
getHLSStreamingSessionURL_playbackMode :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe HLSPlaybackMode)
getHLSStreamingSessionURL_playbackMode = Lens.lens (\GetHLSStreamingSessionURL' {playbackMode} -> playbackMode) (\s@GetHLSStreamingSessionURL' {} a -> s {playbackMode = a} :: GetHLSStreamingSessionURL)

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the
-- HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
getHLSStreamingSessionURL_streamARN :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe Prelude.Text)
getHLSStreamingSessionURL_streamARN = Lens.lens (\GetHLSStreamingSessionURL' {streamARN} -> streamARN) (\s@GetHLSStreamingSessionURL' {} a -> s {streamARN = a} :: GetHLSStreamingSessionURL)

-- | The name of the stream for which to retrieve the HLS master playlist
-- URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@.
getHLSStreamingSessionURL_streamName :: Lens.Lens' GetHLSStreamingSessionURL (Prelude.Maybe Prelude.Text)
getHLSStreamingSessionURL_streamName = Lens.lens (\GetHLSStreamingSessionURL' {streamName} -> streamName) (\s@GetHLSStreamingSessionURL' {} a -> s {streamName = a} :: GetHLSStreamingSessionURL)

instance Core.AWSRequest GetHLSStreamingSessionURL where
  type
    AWSResponse GetHLSStreamingSessionURL =
      GetHLSStreamingSessionURLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHLSStreamingSessionURLResponse'
            Prelude.<$> (x Data..?> "HLSStreamingSessionURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHLSStreamingSessionURL where
  hashWithSalt _salt GetHLSStreamingSessionURL' {..} =
    _salt `Prelude.hashWithSalt` containerFormat
      `Prelude.hashWithSalt` discontinuityMode
      `Prelude.hashWithSalt` displayFragmentTimestamp
      `Prelude.hashWithSalt` expires
      `Prelude.hashWithSalt` hLSFragmentSelector
      `Prelude.hashWithSalt` maxMediaPlaylistFragmentResults
      `Prelude.hashWithSalt` playbackMode
      `Prelude.hashWithSalt` streamARN
      `Prelude.hashWithSalt` streamName

instance Prelude.NFData GetHLSStreamingSessionURL where
  rnf GetHLSStreamingSessionURL' {..} =
    Prelude.rnf containerFormat
      `Prelude.seq` Prelude.rnf discontinuityMode
      `Prelude.seq` Prelude.rnf displayFragmentTimestamp
      `Prelude.seq` Prelude.rnf expires
      `Prelude.seq` Prelude.rnf hLSFragmentSelector
      `Prelude.seq` Prelude.rnf maxMediaPlaylistFragmentResults
      `Prelude.seq` Prelude.rnf playbackMode
      `Prelude.seq` Prelude.rnf streamARN
      `Prelude.seq` Prelude.rnf streamName

instance Data.ToHeaders GetHLSStreamingSessionURL where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetHLSStreamingSessionURL where
  toJSON GetHLSStreamingSessionURL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContainerFormat" Data..=)
              Prelude.<$> containerFormat,
            ("DiscontinuityMode" Data..=)
              Prelude.<$> discontinuityMode,
            ("DisplayFragmentTimestamp" Data..=)
              Prelude.<$> displayFragmentTimestamp,
            ("Expires" Data..=) Prelude.<$> expires,
            ("HLSFragmentSelector" Data..=)
              Prelude.<$> hLSFragmentSelector,
            ("MaxMediaPlaylistFragmentResults" Data..=)
              Prelude.<$> maxMediaPlaylistFragmentResults,
            ("PlaybackMode" Data..=) Prelude.<$> playbackMode,
            ("StreamARN" Data..=) Prelude.<$> streamARN,
            ("StreamName" Data..=) Prelude.<$> streamName
          ]
      )

instance Data.ToPath GetHLSStreamingSessionURL where
  toPath = Prelude.const "/getHLSStreamingSessionURL"

instance Data.ToQuery GetHLSStreamingSessionURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHLSStreamingSessionURLResponse' smart constructor.
data GetHLSStreamingSessionURLResponse = GetHLSStreamingSessionURLResponse'
  { -- | The URL (containing the session token) that a media player can use to
    -- retrieve the HLS master playlist.
    hLSStreamingSessionURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHLSStreamingSessionURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hLSStreamingSessionURL', 'getHLSStreamingSessionURLResponse_hLSStreamingSessionURL' - The URL (containing the session token) that a media player can use to
-- retrieve the HLS master playlist.
--
-- 'httpStatus', 'getHLSStreamingSessionURLResponse_httpStatus' - The response's http status code.
newGetHLSStreamingSessionURLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHLSStreamingSessionURLResponse
newGetHLSStreamingSessionURLResponse pHttpStatus_ =
  GetHLSStreamingSessionURLResponse'
    { hLSStreamingSessionURL =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL (containing the session token) that a media player can use to
-- retrieve the HLS master playlist.
getHLSStreamingSessionURLResponse_hLSStreamingSessionURL :: Lens.Lens' GetHLSStreamingSessionURLResponse (Prelude.Maybe Prelude.Text)
getHLSStreamingSessionURLResponse_hLSStreamingSessionURL = Lens.lens (\GetHLSStreamingSessionURLResponse' {hLSStreamingSessionURL} -> hLSStreamingSessionURL) (\s@GetHLSStreamingSessionURLResponse' {} a -> s {hLSStreamingSessionURL = a} :: GetHLSStreamingSessionURLResponse)

-- | The response's http status code.
getHLSStreamingSessionURLResponse_httpStatus :: Lens.Lens' GetHLSStreamingSessionURLResponse Prelude.Int
getHLSStreamingSessionURLResponse_httpStatus = Lens.lens (\GetHLSStreamingSessionURLResponse' {httpStatus} -> httpStatus) (\s@GetHLSStreamingSessionURLResponse' {} a -> s {httpStatus = a} :: GetHLSStreamingSessionURLResponse)

instance
  Prelude.NFData
    GetHLSStreamingSessionURLResponse
  where
  rnf GetHLSStreamingSessionURLResponse' {..} =
    Prelude.rnf hLSStreamingSessionURL
      `Prelude.seq` Prelude.rnf httpStatus
