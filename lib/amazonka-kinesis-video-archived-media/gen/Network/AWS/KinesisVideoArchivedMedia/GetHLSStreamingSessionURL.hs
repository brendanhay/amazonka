{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an HTTP Live Streaming (HLS) URL for the stream. You can then open the URL in a browser or media player to view the stream contents.
--
-- Both the @StreamName@ and the @StreamARN@ parameters are optional, but you must specify either the @StreamName@ or the @StreamARN@ when invoking this API operation.
-- An Amazon Kinesis video stream has the following requirements for providing data through HLS:
--
--     * The media must contain h.264 or h.265 encoded video and, optionally, AAC encoded audio. Specifically, the codec ID of track 1 should be @V_MPEG/ISO/AVC@ (for h.264) or @V_MPEG/ISO/HEVC@ (for h.265). Optionally, the codec ID of track 2 should be @A_AAC@ .
--
--
--     * Data retention must be greater than 0.
--
--
--     * The video track of each fragment must contain codec private data in the Advanced Video Coding (AVC) for H.264 format or HEVC for H.265 format (<https://www.iso.org/standard/55980.html MPEG-4 specification ISO/IEC 14496-15> ). For information about adapting stream data to a given format, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags> .
--
--
--     * The audio track (if present) of each fragment must contain codec private data in the AAC format (<https://www.iso.org/standard/43345.html AAC specification ISO/IEC 13818-7> ).
--
--
-- Kinesis Video Streams HLS sessions contain fragments in the fragmented MPEG-4 form (also called fMP4 or CMAF) or the MPEG-2 form (also called TS chunks, which the HLS specification also supports). For more information about HLS fragment types, see the <https://tools.ietf.org/html/draft-pantos-http-live-streaming-23 HLS specification> .
-- The following procedure shows how to use HLS with Kinesis Video Streams:
--
--     * Get an endpoint using <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_GetDataEndpoint.html GetDataEndpoint> , specifying @GET_HLS_STREAMING_SESSION_URL@ for the @APIName@ parameter.
--
--
--     * Retrieve the HLS URL using @GetHLSStreamingSessionURL@ . Kinesis Video Streams creates an HLS streaming session to be used for accessing content in a stream using the HLS protocol. @GetHLSStreamingSessionURL@ returns an authenticated URL (that includes an encrypted session token) for the session's HLS /master playlist/ (the root resource needed for streaming with HLS).
-- The media that is made available through the playlist consists only of the requested stream, time range, and format. No other media data (such as frames outside the requested window or alternate bitrates) is made available.
--
--
--     * Provide the URL (containing the encrypted session token) for the HLS master playlist to a media player that supports the HLS protocol. Kinesis Video Streams makes the HLS media playlist, initialization fragment, and media fragments available through the master playlist URL. The initialization fragment contains the codec private data for the stream, and other data needed to set up the video or audio decoder and renderer. The media fragments contain H.264-encoded video frames or AAC-encoded audio samples.
--
--
--     * The media player receives the authenticated URL and requests stream metadata and media data normally. When the media player requests data, it calls the following actions:
--
--     * __GetHLSMasterPlaylist:__ Retrieves an HLS master playlist, which contains a URL for the @GetHLSMediaPlaylist@ action for each track, and additional metadata for the media player, including estimated bitrate and resolution.
--
--
--     * __GetHLSMediaPlaylist:__ Retrieves an HLS media playlist, which contains a URL to access the MP4 initialization fragment with the @GetMP4InitFragment@ action, and URLs to access the MP4 media fragments with the @GetMP4MediaFragment@ actions. The HLS media playlist also contains metadata about the stream that the player needs to play it, such as whether the @PlaybackMode@ is @LIVE@ or @ON_DEMAND@ . The HLS media playlist is typically static for sessions with a @PlaybackType@ of @ON_DEMAND@ . The HLS media playlist is continually updated with new fragments for sessions with a @PlaybackType@ of @LIVE@ . There is a distinct HLS media playlist for the video track and the audio track (if applicable) that contains MP4 media URLs for the specific track.
--
--
--     * __GetMP4InitFragment:__ Retrieves the MP4 initialization fragment. The media player typically loads the initialization fragment before loading any media fragments. This fragment contains the "@fytp@ " and "@moov@ " MP4 atoms, and the child atoms that are needed to initialize the media player decoder.
-- The initialization fragment does not correspond to a fragment in a Kinesis video stream. It contains only the codec private data for the stream and respective track, which the media player needs to decode the media frames.
--
--
--     * __GetMP4MediaFragment:__ Retrieves MP4 media fragments. These fragments contain the "@moof@ " and "@mdat@ " MP4 atoms and their child atoms, containing the encoded fragment's media frames and their timestamps.
-- Data retrieved with this action is billable. See <https://aws.amazon.com/kinesis/video-streams/pricing/ Pricing> for details.
--
--
--     * __GetTSFragment:__ Retrieves MPEG TS fragments containing both initialization and media data for all tracks in the stream.
-- Data retrieved with this action is billable. For more information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Kinesis Video Streams pricing> .
--
--
--
--
-- You can monitor the amount of data that the media player consumes by monitoring the @GetMP4MediaFragment.OutgoingBytes@ Amazon CloudWatch metric. For information about using CloudWatch to monitor Kinesis Video Streams, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams> . For pricing information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing> and <https://aws.amazon.com/pricing/ AWS Pricing> . Charges for both HLS sessions and outgoing AWS data apply.
-- For more information about HLS, see <https://developer.apple.com/streaming/ HTTP Live Streaming> on the <https://developer.apple.com Apple Developer site> .
-- /Important:/ If an error is thrown after invoking a Kinesis Video Streams archived media API, in addition to the HTTP status code and the response body, it includes the following pieces of information:
--
--     * @x-amz-ErrorType@ HTTP header – contains a more specific error type in addition to what the HTTP status code provides.
--
--
--     * @x-amz-RequestId@ HTTP header – if you want to report an issue to AWS, the support team can better diagnose the problem if given the Request Id.
--
--
-- Both the HTTP status code and the ErrorType header can be utilized to make programmatic decisions about whether errors are retry-able and under what conditions, as well as provide information on what actions the client programmer might need to take in order to successfully try again.
-- For more information, see the __Errors__ section at the bottom of this topic, as well as <https://docs.aws.amazon.com/kinesisvideostreams/latest/dg/CommonErrors.html Common Errors> .
module Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
  ( -- * Creating a request
    GetHLSStreamingSessionURL (..),
    mkGetHLSStreamingSessionURL,

    -- ** Request lenses
    ghlsssuDisplayFragmentTimestamp,
    ghlsssuHLSFragmentSelector,
    ghlsssuExpires,
    ghlsssuStreamARN,
    ghlsssuPlaybackMode,
    ghlsssuContainerFormat,
    ghlsssuMaxMediaPlaylistFragmentResults,
    ghlsssuDiscontinuityMode,
    ghlsssuStreamName,

    -- * Destructuring the response
    GetHLSStreamingSessionURLResponse (..),
    mkGetHLSStreamingSessionURLResponse,

    -- ** Response lenses
    ghlsssursHLSStreamingSessionURL,
    ghlsssursResponseStatus,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetHLSStreamingSessionURL' smart constructor.
data GetHLSStreamingSessionURL = GetHLSStreamingSessionURL'
  { displayFragmentTimestamp ::
      Lude.Maybe HLSDisplayFragmentTimestamp,
    hLSFragmentSelector ::
      Lude.Maybe HLSFragmentSelector,
    expires :: Lude.Maybe Lude.Natural,
    streamARN :: Lude.Maybe Lude.Text,
    playbackMode ::
      Lude.Maybe HLSPlaybackMode,
    containerFormat ::
      Lude.Maybe ContainerFormat,
    maxMediaPlaylistFragmentResults ::
      Lude.Maybe Lude.Natural,
    discontinuityMode ::
      Lude.Maybe HLSDiscontinuityMode,
    streamName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHLSStreamingSessionURL' with the minimum fields required to make a request.
--
-- * 'containerFormat' - Specifies which format should be used for packaging the media. Specifying the @FRAGMENTED_MP4@ container format packages the media into MP4 fragments (fMP4 or CMAF). This is the recommended packaging because there is minimal packaging overhead. The other container format option is @MPEG_TS@ . HLS has supported MPEG TS chunks since it was released and is sometimes the only supported packaging on older HLS players. MPEG TS typically has a 5-25 percent packaging overhead. This means MPEG TS typically requires 5-25 percent more bandwidth and cost than fMP4.
--
-- The default is @FRAGMENTED_MP4@ .
-- * 'discontinuityMode' - Specifies when flags marking discontinuities between fragments are added to the media playlists.
--
-- Media players typically build a timeline of media content to play, based on the timestamps of each fragment. This means that if there is any overlap or gap between fragments (as is typical if 'HLSFragmentSelector' is set to @SERVER_TIMESTAMP@ ), the media player timeline will also have small gaps between fragments in some places, and will overwrite frames in other places. Gaps in the media player timeline can cause playback to stall and overlaps can cause playback to be jittery. When there are discontinuity flags between fragments, the media player is expected to reset the timeline, resulting in the next fragment being played immediately after the previous fragment.
-- The following modes are supported:
--
--     * @ALWAYS@ : a discontinuity marker is placed between every fragment in the HLS media playlist. It is recommended to use a value of @ALWAYS@ if the fragment timestamps are not accurate.
--
--
--     * @NEVER@ : no discontinuity markers are placed anywhere. It is recommended to use a value of @NEVER@ to ensure the media player timeline most accurately maps to the producer timestamps.
--
--
--     * @ON_DISCONTIUNITY@ : a discontinuity marker is placed between fragments that have a gap or overlap of more than 50 milliseconds. For most playback scenarios, it is recommended to use a value of @ON_DISCONTINUITY@ so that the media player timeline is only reset when there is a significant issue with the media timeline (e.g. a missing fragment).
--
--
-- The default is @ALWAYS@ when 'HLSFragmentSelector' is set to @SERVER_TIMESTAMP@ , and @NEVER@ when it is set to @PRODUCER_TIMESTAMP@ .
-- * 'displayFragmentTimestamp' - Specifies when the fragment start timestamps should be included in the HLS media playlist. Typically, media players report the playhead position as a time relative to the start of the first fragment in the playback session. However, when the start timestamps are included in the HLS media playlist, some media players might report the current playhead as an absolute time based on the fragment timestamps. This can be useful for creating a playback experience that shows viewers the wall-clock time of the media.
--
-- The default is @NEVER@ . When 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'HLSFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
-- * 'expires' - The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetHLSMasterPlaylist@ , @GetHLSMediaPlaylist@ , @GetMP4InitFragment@ , @GetMP4MediaFragment@ , or @GetTSFragment@ can be made for that session.
-- The default is 300 (5 minutes).
-- * 'hLSFragmentSelector' - The time range of the requested fragment and the source of the timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ . This parameter is optional if PlaybackMode is@LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
-- * 'maxMediaPlaylistFragmentResults' - The maximum number of fragments that are returned in the HLS media playlists.
--
-- When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number.
-- When there are a higher number of fragments available in a live HLS media playlist, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live HLS media playlist have a minimum of 3 fragments and a maximum of 10 fragments.
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
-- * 'playbackMode' - Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
--     * __@LIVE@ __ : For sessions of this type, the HLS media playlist is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new playlist on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.
--
--
--     * __@LIVE_REPLAY@ __ : For sessions of this type, the HLS media playlist is updated similarly to how it is updated for @LIVE@ mode except that it starts by including fragments from a given start time. Instead of fragments being added as they are ingested, fragments are added as the duration of the next fragment elapses. For example, if the fragments in the session are two seconds long, then a new fragment is added to the media playlist every two seconds. This mode is useful to be able to start playback from when an event is detected and continue live streaming media that has not yet been ingested as of the time of the session creation. This mode is also useful to stream previously archived media without being limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
--
--     * __@ON_DEMAND@ __ : For sessions of this type, the HLS media playlist contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The playlist must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display.
--
--
-- In all playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the HLS media playlist. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the HLS media playlist. This can lead to unexpected behavior in the media player.
-- The default is @LIVE@ .
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
-- * 'streamName' - The name of the stream for which to retrieve the HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
mkGetHLSStreamingSessionURL ::
  GetHLSStreamingSessionURL
mkGetHLSStreamingSessionURL =
  GetHLSStreamingSessionURL'
    { displayFragmentTimestamp =
        Lude.Nothing,
      hLSFragmentSelector = Lude.Nothing,
      expires = Lude.Nothing,
      streamARN = Lude.Nothing,
      playbackMode = Lude.Nothing,
      containerFormat = Lude.Nothing,
      maxMediaPlaylistFragmentResults = Lude.Nothing,
      discontinuityMode = Lude.Nothing,
      streamName = Lude.Nothing
    }

-- | Specifies when the fragment start timestamps should be included in the HLS media playlist. Typically, media players report the playhead position as a time relative to the start of the first fragment in the playback session. However, when the start timestamps are included in the HLS media playlist, some media players might report the current playhead as an absolute time based on the fragment timestamps. This can be useful for creating a playback experience that shows viewers the wall-clock time of the media.
--
-- The default is @NEVER@ . When 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'HLSFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
--
-- /Note:/ Consider using 'displayFragmentTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuDisplayFragmentTimestamp :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe HLSDisplayFragmentTimestamp)
ghlsssuDisplayFragmentTimestamp = Lens.lens (displayFragmentTimestamp :: GetHLSStreamingSessionURL -> Lude.Maybe HLSDisplayFragmentTimestamp) (\s a -> s {displayFragmentTimestamp = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuDisplayFragmentTimestamp "Use generic-lens or generic-optics with 'displayFragmentTimestamp' instead." #-}

-- | The time range of the requested fragment and the source of the timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ . This parameter is optional if PlaybackMode is@LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
--
-- /Note:/ Consider using 'hLSFragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuHLSFragmentSelector :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe HLSFragmentSelector)
ghlsssuHLSFragmentSelector = Lens.lens (hLSFragmentSelector :: GetHLSStreamingSessionURL -> Lude.Maybe HLSFragmentSelector) (\s a -> s {hLSFragmentSelector = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuHLSFragmentSelector "Use generic-lens or generic-optics with 'hLSFragmentSelector' instead." #-}

-- | The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetHLSMasterPlaylist@ , @GetHLSMediaPlaylist@ , @GetMP4InitFragment@ , @GetMP4MediaFragment@ , or @GetTSFragment@ can be made for that session.
-- The default is 300 (5 minutes).
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuExpires :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe Lude.Natural)
ghlsssuExpires = Lens.lens (expires :: GetHLSStreamingSessionURL -> Lude.Maybe Lude.Natural) (\s a -> s {expires = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuStreamARN :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe Lude.Text)
ghlsssuStreamARN = Lens.lens (streamARN :: GetHLSStreamingSessionURL -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
--     * __@LIVE@ __ : For sessions of this type, the HLS media playlist is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new playlist on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.
--
--
--     * __@LIVE_REPLAY@ __ : For sessions of this type, the HLS media playlist is updated similarly to how it is updated for @LIVE@ mode except that it starts by including fragments from a given start time. Instead of fragments being added as they are ingested, fragments are added as the duration of the next fragment elapses. For example, if the fragments in the session are two seconds long, then a new fragment is added to the media playlist every two seconds. This mode is useful to be able to start playback from when an event is detected and continue live streaming media that has not yet been ingested as of the time of the session creation. This mode is also useful to stream previously archived media without being limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
--
--     * __@ON_DEMAND@ __ : For sessions of this type, the HLS media playlist contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The playlist must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display.
--
--
-- In all playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the HLS media playlist. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the HLS media playlist. This can lead to unexpected behavior in the media player.
-- The default is @LIVE@ .
--
-- /Note:/ Consider using 'playbackMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuPlaybackMode :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe HLSPlaybackMode)
ghlsssuPlaybackMode = Lens.lens (playbackMode :: GetHLSStreamingSessionURL -> Lude.Maybe HLSPlaybackMode) (\s a -> s {playbackMode = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuPlaybackMode "Use generic-lens or generic-optics with 'playbackMode' instead." #-}

-- | Specifies which format should be used for packaging the media. Specifying the @FRAGMENTED_MP4@ container format packages the media into MP4 fragments (fMP4 or CMAF). This is the recommended packaging because there is minimal packaging overhead. The other container format option is @MPEG_TS@ . HLS has supported MPEG TS chunks since it was released and is sometimes the only supported packaging on older HLS players. MPEG TS typically has a 5-25 percent packaging overhead. This means MPEG TS typically requires 5-25 percent more bandwidth and cost than fMP4.
--
-- The default is @FRAGMENTED_MP4@ .
--
-- /Note:/ Consider using 'containerFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuContainerFormat :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe ContainerFormat)
ghlsssuContainerFormat = Lens.lens (containerFormat :: GetHLSStreamingSessionURL -> Lude.Maybe ContainerFormat) (\s a -> s {containerFormat = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuContainerFormat "Use generic-lens or generic-optics with 'containerFormat' instead." #-}

-- | The maximum number of fragments that are returned in the HLS media playlists.
--
-- When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number.
-- When there are a higher number of fragments available in a live HLS media playlist, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live HLS media playlist have a minimum of 3 fragments and a maximum of 10 fragments.
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
--
-- /Note:/ Consider using 'maxMediaPlaylistFragmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuMaxMediaPlaylistFragmentResults :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe Lude.Natural)
ghlsssuMaxMediaPlaylistFragmentResults = Lens.lens (maxMediaPlaylistFragmentResults :: GetHLSStreamingSessionURL -> Lude.Maybe Lude.Natural) (\s a -> s {maxMediaPlaylistFragmentResults = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuMaxMediaPlaylistFragmentResults "Use generic-lens or generic-optics with 'maxMediaPlaylistFragmentResults' instead." #-}

-- | Specifies when flags marking discontinuities between fragments are added to the media playlists.
--
-- Media players typically build a timeline of media content to play, based on the timestamps of each fragment. This means that if there is any overlap or gap between fragments (as is typical if 'HLSFragmentSelector' is set to @SERVER_TIMESTAMP@ ), the media player timeline will also have small gaps between fragments in some places, and will overwrite frames in other places. Gaps in the media player timeline can cause playback to stall and overlaps can cause playback to be jittery. When there are discontinuity flags between fragments, the media player is expected to reset the timeline, resulting in the next fragment being played immediately after the previous fragment.
-- The following modes are supported:
--
--     * @ALWAYS@ : a discontinuity marker is placed between every fragment in the HLS media playlist. It is recommended to use a value of @ALWAYS@ if the fragment timestamps are not accurate.
--
--
--     * @NEVER@ : no discontinuity markers are placed anywhere. It is recommended to use a value of @NEVER@ to ensure the media player timeline most accurately maps to the producer timestamps.
--
--
--     * @ON_DISCONTIUNITY@ : a discontinuity marker is placed between fragments that have a gap or overlap of more than 50 milliseconds. For most playback scenarios, it is recommended to use a value of @ON_DISCONTINUITY@ so that the media player timeline is only reset when there is a significant issue with the media timeline (e.g. a missing fragment).
--
--
-- The default is @ALWAYS@ when 'HLSFragmentSelector' is set to @SERVER_TIMESTAMP@ , and @NEVER@ when it is set to @PRODUCER_TIMESTAMP@ .
--
-- /Note:/ Consider using 'discontinuityMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuDiscontinuityMode :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe HLSDiscontinuityMode)
ghlsssuDiscontinuityMode = Lens.lens (discontinuityMode :: GetHLSStreamingSessionURL -> Lude.Maybe HLSDiscontinuityMode) (\s a -> s {discontinuityMode = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuDiscontinuityMode "Use generic-lens or generic-optics with 'discontinuityMode' instead." #-}

-- | The name of the stream for which to retrieve the HLS master playlist URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssuStreamName :: Lens.Lens' GetHLSStreamingSessionURL (Lude.Maybe Lude.Text)
ghlsssuStreamName = Lens.lens (streamName :: GetHLSStreamingSessionURL -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: GetHLSStreamingSessionURL)
{-# DEPRECATED ghlsssuStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Lude.AWSRequest GetHLSStreamingSessionURL where
  type
    Rs GetHLSStreamingSessionURL =
      GetHLSStreamingSessionURLResponse
  request = Req.postJSON kinesisVideoArchivedMediaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetHLSStreamingSessionURLResponse'
            Lude.<$> (x Lude..?> "HLSStreamingSessionURL")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetHLSStreamingSessionURL where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetHLSStreamingSessionURL where
  toJSON GetHLSStreamingSessionURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayFragmentTimestamp" Lude..=)
              Lude.<$> displayFragmentTimestamp,
            ("HLSFragmentSelector" Lude..=) Lude.<$> hLSFragmentSelector,
            ("Expires" Lude..=) Lude.<$> expires,
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("PlaybackMode" Lude..=) Lude.<$> playbackMode,
            ("ContainerFormat" Lude..=) Lude.<$> containerFormat,
            ("MaxMediaPlaylistFragmentResults" Lude..=)
              Lude.<$> maxMediaPlaylistFragmentResults,
            ("DiscontinuityMode" Lude..=) Lude.<$> discontinuityMode,
            ("StreamName" Lude..=) Lude.<$> streamName
          ]
      )

instance Lude.ToPath GetHLSStreamingSessionURL where
  toPath = Lude.const "/getHLSStreamingSessionURL"

instance Lude.ToQuery GetHLSStreamingSessionURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetHLSStreamingSessionURLResponse' smart constructor.
data GetHLSStreamingSessionURLResponse = GetHLSStreamingSessionURLResponse'
  { hLSStreamingSessionURL ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHLSStreamingSessionURLResponse' with the minimum fields required to make a request.
--
-- * 'hLSStreamingSessionURL' - The URL (containing the session token) that a media player can use to retrieve the HLS master playlist.
-- * 'responseStatus' - The response status code.
mkGetHLSStreamingSessionURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetHLSStreamingSessionURLResponse
mkGetHLSStreamingSessionURLResponse pResponseStatus_ =
  GetHLSStreamingSessionURLResponse'
    { hLSStreamingSessionURL =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URL (containing the session token) that a media player can use to retrieve the HLS master playlist.
--
-- /Note:/ Consider using 'hLSStreamingSessionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssursHLSStreamingSessionURL :: Lens.Lens' GetHLSStreamingSessionURLResponse (Lude.Maybe Lude.Text)
ghlsssursHLSStreamingSessionURL = Lens.lens (hLSStreamingSessionURL :: GetHLSStreamingSessionURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {hLSStreamingSessionURL = a} :: GetHLSStreamingSessionURLResponse)
{-# DEPRECATED ghlsssursHLSStreamingSessionURL "Use generic-lens or generic-optics with 'hLSStreamingSessionURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghlsssursResponseStatus :: Lens.Lens' GetHLSStreamingSessionURLResponse Lude.Int
ghlsssursResponseStatus = Lens.lens (responseStatus :: GetHLSStreamingSessionURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHLSStreamingSessionURLResponse)
{-# DEPRECATED ghlsssursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
