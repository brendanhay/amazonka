{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an HTTP Live Streaming (HLS) URL for the stream. You can then open the URL in a browser or media player to view the stream contents.
--
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
--
-- An Amazon Kinesis video stream has the following requirements for providing data through HLS:
--
--     * The media must contain h.264 encoded video and, optionally, AAC encoded audio. Specifically, the codec id of track 1 should be @V_MPEG/ISO/AVC@ . Optionally, the codec id of track 2 should be @A_AAC@ .
--
--     * Data retention must be greater than 0.
--
--     * The video track of each fragment must contain codec private data in the Advanced Video Coding (AVC) for H.264 format (<https://www.iso.org/standard/55980.html MPEG-4 specification ISO/IEC 14496-15> ). For information about adapting stream data to a given format, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags> .
--
--     * The audio track (if present) of each fragment must contain codec private data in the AAC format (<https://www.iso.org/standard/43345.html AAC specification ISO/IEC 13818-7> ).
--
--
--
-- Kinesis Video Streams HLS sessions contain fragments in the fragmented MPEG-4 form (also called fMP4 or CMAF), rather than the MPEG-2 form (also called TS chunks, which the HLS specification also supports). For more information about HLS fragment types, see the <https://tools.ietf.org/html/draft-pantos-http-live-streaming-23 HLS specification> .
--
-- The following procedure shows how to use HLS with Kinesis Video Streams:
--
--     * Get an endpoint using <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_GetDataEndpoint.html GetDataEndpoint> , specifying @GET_HLS_STREAMING_SESSION_URL@ for the @APIName@ parameter.
--
--     * Retrieve the HLS URL using @GetHLSStreamingSessionURL@ . Kinesis Video Streams creates an HLS streaming session to be used for accessing content in a stream using the HLS protocol. @GetHLSStreamingSessionURL@ returns an authenticated URL (that includes an encrypted session token) for the session's HLS /master playlist/ (the root resource needed for streaming with HLS).
--
-- The media that is made available through the playlist consists only of the requested stream, time range, and format. No other media data (such as frames outside the requested window or alternate bitrates) is made available.
--
--     * Provide the URL (containing the encrypted session token) for the HLS master playlist to a media player that supports the HLS protocol. Kinesis Video Streams makes the HLS media playlist, initialization fragment, and media fragments available through the master playlist URL. The initialization fragment contains the codec private data for the stream, and other data needed to set up the video or audio decoder and renderer. The media fragments contain H.264-encoded video frames or AAC-encoded audio samples.
--
--     * The media player receives the authenticated URL and requests stream metadata and media data normally. When the media player requests data, it calls the following actions:
--
--     * __GetHLSMasterPlaylist:__ Retrieves an HLS master playlist, which contains a URL for the @GetHLSMediaPlaylist@ action for each track, and additional metadata for the media player, including estimated bitrate and resolution.
--
--     * __GetHLSMediaPlaylist:__ Retrieves an HLS media playlist, which contains a URL to access the MP4 initialization fragment with the @GetMP4InitFragment@ action, and URLs to access the MP4 media fragments with the @GetMP4MediaFragment@ actions. The HLS media playlist also contains metadata about the stream that the player needs to play it, such as whether the @PlaybackMode@ is @LIVE@ or @ON_DEMAND@ . The HLS media playlist is typically static for sessions with a @PlaybackType@ of @ON_DEMAND@ . The HLS media playlist is continually updated with new fragments for sessions with a @PlaybackType@ of @LIVE@ . There is a distinct HLS media playlist for the video track and the audio track (if applicable) that contains MP4 media URLs for the specific track.
--
--     * __GetMP4InitFragment:__ Retrieves the MP4 initialization fragment. The media player typically loads the initialization fragment before loading any media fragments. This fragment contains the "@fytp@ " and "@moov@ " MP4 atoms, and the child atoms that are needed to initialize the media player decoder.
--
-- The initialization fragment does not correspond to a fragment in a Kinesis video stream. It contains only the codec private data for the stream and respective track, which the media player needs to decode the media frames.
--
--     * __GetMP4MediaFragment:__ Retrieves MP4 media fragments. These fragments contain the "@moof@ " and "@mdat@ " MP4 atoms and their child atoms, containing the encoded fragment's media frames and their timestamps.
--
-- Data retrieved with this action is billable. See <https://aws.amazon.com/kinesis/video-streams/pricing/ Pricing> for details.
--
--     * __GetTSFragment:__ Retrieves MPEG TS fragments containing both initialization and media data for all tracks in the stream.
--
-- Data retrieved with this action is billable. For more information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Kinesis Video Streams pricing> .
--
--
--
--
--
-- You can monitor the amount of data that the media player consumes by monitoring the @GetMP4MediaFragment.OutgoingBytes@ Amazon CloudWatch metric. For information about using CloudWatch to monitor Kinesis Video Streams, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/monitoring.html Monitoring Kinesis Video Streams> . For pricing information, see <https://aws.amazon.com/kinesis/video-streams/pricing/ Amazon Kinesis Video Streams Pricing> and <https://aws.amazon.com/pricing/ AWS Pricing> . Charges for both HLS sessions and outgoing AWS data apply.
--
-- For more information about HLS, see <https://developer.apple.com/streaming/ HTTP Live Streaming> on the <https://developer.apple.com Apple Developer site> .
--
module Network.AWS.KinesisVideoArchivedMedia.GetHLSStreamingSessionURL
    (
    -- * Creating a Request
      getHLSStreamingSessionURL
    , GetHLSStreamingSessionURL
    -- * Request Lenses
    , ghlsssuDisplayFragmentTimestamp
    , ghlsssuHLSFragmentSelector
    , ghlsssuExpires
    , ghlsssuStreamARN
    , ghlsssuPlaybackMode
    , ghlsssuContainerFormat
    , ghlsssuMaxMediaPlaylistFragmentResults
    , ghlsssuDiscontinuityMode
    , ghlsssuStreamName

    -- * Destructuring the Response
    , getHLSStreamingSessionURLResponse
    , GetHLSStreamingSessionURLResponse
    -- * Response Lenses
    , ghlsssursHLSStreamingSessionURL
    , ghlsssursResponseStatus
    ) where

import Network.AWS.KinesisVideoArchivedMedia.Types
import Network.AWS.KinesisVideoArchivedMedia.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getHLSStreamingSessionURL' smart constructor.
data GetHLSStreamingSessionURL = GetHLSStreamingSessionURL'
  { _ghlsssuDisplayFragmentTimestamp        :: !(Maybe DisplayFragmentTimestamp)
  , _ghlsssuHLSFragmentSelector             :: !(Maybe HLSFragmentSelector)
  , _ghlsssuExpires                         :: !(Maybe Nat)
  , _ghlsssuStreamARN                       :: !(Maybe Text)
  , _ghlsssuPlaybackMode                    :: !(Maybe PlaybackMode)
  , _ghlsssuContainerFormat                 :: !(Maybe ContainerFormat)
  , _ghlsssuMaxMediaPlaylistFragmentResults :: !(Maybe Nat)
  , _ghlsssuDiscontinuityMode               :: !(Maybe DiscontinuityMode)
  , _ghlsssuStreamName                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHLSStreamingSessionURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghlsssuDisplayFragmentTimestamp' - Specifies when the fragment start timestamps should be included in the HLS media playlist. Typically, media players report the playhead position as a time relative to the start of the first fragment in the playback session. However, when the start timestamps are included in the HLS media playlist, some media players might report the current playhead as an absolute time based on the fragment timestamps. This can be useful for creating a playback experience that shows viewers the wall-clock time of the media. The default is @NEVER@ . When 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'HLSFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
--
-- * 'ghlsssuHLSFragmentSelector' - The time range of the requested fragment, and the source of the timestamps. This parameter is required if @PlaybackMode@ is @ON_DEMAND@ . This parameter is optional if @PlaybackMode@ is @LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
--
-- * 'ghlsssuExpires' - The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours). When a session expires, no new calls to @GetHLSMasterPlaylist@ , @GetHLSMediaPlaylist@ , @GetMP4InitFragment@ , or @GetMP4MediaFragment@ can be made for that session. The default is 300 (5 minutes).
--
-- * 'ghlsssuStreamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the HLS master playlist URL. You must specify either the @StreamName@ or the @StreamARN@ .
--
-- * 'ghlsssuPlaybackMode' - Whether to retrieve live or archived, on-demand data. Features of the two types of session include the following:     * __@LIVE@ __ : For sessions of this type, the HLS media playlist is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new playlist on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.     * __@ON_DEMAND@ __ : For sessions of this type, the HLS media playlist contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The playlist must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display. In both playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the HLS media playlist. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the HLS media playlist. This can lead to unexpected behavior in the media player. The default is @LIVE@ .
--
-- * 'ghlsssuContainerFormat' - Specifies which format should be used for packaging the media. Specifying the @FRAGMENTED_MP4@ container format packages the media into MP4 fragments (fMP4 or CMAF). This is the recommended packaging because there is minimal packaging overhead. The other container format option is @MPEG_TS@ . HLS has supported MPEG TS chunks since it was released and is sometimes the only supported packaging on older HLS players. MPEG TS typically has a 5-25 percent packaging overhead. This means MPEG TS typically requires 5-25 percent more bandwidth and cost than fMP4. The default is @FRAGMENTED_MP4@ .
--
-- * 'ghlsssuMaxMediaPlaylistFragmentResults' - The maximum number of fragments that are returned in the HLS media playlists. When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number. When there are a higher number of fragments available in a live HLS media playlist, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live HLS media playlist have a minimum of 3 fragments and a maximum of 10 fragments. The default is 5 fragments if @PlaybackMode@ is @LIVE@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .  The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
--
-- * 'ghlsssuDiscontinuityMode' - Specifies when flags marking discontinuities between fragments will be added to the media playlists. The default is @ALWAYS@ when 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , and @NEVER@ when it is @PRODUCER_TIMESTAMP@ . Media players typically build a timeline of media content to play, based on the timestamps of each fragment. This means that if there is any overlap between fragments (as is typical if 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ ), the media player timeline has small gaps between fragments in some places, and overwrites frames in other places. When there are discontinuity flags between fragments, the media player is expected to reset the timeline, resulting in the fragment being played immediately after the previous fragment. We recommend that you always have discontinuity flags between fragments if the fragment timestamps are not accurate or if fragments might be missing. You should not place discontinuity flags between fragments for the player timeline to accurately map to the producer timestamps.
--
-- * 'ghlsssuStreamName' - The name of the stream for which to retrieve the HLS master playlist URL. You must specify either the @StreamName@ or the @StreamARN@ .
getHLSStreamingSessionURL
    :: GetHLSStreamingSessionURL
getHLSStreamingSessionURL =
  GetHLSStreamingSessionURL'
    { _ghlsssuDisplayFragmentTimestamp = Nothing
    , _ghlsssuHLSFragmentSelector = Nothing
    , _ghlsssuExpires = Nothing
    , _ghlsssuStreamARN = Nothing
    , _ghlsssuPlaybackMode = Nothing
    , _ghlsssuContainerFormat = Nothing
    , _ghlsssuMaxMediaPlaylistFragmentResults = Nothing
    , _ghlsssuDiscontinuityMode = Nothing
    , _ghlsssuStreamName = Nothing
    }


-- | Specifies when the fragment start timestamps should be included in the HLS media playlist. Typically, media players report the playhead position as a time relative to the start of the first fragment in the playback session. However, when the start timestamps are included in the HLS media playlist, some media players might report the current playhead as an absolute time based on the fragment timestamps. This can be useful for creating a playback experience that shows viewers the wall-clock time of the media. The default is @NEVER@ . When 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'HLSFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
ghlsssuDisplayFragmentTimestamp :: Lens' GetHLSStreamingSessionURL (Maybe DisplayFragmentTimestamp)
ghlsssuDisplayFragmentTimestamp = lens _ghlsssuDisplayFragmentTimestamp (\ s a -> s{_ghlsssuDisplayFragmentTimestamp = a})

-- | The time range of the requested fragment, and the source of the timestamps. This parameter is required if @PlaybackMode@ is @ON_DEMAND@ . This parameter is optional if @PlaybackMode@ is @LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
ghlsssuHLSFragmentSelector :: Lens' GetHLSStreamingSessionURL (Maybe HLSFragmentSelector)
ghlsssuHLSFragmentSelector = lens _ghlsssuHLSFragmentSelector (\ s a -> s{_ghlsssuHLSFragmentSelector = a})

-- | The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours). When a session expires, no new calls to @GetHLSMasterPlaylist@ , @GetHLSMediaPlaylist@ , @GetMP4InitFragment@ , or @GetMP4MediaFragment@ can be made for that session. The default is 300 (5 minutes).
ghlsssuExpires :: Lens' GetHLSStreamingSessionURL (Maybe Natural)
ghlsssuExpires = lens _ghlsssuExpires (\ s a -> s{_ghlsssuExpires = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the HLS master playlist URL. You must specify either the @StreamName@ or the @StreamARN@ .
ghlsssuStreamARN :: Lens' GetHLSStreamingSessionURL (Maybe Text)
ghlsssuStreamARN = lens _ghlsssuStreamARN (\ s a -> s{_ghlsssuStreamARN = a})

-- | Whether to retrieve live or archived, on-demand data. Features of the two types of session include the following:     * __@LIVE@ __ : For sessions of this type, the HLS media playlist is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new playlist on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.     * __@ON_DEMAND@ __ : For sessions of this type, the HLS media playlist contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The playlist must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display. In both playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the HLS media playlist. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the HLS media playlist. This can lead to unexpected behavior in the media player. The default is @LIVE@ .
ghlsssuPlaybackMode :: Lens' GetHLSStreamingSessionURL (Maybe PlaybackMode)
ghlsssuPlaybackMode = lens _ghlsssuPlaybackMode (\ s a -> s{_ghlsssuPlaybackMode = a})

-- | Specifies which format should be used for packaging the media. Specifying the @FRAGMENTED_MP4@ container format packages the media into MP4 fragments (fMP4 or CMAF). This is the recommended packaging because there is minimal packaging overhead. The other container format option is @MPEG_TS@ . HLS has supported MPEG TS chunks since it was released and is sometimes the only supported packaging on older HLS players. MPEG TS typically has a 5-25 percent packaging overhead. This means MPEG TS typically requires 5-25 percent more bandwidth and cost than fMP4. The default is @FRAGMENTED_MP4@ .
ghlsssuContainerFormat :: Lens' GetHLSStreamingSessionURL (Maybe ContainerFormat)
ghlsssuContainerFormat = lens _ghlsssuContainerFormat (\ s a -> s{_ghlsssuContainerFormat = a})

-- | The maximum number of fragments that are returned in the HLS media playlists. When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number. When there are a higher number of fragments available in a live HLS media playlist, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live HLS media playlist have a minimum of 3 fragments and a maximum of 10 fragments. The default is 5 fragments if @PlaybackMode@ is @LIVE@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .  The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
ghlsssuMaxMediaPlaylistFragmentResults :: Lens' GetHLSStreamingSessionURL (Maybe Natural)
ghlsssuMaxMediaPlaylistFragmentResults = lens _ghlsssuMaxMediaPlaylistFragmentResults (\ s a -> s{_ghlsssuMaxMediaPlaylistFragmentResults = a}) . mapping _Nat

-- | Specifies when flags marking discontinuities between fragments will be added to the media playlists. The default is @ALWAYS@ when 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ , and @NEVER@ when it is @PRODUCER_TIMESTAMP@ . Media players typically build a timeline of media content to play, based on the timestamps of each fragment. This means that if there is any overlap between fragments (as is typical if 'HLSFragmentSelector' is @SERVER_TIMESTAMP@ ), the media player timeline has small gaps between fragments in some places, and overwrites frames in other places. When there are discontinuity flags between fragments, the media player is expected to reset the timeline, resulting in the fragment being played immediately after the previous fragment. We recommend that you always have discontinuity flags between fragments if the fragment timestamps are not accurate or if fragments might be missing. You should not place discontinuity flags between fragments for the player timeline to accurately map to the producer timestamps.
ghlsssuDiscontinuityMode :: Lens' GetHLSStreamingSessionURL (Maybe DiscontinuityMode)
ghlsssuDiscontinuityMode = lens _ghlsssuDiscontinuityMode (\ s a -> s{_ghlsssuDiscontinuityMode = a})

-- | The name of the stream for which to retrieve the HLS master playlist URL. You must specify either the @StreamName@ or the @StreamARN@ .
ghlsssuStreamName :: Lens' GetHLSStreamingSessionURL (Maybe Text)
ghlsssuStreamName = lens _ghlsssuStreamName (\ s a -> s{_ghlsssuStreamName = a})

instance AWSRequest GetHLSStreamingSessionURL where
        type Rs GetHLSStreamingSessionURL =
             GetHLSStreamingSessionURLResponse
        request = postJSON kinesisVideoArchivedMedia
        response
          = receiveJSON
              (\ s h x ->
                 GetHLSStreamingSessionURLResponse' <$>
                   (x .?> "HLSStreamingSessionURL") <*>
                     (pure (fromEnum s)))

instance Hashable GetHLSStreamingSessionURL where

instance NFData GetHLSStreamingSessionURL where

instance ToHeaders GetHLSStreamingSessionURL where
        toHeaders = const mempty

instance ToJSON GetHLSStreamingSessionURL where
        toJSON GetHLSStreamingSessionURL'{..}
          = object
              (catMaybes
                 [("DisplayFragmentTimestamp" .=) <$>
                    _ghlsssuDisplayFragmentTimestamp,
                  ("HLSFragmentSelector" .=) <$>
                    _ghlsssuHLSFragmentSelector,
                  ("Expires" .=) <$> _ghlsssuExpires,
                  ("StreamARN" .=) <$> _ghlsssuStreamARN,
                  ("PlaybackMode" .=) <$> _ghlsssuPlaybackMode,
                  ("ContainerFormat" .=) <$> _ghlsssuContainerFormat,
                  ("MaxMediaPlaylistFragmentResults" .=) <$>
                    _ghlsssuMaxMediaPlaylistFragmentResults,
                  ("DiscontinuityMode" .=) <$>
                    _ghlsssuDiscontinuityMode,
                  ("StreamName" .=) <$> _ghlsssuStreamName])

instance ToPath GetHLSStreamingSessionURL where
        toPath = const "/getHLSStreamingSessionURL"

instance ToQuery GetHLSStreamingSessionURL where
        toQuery = const mempty

-- | /See:/ 'getHLSStreamingSessionURLResponse' smart constructor.
data GetHLSStreamingSessionURLResponse = GetHLSStreamingSessionURLResponse'
  { _ghlsssursHLSStreamingSessionURL :: !(Maybe Text)
  , _ghlsssursResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetHLSStreamingSessionURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ghlsssursHLSStreamingSessionURL' - The URL (containing the session token) that a media player can use to retrieve the HLS master playlist.
--
-- * 'ghlsssursResponseStatus' - -- | The response status code.
getHLSStreamingSessionURLResponse
    :: Int -- ^ 'ghlsssursResponseStatus'
    -> GetHLSStreamingSessionURLResponse
getHLSStreamingSessionURLResponse pResponseStatus_ =
  GetHLSStreamingSessionURLResponse'
    { _ghlsssursHLSStreamingSessionURL = Nothing
    , _ghlsssursResponseStatus = pResponseStatus_
    }


-- | The URL (containing the session token) that a media player can use to retrieve the HLS master playlist.
ghlsssursHLSStreamingSessionURL :: Lens' GetHLSStreamingSessionURLResponse (Maybe Text)
ghlsssursHLSStreamingSessionURL = lens _ghlsssursHLSStreamingSessionURL (\ s a -> s{_ghlsssursHLSStreamingSessionURL = a})

-- | -- | The response status code.
ghlsssursResponseStatus :: Lens' GetHLSStreamingSessionURLResponse Int
ghlsssursResponseStatus = lens _ghlsssursResponseStatus (\ s a -> s{_ghlsssursResponseStatus = a})

instance NFData GetHLSStreamingSessionURLResponse
         where
