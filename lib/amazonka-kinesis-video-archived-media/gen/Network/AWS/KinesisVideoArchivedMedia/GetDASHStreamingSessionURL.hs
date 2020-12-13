{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an MPEG Dynamic Adaptive Streaming over HTTP (DASH) URL for the stream. You can then open the URL in a media player to view the stream contents.
--
-- Both the @StreamName@ and the @StreamARN@ parameters are optional, but you must specify either the @StreamName@ or the @StreamARN@ when invoking this API operation.
-- An Amazon Kinesis video stream has the following requirements for providing data through MPEG-DASH:
--
--     * The media must contain h.264 or h.265 encoded video and, optionally, AAC or G.711 encoded audio. Specifically, the codec ID of track 1 should be @V_MPEG/ISO/AVC@ (for h.264) or V_MPEGH/ISO/HEVC (for H.265). Optionally, the codec ID of track 2 should be @A_AAC@ (for AAC) or A_MS/ACM (for G.711).
--
--
--     * Data retention must be greater than 0.
--
--
--     * The video track of each fragment must contain codec private data in the Advanced Video Coding (AVC) for H.264 format and HEVC for H.265 format. For more information, see <https://www.iso.org/standard/55980.html MPEG-4 specification ISO/IEC 14496-15> . For information about adapting stream data to a given format, see <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/producer-reference-nal.html NAL Adaptation Flags> .
--
--
--     * The audio track (if present) of each fragment must contain codec private data in the AAC format (<https://www.iso.org/standard/43345.html AAC specification ISO/IEC 13818-7> ) or the <http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html MS Wave format> .
--
--
-- The following procedure shows how to use MPEG-DASH with Kinesis Video Streams:
--
--     * Get an endpoint using <http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/API_GetDataEndpoint.html GetDataEndpoint> , specifying @GET_DASH_STREAMING_SESSION_URL@ for the @APIName@ parameter.
--
--
--     * Retrieve the MPEG-DASH URL using @GetDASHStreamingSessionURL@ . Kinesis Video Streams creates an MPEG-DASH streaming session to be used for accessing content in a stream using the MPEG-DASH protocol. @GetDASHStreamingSessionURL@ returns an authenticated URL (that includes an encrypted session token) for the session's MPEG-DASH /manifest/ (the root resource needed for streaming with MPEG-DASH).
-- The media that is made available through the manifest consists only of the requested stream, time range, and format. No other media data (such as frames outside the requested window or alternate bitrates) is made available.
--
--
--     * Provide the URL (containing the encrypted session token) for the MPEG-DASH manifest to a media player that supports the MPEG-DASH protocol. Kinesis Video Streams makes the initialization fragment and media fragments available through the manifest URL. The initialization fragment contains the codec private data for the stream, and other data needed to set up the video or audio decoder and renderer. The media fragments contain encoded video frames or encoded audio samples.
--
--
--     * The media player receives the authenticated URL and requests stream metadata and media data normally. When the media player requests data, it calls the following actions:
--
--     * __GetDASHManifest:__ Retrieves an MPEG DASH manifest, which contains the metadata for the media that you want to playback.
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
module Network.AWS.KinesisVideoArchivedMedia.GetDASHStreamingSessionURL
  ( -- * Creating a request
    GetDASHStreamingSessionURL (..),
    mkGetDASHStreamingSessionURL,

    -- ** Request lenses
    gdashssuDisplayFragmentTimestamp,
    gdashssuExpires,
    gdashssuDASHFragmentSelector,
    gdashssuMaxManifestFragmentResults,
    gdashssuStreamARN,
    gdashssuPlaybackMode,
    gdashssuStreamName,
    gdashssuDisplayFragmentNumber,

    -- * Destructuring the response
    GetDASHStreamingSessionURLResponse (..),
    mkGetDASHStreamingSessionURLResponse,

    -- ** Response lenses
    gdashssursDASHStreamingSessionURL,
    gdashssursResponseStatus,
  )
where

import Network.AWS.KinesisVideoArchivedMedia.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDASHStreamingSessionURL' smart constructor.
data GetDASHStreamingSessionURL = GetDASHStreamingSessionURL'
  { -- | Per the MPEG-DASH specification, the wall-clock time of fragments in the manifest file can be derived using attributes in the manifest itself. However, typically, MPEG-DASH compatible media players do not properly handle gaps in the media timeline. Kinesis Video Streams adjusts the media timeline in the manifest file to enable playback of media with discontinuities. Therefore, the wall-clock time derived from the manifest file may be inaccurate. If DisplayFragmentTimestamp is set to @ALWAYS@ , the accurate fragment timestamp is added to each S element in the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH media player is necessary to leverage this custom attribute.
    --
    -- The default value is @NEVER@ . When 'DASHFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'DASHFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
    displayFragmentTimestamp :: Lude.Maybe DASHDisplayFragmentTimestamp,
    -- | The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours).
    --
    -- When a session expires, no new calls to @GetDashManifest@ , @GetMP4InitFragment@ , or @GetMP4MediaFragment@ can be made for that session.
    -- The default is 300 (5 minutes).
    expires :: Lude.Maybe Lude.Natural,
    -- | The time range of the requested fragment and the source of the timestamps.
    --
    -- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ . This parameter is optional if PlaybackMode is@LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
    dASHFragmentSelector :: Lude.Maybe DASHFragmentSelector,
    -- | The maximum number of fragments that are returned in the MPEG-DASH manifest.
    --
    -- When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number.
    -- When there are a higher number of fragments available in a live MPEG-DASH manifest, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live MPEG-DASH manifest have a minimum of 3 fragments and a maximum of 10 fragments.
    -- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .
    -- The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
    maxManifestFragmentResults :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the stream for which to retrieve the MPEG-DASH manifest URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@ .
    streamARN :: Lude.Maybe Lude.Text,
    -- | Whether to retrieve live, live replay, or archived, on-demand data.
    --
    -- Features of the three types of sessions include the following:
    --
    --     * __@LIVE@ __ : For sessions of this type, the MPEG-DASH manifest is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new manifest on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.
    --
    --
    --     * __@LIVE_REPLAY@ __ : For sessions of this type, the MPEG-DASH manifest is updated similarly to how it is updated for @LIVE@ mode except that it starts by including fragments from a given start time. Instead of fragments being added as they are ingested, fragments are added as the duration of the next fragment elapses. For example, if the fragments in the session are two seconds long, then a new fragment is added to the manifest every two seconds. This mode is useful to be able to start playback from when an event is detected and continue live streaming media that has not yet been ingested as of the time of the session creation. This mode is also useful to stream previously archived media without being limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
    --
    --
    --     * __@ON_DEMAND@ __ : For sessions of this type, the MPEG-DASH manifest contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The manifest must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display.
    --
    --
    -- In all playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the MPEG-DASH manifest. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the MPEG-DASH manifest. This can lead to unexpected behavior in the media player.
    -- The default is @LIVE@ .
    playbackMode :: Lude.Maybe DASHPlaybackMode,
    -- | The name of the stream for which to retrieve the MPEG-DASH manifest URL.
    --
    -- You must specify either the @StreamName@ or the @StreamARN@ .
    streamName :: Lude.Maybe Lude.Text,
    -- | Fragments are identified in the manifest file based on their sequence number in the session. If DisplayFragmentNumber is set to @ALWAYS@ , the Kinesis Video Streams fragment number is added to each S element in the manifest file with the attribute name “kvs:fn”. These fragment numbers can be used for logging or for use with other APIs (e.g. @GetMedia@ and @GetMediaForFragmentList@ ). A custom MPEG-DASH media player is necessary to leverage these this custom attribute.
    --
    -- The default value is @NEVER@ .
    displayFragmentNumber :: Lude.Maybe DASHDisplayFragmentNumber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDASHStreamingSessionURL' with the minimum fields required to make a request.
--
-- * 'displayFragmentTimestamp' - Per the MPEG-DASH specification, the wall-clock time of fragments in the manifest file can be derived using attributes in the manifest itself. However, typically, MPEG-DASH compatible media players do not properly handle gaps in the media timeline. Kinesis Video Streams adjusts the media timeline in the manifest file to enable playback of media with discontinuities. Therefore, the wall-clock time derived from the manifest file may be inaccurate. If DisplayFragmentTimestamp is set to @ALWAYS@ , the accurate fragment timestamp is added to each S element in the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH media player is necessary to leverage this custom attribute.
--
-- The default value is @NEVER@ . When 'DASHFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'DASHFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
-- * 'expires' - The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetDashManifest@ , @GetMP4InitFragment@ , or @GetMP4MediaFragment@ can be made for that session.
-- The default is 300 (5 minutes).
-- * 'dASHFragmentSelector' - The time range of the requested fragment and the source of the timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ . This parameter is optional if PlaybackMode is@LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
-- * 'maxManifestFragmentResults' - The maximum number of fragments that are returned in the MPEG-DASH manifest.
--
-- When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number.
-- When there are a higher number of fragments available in a live MPEG-DASH manifest, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live MPEG-DASH manifest have a minimum of 3 fragments and a maximum of 10 fragments.
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
-- * 'streamARN' - The Amazon Resource Name (ARN) of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
-- * 'playbackMode' - Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
--     * __@LIVE@ __ : For sessions of this type, the MPEG-DASH manifest is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new manifest on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.
--
--
--     * __@LIVE_REPLAY@ __ : For sessions of this type, the MPEG-DASH manifest is updated similarly to how it is updated for @LIVE@ mode except that it starts by including fragments from a given start time. Instead of fragments being added as they are ingested, fragments are added as the duration of the next fragment elapses. For example, if the fragments in the session are two seconds long, then a new fragment is added to the manifest every two seconds. This mode is useful to be able to start playback from when an event is detected and continue live streaming media that has not yet been ingested as of the time of the session creation. This mode is also useful to stream previously archived media without being limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
--
--     * __@ON_DEMAND@ __ : For sessions of this type, the MPEG-DASH manifest contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The manifest must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display.
--
--
-- In all playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the MPEG-DASH manifest. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the MPEG-DASH manifest. This can lead to unexpected behavior in the media player.
-- The default is @LIVE@ .
-- * 'streamName' - The name of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
-- * 'displayFragmentNumber' - Fragments are identified in the manifest file based on their sequence number in the session. If DisplayFragmentNumber is set to @ALWAYS@ , the Kinesis Video Streams fragment number is added to each S element in the manifest file with the attribute name “kvs:fn”. These fragment numbers can be used for logging or for use with other APIs (e.g. @GetMedia@ and @GetMediaForFragmentList@ ). A custom MPEG-DASH media player is necessary to leverage these this custom attribute.
--
-- The default value is @NEVER@ .
mkGetDASHStreamingSessionURL ::
  GetDASHStreamingSessionURL
mkGetDASHStreamingSessionURL =
  GetDASHStreamingSessionURL'
    { displayFragmentTimestamp =
        Lude.Nothing,
      expires = Lude.Nothing,
      dASHFragmentSelector = Lude.Nothing,
      maxManifestFragmentResults = Lude.Nothing,
      streamARN = Lude.Nothing,
      playbackMode = Lude.Nothing,
      streamName = Lude.Nothing,
      displayFragmentNumber = Lude.Nothing
    }

-- | Per the MPEG-DASH specification, the wall-clock time of fragments in the manifest file can be derived using attributes in the manifest itself. However, typically, MPEG-DASH compatible media players do not properly handle gaps in the media timeline. Kinesis Video Streams adjusts the media timeline in the manifest file to enable playback of media with discontinuities. Therefore, the wall-clock time derived from the manifest file may be inaccurate. If DisplayFragmentTimestamp is set to @ALWAYS@ , the accurate fragment timestamp is added to each S element in the manifest file with the attribute name “kvs:ts”. A custom MPEG-DASH media player is necessary to leverage this custom attribute.
--
-- The default value is @NEVER@ . When 'DASHFragmentSelector' is @SERVER_TIMESTAMP@ , the timestamps will be the server start timestamps. Similarly, when 'DASHFragmentSelector' is @PRODUCER_TIMESTAMP@ , the timestamps will be the producer start timestamps.
--
-- /Note:/ Consider using 'displayFragmentTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuDisplayFragmentTimestamp :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe DASHDisplayFragmentTimestamp)
gdashssuDisplayFragmentTimestamp = Lens.lens (displayFragmentTimestamp :: GetDASHStreamingSessionURL -> Lude.Maybe DASHDisplayFragmentTimestamp) (\s a -> s {displayFragmentTimestamp = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuDisplayFragmentTimestamp "Use generic-lens or generic-optics with 'displayFragmentTimestamp' instead." #-}

-- | The time in seconds until the requested session expires. This value can be between 300 (5 minutes) and 43200 (12 hours).
--
-- When a session expires, no new calls to @GetDashManifest@ , @GetMP4InitFragment@ , or @GetMP4MediaFragment@ can be made for that session.
-- The default is 300 (5 minutes).
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuExpires :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe Lude.Natural)
gdashssuExpires = Lens.lens (expires :: GetDASHStreamingSessionURL -> Lude.Maybe Lude.Natural) (\s a -> s {expires = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The time range of the requested fragment and the source of the timestamps.
--
-- This parameter is required if @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ . This parameter is optional if PlaybackMode is@LIVE@ . If @PlaybackMode@ is @LIVE@ , the @FragmentSelectorType@ can be set, but the @TimestampRange@ should not be set. If @PlaybackMode@ is @ON_DEMAND@ or @LIVE_REPLAY@ , both @FragmentSelectorType@ and @TimestampRange@ must be set.
--
-- /Note:/ Consider using 'dASHFragmentSelector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuDASHFragmentSelector :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe DASHFragmentSelector)
gdashssuDASHFragmentSelector = Lens.lens (dASHFragmentSelector :: GetDASHStreamingSessionURL -> Lude.Maybe DASHFragmentSelector) (\s a -> s {dASHFragmentSelector = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuDASHFragmentSelector "Use generic-lens or generic-optics with 'dASHFragmentSelector' instead." #-}

-- | The maximum number of fragments that are returned in the MPEG-DASH manifest.
--
-- When the @PlaybackMode@ is @LIVE@ , the most recent fragments are returned up to this value. When the @PlaybackMode@ is @ON_DEMAND@ , the oldest fragments are returned, up to this maximum number.
-- When there are a higher number of fragments available in a live MPEG-DASH manifest, video players often buffer content before starting playback. Increasing the buffer size increases the playback latency, but it decreases the likelihood that rebuffering will occur during playback. We recommend that a live MPEG-DASH manifest have a minimum of 3 fragments and a maximum of 10 fragments.
-- The default is 5 fragments if @PlaybackMode@ is @LIVE@ or @LIVE_REPLAY@ , and 1,000 if @PlaybackMode@ is @ON_DEMAND@ .
-- The maximum value of 1,000 fragments corresponds to more than 16 minutes of video on streams with 1-second fragments, and more than 2 1/2 hours of video on streams with 10-second fragments.
--
-- /Note:/ Consider using 'maxManifestFragmentResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuMaxManifestFragmentResults :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe Lude.Natural)
gdashssuMaxManifestFragmentResults = Lens.lens (maxManifestFragmentResults :: GetDASHStreamingSessionURL -> Lude.Maybe Lude.Natural) (\s a -> s {maxManifestFragmentResults = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuMaxManifestFragmentResults "Use generic-lens or generic-optics with 'maxManifestFragmentResults' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuStreamARN :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe Lude.Text)
gdashssuStreamARN = Lens.lens (streamARN :: GetDASHStreamingSessionURL -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | Whether to retrieve live, live replay, or archived, on-demand data.
--
-- Features of the three types of sessions include the following:
--
--     * __@LIVE@ __ : For sessions of this type, the MPEG-DASH manifest is continually updated with the latest fragments as they become available. We recommend that the media player retrieve a new manifest on a one-second interval. When this type of session is played in a media player, the user interface typically displays a "live" notification, with no scrubber control for choosing the position in the playback window to display.
--
--
--     * __@LIVE_REPLAY@ __ : For sessions of this type, the MPEG-DASH manifest is updated similarly to how it is updated for @LIVE@ mode except that it starts by including fragments from a given start time. Instead of fragments being added as they are ingested, fragments are added as the duration of the next fragment elapses. For example, if the fragments in the session are two seconds long, then a new fragment is added to the manifest every two seconds. This mode is useful to be able to start playback from when an event is detected and continue live streaming media that has not yet been ingested as of the time of the session creation. This mode is also useful to stream previously archived media without being limited by the 1,000 fragment limit in the @ON_DEMAND@ mode.
--
--
--     * __@ON_DEMAND@ __ : For sessions of this type, the MPEG-DASH manifest contains all the fragments for the session, up to the number that is specified in @MaxMediaPlaylistFragmentResults@ . The manifest must be retrieved only once for each session. When this type of session is played in a media player, the user interface typically displays a scrubber control for choosing the position in the playback window to display.
--
--
-- In all playback modes, if @FragmentSelectorType@ is @PRODUCER_TIMESTAMP@ , and if there are multiple fragments with the same start timestamp, the fragment that has the larger fragment number (that is, the newer fragment) is included in the MPEG-DASH manifest. The other fragments are not included. Fragments that have different timestamps but have overlapping durations are still included in the MPEG-DASH manifest. This can lead to unexpected behavior in the media player.
-- The default is @LIVE@ .
--
-- /Note:/ Consider using 'playbackMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuPlaybackMode :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe DASHPlaybackMode)
gdashssuPlaybackMode = Lens.lens (playbackMode :: GetDASHStreamingSessionURL -> Lude.Maybe DASHPlaybackMode) (\s a -> s {playbackMode = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuPlaybackMode "Use generic-lens or generic-optics with 'playbackMode' instead." #-}

-- | The name of the stream for which to retrieve the MPEG-DASH manifest URL.
--
-- You must specify either the @StreamName@ or the @StreamARN@ .
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuStreamName :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe Lude.Text)
gdashssuStreamName = Lens.lens (streamName :: GetDASHStreamingSessionURL -> Lude.Maybe Lude.Text) (\s a -> s {streamName = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | Fragments are identified in the manifest file based on their sequence number in the session. If DisplayFragmentNumber is set to @ALWAYS@ , the Kinesis Video Streams fragment number is added to each S element in the manifest file with the attribute name “kvs:fn”. These fragment numbers can be used for logging or for use with other APIs (e.g. @GetMedia@ and @GetMediaForFragmentList@ ). A custom MPEG-DASH media player is necessary to leverage these this custom attribute.
--
-- The default value is @NEVER@ .
--
-- /Note:/ Consider using 'displayFragmentNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssuDisplayFragmentNumber :: Lens.Lens' GetDASHStreamingSessionURL (Lude.Maybe DASHDisplayFragmentNumber)
gdashssuDisplayFragmentNumber = Lens.lens (displayFragmentNumber :: GetDASHStreamingSessionURL -> Lude.Maybe DASHDisplayFragmentNumber) (\s a -> s {displayFragmentNumber = a} :: GetDASHStreamingSessionURL)
{-# DEPRECATED gdashssuDisplayFragmentNumber "Use generic-lens or generic-optics with 'displayFragmentNumber' instead." #-}

instance Lude.AWSRequest GetDASHStreamingSessionURL where
  type
    Rs GetDASHStreamingSessionURL =
      GetDASHStreamingSessionURLResponse
  request = Req.postJSON kinesisVideoArchivedMediaService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDASHStreamingSessionURLResponse'
            Lude.<$> (x Lude..?> "DASHStreamingSessionURL")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDASHStreamingSessionURL where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetDASHStreamingSessionURL where
  toJSON GetDASHStreamingSessionURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DisplayFragmentTimestamp" Lude..=)
              Lude.<$> displayFragmentTimestamp,
            ("Expires" Lude..=) Lude.<$> expires,
            ("DASHFragmentSelector" Lude..=) Lude.<$> dASHFragmentSelector,
            ("MaxManifestFragmentResults" Lude..=)
              Lude.<$> maxManifestFragmentResults,
            ("StreamARN" Lude..=) Lude.<$> streamARN,
            ("PlaybackMode" Lude..=) Lude.<$> playbackMode,
            ("StreamName" Lude..=) Lude.<$> streamName,
            ("DisplayFragmentNumber" Lude..=) Lude.<$> displayFragmentNumber
          ]
      )

instance Lude.ToPath GetDASHStreamingSessionURL where
  toPath = Lude.const "/getDASHStreamingSessionURL"

instance Lude.ToQuery GetDASHStreamingSessionURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDASHStreamingSessionURLResponse' smart constructor.
data GetDASHStreamingSessionURLResponse = GetDASHStreamingSessionURLResponse'
  { -- | The URL (containing the session token) that a media player can use to retrieve the MPEG-DASH manifest.
    dASHStreamingSessionURL :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDASHStreamingSessionURLResponse' with the minimum fields required to make a request.
--
-- * 'dASHStreamingSessionURL' - The URL (containing the session token) that a media player can use to retrieve the MPEG-DASH manifest.
-- * 'responseStatus' - The response status code.
mkGetDASHStreamingSessionURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDASHStreamingSessionURLResponse
mkGetDASHStreamingSessionURLResponse pResponseStatus_ =
  GetDASHStreamingSessionURLResponse'
    { dASHStreamingSessionURL =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The URL (containing the session token) that a media player can use to retrieve the MPEG-DASH manifest.
--
-- /Note:/ Consider using 'dASHStreamingSessionURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssursDASHStreamingSessionURL :: Lens.Lens' GetDASHStreamingSessionURLResponse (Lude.Maybe Lude.Text)
gdashssursDASHStreamingSessionURL = Lens.lens (dASHStreamingSessionURL :: GetDASHStreamingSessionURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {dASHStreamingSessionURL = a} :: GetDASHStreamingSessionURLResponse)
{-# DEPRECATED gdashssursDASHStreamingSessionURL "Use generic-lens or generic-optics with 'dASHStreamingSessionURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdashssursResponseStatus :: Lens.Lens' GetDASHStreamingSessionURLResponse Lude.Int
gdashssursResponseStatus = Lens.lens (responseStatus :: GetDASHStreamingSessionURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDASHStreamingSessionURLResponse)
{-# DEPRECATED gdashssursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
