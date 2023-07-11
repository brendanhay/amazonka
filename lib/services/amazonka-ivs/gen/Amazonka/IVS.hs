{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.IVS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-07-14@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- __Introduction__
--
-- The Amazon Interactive Video Service (IVS) API is REST compatible, using
-- a standard HTTP API and an Amazon Web Services EventBridge event stream
-- for responses. JSON is used for both requests and responses, including
-- errors.
--
-- The API is an Amazon Web Services regional service. For a list of
-- supported regions and Amazon IVS HTTPS service endpoints, see the
-- <https://docs.aws.amazon.com/general/latest/gr/ivs.html Amazon IVS page>
-- in the /Amazon Web Services General Reference/.
--
-- /__All API request parameters and URLs are case sensitive.__/
--
-- For a summary of notable documentation changes in each release, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/doc-history.html Document History>.
--
-- __Allowed Header Values__
--
-- -   @ @__@Accept:@__@ @ application\/json
--
-- -   @ @__@Accept-Encoding:@__@ @ gzip, deflate
--
-- -   @ @__@Content-Type:@__@ @application\/json
--
-- __Resources__
--
-- The following resources contain information about your IVS live stream
-- (see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/getting-started.html Getting Started with Amazon IVS>):
--
-- -   Channel — Stores configuration data related to your live stream. You
--     first create a channel and then use the channel’s stream key to
--     start your live stream. See the Channel endpoints for more
--     information.
--
-- -   Stream key — An identifier assigned by Amazon IVS when you create a
--     channel, which is then used to authorize streaming. See the
--     StreamKey endpoints for more information. /__Treat the stream key
--     like a secret, since it allows anyone to stream to the channel.__/
--
-- -   Playback key pair — Video playback may be restricted using
--     playback-authorization tokens, which use public-key encryption. A
--     playback key pair is the public-private pair of keys used to sign
--     and validate the playback-authorization token. See the
--     PlaybackKeyPair endpoints for more information.
--
-- -   Recording configuration — Stores configuration related to recording
--     a live stream and where to store the recorded content. Multiple
--     channels can reference the same recording configuration. See the
--     Recording Configuration endpoints for more information.
--
-- __Tagging__
--
-- A /tag/ is a metadata label that you assign to an Amazon Web Services
-- resource. A tag comprises a /key/ and a /value/, both set by you. For
-- example, you might set a tag as @topic:nature@ to label a particular
-- video category. See
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- for more information, including restrictions that apply to tags and
-- \"Tag naming limits and requirements\"; Amazon IVS has no
-- service-specific constraints beyond what is documented there.
--
-- Tags can help you identify and organize your Amazon Web Services
-- resources. For example, you can use the same tag for different resources
-- to indicate that they are related. You can also use tags to manage
-- access (see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html Access Tags>).
--
-- The Amazon IVS API has these tag-related endpoints: TagResource,
-- UntagResource, and ListTagsForResource. The following resources support
-- tagging: Channels, Stream Keys, Playback Key Pairs, and Recording
-- Configurations.
--
-- At most 50 tags can be applied to a resource.
--
-- __Authentication versus Authorization__
--
-- Note the differences between these concepts:
--
-- -   /Authentication/ is about verifying identity. You need to be
--     authenticated to sign Amazon IVS API requests.
--
-- -   /Authorization/ is about granting permissions. Your IAM roles need
--     to have permissions for Amazon IVS API requests. In addition,
--     authorization is needed to view
--     <https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html Amazon IVS private channels>.
--     (Private channels are channels that are enabled for \"playback
--     authorization.\")
--
-- __Authentication__
--
-- All Amazon IVS API requests must be authenticated with a signature. The
-- Amazon Web Services Command-Line Interface (CLI) and Amazon IVS Player
-- SDKs take care of signing the underlying API calls for you. However, if
-- your application calls the Amazon IVS API directly, it’s your
-- responsibility to sign the requests.
--
-- You generate a signature using valid Amazon Web Services credentials
-- that have permission to perform the requested action. For example, you
-- must sign PutMetadata requests with a signature generated from an IAM
-- user account that has the @ivs:PutMetadata@ permission.
--
-- For more information:
--
-- -   Authentication and generating signatures — See
--     <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>
--     in the /Amazon Web Services General Reference/.
--
-- -   Managing Amazon IVS permissions — See
--     <https://docs.aws.amazon.com/ivs/latest/userguide/security-iam.html Identity and Access Management>
--     on the Security page of the /Amazon IVS User Guide/.
--
-- __Amazon Resource Names (ARNs)__
--
-- ARNs uniquely identify AWS resources. An ARN is required when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM
-- policies and API calls. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names>
-- in the /AWS General Reference/.
--
-- __Channel Endpoints__
--
-- -   CreateChannel — Creates a new channel and an associated stream key
--     to start streaming.
--
-- -   GetChannel — Gets the channel configuration for the specified
--     channel ARN.
--
-- -   BatchGetChannel — Performs GetChannel on multiple ARNs
--     simultaneously.
--
-- -   ListChannels — Gets summary information about all channels in your
--     account, in the Amazon Web Services region where the API request is
--     processed. This list can be filtered to match a specified name or
--     recording-configuration ARN. Filters are mutually exclusive and
--     cannot be used together. If you try to use both filters, you will
--     get an error (409 Conflict Exception).
--
-- -   UpdateChannel — Updates a channel\'s configuration. This does not
--     affect an ongoing stream of this channel. You must stop and restart
--     the stream for the changes to take effect.
--
-- -   DeleteChannel — Deletes the specified channel.
--
-- __StreamKey Endpoints__
--
-- -   CreateStreamKey — Creates a stream key, used to initiate a stream,
--     for the specified channel ARN.
--
-- -   GetStreamKey — Gets stream key information for the specified ARN.
--
-- -   BatchGetStreamKey — Performs GetStreamKey on multiple ARNs
--     simultaneously.
--
-- -   ListStreamKeys — Gets summary information about stream keys for the
--     specified channel.
--
-- -   DeleteStreamKey — Deletes the stream key for the specified ARN, so
--     it can no longer be used to stream.
--
-- __Stream Endpoints__
--
-- -   GetStream — Gets information about the active (live) stream on a
--     specified channel.
--
-- -   GetStreamSession — Gets metadata on a specified stream.
--
-- -   ListStreams — Gets summary information about live streams in your
--     account, in the Amazon Web Services region where the API request is
--     processed.
--
-- -   ListStreamSessions — Gets a summary of current and previous streams
--     for a specified channel in your account, in the AWS region where the
--     API request is processed.
--
-- -   StopStream — Disconnects the incoming RTMPS stream for the specified
--     channel. Can be used in conjunction with DeleteStreamKey to prevent
--     further streaming to a channel.
--
-- -   PutMetadata — Inserts metadata into the active stream of the
--     specified channel. At most 5 requests per second per channel are
--     allowed, each with a maximum 1 KB payload. (If 5 TPS is not
--     sufficient for your needs, we recommend batching your data into a
--     single PutMetadata call.) At most 155 requests per second per
--     account are allowed.
--
-- __PlaybackKeyPair Endpoints__
--
-- For more information, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html Setting Up Private Channels>
-- in the /Amazon IVS User Guide/.
--
-- -   ImportPlaybackKeyPair — Imports the public portion of a new key pair
--     and returns its @arn@ and @fingerprint@. The @privateKey@ can then
--     be used to generate viewer authorization tokens, to grant viewers
--     access to private channels (channels enabled for playback
--     authorization).
--
-- -   GetPlaybackKeyPair — Gets a specified playback authorization key
--     pair and returns the @arn@ and @fingerprint@. The @privateKey@ held
--     by the caller can be used to generate viewer authorization tokens,
--     to grant viewers access to private channels.
--
-- -   ListPlaybackKeyPairs — Gets summary information about playback key
--     pairs.
--
-- -   DeletePlaybackKeyPair — Deletes a specified authorization key pair.
--     This invalidates future viewer tokens generated using the key pair’s
--     @privateKey@.
--
-- __RecordingConfiguration Endpoints__
--
-- -   CreateRecordingConfiguration — Creates a new recording
--     configuration, used to enable recording to Amazon S3.
--
-- -   GetRecordingConfiguration — Gets the recording-configuration
--     metadata for the specified ARN.
--
-- -   ListRecordingConfigurations — Gets summary information about all
--     recording configurations in your account, in the Amazon Web Services
--     region where the API request is processed.
--
-- -   DeleteRecordingConfiguration — Deletes the recording configuration
--     for the specified ARN.
--
-- __Amazon Web Services Tags Endpoints__
--
-- -   TagResource — Adds or updates tags for the Amazon Web Services
--     resource with the specified ARN.
--
-- -   UntagResource — Removes tags from the resource with the specified
--     ARN.
--
-- -   ListTagsForResource — Gets information about Amazon Web Services
--     tags for the specified ARN.
module Amazonka.IVS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ChannelNotBroadcasting
    _ChannelNotBroadcasting,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** PendingVerification
    _PendingVerification,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** StreamUnavailable
    _StreamUnavailable,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchGetChannel
    BatchGetChannel (BatchGetChannel'),
    newBatchGetChannel,
    BatchGetChannelResponse (BatchGetChannelResponse'),
    newBatchGetChannelResponse,

    -- ** BatchGetStreamKey
    BatchGetStreamKey (BatchGetStreamKey'),
    newBatchGetStreamKey,
    BatchGetStreamKeyResponse (BatchGetStreamKeyResponse'),
    newBatchGetStreamKeyResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateRecordingConfiguration
    CreateRecordingConfiguration (CreateRecordingConfiguration'),
    newCreateRecordingConfiguration,
    CreateRecordingConfigurationResponse (CreateRecordingConfigurationResponse'),
    newCreateRecordingConfigurationResponse,

    -- ** CreateStreamKey
    CreateStreamKey (CreateStreamKey'),
    newCreateStreamKey,
    CreateStreamKeyResponse (CreateStreamKeyResponse'),
    newCreateStreamKeyResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeletePlaybackKeyPair
    DeletePlaybackKeyPair (DeletePlaybackKeyPair'),
    newDeletePlaybackKeyPair,
    DeletePlaybackKeyPairResponse (DeletePlaybackKeyPairResponse'),
    newDeletePlaybackKeyPairResponse,

    -- ** DeleteRecordingConfiguration
    DeleteRecordingConfiguration (DeleteRecordingConfiguration'),
    newDeleteRecordingConfiguration,
    DeleteRecordingConfigurationResponse (DeleteRecordingConfigurationResponse'),
    newDeleteRecordingConfigurationResponse,

    -- ** DeleteStreamKey
    DeleteStreamKey (DeleteStreamKey'),
    newDeleteStreamKey,
    DeleteStreamKeyResponse (DeleteStreamKeyResponse'),
    newDeleteStreamKeyResponse,

    -- ** GetChannel
    GetChannel (GetChannel'),
    newGetChannel,
    GetChannelResponse (GetChannelResponse'),
    newGetChannelResponse,

    -- ** GetPlaybackKeyPair
    GetPlaybackKeyPair (GetPlaybackKeyPair'),
    newGetPlaybackKeyPair,
    GetPlaybackKeyPairResponse (GetPlaybackKeyPairResponse'),
    newGetPlaybackKeyPairResponse,

    -- ** GetRecordingConfiguration
    GetRecordingConfiguration (GetRecordingConfiguration'),
    newGetRecordingConfiguration,
    GetRecordingConfigurationResponse (GetRecordingConfigurationResponse'),
    newGetRecordingConfigurationResponse,

    -- ** GetStream
    GetStream (GetStream'),
    newGetStream,
    GetStreamResponse (GetStreamResponse'),
    newGetStreamResponse,

    -- ** GetStreamKey
    GetStreamKey (GetStreamKey'),
    newGetStreamKey,
    GetStreamKeyResponse (GetStreamKeyResponse'),
    newGetStreamKeyResponse,

    -- ** GetStreamSession
    GetStreamSession (GetStreamSession'),
    newGetStreamSession,
    GetStreamSessionResponse (GetStreamSessionResponse'),
    newGetStreamSessionResponse,

    -- ** ImportPlaybackKeyPair
    ImportPlaybackKeyPair (ImportPlaybackKeyPair'),
    newImportPlaybackKeyPair,
    ImportPlaybackKeyPairResponse (ImportPlaybackKeyPairResponse'),
    newImportPlaybackKeyPairResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListPlaybackKeyPairs (Paginated)
    ListPlaybackKeyPairs (ListPlaybackKeyPairs'),
    newListPlaybackKeyPairs,
    ListPlaybackKeyPairsResponse (ListPlaybackKeyPairsResponse'),
    newListPlaybackKeyPairsResponse,

    -- ** ListRecordingConfigurations (Paginated)
    ListRecordingConfigurations (ListRecordingConfigurations'),
    newListRecordingConfigurations,
    ListRecordingConfigurationsResponse (ListRecordingConfigurationsResponse'),
    newListRecordingConfigurationsResponse,

    -- ** ListStreamKeys (Paginated)
    ListStreamKeys (ListStreamKeys'),
    newListStreamKeys,
    ListStreamKeysResponse (ListStreamKeysResponse'),
    newListStreamKeysResponse,

    -- ** ListStreamSessions
    ListStreamSessions (ListStreamSessions'),
    newListStreamSessions,
    ListStreamSessionsResponse (ListStreamSessionsResponse'),
    newListStreamSessionsResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutMetadata
    PutMetadata (PutMetadata'),
    newPutMetadata,
    PutMetadataResponse (PutMetadataResponse'),
    newPutMetadataResponse,

    -- ** StopStream
    StopStream (StopStream'),
    newStopStream,
    StopStreamResponse (StopStreamResponse'),
    newStopStreamResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- * Types

    -- ** ChannelLatencyMode
    ChannelLatencyMode (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** RecordingConfigurationState
    RecordingConfigurationState (..),

    -- ** RecordingMode
    RecordingMode (..),

    -- ** StreamHealth
    StreamHealth (..),

    -- ** StreamState
    StreamState (..),

    -- ** AudioConfiguration
    AudioConfiguration (AudioConfiguration'),
    newAudioConfiguration,

    -- ** BatchError
    BatchError (BatchError'),
    newBatchError,

    -- ** Channel
    Channel (Channel'),
    newChannel,

    -- ** ChannelSummary
    ChannelSummary (ChannelSummary'),
    newChannelSummary,

    -- ** DestinationConfiguration
    DestinationConfiguration (DestinationConfiguration'),
    newDestinationConfiguration,

    -- ** IngestConfiguration
    IngestConfiguration (IngestConfiguration'),
    newIngestConfiguration,

    -- ** PlaybackKeyPair
    PlaybackKeyPair (PlaybackKeyPair'),
    newPlaybackKeyPair,

    -- ** PlaybackKeyPairSummary
    PlaybackKeyPairSummary (PlaybackKeyPairSummary'),
    newPlaybackKeyPairSummary,

    -- ** RecordingConfiguration
    RecordingConfiguration (RecordingConfiguration'),
    newRecordingConfiguration,

    -- ** RecordingConfigurationSummary
    RecordingConfigurationSummary (RecordingConfigurationSummary'),
    newRecordingConfigurationSummary,

    -- ** S3DestinationConfiguration
    S3DestinationConfiguration (S3DestinationConfiguration'),
    newS3DestinationConfiguration,

    -- ** Stream
    Stream (Stream'),
    newStream,

    -- ** StreamEvent
    StreamEvent (StreamEvent'),
    newStreamEvent,

    -- ** StreamFilters
    StreamFilters (StreamFilters'),
    newStreamFilters,

    -- ** StreamKey
    StreamKey (StreamKey'),
    newStreamKey,

    -- ** StreamKeySummary
    StreamKeySummary (StreamKeySummary'),
    newStreamKeySummary,

    -- ** StreamSession
    StreamSession (StreamSession'),
    newStreamSession,

    -- ** StreamSessionSummary
    StreamSessionSummary (StreamSessionSummary'),
    newStreamSessionSummary,

    -- ** StreamSummary
    StreamSummary (StreamSummary'),
    newStreamSummary,

    -- ** ThumbnailConfiguration
    ThumbnailConfiguration (ThumbnailConfiguration'),
    newThumbnailConfiguration,

    -- ** VideoConfiguration
    VideoConfiguration (VideoConfiguration'),
    newVideoConfiguration,
  )
where

import Amazonka.IVS.BatchGetChannel
import Amazonka.IVS.BatchGetStreamKey
import Amazonka.IVS.CreateChannel
import Amazonka.IVS.CreateRecordingConfiguration
import Amazonka.IVS.CreateStreamKey
import Amazonka.IVS.DeleteChannel
import Amazonka.IVS.DeletePlaybackKeyPair
import Amazonka.IVS.DeleteRecordingConfiguration
import Amazonka.IVS.DeleteStreamKey
import Amazonka.IVS.GetChannel
import Amazonka.IVS.GetPlaybackKeyPair
import Amazonka.IVS.GetRecordingConfiguration
import Amazonka.IVS.GetStream
import Amazonka.IVS.GetStreamKey
import Amazonka.IVS.GetStreamSession
import Amazonka.IVS.ImportPlaybackKeyPair
import Amazonka.IVS.Lens
import Amazonka.IVS.ListChannels
import Amazonka.IVS.ListPlaybackKeyPairs
import Amazonka.IVS.ListRecordingConfigurations
import Amazonka.IVS.ListStreamKeys
import Amazonka.IVS.ListStreamSessions
import Amazonka.IVS.ListStreams
import Amazonka.IVS.ListTagsForResource
import Amazonka.IVS.PutMetadata
import Amazonka.IVS.StopStream
import Amazonka.IVS.TagResource
import Amazonka.IVS.Types
import Amazonka.IVS.UntagResource
import Amazonka.IVS.UpdateChannel
import Amazonka.IVS.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IVS'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
