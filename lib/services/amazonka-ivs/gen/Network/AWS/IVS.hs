{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IVS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
-- >  <p> <b>Allowed Header Values</b> </p> <ul> <li> <p> <code> <b>Accept:</b> </code> application/json</p> </li> <li> <p> <code> <b>Accept-Encoding:</b> </code> gzip, deflate</p> </li> <li> <p> <code> <b>Content-Type:</b> </code>application/json</p> </li> </ul> <p> <b>Resources</b> </p> <p>The following resources contain information about your IVS live stream (see <a href="https://docs.aws.amazon.com/ivs/latest/userguide/getting-started.html"> Getting Started with Amazon IVS</a>):</p> <ul> <li> <p>Channel — Stores configuration data related to your live stream. You first create a channel and then use the channel’s stream key to start your live stream. See the Channel endpoints for more information. </p> </li> <li> <p>Stream key — An identifier assigned by Amazon IVS when you create a channel, which is then used to authorize streaming. See the StreamKey endpoints for more information. <i> <b>Treat the stream key like a secret, since it allows anyone to stream to the channel.</b> </i> </p> </li> <li> <p>Playback key pair — Video playback may be restricted using playback-authorization tokens, which use public-key encryption. A playback key pair is the public-private pair of keys used to sign and validate the playback-authorization token. See the PlaybackKeyPair endpoints for more information.</p> </li> <li> <p>Recording configuration — Stores configuration related to recording a live stream and where to store the recorded content. Multiple channels can reference the same recording configuration. See the Recording Configuration endpoints for more information.</p> </li> </ul> <p> <b>Tagging</b> </p> <p>A <i>tag</i> is a metadata label that you assign to an Amazon Web Services resource. A tag comprises a <i>key</i> and a <i>value</i>, both set by you. For example, you might set a tag as <code>topic:nature</code> to label a particular video category. See <a href="https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html">Tagging Amazon Web Services Resources</a> for more information, including restrictions that apply to tags.</p> <p>Tags can help you identify and organize your Amazon Web Services resources. For example, you can use the same tag for different resources to indicate that they are related. You can also use tags to manage access (see <a href="https://docs.aws.amazon.com/IAM/latest/UserGuide/access_tags.html"> Access Tags</a>). </p> <p>The Amazon IVS API has these tag-related endpoints: <a>TagResource</a>, <a>UntagResource</a>, and <a>ListTagsForResource</a>. The following resources support tagging: Channels, Stream Keys, Playback Key Pairs, and Recording Configurations.</p> <p>At most 50 tags can be applied to a resource. </p> <p> <b>Authentication versus Authorization</b> </p> <p>Note the differences between these concepts:</p> <ul> <li> <p> <i>Authentication</i> is about verifying identity. You need to be authenticated to sign Amazon IVS API requests.</p> </li> <li> <p> <i>Authorization</i> is about granting permissions. You need to be authorized to view <a href="https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html">Amazon IVS private channels</a>. (Private channels are channels that are enabled for "playback authorization.")</p> </li> </ul> <p> <b>Authentication</b> </p> <p>All Amazon IVS API requests must be authenticated with a signature. The Amazon Web Services Command-Line Interface (CLI) and Amazon IVS Player SDKs take care of signing the underlying API calls for you. However, if your application calls the Amazon IVS API directly, it’s your responsibility to sign the requests.</p> <p>You generate a signature using valid Amazon Web Services credentials that have permission to perform the requested action. For example, you must sign PutMetadata requests with a signature generated from an IAM user account that has the <code>ivs:PutMetadata</code> permission.</p> <p>For more information:</p> <ul> <li> <p>Authentication and generating signatures — See <a href="https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html">Authenticating Requests (Amazon Web Services Signature Version 4)</a> in the <i>Amazon Web Services General Reference</i>.</p> </li> <li> <p>Managing Amazon IVS permissions — See <a href="https://docs.aws.amazon.com/ivs/latest/userguide/security-iam.html">Identity and Access Management</a> on the Security page of the <i>Amazon IVS User Guide</i>.</p> </li> </ul> <p> <b>Channel Endpoints</b> </p> <ul> <li> <p> <a>CreateChannel</a> — Creates a new channel and an associated stream key to start streaming.</p> </li> <li> <p> <a>GetChannel</a> — Gets the channel configuration for the specified channel ARN (Amazon Resource Name).</p> </li> <li> <p> <a>BatchGetChannel</a> — Performs <a>GetChannel</a> on multiple ARNs simultaneously.</p> </li> <li> <p> <a>ListChannels</a> — Gets summary information about all channels in your account, in the Amazon Web Services region where the API request is processed. This list can be filtered to match a specified name or recording-configuration ARN. Filters are mutually exclusive and cannot be used together. If you try to use both filters, you will get an error (409 Conflict Exception).</p> </li> <li> <p> <a>UpdateChannel</a> — Updates a channel's configuration. This does not affect an ongoing stream of this channel. You must stop and restart the stream for the changes to take effect.</p> </li> <li> <p> <a>DeleteChannel</a> — Deletes the specified channel.</p> </li> </ul> <p> <b>StreamKey Endpoints</b> </p> <ul> <li> <p> <a>CreateStreamKey</a> — Creates a stream key, used to initiate a stream, for the specified channel ARN.</p> </li> <li> <p> <a>GetStreamKey</a> — Gets stream key information for the specified ARN.</p> </li> <li> <p> <a>BatchGetStreamKey</a> — Performs <a>GetStreamKey</a> on multiple ARNs simultaneously.</p> </li> <li> <p> <a>ListStreamKeys</a> — Gets summary information about stream keys for the specified channel.</p> </li> <li> <p> <a>DeleteStreamKey</a> — Deletes the stream key for the specified ARN, so it can no longer be used to stream.</p> </li> </ul> <p> <b>Stream Endpoints</b> </p> <ul> <li> <p> <a>GetStream</a> — Gets information about the active (live) stream on a specified channel.</p> </li> <li> <p> <a>ListStreams</a> — Gets summary information about live streams in your account, in the Amazon Web Services region where the API request is processed.</p> </li> <li> <p> <a>StopStream</a> — Disconnects the incoming RTMPS stream for the specified channel. Can be used in conjunction with <a>DeleteStreamKey</a> to prevent further streaming to a channel.</p> </li> <li> <p> <a>PutMetadata</a> — Inserts metadata into the active stream of the specified channel. At most 5 requests per second per channel are allowed, each with a maximum 1 KB payload. (If 5 TPS is not sufficient for your needs, we recommend batching your data into a single PutMetadata call.) At most 155 requests per second per account are allowed.</p> </li> </ul> <p> <b>PlaybackKeyPair Endpoints</b> </p> <p>For more information, see <a href="https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html">Setting Up Private Channels</a> in the <i>Amazon IVS User Guide</i>.</p> <ul> <li> <p> <a>ImportPlaybackKeyPair</a> — Imports the public portion of a new key pair and returns its <code>arn</code> and <code>fingerprint</code>. The <code>privateKey</code> can then be used to generate viewer authorization tokens, to grant viewers access to private channels (channels enabled for playback authorization).</p> </li> <li> <p> <a>GetPlaybackKeyPair</a> — Gets a specified playback authorization key pair and returns the <code>arn</code> and <code>fingerprint</code>. The <code>privateKey</code> held by the caller can be used to generate viewer authorization tokens, to grant viewers access to private channels.</p> </li> <li> <p> <a>ListPlaybackKeyPairs</a> — Gets summary information about playback key pairs.</p> </li> <li> <p> <a>DeletePlaybackKeyPair</a> — Deletes a specified authorization key pair. This invalidates future viewer tokens generated using the key pair’s <code>privateKey</code>.</p> </li> </ul> <p> <b>RecordingConfiguration Endpoints</b> </p> <ul> <li> <p> <a>CreateRecordingConfiguration</a> — Creates a new recording configuration, used to enable recording to Amazon S3.</p> </li> <li> <p> <a>GetRecordingConfiguration</a> — Gets the recording-configuration metadata for the specified ARN.</p> </li> <li> <p> <a>ListRecordingConfigurations</a> — Gets summary information about all recording configurations in your account, in the Amazon Web Services region where the API request is processed.</p> </li> <li> <p> <a>DeleteRecordingConfiguration</a> — Deletes the recording configuration for the specified ARN.</p> </li> </ul> <p> <b>Amazon Web Services Tags Endpoints</b> </p> <ul> <li> <p> <a>TagResource</a> — Adds or updates tags for the Amazon Web Services resource with the specified ARN.</p> </li> <li> <p> <a>UntagResource</a> — Removes tags from the resource with the specified ARN.</p> </li> <li> <p> <a>ListTagsForResource</a> — Gets information about Amazon Web Services tags for the specified ARN.</p> </li> </ul>
module Network.AWS.IVS
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** StreamUnavailable
    _StreamUnavailable,

    -- ** PendingVerification
    _PendingVerification,

    -- ** ChannelNotBroadcasting
    _ChannelNotBroadcasting,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PutMetadata
    PutMetadata (PutMetadata'),
    newPutMetadata,
    PutMetadataResponse (PutMetadataResponse'),
    newPutMetadataResponse,

    -- ** ListRecordingConfigurations (Paginated)
    ListRecordingConfigurations (ListRecordingConfigurations'),
    newListRecordingConfigurations,
    ListRecordingConfigurationsResponse (ListRecordingConfigurationsResponse'),
    newListRecordingConfigurationsResponse,

    -- ** CreateStreamKey
    CreateStreamKey (CreateStreamKey'),
    newCreateStreamKey,
    CreateStreamKeyResponse (CreateStreamKeyResponse'),
    newCreateStreamKeyResponse,

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

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** UpdateChannel
    UpdateChannel (UpdateChannel'),
    newUpdateChannel,
    UpdateChannelResponse (UpdateChannelResponse'),
    newUpdateChannelResponse,

    -- ** GetStreamKey
    GetStreamKey (GetStreamKey'),
    newGetStreamKey,
    GetStreamKeyResponse (GetStreamKeyResponse'),
    newGetStreamKeyResponse,

    -- ** DeletePlaybackKeyPair
    DeletePlaybackKeyPair (DeletePlaybackKeyPair'),
    newDeletePlaybackKeyPair,
    DeletePlaybackKeyPairResponse (DeletePlaybackKeyPairResponse'),
    newDeletePlaybackKeyPairResponse,

    -- ** BatchGetStreamKey
    BatchGetStreamKey (BatchGetStreamKey'),
    newBatchGetStreamKey,
    BatchGetStreamKeyResponse (BatchGetStreamKeyResponse'),
    newBatchGetStreamKeyResponse,

    -- ** GetPlaybackKeyPair
    GetPlaybackKeyPair (GetPlaybackKeyPair'),
    newGetPlaybackKeyPair,
    GetPlaybackKeyPairResponse (GetPlaybackKeyPairResponse'),
    newGetPlaybackKeyPairResponse,

    -- ** DeleteRecordingConfiguration
    DeleteRecordingConfiguration (DeleteRecordingConfiguration'),
    newDeleteRecordingConfiguration,
    DeleteRecordingConfigurationResponse (DeleteRecordingConfigurationResponse'),
    newDeleteRecordingConfigurationResponse,

    -- ** StopStream
    StopStream (StopStream'),
    newStopStream,
    StopStreamResponse (StopStreamResponse'),
    newStopStreamResponse,

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** DeleteStreamKey
    DeleteStreamKey (DeleteStreamKey'),
    newDeleteStreamKey,
    DeleteStreamKeyResponse (DeleteStreamKeyResponse'),
    newDeleteStreamKeyResponse,

    -- ** GetStream
    GetStream (GetStream'),
    newGetStream,
    GetStreamResponse (GetStreamResponse'),
    newGetStreamResponse,

    -- ** ListStreamKeys (Paginated)
    ListStreamKeys (ListStreamKeys'),
    newListStreamKeys,
    ListStreamKeysResponse (ListStreamKeysResponse'),
    newListStreamKeysResponse,

    -- ** GetChannel
    GetChannel (GetChannel'),
    newGetChannel,
    GetChannelResponse (GetChannelResponse'),
    newGetChannelResponse,

    -- ** ListStreams (Paginated)
    ListStreams (ListStreams'),
    newListStreams,
    ListStreamsResponse (ListStreamsResponse'),
    newListStreamsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetRecordingConfiguration
    GetRecordingConfiguration (GetRecordingConfiguration'),
    newGetRecordingConfiguration,
    GetRecordingConfigurationResponse (GetRecordingConfigurationResponse'),
    newGetRecordingConfigurationResponse,

    -- ** ListPlaybackKeyPairs (Paginated)
    ListPlaybackKeyPairs (ListPlaybackKeyPairs'),
    newListPlaybackKeyPairs,
    ListPlaybackKeyPairsResponse (ListPlaybackKeyPairsResponse'),
    newListPlaybackKeyPairsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateRecordingConfiguration
    CreateRecordingConfiguration (CreateRecordingConfiguration'),
    newCreateRecordingConfiguration,
    CreateRecordingConfigurationResponse (CreateRecordingConfigurationResponse'),
    newCreateRecordingConfigurationResponse,

    -- ** BatchGetChannel
    BatchGetChannel (BatchGetChannel'),
    newBatchGetChannel,
    BatchGetChannelResponse (BatchGetChannelResponse'),
    newBatchGetChannelResponse,

    -- * Types

    -- ** ChannelLatencyMode
    ChannelLatencyMode (..),

    -- ** ChannelType
    ChannelType (..),

    -- ** RecordingConfigurationState
    RecordingConfigurationState (..),

    -- ** StreamHealth
    StreamHealth (..),

    -- ** StreamState
    StreamState (..),

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

    -- ** StreamKey
    StreamKey (StreamKey'),
    newStreamKey,

    -- ** StreamKeySummary
    StreamKeySummary (StreamKeySummary'),
    newStreamKeySummary,

    -- ** StreamSummary
    StreamSummary (StreamSummary'),
    newStreamSummary,
  )
where

import Network.AWS.IVS.BatchGetChannel
import Network.AWS.IVS.BatchGetStreamKey
import Network.AWS.IVS.CreateChannel
import Network.AWS.IVS.CreateRecordingConfiguration
import Network.AWS.IVS.CreateStreamKey
import Network.AWS.IVS.DeleteChannel
import Network.AWS.IVS.DeletePlaybackKeyPair
import Network.AWS.IVS.DeleteRecordingConfiguration
import Network.AWS.IVS.DeleteStreamKey
import Network.AWS.IVS.GetChannel
import Network.AWS.IVS.GetPlaybackKeyPair
import Network.AWS.IVS.GetRecordingConfiguration
import Network.AWS.IVS.GetStream
import Network.AWS.IVS.GetStreamKey
import Network.AWS.IVS.ImportPlaybackKeyPair
import Network.AWS.IVS.Lens
import Network.AWS.IVS.ListChannels
import Network.AWS.IVS.ListPlaybackKeyPairs
import Network.AWS.IVS.ListRecordingConfigurations
import Network.AWS.IVS.ListStreamKeys
import Network.AWS.IVS.ListStreams
import Network.AWS.IVS.ListTagsForResource
import Network.AWS.IVS.PutMetadata
import Network.AWS.IVS.StopStream
import Network.AWS.IVS.TagResource
import Network.AWS.IVS.Types
import Network.AWS.IVS.UntagResource
import Network.AWS.IVS.UpdateChannel
import Network.AWS.IVS.Waiters

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
