{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.MediaPackageV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-12-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This guide is intended for creating AWS Elemental MediaPackage resources
-- in MediaPackage Version 2 (v2) starting from May 2023. To get started
-- with MediaPackage v2, create your MediaPackage resources. There isn\'t
-- an automated process to migrate your resources from MediaPackage v1 to
-- MediaPackage v2.
--
-- The names of the entities that you use to access this API, like URLs and
-- ARNs, all have the versioning information added, like \"v2\", to
-- distinguish from the prior version. If you used MediaPackage prior to
-- this release, you can\'t use the MediaPackage v2 CLI or the MediaPackage
-- v2 API to access any MediaPackage v1 resources.
--
-- If you created resources in MediaPackage v1, use video on demand (VOD)
-- workflows, and aren\'t looking to migrate to MediaPackage v2 yet, see
-- the
-- <https://docs.aws.amazon.com/mediapackage/latest/apireference/what-is.html MediaPackage v1 Live API Reference>.
--
-- This is the AWS Elemental MediaPackage v2 Live REST API Reference. It
-- describes all the MediaPackage API operations for live content in
-- detail, and provides sample requests, responses, and errors for the
-- supported web services protocols.
--
-- We assume that you have the IAM permissions that you need to use
-- MediaPackage via the REST API. We also assume that you are familiar with
-- the features and operations of MediaPackage, as described in the AWS
-- Elemental MediaPackage User Guide.
module Amazonka.MediaPackageV2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateChannel
    CreateChannel (CreateChannel'),
    newCreateChannel,
    CreateChannelResponse (CreateChannelResponse'),
    newCreateChannelResponse,

    -- ** CreateChannelGroup
    CreateChannelGroup (CreateChannelGroup'),
    newCreateChannelGroup,
    CreateChannelGroupResponse (CreateChannelGroupResponse'),
    newCreateChannelGroupResponse,

    -- ** CreateOriginEndpoint
    CreateOriginEndpoint (CreateOriginEndpoint'),
    newCreateOriginEndpoint,
    CreateOriginEndpointResponse (CreateOriginEndpointResponse'),
    newCreateOriginEndpointResponse,

    -- ** DeleteChannel
    DeleteChannel (DeleteChannel'),
    newDeleteChannel,
    DeleteChannelResponse (DeleteChannelResponse'),
    newDeleteChannelResponse,

    -- ** DeleteChannelGroup
    DeleteChannelGroup (DeleteChannelGroup'),
    newDeleteChannelGroup,
    DeleteChannelGroupResponse (DeleteChannelGroupResponse'),
    newDeleteChannelGroupResponse,

    -- ** DeleteChannelPolicy
    DeleteChannelPolicy (DeleteChannelPolicy'),
    newDeleteChannelPolicy,
    DeleteChannelPolicyResponse (DeleteChannelPolicyResponse'),
    newDeleteChannelPolicyResponse,

    -- ** DeleteOriginEndpoint
    DeleteOriginEndpoint (DeleteOriginEndpoint'),
    newDeleteOriginEndpoint,
    DeleteOriginEndpointResponse (DeleteOriginEndpointResponse'),
    newDeleteOriginEndpointResponse,

    -- ** DeleteOriginEndpointPolicy
    DeleteOriginEndpointPolicy (DeleteOriginEndpointPolicy'),
    newDeleteOriginEndpointPolicy,
    DeleteOriginEndpointPolicyResponse (DeleteOriginEndpointPolicyResponse'),
    newDeleteOriginEndpointPolicyResponse,

    -- ** GetChannel
    GetChannel (GetChannel'),
    newGetChannel,
    GetChannelResponse (GetChannelResponse'),
    newGetChannelResponse,

    -- ** GetChannelGroup
    GetChannelGroup (GetChannelGroup'),
    newGetChannelGroup,
    GetChannelGroupResponse (GetChannelGroupResponse'),
    newGetChannelGroupResponse,

    -- ** GetChannelPolicy
    GetChannelPolicy (GetChannelPolicy'),
    newGetChannelPolicy,
    GetChannelPolicyResponse (GetChannelPolicyResponse'),
    newGetChannelPolicyResponse,

    -- ** GetOriginEndpoint
    GetOriginEndpoint (GetOriginEndpoint'),
    newGetOriginEndpoint,
    GetOriginEndpointResponse (GetOriginEndpointResponse'),
    newGetOriginEndpointResponse,

    -- ** GetOriginEndpointPolicy
    GetOriginEndpointPolicy (GetOriginEndpointPolicy'),
    newGetOriginEndpointPolicy,
    GetOriginEndpointPolicyResponse (GetOriginEndpointPolicyResponse'),
    newGetOriginEndpointPolicyResponse,

    -- ** ListChannelGroups (Paginated)
    ListChannelGroups (ListChannelGroups'),
    newListChannelGroups,
    ListChannelGroupsResponse (ListChannelGroupsResponse'),
    newListChannelGroupsResponse,

    -- ** ListChannels (Paginated)
    ListChannels (ListChannels'),
    newListChannels,
    ListChannelsResponse (ListChannelsResponse'),
    newListChannelsResponse,

    -- ** ListOriginEndpoints (Paginated)
    ListOriginEndpoints (ListOriginEndpoints'),
    newListOriginEndpoints,
    ListOriginEndpointsResponse (ListOriginEndpointsResponse'),
    newListOriginEndpointsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** PutChannelPolicy
    PutChannelPolicy (PutChannelPolicy'),
    newPutChannelPolicy,
    PutChannelPolicyResponse (PutChannelPolicyResponse'),
    newPutChannelPolicyResponse,

    -- ** PutOriginEndpointPolicy
    PutOriginEndpointPolicy (PutOriginEndpointPolicy'),
    newPutOriginEndpointPolicy,
    PutOriginEndpointPolicyResponse (PutOriginEndpointPolicyResponse'),
    newPutOriginEndpointPolicyResponse,

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

    -- ** UpdateChannelGroup
    UpdateChannelGroup (UpdateChannelGroup'),
    newUpdateChannelGroup,
    UpdateChannelGroupResponse (UpdateChannelGroupResponse'),
    newUpdateChannelGroupResponse,

    -- ** UpdateOriginEndpoint
    UpdateOriginEndpoint (UpdateOriginEndpoint'),
    newUpdateOriginEndpoint,
    UpdateOriginEndpointResponse (UpdateOriginEndpointResponse'),
    newUpdateOriginEndpointResponse,

    -- * Types

    -- ** AdMarkerHls
    AdMarkerHls (..),

    -- ** CmafEncryptionMethod
    CmafEncryptionMethod (..),

    -- ** ContainerType
    ContainerType (..),

    -- ** DrmSystem
    DrmSystem (..),

    -- ** PresetSpeke20Audio
    PresetSpeke20Audio (..),

    -- ** PresetSpeke20Video
    PresetSpeke20Video (..),

    -- ** ScteFilter
    ScteFilter (..),

    -- ** TsEncryptionMethod
    TsEncryptionMethod (..),

    -- ** ChannelGroupListConfiguration
    ChannelGroupListConfiguration (ChannelGroupListConfiguration'),
    newChannelGroupListConfiguration,

    -- ** ChannelListConfiguration
    ChannelListConfiguration (ChannelListConfiguration'),
    newChannelListConfiguration,

    -- ** CreateHlsManifestConfiguration
    CreateHlsManifestConfiguration (CreateHlsManifestConfiguration'),
    newCreateHlsManifestConfiguration,

    -- ** CreateLowLatencyHlsManifestConfiguration
    CreateLowLatencyHlsManifestConfiguration (CreateLowLatencyHlsManifestConfiguration'),
    newCreateLowLatencyHlsManifestConfiguration,

    -- ** Encryption
    Encryption (Encryption'),
    newEncryption,

    -- ** EncryptionContractConfiguration
    EncryptionContractConfiguration (EncryptionContractConfiguration'),
    newEncryptionContractConfiguration,

    -- ** EncryptionMethod
    EncryptionMethod (EncryptionMethod'),
    newEncryptionMethod,

    -- ** GetHlsManifestConfiguration
    GetHlsManifestConfiguration (GetHlsManifestConfiguration'),
    newGetHlsManifestConfiguration,

    -- ** GetLowLatencyHlsManifestConfiguration
    GetLowLatencyHlsManifestConfiguration (GetLowLatencyHlsManifestConfiguration'),
    newGetLowLatencyHlsManifestConfiguration,

    -- ** IngestEndpoint
    IngestEndpoint (IngestEndpoint'),
    newIngestEndpoint,

    -- ** ListHlsManifestConfiguration
    ListHlsManifestConfiguration (ListHlsManifestConfiguration'),
    newListHlsManifestConfiguration,

    -- ** ListLowLatencyHlsManifestConfiguration
    ListLowLatencyHlsManifestConfiguration (ListLowLatencyHlsManifestConfiguration'),
    newListLowLatencyHlsManifestConfiguration,

    -- ** OriginEndpointListConfiguration
    OriginEndpointListConfiguration (OriginEndpointListConfiguration'),
    newOriginEndpointListConfiguration,

    -- ** Scte
    Scte (Scte'),
    newScte,

    -- ** ScteHls
    ScteHls (ScteHls'),
    newScteHls,

    -- ** Segment
    Segment (Segment'),
    newSegment,

    -- ** SpekeKeyProvider
    SpekeKeyProvider (SpekeKeyProvider'),
    newSpekeKeyProvider,
  )
where

import Amazonka.MediaPackageV2.CreateChannel
import Amazonka.MediaPackageV2.CreateChannelGroup
import Amazonka.MediaPackageV2.CreateOriginEndpoint
import Amazonka.MediaPackageV2.DeleteChannel
import Amazonka.MediaPackageV2.DeleteChannelGroup
import Amazonka.MediaPackageV2.DeleteChannelPolicy
import Amazonka.MediaPackageV2.DeleteOriginEndpoint
import Amazonka.MediaPackageV2.DeleteOriginEndpointPolicy
import Amazonka.MediaPackageV2.GetChannel
import Amazonka.MediaPackageV2.GetChannelGroup
import Amazonka.MediaPackageV2.GetChannelPolicy
import Amazonka.MediaPackageV2.GetOriginEndpoint
import Amazonka.MediaPackageV2.GetOriginEndpointPolicy
import Amazonka.MediaPackageV2.Lens
import Amazonka.MediaPackageV2.ListChannelGroups
import Amazonka.MediaPackageV2.ListChannels
import Amazonka.MediaPackageV2.ListOriginEndpoints
import Amazonka.MediaPackageV2.ListTagsForResource
import Amazonka.MediaPackageV2.PutChannelPolicy
import Amazonka.MediaPackageV2.PutOriginEndpointPolicy
import Amazonka.MediaPackageV2.TagResource
import Amazonka.MediaPackageV2.Types
import Amazonka.MediaPackageV2.UntagResource
import Amazonka.MediaPackageV2.UpdateChannel
import Amazonka.MediaPackageV2.UpdateChannelGroup
import Amazonka.MediaPackageV2.UpdateOriginEndpoint
import Amazonka.MediaPackageV2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'MediaPackageV2'.

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
