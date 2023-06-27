{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Lens
  ( -- * Operations

    -- ** CreateChannel
    createChannel_clientToken,
    createChannel_description,
    createChannel_tags,
    createChannel_channelGroupName,
    createChannel_channelName,
    createChannelResponse_description,
    createChannelResponse_ingestEndpoints,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,
    createChannelResponse_arn,
    createChannelResponse_channelName,
    createChannelResponse_channelGroupName,
    createChannelResponse_createdAt,
    createChannelResponse_modifiedAt,

    -- ** CreateChannelGroup
    createChannelGroup_clientToken,
    createChannelGroup_description,
    createChannelGroup_tags,
    createChannelGroup_channelGroupName,
    createChannelGroupResponse_description,
    createChannelGroupResponse_tags,
    createChannelGroupResponse_httpStatus,
    createChannelGroupResponse_channelGroupName,
    createChannelGroupResponse_arn,
    createChannelGroupResponse_egressDomain,
    createChannelGroupResponse_createdAt,
    createChannelGroupResponse_modifiedAt,

    -- ** CreateOriginEndpoint
    createOriginEndpoint_clientToken,
    createOriginEndpoint_description,
    createOriginEndpoint_hlsManifests,
    createOriginEndpoint_lowLatencyHlsManifests,
    createOriginEndpoint_segment,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_tags,
    createOriginEndpoint_channelGroupName,
    createOriginEndpoint_channelName,
    createOriginEndpoint_originEndpointName,
    createOriginEndpoint_containerType,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_hlsManifests,
    createOriginEndpointResponse_lowLatencyHlsManifests,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_httpStatus,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_channelGroupName,
    createOriginEndpointResponse_channelName,
    createOriginEndpointResponse_originEndpointName,
    createOriginEndpointResponse_containerType,
    createOriginEndpointResponse_segment,
    createOriginEndpointResponse_createdAt,
    createOriginEndpointResponse_modifiedAt,

    -- ** DeleteChannel
    deleteChannel_channelGroupName,
    deleteChannel_channelName,
    deleteChannelResponse_httpStatus,

    -- ** DeleteChannelGroup
    deleteChannelGroup_channelGroupName,
    deleteChannelGroupResponse_httpStatus,

    -- ** DeleteChannelPolicy
    deleteChannelPolicy_channelGroupName,
    deleteChannelPolicy_channelName,
    deleteChannelPolicyResponse_httpStatus,

    -- ** DeleteOriginEndpoint
    deleteOriginEndpoint_channelGroupName,
    deleteOriginEndpoint_channelName,
    deleteOriginEndpoint_originEndpointName,
    deleteOriginEndpointResponse_httpStatus,

    -- ** DeleteOriginEndpointPolicy
    deleteOriginEndpointPolicy_channelGroupName,
    deleteOriginEndpointPolicy_channelName,
    deleteOriginEndpointPolicy_originEndpointName,
    deleteOriginEndpointPolicyResponse_httpStatus,

    -- ** GetChannel
    getChannel_channelGroupName,
    getChannel_channelName,
    getChannelResponse_description,
    getChannelResponse_ingestEndpoints,
    getChannelResponse_tags,
    getChannelResponse_httpStatus,
    getChannelResponse_arn,
    getChannelResponse_channelName,
    getChannelResponse_channelGroupName,
    getChannelResponse_createdAt,
    getChannelResponse_modifiedAt,

    -- ** GetChannelGroup
    getChannelGroup_channelGroupName,
    getChannelGroupResponse_description,
    getChannelGroupResponse_tags,
    getChannelGroupResponse_httpStatus,
    getChannelGroupResponse_channelGroupName,
    getChannelGroupResponse_arn,
    getChannelGroupResponse_egressDomain,
    getChannelGroupResponse_createdAt,
    getChannelGroupResponse_modifiedAt,

    -- ** GetChannelPolicy
    getChannelPolicy_channelGroupName,
    getChannelPolicy_channelName,
    getChannelPolicyResponse_httpStatus,
    getChannelPolicyResponse_channelGroupName,
    getChannelPolicyResponse_channelName,
    getChannelPolicyResponse_policy,

    -- ** GetOriginEndpoint
    getOriginEndpoint_channelGroupName,
    getOriginEndpoint_channelName,
    getOriginEndpoint_originEndpointName,
    getOriginEndpointResponse_description,
    getOriginEndpointResponse_hlsManifests,
    getOriginEndpointResponse_lowLatencyHlsManifests,
    getOriginEndpointResponse_startoverWindowSeconds,
    getOriginEndpointResponse_tags,
    getOriginEndpointResponse_httpStatus,
    getOriginEndpointResponse_arn,
    getOriginEndpointResponse_channelGroupName,
    getOriginEndpointResponse_channelName,
    getOriginEndpointResponse_originEndpointName,
    getOriginEndpointResponse_containerType,
    getOriginEndpointResponse_segment,
    getOriginEndpointResponse_createdAt,
    getOriginEndpointResponse_modifiedAt,

    -- ** GetOriginEndpointPolicy
    getOriginEndpointPolicy_channelGroupName,
    getOriginEndpointPolicy_channelName,
    getOriginEndpointPolicy_originEndpointName,
    getOriginEndpointPolicyResponse_httpStatus,
    getOriginEndpointPolicyResponse_channelGroupName,
    getOriginEndpointPolicyResponse_channelName,
    getOriginEndpointPolicyResponse_originEndpointName,
    getOriginEndpointPolicyResponse_policy,

    -- ** ListChannelGroups
    listChannelGroups_maxResults,
    listChannelGroups_nextToken,
    listChannelGroupsResponse_items,
    listChannelGroupsResponse_nextToken,
    listChannelGroupsResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannels_channelGroupName,
    listChannelsResponse_items,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListOriginEndpoints
    listOriginEndpoints_maxResults,
    listOriginEndpoints_nextToken,
    listOriginEndpoints_channelGroupName,
    listOriginEndpoints_channelName,
    listOriginEndpointsResponse_items,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutChannelPolicy
    putChannelPolicy_channelGroupName,
    putChannelPolicy_channelName,
    putChannelPolicy_policy,
    putChannelPolicyResponse_httpStatus,

    -- ** PutOriginEndpointPolicy
    putOriginEndpointPolicy_channelGroupName,
    putOriginEndpointPolicy_channelName,
    putOriginEndpointPolicy_originEndpointName,
    putOriginEndpointPolicy_policy,
    putOriginEndpointPolicyResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateChannel
    updateChannel_description,
    updateChannel_channelGroupName,
    updateChannel_channelName,
    updateChannelResponse_description,
    updateChannelResponse_ingestEndpoints,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,
    updateChannelResponse_arn,
    updateChannelResponse_channelName,
    updateChannelResponse_channelGroupName,
    updateChannelResponse_createdAt,
    updateChannelResponse_modifiedAt,

    -- ** UpdateChannelGroup
    updateChannelGroup_description,
    updateChannelGroup_channelGroupName,
    updateChannelGroupResponse_description,
    updateChannelGroupResponse_tags,
    updateChannelGroupResponse_httpStatus,
    updateChannelGroupResponse_channelGroupName,
    updateChannelGroupResponse_arn,
    updateChannelGroupResponse_egressDomain,
    updateChannelGroupResponse_createdAt,
    updateChannelGroupResponse_modifiedAt,

    -- ** UpdateOriginEndpoint
    updateOriginEndpoint_description,
    updateOriginEndpoint_hlsManifests,
    updateOriginEndpoint_lowLatencyHlsManifests,
    updateOriginEndpoint_segment,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_channelGroupName,
    updateOriginEndpoint_channelName,
    updateOriginEndpoint_originEndpointName,
    updateOriginEndpoint_containerType,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_hlsManifests,
    updateOriginEndpointResponse_lowLatencyHlsManifests,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_httpStatus,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_channelGroupName,
    updateOriginEndpointResponse_channelName,
    updateOriginEndpointResponse_originEndpointName,
    updateOriginEndpointResponse_containerType,
    updateOriginEndpointResponse_segment,
    updateOriginEndpointResponse_createdAt,
    updateOriginEndpointResponse_modifiedAt,

    -- * Types

    -- ** ChannelGroupListConfiguration
    channelGroupListConfiguration_description,
    channelGroupListConfiguration_channelGroupName,
    channelGroupListConfiguration_arn,
    channelGroupListConfiguration_createdAt,
    channelGroupListConfiguration_modifiedAt,

    -- ** ChannelListConfiguration
    channelListConfiguration_description,
    channelListConfiguration_arn,
    channelListConfiguration_channelName,
    channelListConfiguration_channelGroupName,
    channelListConfiguration_createdAt,
    channelListConfiguration_modifiedAt,

    -- ** CreateHlsManifestConfiguration
    createHlsManifestConfiguration_childManifestName,
    createHlsManifestConfiguration_manifestWindowSeconds,
    createHlsManifestConfiguration_programDateTimeIntervalSeconds,
    createHlsManifestConfiguration_scteHls,
    createHlsManifestConfiguration_manifestName,

    -- ** CreateLowLatencyHlsManifestConfiguration
    createLowLatencyHlsManifestConfiguration_childManifestName,
    createLowLatencyHlsManifestConfiguration_manifestWindowSeconds,
    createLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds,
    createLowLatencyHlsManifestConfiguration_scteHls,
    createLowLatencyHlsManifestConfiguration_manifestName,

    -- ** Encryption
    encryption_constantInitializationVector,
    encryption_keyRotationIntervalSeconds,
    encryption_encryptionMethod,
    encryption_spekeKeyProvider,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** EncryptionMethod
    encryptionMethod_cmafEncryptionMethod,
    encryptionMethod_tsEncryptionMethod,

    -- ** GetHlsManifestConfiguration
    getHlsManifestConfiguration_childManifestName,
    getHlsManifestConfiguration_manifestWindowSeconds,
    getHlsManifestConfiguration_programDateTimeIntervalSeconds,
    getHlsManifestConfiguration_scteHls,
    getHlsManifestConfiguration_manifestName,
    getHlsManifestConfiguration_url,

    -- ** GetLowLatencyHlsManifestConfiguration
    getLowLatencyHlsManifestConfiguration_childManifestName,
    getLowLatencyHlsManifestConfiguration_manifestWindowSeconds,
    getLowLatencyHlsManifestConfiguration_programDateTimeIntervalSeconds,
    getLowLatencyHlsManifestConfiguration_scteHls,
    getLowLatencyHlsManifestConfiguration_manifestName,
    getLowLatencyHlsManifestConfiguration_url,

    -- ** IngestEndpoint
    ingestEndpoint_id,
    ingestEndpoint_url,

    -- ** ListHlsManifestConfiguration
    listHlsManifestConfiguration_childManifestName,
    listHlsManifestConfiguration_url,
    listHlsManifestConfiguration_manifestName,

    -- ** ListLowLatencyHlsManifestConfiguration
    listLowLatencyHlsManifestConfiguration_childManifestName,
    listLowLatencyHlsManifestConfiguration_url,
    listLowLatencyHlsManifestConfiguration_manifestName,

    -- ** OriginEndpointListConfiguration
    originEndpointListConfiguration_createdAt,
    originEndpointListConfiguration_description,
    originEndpointListConfiguration_hlsManifests,
    originEndpointListConfiguration_lowLatencyHlsManifests,
    originEndpointListConfiguration_modifiedAt,
    originEndpointListConfiguration_arn,
    originEndpointListConfiguration_channelGroupName,
    originEndpointListConfiguration_channelName,
    originEndpointListConfiguration_originEndpointName,
    originEndpointListConfiguration_containerType,

    -- ** Scte
    scte_scteFilter,

    -- ** ScteHls
    scteHls_adMarkerHls,

    -- ** Segment
    segment_encryption,
    segment_includeIframeOnlyStreams,
    segment_scte,
    segment_segmentDurationSeconds,
    segment_segmentName,
    segment_tsIncludeDvbSubtitles,
    segment_tsUseAudioRenditionGroup,

    -- ** SpekeKeyProvider
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_drmSystems,
    spekeKeyProvider_roleArn,
    spekeKeyProvider_url,
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
import Amazonka.MediaPackageV2.ListChannelGroups
import Amazonka.MediaPackageV2.ListChannels
import Amazonka.MediaPackageV2.ListOriginEndpoints
import Amazonka.MediaPackageV2.ListTagsForResource
import Amazonka.MediaPackageV2.PutChannelPolicy
import Amazonka.MediaPackageV2.PutOriginEndpointPolicy
import Amazonka.MediaPackageV2.TagResource
import Amazonka.MediaPackageV2.Types.ChannelGroupListConfiguration
import Amazonka.MediaPackageV2.Types.ChannelListConfiguration
import Amazonka.MediaPackageV2.Types.CreateHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.CreateLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.Encryption
import Amazonka.MediaPackageV2.Types.EncryptionContractConfiguration
import Amazonka.MediaPackageV2.Types.EncryptionMethod
import Amazonka.MediaPackageV2.Types.GetHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.GetLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.IngestEndpoint
import Amazonka.MediaPackageV2.Types.ListHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.ListLowLatencyHlsManifestConfiguration
import Amazonka.MediaPackageV2.Types.OriginEndpointListConfiguration
import Amazonka.MediaPackageV2.Types.Scte
import Amazonka.MediaPackageV2.Types.ScteHls
import Amazonka.MediaPackageV2.Types.Segment
import Amazonka.MediaPackageV2.Types.SpekeKeyProvider
import Amazonka.MediaPackageV2.UntagResource
import Amazonka.MediaPackageV2.UpdateChannel
import Amazonka.MediaPackageV2.UpdateChannelGroup
import Amazonka.MediaPackageV2.UpdateOriginEndpoint
