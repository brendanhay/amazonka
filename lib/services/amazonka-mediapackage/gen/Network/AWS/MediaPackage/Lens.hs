{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Lens
  ( -- * Operations

    -- ** CreateHarvestJob
    createHarvestJob_s3Destination,
    createHarvestJob_endTime,
    createHarvestJob_originEndpointId,
    createHarvestJob_startTime,
    createHarvestJob_id,
    createHarvestJobResponse_status,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_arn,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_id,
    createHarvestJobResponse_httpStatus,

    -- ** ConfigureLogs
    configureLogs_ingressAccessLogs,
    configureLogs_egressAccessLogs,
    configureLogs_id,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_arn,
    configureLogsResponse_id,
    configureLogsResponse_description,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_tags,
    configureLogsResponse_httpStatus,

    -- ** DescribeOriginEndpoint
    describeOriginEndpoint_id,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_id,
    deleteChannelResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_description,
    updateChannel_id,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_arn,
    updateChannelResponse_id,
    updateChannelResponse_description,
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,

    -- ** DescribeHarvestJob
    describeHarvestJob_id,
    describeHarvestJobResponse_status,
    describeHarvestJobResponse_originEndpointId,
    describeHarvestJobResponse_startTime,
    describeHarvestJobResponse_arn,
    describeHarvestJobResponse_createdAt,
    describeHarvestJobResponse_channelId,
    describeHarvestJobResponse_s3Destination,
    describeHarvestJobResponse_endTime,
    describeHarvestJobResponse_id,
    describeHarvestJobResponse_httpStatus,

    -- ** RotateIngestEndpointCredentials
    rotateIngestEndpointCredentials_ingestEndpointId,
    rotateIngestEndpointCredentials_id,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_httpStatus,

    -- ** CreateOriginEndpoint
    createOriginEndpoint_whitelist,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_authorization,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_description,
    createOriginEndpoint_tags,
    createOriginEndpoint_origination,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_httpStatus,

    -- ** ListOriginEndpoints
    listOriginEndpoints_channelId,
    listOriginEndpoints_nextToken,
    listOriginEndpoints_maxResults,
    listOriginEndpointsResponse_originEndpoints,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_httpStatus,

    -- ** ListHarvestJobs
    listHarvestJobs_includeStatus,
    listHarvestJobs_nextToken,
    listHarvestJobs_includeChannelId,
    listHarvestJobs_maxResults,
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_httpStatus,

    -- ** CreateChannel
    createChannel_description,
    createChannel_tags,
    createChannel_id,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_hlsIngest,
    createChannelResponse_arn,
    createChannelResponse_id,
    createChannelResponse_description,
    createChannelResponse_egressAccessLogs,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** DescribeChannel
    describeChannel_id,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_description,
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_tags,
    describeChannelResponse_httpStatus,

    -- ** DeleteOriginEndpoint
    deleteOriginEndpoint_id,
    deleteOriginEndpointResponse_httpStatus,

    -- ** UpdateOriginEndpoint
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_description,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_id,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_httpStatus,

    -- * Types

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** Channel
    channel_ingressAccessLogs,
    channel_hlsIngest,
    channel_arn,
    channel_id,
    channel_description,
    channel_egressAccessLogs,
    channel_tags,

    -- ** CmafEncryption
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_constantInitializationVector,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_hlsManifests,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_streamSelection,
    cmafPackage_encryption,
    cmafPackage_segmentPrefix,

    -- ** CmafPackageCreateOrUpdateParameters
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,
    cmafPackageCreateOrUpdateParameters_streamSelection,
    cmafPackageCreateOrUpdateParameters_encryption,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,

    -- ** DashEncryption
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- ** DashPackage
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_minBufferTimeSeconds,
    dashPackage_utcTiming,
    dashPackage_segmentTemplateFormat,
    dashPackage_profile,
    dashPackage_segmentDurationSeconds,
    dashPackage_utcTimingUri,
    dashPackage_streamSelection,
    dashPackage_encryption,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_manifestLayout,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_manifestWindowSeconds,
    dashPackage_adTriggers,
    dashPackage_periodTriggers,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** HarvestJob
    harvestJob_status,
    harvestJob_originEndpointId,
    harvestJob_startTime,
    harvestJob_arn,
    harvestJob_createdAt,
    harvestJob_channelId,
    harvestJob_s3Destination,
    harvestJob_endTime,
    harvestJob_id,

    -- ** HlsEncryption
    hlsEncryption_encryptionMethod,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_repeatExtXKey,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsIngest
    hlsIngest_ingestEndpoints,

    -- ** HlsManifest
    hlsManifest_manifestName,
    hlsManifest_url,
    hlsManifest_playlistType,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_adMarkers,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_playlistWindowSeconds,
    hlsManifest_id,

    -- ** HlsManifestCreateOrUpdateParameters
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_id,

    -- ** HlsPackage
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_playlistType,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_streamSelection,
    hlsPackage_adMarkers,
    hlsPackage_encryption,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_adTriggers,
    hlsPackage_playlistWindowSeconds,

    -- ** IngestEndpoint
    ingestEndpoint_url,
    ingestEndpoint_username,
    ingestEndpoint_password,
    ingestEndpoint_id,

    -- ** IngressAccessLogs
    ingressAccessLogs_logGroupName,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssPackage
    mssPackage_segmentDurationSeconds,
    mssPackage_streamSelection,
    mssPackage_encryption,
    mssPackage_manifestWindowSeconds,

    -- ** OriginEndpoint
    originEndpoint_whitelist,
    originEndpoint_hlsPackage,
    originEndpoint_arn,
    originEndpoint_manifestName,
    originEndpoint_url,
    originEndpoint_authorization,
    originEndpoint_channelId,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_dashPackage,
    originEndpoint_mssPackage,
    originEndpoint_id,
    originEndpoint_timeDelaySeconds,
    originEndpoint_cmafPackage,
    originEndpoint_description,
    originEndpoint_tags,
    originEndpoint_origination,

    -- ** S3Destination
    s3Destination_manifestKey,
    s3Destination_bucketName,
    s3Destination_roleArn,

    -- ** SpekeKeyProvider
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- ** StreamSelection
    streamSelection_streamOrder,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
  )
where

import Network.AWS.MediaPackage.ConfigureLogs
import Network.AWS.MediaPackage.CreateChannel
import Network.AWS.MediaPackage.CreateHarvestJob
import Network.AWS.MediaPackage.CreateOriginEndpoint
import Network.AWS.MediaPackage.DeleteChannel
import Network.AWS.MediaPackage.DeleteOriginEndpoint
import Network.AWS.MediaPackage.DescribeChannel
import Network.AWS.MediaPackage.DescribeHarvestJob
import Network.AWS.MediaPackage.DescribeOriginEndpoint
import Network.AWS.MediaPackage.ListChannels
import Network.AWS.MediaPackage.ListHarvestJobs
import Network.AWS.MediaPackage.ListOriginEndpoints
import Network.AWS.MediaPackage.ListTagsForResource
import Network.AWS.MediaPackage.RotateIngestEndpointCredentials
import Network.AWS.MediaPackage.TagResource
import Network.AWS.MediaPackage.Types.Authorization
import Network.AWS.MediaPackage.Types.Channel
import Network.AWS.MediaPackage.Types.CmafEncryption
import Network.AWS.MediaPackage.Types.CmafPackage
import Network.AWS.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.DashEncryption
import Network.AWS.MediaPackage.Types.DashPackage
import Network.AWS.MediaPackage.Types.EgressAccessLogs
import Network.AWS.MediaPackage.Types.EncryptionContractConfiguration
import Network.AWS.MediaPackage.Types.HarvestJob
import Network.AWS.MediaPackage.Types.HlsEncryption
import Network.AWS.MediaPackage.Types.HlsIngest
import Network.AWS.MediaPackage.Types.HlsManifest
import Network.AWS.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Network.AWS.MediaPackage.Types.HlsPackage
import Network.AWS.MediaPackage.Types.IngestEndpoint
import Network.AWS.MediaPackage.Types.IngressAccessLogs
import Network.AWS.MediaPackage.Types.MssEncryption
import Network.AWS.MediaPackage.Types.MssPackage
import Network.AWS.MediaPackage.Types.OriginEndpoint
import Network.AWS.MediaPackage.Types.S3Destination
import Network.AWS.MediaPackage.Types.SpekeKeyProvider
import Network.AWS.MediaPackage.Types.StreamSelection
import Network.AWS.MediaPackage.UntagResource
import Network.AWS.MediaPackage.UpdateChannel
import Network.AWS.MediaPackage.UpdateOriginEndpoint
