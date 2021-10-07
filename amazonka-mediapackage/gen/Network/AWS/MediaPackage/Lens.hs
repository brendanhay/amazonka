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

    -- ** CreateChannel
    createChannel_tags,
    createChannel_description,
    createChannel_id,
    createChannelResponse_egressAccessLogs,
    createChannelResponse_arn,
    createChannelResponse_id,
    createChannelResponse_hlsIngest,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_tags,
    createChannelResponse_description,
    createChannelResponse_httpStatus,

    -- ** CreateHarvestJob
    createHarvestJob_s3Destination,
    createHarvestJob_endTime,
    createHarvestJob_originEndpointId,
    createHarvestJob_startTime,
    createHarvestJob_id,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_status,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_arn,
    createHarvestJobResponse_id,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_httpStatus,

    -- ** ListOriginEndpoints
    listOriginEndpoints_nextToken,
    listOriginEndpoints_maxResults,
    listOriginEndpoints_channelId,
    listOriginEndpointsResponse_originEndpoints,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_httpStatus,

    -- ** DeleteOriginEndpoint
    deleteOriginEndpoint_id,
    deleteOriginEndpointResponse_httpStatus,

    -- ** UpdateOriginEndpoint
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_description,
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_id,
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** CreateOriginEndpoint
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_origination,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_whitelist,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_tags,
    createOriginEndpoint_description,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_authorization,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_httpStatus,

    -- ** DescribeOriginEndpoint
    describeOriginEndpoint_id,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_httpStatus,

    -- ** ConfigureLogs
    configureLogs_egressAccessLogs,
    configureLogs_ingressAccessLogs,
    configureLogs_id,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_arn,
    configureLogsResponse_id,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_tags,
    configureLogsResponse_description,
    configureLogsResponse_httpStatus,

    -- ** ListHarvestJobs
    listHarvestJobs_nextToken,
    listHarvestJobs_maxResults,
    listHarvestJobs_includeStatus,
    listHarvestJobs_includeChannelId,
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_id,
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_tags,
    describeChannelResponse_description,
    describeChannelResponse_httpStatus,

    -- ** RotateIngestEndpointCredentials
    rotateIngestEndpointCredentials_ingestEndpointId,
    rotateIngestEndpointCredentials_id,
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_httpStatus,

    -- ** DescribeHarvestJob
    describeHarvestJob_id,
    describeHarvestJobResponse_s3Destination,
    describeHarvestJobResponse_status,
    describeHarvestJobResponse_channelId,
    describeHarvestJobResponse_createdAt,
    describeHarvestJobResponse_arn,
    describeHarvestJobResponse_id,
    describeHarvestJobResponse_startTime,
    describeHarvestJobResponse_endTime,
    describeHarvestJobResponse_originEndpointId,
    describeHarvestJobResponse_httpStatus,

    -- ** UpdateChannel
    updateChannel_description,
    updateChannel_id,
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_arn,
    updateChannelResponse_id,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_description,
    updateChannelResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_id,
    deleteChannelResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- * Types

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** Channel
    channel_egressAccessLogs,
    channel_arn,
    channel_id,
    channel_hlsIngest,
    channel_ingressAccessLogs,
    channel_tags,
    channel_description,

    -- ** CmafEncryption
    cmafEncryption_constantInitializationVector,
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_streamSelection,
    cmafPackage_hlsManifests,
    cmafPackage_segmentPrefix,
    cmafPackage_encryption,
    cmafPackage_segmentDurationSeconds,

    -- ** CmafPackageCreateOrUpdateParameters
    cmafPackageCreateOrUpdateParameters_streamSelection,
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,
    cmafPackageCreateOrUpdateParameters_encryption,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,

    -- ** DashEncryption
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- ** DashPackage
    dashPackage_minBufferTimeSeconds,
    dashPackage_streamSelection,
    dashPackage_periodTriggers,
    dashPackage_adTriggers,
    dashPackage_manifestWindowSeconds,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_manifestLayout,
    dashPackage_encryption,
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_utcTimingUri,
    dashPackage_segmentDurationSeconds,
    dashPackage_profile,
    dashPackage_segmentTemplateFormat,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_utcTiming,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** HarvestJob
    harvestJob_s3Destination,
    harvestJob_status,
    harvestJob_channelId,
    harvestJob_createdAt,
    harvestJob_arn,
    harvestJob_id,
    harvestJob_startTime,
    harvestJob_endTime,
    harvestJob_originEndpointId,

    -- ** HlsEncryption
    hlsEncryption_repeatExtXKey,
    hlsEncryption_encryptionMethod,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsIngest
    hlsIngest_ingestEndpoints,

    -- ** HlsManifest
    hlsManifest_adMarkers,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_playlistWindowSeconds,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_manifestName,
    hlsManifest_playlistType,
    hlsManifest_url,
    hlsManifest_id,

    -- ** HlsManifestCreateOrUpdateParameters
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_id,

    -- ** HlsPackage
    hlsPackage_adMarkers,
    hlsPackage_streamSelection,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_playlistWindowSeconds,
    hlsPackage_adTriggers,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_encryption,
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_playlistType,

    -- ** IngestEndpoint
    ingestEndpoint_id,
    ingestEndpoint_password,
    ingestEndpoint_username,
    ingestEndpoint_url,

    -- ** IngressAccessLogs
    ingressAccessLogs_logGroupName,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssPackage
    mssPackage_streamSelection,
    mssPackage_manifestWindowSeconds,
    mssPackage_encryption,
    mssPackage_segmentDurationSeconds,

    -- ** OriginEndpoint
    originEndpoint_dashPackage,
    originEndpoint_origination,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_channelId,
    originEndpoint_cmafPackage,
    originEndpoint_manifestName,
    originEndpoint_arn,
    originEndpoint_id,
    originEndpoint_whitelist,
    originEndpoint_mssPackage,
    originEndpoint_tags,
    originEndpoint_description,
    originEndpoint_url,
    originEndpoint_timeDelaySeconds,
    originEndpoint_authorization,
    originEndpoint_hlsPackage,

    -- ** S3Destination
    s3Destination_manifestKey,
    s3Destination_bucketName,
    s3Destination_roleArn,

    -- ** SpekeKeyProvider
    spekeKeyProvider_certificateArn,
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_resourceId,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- ** StreamSelection
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
    streamSelection_streamOrder,
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
