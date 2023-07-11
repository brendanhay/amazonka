{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackage.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Lens
  ( -- * Operations

    -- ** ConfigureLogs
    configureLogs_egressAccessLogs,
    configureLogs_ingressAccessLogs,
    configureLogs_id,
    configureLogsResponse_arn,
    configureLogsResponse_description,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_id,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_tags,
    configureLogsResponse_httpStatus,

    -- ** CreateChannel
    createChannel_description,
    createChannel_tags,
    createChannel_id,
    createChannelResponse_arn,
    createChannelResponse_description,
    createChannelResponse_egressAccessLogs,
    createChannelResponse_hlsIngest,
    createChannelResponse_id,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_tags,
    createChannelResponse_httpStatus,

    -- ** CreateHarvestJob
    createHarvestJob_s3Destination,
    createHarvestJob_endTime,
    createHarvestJob_originEndpointId,
    createHarvestJob_startTime,
    createHarvestJob_id,
    createHarvestJobResponse_arn,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_id,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_status,
    createHarvestJobResponse_httpStatus,

    -- ** CreateOriginEndpoint
    createOriginEndpoint_authorization,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_description,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_origination,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_tags,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_whitelist,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_id,
    deleteChannelResponse_httpStatus,

    -- ** DeleteOriginEndpoint
    deleteOriginEndpoint_id,
    deleteOriginEndpointResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_id,
    describeChannelResponse_arn,
    describeChannelResponse_description,
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_id,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_tags,
    describeChannelResponse_httpStatus,

    -- ** DescribeHarvestJob
    describeHarvestJob_id,
    describeHarvestJobResponse_arn,
    describeHarvestJobResponse_channelId,
    describeHarvestJobResponse_createdAt,
    describeHarvestJobResponse_endTime,
    describeHarvestJobResponse_id,
    describeHarvestJobResponse_originEndpointId,
    describeHarvestJobResponse_s3Destination,
    describeHarvestJobResponse_startTime,
    describeHarvestJobResponse_status,
    describeHarvestJobResponse_httpStatus,

    -- ** DescribeOriginEndpoint
    describeOriginEndpoint_id,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_httpStatus,

    -- ** ListChannels
    listChannels_maxResults,
    listChannels_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_nextToken,
    listChannelsResponse_httpStatus,

    -- ** ListHarvestJobs
    listHarvestJobs_includeChannelId,
    listHarvestJobs_includeStatus,
    listHarvestJobs_maxResults,
    listHarvestJobs_nextToken,
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_httpStatus,

    -- ** ListOriginEndpoints
    listOriginEndpoints_channelId,
    listOriginEndpoints_maxResults,
    listOriginEndpoints_nextToken,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_originEndpoints,
    listOriginEndpointsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** RotateIngestEndpointCredentials
    rotateIngestEndpointCredentials_ingestEndpointId,
    rotateIngestEndpointCredentials_id,
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdateChannel
    updateChannel_description,
    updateChannel_id,
    updateChannelResponse_arn,
    updateChannelResponse_description,
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_id,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_tags,
    updateChannelResponse_httpStatus,

    -- ** UpdateOriginEndpoint
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_description,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_id,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_httpStatus,

    -- * Types

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** Channel
    channel_arn,
    channel_description,
    channel_egressAccessLogs,
    channel_hlsIngest,
    channel_id,
    channel_ingressAccessLogs,
    channel_tags,

    -- ** CmafEncryption
    cmafEncryption_constantInitializationVector,
    cmafEncryption_encryptionMethod,
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_encryption,
    cmafPackage_hlsManifests,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_segmentPrefix,
    cmafPackage_streamSelection,

    -- ** CmafPackageCreateOrUpdateParameters
    cmafPackageCreateOrUpdateParameters_encryption,
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,
    cmafPackageCreateOrUpdateParameters_streamSelection,

    -- ** DashEncryption
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- ** DashPackage
    dashPackage_adTriggers,
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_encryption,
    dashPackage_includeIframeOnlyStream,
    dashPackage_manifestLayout,
    dashPackage_manifestWindowSeconds,
    dashPackage_minBufferTimeSeconds,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_periodTriggers,
    dashPackage_profile,
    dashPackage_segmentDurationSeconds,
    dashPackage_segmentTemplateFormat,
    dashPackage_streamSelection,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_utcTiming,
    dashPackage_utcTimingUri,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** HarvestJob
    harvestJob_arn,
    harvestJob_channelId,
    harvestJob_createdAt,
    harvestJob_endTime,
    harvestJob_id,
    harvestJob_originEndpointId,
    harvestJob_s3Destination,
    harvestJob_startTime,
    harvestJob_status,

    -- ** HlsEncryption
    hlsEncryption_constantInitializationVector,
    hlsEncryption_encryptionMethod,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_repeatExtXKey,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsIngest
    hlsIngest_ingestEndpoints,

    -- ** HlsManifest
    hlsManifest_adMarkers,
    hlsManifest_adTriggers,
    hlsManifest_adsOnDeliveryRestrictions,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_manifestName,
    hlsManifest_playlistType,
    hlsManifest_playlistWindowSeconds,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_url,
    hlsManifest_id,

    -- ** HlsManifestCreateOrUpdateParameters
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_id,

    -- ** HlsPackage
    hlsPackage_adMarkers,
    hlsPackage_adTriggers,
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_encryption,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_playlistType,
    hlsPackage_playlistWindowSeconds,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_streamSelection,
    hlsPackage_useAudioRenditionGroup,

    -- ** IngestEndpoint
    ingestEndpoint_id,
    ingestEndpoint_password,
    ingestEndpoint_url,
    ingestEndpoint_username,

    -- ** IngressAccessLogs
    ingressAccessLogs_logGroupName,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssPackage
    mssPackage_encryption,
    mssPackage_manifestWindowSeconds,
    mssPackage_segmentDurationSeconds,
    mssPackage_streamSelection,

    -- ** OriginEndpoint
    originEndpoint_arn,
    originEndpoint_authorization,
    originEndpoint_channelId,
    originEndpoint_cmafPackage,
    originEndpoint_dashPackage,
    originEndpoint_description,
    originEndpoint_hlsPackage,
    originEndpoint_id,
    originEndpoint_manifestName,
    originEndpoint_mssPackage,
    originEndpoint_origination,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_tags,
    originEndpoint_timeDelaySeconds,
    originEndpoint_url,
    originEndpoint_whitelist,

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
    streamSelection_maxVideoBitsPerSecond,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_streamOrder,
  )
where

import Amazonka.MediaPackage.ConfigureLogs
import Amazonka.MediaPackage.CreateChannel
import Amazonka.MediaPackage.CreateHarvestJob
import Amazonka.MediaPackage.CreateOriginEndpoint
import Amazonka.MediaPackage.DeleteChannel
import Amazonka.MediaPackage.DeleteOriginEndpoint
import Amazonka.MediaPackage.DescribeChannel
import Amazonka.MediaPackage.DescribeHarvestJob
import Amazonka.MediaPackage.DescribeOriginEndpoint
import Amazonka.MediaPackage.ListChannels
import Amazonka.MediaPackage.ListHarvestJobs
import Amazonka.MediaPackage.ListOriginEndpoints
import Amazonka.MediaPackage.ListTagsForResource
import Amazonka.MediaPackage.RotateIngestEndpointCredentials
import Amazonka.MediaPackage.TagResource
import Amazonka.MediaPackage.Types.Authorization
import Amazonka.MediaPackage.Types.Channel
import Amazonka.MediaPackage.Types.CmafEncryption
import Amazonka.MediaPackage.Types.CmafPackage
import Amazonka.MediaPackage.Types.CmafPackageCreateOrUpdateParameters
import Amazonka.MediaPackage.Types.DashEncryption
import Amazonka.MediaPackage.Types.DashPackage
import Amazonka.MediaPackage.Types.EgressAccessLogs
import Amazonka.MediaPackage.Types.EncryptionContractConfiguration
import Amazonka.MediaPackage.Types.HarvestJob
import Amazonka.MediaPackage.Types.HlsEncryption
import Amazonka.MediaPackage.Types.HlsIngest
import Amazonka.MediaPackage.Types.HlsManifest
import Amazonka.MediaPackage.Types.HlsManifestCreateOrUpdateParameters
import Amazonka.MediaPackage.Types.HlsPackage
import Amazonka.MediaPackage.Types.IngestEndpoint
import Amazonka.MediaPackage.Types.IngressAccessLogs
import Amazonka.MediaPackage.Types.MssEncryption
import Amazonka.MediaPackage.Types.MssPackage
import Amazonka.MediaPackage.Types.OriginEndpoint
import Amazonka.MediaPackage.Types.S3Destination
import Amazonka.MediaPackage.Types.SpekeKeyProvider
import Amazonka.MediaPackage.Types.StreamSelection
import Amazonka.MediaPackage.UntagResource
import Amazonka.MediaPackage.UpdateChannel
import Amazonka.MediaPackage.UpdateOriginEndpoint
