{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackage.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Lens
  ( -- * Operations

    -- ** ConfigureLogs
    configureLogs_ingressAccessLogs,
    configureLogs_egressAccessLogs,
    configureLogs_id,
    configureLogsResponse_tags,
    configureLogsResponse_ingressAccessLogs,
    configureLogsResponse_arn,
    configureLogsResponse_id,
    configureLogsResponse_description,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_hlsIngest,
    configureLogsResponse_httpStatus,

    -- ** CreateChannel
    createChannel_tags,
    createChannel_description,
    createChannel_id,
    createChannelResponse_tags,
    createChannelResponse_ingressAccessLogs,
    createChannelResponse_arn,
    createChannelResponse_id,
    createChannelResponse_description,
    createChannelResponse_egressAccessLogs,
    createChannelResponse_hlsIngest,
    createChannelResponse_httpStatus,

    -- ** CreateHarvestJob
    createHarvestJob_s3Destination,
    createHarvestJob_endTime,
    createHarvestJob_originEndpointId,
    createHarvestJob_startTime,
    createHarvestJob_id,
    createHarvestJobResponse_arn,
    createHarvestJobResponse_status,
    createHarvestJobResponse_endTime,
    createHarvestJobResponse_id,
    createHarvestJobResponse_channelId,
    createHarvestJobResponse_s3Destination,
    createHarvestJobResponse_originEndpointId,
    createHarvestJobResponse_createdAt,
    createHarvestJobResponse_startTime,
    createHarvestJobResponse_httpStatus,

    -- ** CreateOriginEndpoint
    createOriginEndpoint_tags,
    createOriginEndpoint_timeDelaySeconds,
    createOriginEndpoint_startoverWindowSeconds,
    createOriginEndpoint_mssPackage,
    createOriginEndpoint_whitelist,
    createOriginEndpoint_description,
    createOriginEndpoint_manifestName,
    createOriginEndpoint_authorization,
    createOriginEndpoint_dashPackage,
    createOriginEndpoint_cmafPackage,
    createOriginEndpoint_hlsPackage,
    createOriginEndpoint_origination,
    createOriginEndpoint_channelId,
    createOriginEndpoint_id,
    createOriginEndpointResponse_tags,
    createOriginEndpointResponse_timeDelaySeconds,
    createOriginEndpointResponse_startoverWindowSeconds,
    createOriginEndpointResponse_mssPackage,
    createOriginEndpointResponse_arn,
    createOriginEndpointResponse_whitelist,
    createOriginEndpointResponse_url,
    createOriginEndpointResponse_id,
    createOriginEndpointResponse_description,
    createOriginEndpointResponse_manifestName,
    createOriginEndpointResponse_channelId,
    createOriginEndpointResponse_authorization,
    createOriginEndpointResponse_dashPackage,
    createOriginEndpointResponse_cmafPackage,
    createOriginEndpointResponse_hlsPackage,
    createOriginEndpointResponse_origination,
    createOriginEndpointResponse_httpStatus,

    -- ** DeleteChannel
    deleteChannel_id,
    deleteChannelResponse_httpStatus,

    -- ** DeleteOriginEndpoint
    deleteOriginEndpoint_id,
    deleteOriginEndpointResponse_httpStatus,

    -- ** DescribeChannel
    describeChannel_id,
    describeChannelResponse_tags,
    describeChannelResponse_ingressAccessLogs,
    describeChannelResponse_arn,
    describeChannelResponse_id,
    describeChannelResponse_description,
    describeChannelResponse_egressAccessLogs,
    describeChannelResponse_hlsIngest,
    describeChannelResponse_httpStatus,

    -- ** DescribeHarvestJob
    describeHarvestJob_id,
    describeHarvestJobResponse_arn,
    describeHarvestJobResponse_status,
    describeHarvestJobResponse_endTime,
    describeHarvestJobResponse_id,
    describeHarvestJobResponse_channelId,
    describeHarvestJobResponse_s3Destination,
    describeHarvestJobResponse_originEndpointId,
    describeHarvestJobResponse_createdAt,
    describeHarvestJobResponse_startTime,
    describeHarvestJobResponse_httpStatus,

    -- ** DescribeOriginEndpoint
    describeOriginEndpoint_id,
    describeOriginEndpointResponse_tags,
    describeOriginEndpointResponse_timeDelaySeconds,
    describeOriginEndpointResponse_startoverWindowSeconds,
    describeOriginEndpointResponse_mssPackage,
    describeOriginEndpointResponse_arn,
    describeOriginEndpointResponse_whitelist,
    describeOriginEndpointResponse_url,
    describeOriginEndpointResponse_id,
    describeOriginEndpointResponse_description,
    describeOriginEndpointResponse_manifestName,
    describeOriginEndpointResponse_channelId,
    describeOriginEndpointResponse_authorization,
    describeOriginEndpointResponse_dashPackage,
    describeOriginEndpointResponse_cmafPackage,
    describeOriginEndpointResponse_hlsPackage,
    describeOriginEndpointResponse_origination,
    describeOriginEndpointResponse_httpStatus,

    -- ** ListChannels
    listChannels_nextToken,
    listChannels_maxResults,
    listChannelsResponse_nextToken,
    listChannelsResponse_channels,
    listChannelsResponse_httpStatus,

    -- ** ListHarvestJobs
    listHarvestJobs_nextToken,
    listHarvestJobs_includeStatus,
    listHarvestJobs_maxResults,
    listHarvestJobs_includeChannelId,
    listHarvestJobsResponse_nextToken,
    listHarvestJobsResponse_harvestJobs,
    listHarvestJobsResponse_httpStatus,

    -- ** ListOriginEndpoints
    listOriginEndpoints_nextToken,
    listOriginEndpoints_maxResults,
    listOriginEndpoints_channelId,
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
    rotateIngestEndpointCredentialsResponse_tags,
    rotateIngestEndpointCredentialsResponse_ingressAccessLogs,
    rotateIngestEndpointCredentialsResponse_arn,
    rotateIngestEndpointCredentialsResponse_id,
    rotateIngestEndpointCredentialsResponse_description,
    rotateIngestEndpointCredentialsResponse_egressAccessLogs,
    rotateIngestEndpointCredentialsResponse_hlsIngest,
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
    updateChannelResponse_tags,
    updateChannelResponse_ingressAccessLogs,
    updateChannelResponse_arn,
    updateChannelResponse_id,
    updateChannelResponse_description,
    updateChannelResponse_egressAccessLogs,
    updateChannelResponse_hlsIngest,
    updateChannelResponse_httpStatus,

    -- ** UpdateOriginEndpoint
    updateOriginEndpoint_timeDelaySeconds,
    updateOriginEndpoint_startoverWindowSeconds,
    updateOriginEndpoint_mssPackage,
    updateOriginEndpoint_whitelist,
    updateOriginEndpoint_description,
    updateOriginEndpoint_manifestName,
    updateOriginEndpoint_authorization,
    updateOriginEndpoint_dashPackage,
    updateOriginEndpoint_cmafPackage,
    updateOriginEndpoint_hlsPackage,
    updateOriginEndpoint_origination,
    updateOriginEndpoint_id,
    updateOriginEndpointResponse_tags,
    updateOriginEndpointResponse_timeDelaySeconds,
    updateOriginEndpointResponse_startoverWindowSeconds,
    updateOriginEndpointResponse_mssPackage,
    updateOriginEndpointResponse_arn,
    updateOriginEndpointResponse_whitelist,
    updateOriginEndpointResponse_url,
    updateOriginEndpointResponse_id,
    updateOriginEndpointResponse_description,
    updateOriginEndpointResponse_manifestName,
    updateOriginEndpointResponse_channelId,
    updateOriginEndpointResponse_authorization,
    updateOriginEndpointResponse_dashPackage,
    updateOriginEndpointResponse_cmafPackage,
    updateOriginEndpointResponse_hlsPackage,
    updateOriginEndpointResponse_origination,
    updateOriginEndpointResponse_httpStatus,

    -- * Types

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** Channel
    channel_tags,
    channel_ingressAccessLogs,
    channel_arn,
    channel_id,
    channel_description,
    channel_egressAccessLogs,
    channel_hlsIngest,

    -- ** CmafEncryption
    cmafEncryption_constantInitializationVector,
    cmafEncryption_keyRotationIntervalSeconds,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_streamSelection,
    cmafPackage_hlsManifests,
    cmafPackage_segmentPrefix,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_encryption,

    -- ** CmafPackageCreateOrUpdateParameters
    cmafPackageCreateOrUpdateParameters_streamSelection,
    cmafPackageCreateOrUpdateParameters_hlsManifests,
    cmafPackageCreateOrUpdateParameters_segmentPrefix,
    cmafPackageCreateOrUpdateParameters_segmentDurationSeconds,
    cmafPackageCreateOrUpdateParameters_encryption,

    -- ** DashEncryption
    dashEncryption_keyRotationIntervalSeconds,
    dashEncryption_spekeKeyProvider,

    -- ** DashPackage
    dashPackage_profile,
    dashPackage_adsOnDeliveryRestrictions,
    dashPackage_segmentTemplateFormat,
    dashPackage_streamSelection,
    dashPackage_suggestedPresentationDelaySeconds,
    dashPackage_manifestWindowSeconds,
    dashPackage_minUpdatePeriodSeconds,
    dashPackage_adTriggers,
    dashPackage_segmentDurationSeconds,
    dashPackage_periodTriggers,
    dashPackage_utcTiming,
    dashPackage_encryption,
    dashPackage_manifestLayout,
    dashPackage_minBufferTimeSeconds,
    dashPackage_utcTimingUri,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** HarvestJob
    harvestJob_arn,
    harvestJob_status,
    harvestJob_endTime,
    harvestJob_id,
    harvestJob_channelId,
    harvestJob_s3Destination,
    harvestJob_originEndpointId,
    harvestJob_createdAt,
    harvestJob_startTime,

    -- ** HlsEncryption
    hlsEncryption_constantInitializationVector,
    hlsEncryption_repeatExtXKey,
    hlsEncryption_encryptionMethod,
    hlsEncryption_keyRotationIntervalSeconds,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsIngest
    hlsIngest_ingestEndpoints,

    -- ** HlsManifest
    hlsManifest_playlistWindowSeconds,
    hlsManifest_url,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_adMarkers,
    hlsManifest_manifestName,
    hlsManifest_playlistType,
    hlsManifest_id,

    -- ** HlsManifestCreateOrUpdateParameters
    hlsManifestCreateOrUpdateParameters_playlistWindowSeconds,
    hlsManifestCreateOrUpdateParameters_adsOnDeliveryRestrictions,
    hlsManifestCreateOrUpdateParameters_programDateTimeIntervalSeconds,
    hlsManifestCreateOrUpdateParameters_includeIframeOnlyStream,
    hlsManifestCreateOrUpdateParameters_adMarkers,
    hlsManifestCreateOrUpdateParameters_adTriggers,
    hlsManifestCreateOrUpdateParameters_manifestName,
    hlsManifestCreateOrUpdateParameters_playlistType,
    hlsManifestCreateOrUpdateParameters_id,

    -- ** HlsPackage
    hlsPackage_playlistWindowSeconds,
    hlsPackage_adsOnDeliveryRestrictions,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_streamSelection,
    hlsPackage_programDateTimeIntervalSeconds,
    hlsPackage_includeIframeOnlyStream,
    hlsPackage_adMarkers,
    hlsPackage_adTriggers,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_encryption,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_playlistType,

    -- ** IngestEndpoint
    ingestEndpoint_password,
    ingestEndpoint_username,
    ingestEndpoint_url,
    ingestEndpoint_id,

    -- ** IngressAccessLogs
    ingressAccessLogs_logGroupName,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssPackage
    mssPackage_streamSelection,
    mssPackage_manifestWindowSeconds,
    mssPackage_segmentDurationSeconds,
    mssPackage_encryption,

    -- ** OriginEndpoint
    originEndpoint_tags,
    originEndpoint_timeDelaySeconds,
    originEndpoint_startoverWindowSeconds,
    originEndpoint_mssPackage,
    originEndpoint_arn,
    originEndpoint_whitelist,
    originEndpoint_url,
    originEndpoint_id,
    originEndpoint_description,
    originEndpoint_manifestName,
    originEndpoint_channelId,
    originEndpoint_authorization,
    originEndpoint_dashPackage,
    originEndpoint_cmafPackage,
    originEndpoint_hlsPackage,
    originEndpoint_origination,

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
    streamSelection_streamOrder,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
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
