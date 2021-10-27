{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackageVOD.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackageVOD.Lens
  ( -- * Operations

    -- ** CreatePackagingGroup
    createPackagingGroup_authorization,
    createPackagingGroup_egressAccessLogs,
    createPackagingGroup_tags,
    createPackagingGroup_id,
    createPackagingGroupResponse_arn,
    createPackagingGroupResponse_authorization,
    createPackagingGroupResponse_domainName,
    createPackagingGroupResponse_id,
    createPackagingGroupResponse_egressAccessLogs,
    createPackagingGroupResponse_tags,
    createPackagingGroupResponse_httpStatus,

    -- ** ConfigureLogs
    configureLogs_egressAccessLogs,
    configureLogs_id,
    configureLogsResponse_arn,
    configureLogsResponse_authorization,
    configureLogsResponse_domainName,
    configureLogsResponse_id,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_tags,
    configureLogsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DescribePackagingGroup
    describePackagingGroup_id,
    describePackagingGroupResponse_arn,
    describePackagingGroupResponse_authorization,
    describePackagingGroupResponse_domainName,
    describePackagingGroupResponse_id,
    describePackagingGroupResponse_egressAccessLogs,
    describePackagingGroupResponse_tags,
    describePackagingGroupResponse_httpStatus,

    -- ** DescribeAsset
    describeAsset_id,
    describeAssetResponse_resourceId,
    describeAssetResponse_arn,
    describeAssetResponse_createdAt,
    describeAssetResponse_packagingGroupId,
    describeAssetResponse_sourceArn,
    describeAssetResponse_sourceRoleArn,
    describeAssetResponse_id,
    describeAssetResponse_egressEndpoints,
    describeAssetResponse_tags,
    describeAssetResponse_httpStatus,

    -- ** DeletePackagingConfiguration
    deletePackagingConfiguration_id,
    deletePackagingConfigurationResponse_httpStatus,

    -- ** ListPackagingGroups
    listPackagingGroups_nextToken,
    listPackagingGroups_maxResults,
    listPackagingGroupsResponse_packagingGroups,
    listPackagingGroupsResponse_nextToken,
    listPackagingGroupsResponse_httpStatus,

    -- ** DeleteAsset
    deleteAsset_id,
    deleteAssetResponse_httpStatus,

    -- ** UpdatePackagingGroup
    updatePackagingGroup_authorization,
    updatePackagingGroup_id,
    updatePackagingGroupResponse_arn,
    updatePackagingGroupResponse_authorization,
    updatePackagingGroupResponse_domainName,
    updatePackagingGroupResponse_id,
    updatePackagingGroupResponse_egressAccessLogs,
    updatePackagingGroupResponse_tags,
    updatePackagingGroupResponse_httpStatus,

    -- ** DeletePackagingGroup
    deletePackagingGroup_id,
    deletePackagingGroupResponse_httpStatus,

    -- ** CreateAsset
    createAsset_resourceId,
    createAsset_tags,
    createAsset_sourceArn,
    createAsset_id,
    createAsset_packagingGroupId,
    createAsset_sourceRoleArn,
    createAssetResponse_resourceId,
    createAssetResponse_arn,
    createAssetResponse_createdAt,
    createAssetResponse_packagingGroupId,
    createAssetResponse_sourceArn,
    createAssetResponse_sourceRoleArn,
    createAssetResponse_id,
    createAssetResponse_egressEndpoints,
    createAssetResponse_tags,
    createAssetResponse_httpStatus,

    -- ** DescribePackagingConfiguration
    describePackagingConfiguration_id,
    describePackagingConfigurationResponse_hlsPackage,
    describePackagingConfigurationResponse_arn,
    describePackagingConfigurationResponse_packagingGroupId,
    describePackagingConfigurationResponse_dashPackage,
    describePackagingConfigurationResponse_mssPackage,
    describePackagingConfigurationResponse_id,
    describePackagingConfigurationResponse_cmafPackage,
    describePackagingConfigurationResponse_tags,
    describePackagingConfigurationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** CreatePackagingConfiguration
    createPackagingConfiguration_hlsPackage,
    createPackagingConfiguration_dashPackage,
    createPackagingConfiguration_mssPackage,
    createPackagingConfiguration_cmafPackage,
    createPackagingConfiguration_tags,
    createPackagingConfiguration_id,
    createPackagingConfiguration_packagingGroupId,
    createPackagingConfigurationResponse_hlsPackage,
    createPackagingConfigurationResponse_arn,
    createPackagingConfigurationResponse_packagingGroupId,
    createPackagingConfigurationResponse_dashPackage,
    createPackagingConfigurationResponse_mssPackage,
    createPackagingConfigurationResponse_id,
    createPackagingConfigurationResponse_cmafPackage,
    createPackagingConfigurationResponse_tags,
    createPackagingConfigurationResponse_httpStatus,

    -- ** ListPackagingConfigurations
    listPackagingConfigurations_packagingGroupId,
    listPackagingConfigurations_nextToken,
    listPackagingConfigurations_maxResults,
    listPackagingConfigurationsResponse_packagingConfigurations,
    listPackagingConfigurationsResponse_nextToken,
    listPackagingConfigurationsResponse_httpStatus,

    -- ** ListAssets
    listAssets_packagingGroupId,
    listAssets_nextToken,
    listAssets_maxResults,
    listAssetsResponse_nextToken,
    listAssetsResponse_assets,
    listAssetsResponse_httpStatus,

    -- * Types

    -- ** AssetShallow
    assetShallow_resourceId,
    assetShallow_arn,
    assetShallow_createdAt,
    assetShallow_packagingGroupId,
    assetShallow_sourceArn,
    assetShallow_sourceRoleArn,
    assetShallow_id,
    assetShallow_tags,

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** CmafEncryption
    cmafEncryption_constantInitializationVector,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_includeEncoderConfigurationInSegments,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_encryption,
    cmafPackage_hlsManifests,

    -- ** DashEncryption
    dashEncryption_spekeKeyProvider,

    -- ** DashManifest
    dashManifest_minBufferTimeSeconds,
    dashManifest_manifestName,
    dashManifest_profile,
    dashManifest_streamSelection,
    dashManifest_manifestLayout,

    -- ** DashPackage
    dashPackage_includeEncoderConfigurationInSegments,
    dashPackage_segmentTemplateFormat,
    dashPackage_segmentDurationSeconds,
    dashPackage_encryption,
    dashPackage_periodTriggers,
    dashPackage_dashManifests,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EgressEndpoint
    egressEndpoint_status,
    egressEndpoint_url,
    egressEndpoint_packagingConfigurationId,

    -- ** HlsEncryption
    hlsEncryption_encryptionMethod,
    hlsEncryption_constantInitializationVector,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsManifest
    hlsManifest_manifestName,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_streamSelection,
    hlsManifest_adMarkers,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_repeatExtXKey,

    -- ** HlsPackage
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_encryption,
    hlsPackage_hlsManifests,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssManifest
    mssManifest_manifestName,
    mssManifest_streamSelection,

    -- ** MssPackage
    mssPackage_segmentDurationSeconds,
    mssPackage_encryption,
    mssPackage_mssManifests,

    -- ** PackagingConfiguration
    packagingConfiguration_hlsPackage,
    packagingConfiguration_arn,
    packagingConfiguration_packagingGroupId,
    packagingConfiguration_dashPackage,
    packagingConfiguration_mssPackage,
    packagingConfiguration_id,
    packagingConfiguration_cmafPackage,
    packagingConfiguration_tags,

    -- ** PackagingGroup
    packagingGroup_arn,
    packagingGroup_authorization,
    packagingGroup_domainName,
    packagingGroup_id,
    packagingGroup_egressAccessLogs,
    packagingGroup_tags,

    -- ** SpekeKeyProvider
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- ** StreamSelection
    streamSelection_streamOrder,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_maxVideoBitsPerSecond,
  )
where

import Network.AWS.MediaPackageVOD.ConfigureLogs
import Network.AWS.MediaPackageVOD.CreateAsset
import Network.AWS.MediaPackageVOD.CreatePackagingConfiguration
import Network.AWS.MediaPackageVOD.CreatePackagingGroup
import Network.AWS.MediaPackageVOD.DeleteAsset
import Network.AWS.MediaPackageVOD.DeletePackagingConfiguration
import Network.AWS.MediaPackageVOD.DeletePackagingGroup
import Network.AWS.MediaPackageVOD.DescribeAsset
import Network.AWS.MediaPackageVOD.DescribePackagingConfiguration
import Network.AWS.MediaPackageVOD.DescribePackagingGroup
import Network.AWS.MediaPackageVOD.ListAssets
import Network.AWS.MediaPackageVOD.ListPackagingConfigurations
import Network.AWS.MediaPackageVOD.ListPackagingGroups
import Network.AWS.MediaPackageVOD.ListTagsForResource
import Network.AWS.MediaPackageVOD.TagResource
import Network.AWS.MediaPackageVOD.Types.AssetShallow
import Network.AWS.MediaPackageVOD.Types.Authorization
import Network.AWS.MediaPackageVOD.Types.CmafEncryption
import Network.AWS.MediaPackageVOD.Types.CmafPackage
import Network.AWS.MediaPackageVOD.Types.DashEncryption
import Network.AWS.MediaPackageVOD.Types.DashManifest
import Network.AWS.MediaPackageVOD.Types.DashPackage
import Network.AWS.MediaPackageVOD.Types.EgressAccessLogs
import Network.AWS.MediaPackageVOD.Types.EgressEndpoint
import Network.AWS.MediaPackageVOD.Types.HlsEncryption
import Network.AWS.MediaPackageVOD.Types.HlsManifest
import Network.AWS.MediaPackageVOD.Types.HlsPackage
import Network.AWS.MediaPackageVOD.Types.MssEncryption
import Network.AWS.MediaPackageVOD.Types.MssManifest
import Network.AWS.MediaPackageVOD.Types.MssPackage
import Network.AWS.MediaPackageVOD.Types.PackagingConfiguration
import Network.AWS.MediaPackageVOD.Types.PackagingGroup
import Network.AWS.MediaPackageVOD.Types.SpekeKeyProvider
import Network.AWS.MediaPackageVOD.Types.StreamSelection
import Network.AWS.MediaPackageVOD.UntagResource
import Network.AWS.MediaPackageVOD.UpdatePackagingGroup
