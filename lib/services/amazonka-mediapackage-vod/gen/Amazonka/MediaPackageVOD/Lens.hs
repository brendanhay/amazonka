{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaPackageVOD.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Lens
  ( -- * Operations

    -- ** ConfigureLogs
    configureLogs_egressAccessLogs,
    configureLogs_id,
    configureLogsResponse_arn,
    configureLogsResponse_authorization,
    configureLogsResponse_createdAt,
    configureLogsResponse_domainName,
    configureLogsResponse_egressAccessLogs,
    configureLogsResponse_id,
    configureLogsResponse_tags,
    configureLogsResponse_httpStatus,

    -- ** CreateAsset
    createAsset_resourceId,
    createAsset_tags,
    createAsset_sourceArn,
    createAsset_id,
    createAsset_packagingGroupId,
    createAsset_sourceRoleArn,
    createAssetResponse_arn,
    createAssetResponse_createdAt,
    createAssetResponse_egressEndpoints,
    createAssetResponse_id,
    createAssetResponse_packagingGroupId,
    createAssetResponse_resourceId,
    createAssetResponse_sourceArn,
    createAssetResponse_sourceRoleArn,
    createAssetResponse_tags,
    createAssetResponse_httpStatus,

    -- ** CreatePackagingConfiguration
    createPackagingConfiguration_cmafPackage,
    createPackagingConfiguration_dashPackage,
    createPackagingConfiguration_hlsPackage,
    createPackagingConfiguration_mssPackage,
    createPackagingConfiguration_tags,
    createPackagingConfiguration_id,
    createPackagingConfiguration_packagingGroupId,
    createPackagingConfigurationResponse_arn,
    createPackagingConfigurationResponse_cmafPackage,
    createPackagingConfigurationResponse_createdAt,
    createPackagingConfigurationResponse_dashPackage,
    createPackagingConfigurationResponse_hlsPackage,
    createPackagingConfigurationResponse_id,
    createPackagingConfigurationResponse_mssPackage,
    createPackagingConfigurationResponse_packagingGroupId,
    createPackagingConfigurationResponse_tags,
    createPackagingConfigurationResponse_httpStatus,

    -- ** CreatePackagingGroup
    createPackagingGroup_authorization,
    createPackagingGroup_egressAccessLogs,
    createPackagingGroup_tags,
    createPackagingGroup_id,
    createPackagingGroupResponse_arn,
    createPackagingGroupResponse_authorization,
    createPackagingGroupResponse_createdAt,
    createPackagingGroupResponse_domainName,
    createPackagingGroupResponse_egressAccessLogs,
    createPackagingGroupResponse_id,
    createPackagingGroupResponse_tags,
    createPackagingGroupResponse_httpStatus,

    -- ** DeleteAsset
    deleteAsset_id,
    deleteAssetResponse_httpStatus,

    -- ** DeletePackagingConfiguration
    deletePackagingConfiguration_id,
    deletePackagingConfigurationResponse_httpStatus,

    -- ** DeletePackagingGroup
    deletePackagingGroup_id,
    deletePackagingGroupResponse_httpStatus,

    -- ** DescribeAsset
    describeAsset_id,
    describeAssetResponse_arn,
    describeAssetResponse_createdAt,
    describeAssetResponse_egressEndpoints,
    describeAssetResponse_id,
    describeAssetResponse_packagingGroupId,
    describeAssetResponse_resourceId,
    describeAssetResponse_sourceArn,
    describeAssetResponse_sourceRoleArn,
    describeAssetResponse_tags,
    describeAssetResponse_httpStatus,

    -- ** DescribePackagingConfiguration
    describePackagingConfiguration_id,
    describePackagingConfigurationResponse_arn,
    describePackagingConfigurationResponse_cmafPackage,
    describePackagingConfigurationResponse_createdAt,
    describePackagingConfigurationResponse_dashPackage,
    describePackagingConfigurationResponse_hlsPackage,
    describePackagingConfigurationResponse_id,
    describePackagingConfigurationResponse_mssPackage,
    describePackagingConfigurationResponse_packagingGroupId,
    describePackagingConfigurationResponse_tags,
    describePackagingConfigurationResponse_httpStatus,

    -- ** DescribePackagingGroup
    describePackagingGroup_id,
    describePackagingGroupResponse_approximateAssetCount,
    describePackagingGroupResponse_arn,
    describePackagingGroupResponse_authorization,
    describePackagingGroupResponse_createdAt,
    describePackagingGroupResponse_domainName,
    describePackagingGroupResponse_egressAccessLogs,
    describePackagingGroupResponse_id,
    describePackagingGroupResponse_tags,
    describePackagingGroupResponse_httpStatus,

    -- ** ListAssets
    listAssets_maxResults,
    listAssets_nextToken,
    listAssets_packagingGroupId,
    listAssetsResponse_assets,
    listAssetsResponse_nextToken,
    listAssetsResponse_httpStatus,

    -- ** ListPackagingConfigurations
    listPackagingConfigurations_maxResults,
    listPackagingConfigurations_nextToken,
    listPackagingConfigurations_packagingGroupId,
    listPackagingConfigurationsResponse_nextToken,
    listPackagingConfigurationsResponse_packagingConfigurations,
    listPackagingConfigurationsResponse_httpStatus,

    -- ** ListPackagingGroups
    listPackagingGroups_maxResults,
    listPackagingGroups_nextToken,
    listPackagingGroupsResponse_nextToken,
    listPackagingGroupsResponse_packagingGroups,
    listPackagingGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,

    -- ** UpdatePackagingGroup
    updatePackagingGroup_authorization,
    updatePackagingGroup_id,
    updatePackagingGroupResponse_approximateAssetCount,
    updatePackagingGroupResponse_arn,
    updatePackagingGroupResponse_authorization,
    updatePackagingGroupResponse_createdAt,
    updatePackagingGroupResponse_domainName,
    updatePackagingGroupResponse_egressAccessLogs,
    updatePackagingGroupResponse_id,
    updatePackagingGroupResponse_tags,
    updatePackagingGroupResponse_httpStatus,

    -- * Types

    -- ** AssetShallow
    assetShallow_arn,
    assetShallow_createdAt,
    assetShallow_id,
    assetShallow_packagingGroupId,
    assetShallow_resourceId,
    assetShallow_sourceArn,
    assetShallow_sourceRoleArn,
    assetShallow_tags,

    -- ** Authorization
    authorization_secretsRoleArn,
    authorization_cdnIdentifierSecret,

    -- ** CmafEncryption
    cmafEncryption_constantInitializationVector,
    cmafEncryption_spekeKeyProvider,

    -- ** CmafPackage
    cmafPackage_encryption,
    cmafPackage_includeEncoderConfigurationInSegments,
    cmafPackage_segmentDurationSeconds,
    cmafPackage_hlsManifests,

    -- ** DashEncryption
    dashEncryption_spekeKeyProvider,

    -- ** DashManifest
    dashManifest_manifestLayout,
    dashManifest_manifestName,
    dashManifest_minBufferTimeSeconds,
    dashManifest_profile,
    dashManifest_scteMarkersSource,
    dashManifest_streamSelection,

    -- ** DashPackage
    dashPackage_encryption,
    dashPackage_includeEncoderConfigurationInSegments,
    dashPackage_includeIframeOnlyStream,
    dashPackage_periodTriggers,
    dashPackage_segmentDurationSeconds,
    dashPackage_segmentTemplateFormat,
    dashPackage_dashManifests,

    -- ** EgressAccessLogs
    egressAccessLogs_logGroupName,

    -- ** EgressEndpoint
    egressEndpoint_packagingConfigurationId,
    egressEndpoint_status,
    egressEndpoint_url,

    -- ** EncryptionContractConfiguration
    encryptionContractConfiguration_presetSpeke20Audio,
    encryptionContractConfiguration_presetSpeke20Video,

    -- ** HlsEncryption
    hlsEncryption_constantInitializationVector,
    hlsEncryption_encryptionMethod,
    hlsEncryption_spekeKeyProvider,

    -- ** HlsManifest
    hlsManifest_adMarkers,
    hlsManifest_includeIframeOnlyStream,
    hlsManifest_manifestName,
    hlsManifest_programDateTimeIntervalSeconds,
    hlsManifest_repeatExtXKey,
    hlsManifest_streamSelection,

    -- ** HlsPackage
    hlsPackage_encryption,
    hlsPackage_includeDvbSubtitles,
    hlsPackage_segmentDurationSeconds,
    hlsPackage_useAudioRenditionGroup,
    hlsPackage_hlsManifests,

    -- ** MssEncryption
    mssEncryption_spekeKeyProvider,

    -- ** MssManifest
    mssManifest_manifestName,
    mssManifest_streamSelection,

    -- ** MssPackage
    mssPackage_encryption,
    mssPackage_segmentDurationSeconds,
    mssPackage_mssManifests,

    -- ** PackagingConfiguration
    packagingConfiguration_arn,
    packagingConfiguration_cmafPackage,
    packagingConfiguration_createdAt,
    packagingConfiguration_dashPackage,
    packagingConfiguration_hlsPackage,
    packagingConfiguration_id,
    packagingConfiguration_mssPackage,
    packagingConfiguration_packagingGroupId,
    packagingConfiguration_tags,

    -- ** PackagingGroup
    packagingGroup_approximateAssetCount,
    packagingGroup_arn,
    packagingGroup_authorization,
    packagingGroup_createdAt,
    packagingGroup_domainName,
    packagingGroup_egressAccessLogs,
    packagingGroup_id,
    packagingGroup_tags,

    -- ** SpekeKeyProvider
    spekeKeyProvider_encryptionContractConfiguration,
    spekeKeyProvider_systemIds,
    spekeKeyProvider_url,
    spekeKeyProvider_roleArn,

    -- ** StreamSelection
    streamSelection_maxVideoBitsPerSecond,
    streamSelection_minVideoBitsPerSecond,
    streamSelection_streamOrder,
  )
where

import Amazonka.MediaPackageVOD.ConfigureLogs
import Amazonka.MediaPackageVOD.CreateAsset
import Amazonka.MediaPackageVOD.CreatePackagingConfiguration
import Amazonka.MediaPackageVOD.CreatePackagingGroup
import Amazonka.MediaPackageVOD.DeleteAsset
import Amazonka.MediaPackageVOD.DeletePackagingConfiguration
import Amazonka.MediaPackageVOD.DeletePackagingGroup
import Amazonka.MediaPackageVOD.DescribeAsset
import Amazonka.MediaPackageVOD.DescribePackagingConfiguration
import Amazonka.MediaPackageVOD.DescribePackagingGroup
import Amazonka.MediaPackageVOD.ListAssets
import Amazonka.MediaPackageVOD.ListPackagingConfigurations
import Amazonka.MediaPackageVOD.ListPackagingGroups
import Amazonka.MediaPackageVOD.ListTagsForResource
import Amazonka.MediaPackageVOD.TagResource
import Amazonka.MediaPackageVOD.Types.AssetShallow
import Amazonka.MediaPackageVOD.Types.Authorization
import Amazonka.MediaPackageVOD.Types.CmafEncryption
import Amazonka.MediaPackageVOD.Types.CmafPackage
import Amazonka.MediaPackageVOD.Types.DashEncryption
import Amazonka.MediaPackageVOD.Types.DashManifest
import Amazonka.MediaPackageVOD.Types.DashPackage
import Amazonka.MediaPackageVOD.Types.EgressAccessLogs
import Amazonka.MediaPackageVOD.Types.EgressEndpoint
import Amazonka.MediaPackageVOD.Types.EncryptionContractConfiguration
import Amazonka.MediaPackageVOD.Types.HlsEncryption
import Amazonka.MediaPackageVOD.Types.HlsManifest
import Amazonka.MediaPackageVOD.Types.HlsPackage
import Amazonka.MediaPackageVOD.Types.MssEncryption
import Amazonka.MediaPackageVOD.Types.MssManifest
import Amazonka.MediaPackageVOD.Types.MssPackage
import Amazonka.MediaPackageVOD.Types.PackagingConfiguration
import Amazonka.MediaPackageVOD.Types.PackagingGroup
import Amazonka.MediaPackageVOD.Types.SpekeKeyProvider
import Amazonka.MediaPackageVOD.Types.StreamSelection
import Amazonka.MediaPackageVOD.UntagResource
import Amazonka.MediaPackageVOD.UpdatePackagingGroup
