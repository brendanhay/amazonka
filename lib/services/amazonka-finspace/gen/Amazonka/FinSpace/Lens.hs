{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpace.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpace.Lens
  ( -- * Operations

    -- ** CreateEnvironment
    createEnvironment_dataBundles,
    createEnvironment_description,
    createEnvironment_federationMode,
    createEnvironment_federationParameters,
    createEnvironment_kmsKeyId,
    createEnvironment_superuserParameters,
    createEnvironment_tags,
    createEnvironment_name,
    createEnvironmentResponse_environmentArn,
    createEnvironmentResponse_environmentId,
    createEnvironmentResponse_environmentUrl,
    createEnvironmentResponse_httpStatus,

    -- ** CreateKxChangeset
    createKxChangeset_environmentId,
    createKxChangeset_databaseName,
    createKxChangeset_changeRequests,
    createKxChangeset_clientToken,
    createKxChangesetResponse_changeRequests,
    createKxChangesetResponse_changesetId,
    createKxChangesetResponse_createdTimestamp,
    createKxChangesetResponse_databaseName,
    createKxChangesetResponse_environmentId,
    createKxChangesetResponse_errorInfo,
    createKxChangesetResponse_lastModifiedTimestamp,
    createKxChangesetResponse_status,
    createKxChangesetResponse_httpStatus,

    -- ** CreateKxCluster
    createKxCluster_autoScalingConfiguration,
    createKxCluster_availabilityZoneId,
    createKxCluster_cacheStorageConfigurations,
    createKxCluster_clientToken,
    createKxCluster_clusterDescription,
    createKxCluster_code,
    createKxCluster_commandLineArguments,
    createKxCluster_databases,
    createKxCluster_executionRole,
    createKxCluster_initializationScript,
    createKxCluster_savedownStorageConfiguration,
    createKxCluster_tags,
    createKxCluster_vpcConfiguration,
    createKxCluster_environmentId,
    createKxCluster_clusterName,
    createKxCluster_clusterType,
    createKxCluster_capacityConfiguration,
    createKxCluster_releaseLabel,
    createKxCluster_azMode,
    createKxClusterResponse_autoScalingConfiguration,
    createKxClusterResponse_availabilityZoneId,
    createKxClusterResponse_azMode,
    createKxClusterResponse_cacheStorageConfigurations,
    createKxClusterResponse_capacityConfiguration,
    createKxClusterResponse_clusterDescription,
    createKxClusterResponse_clusterName,
    createKxClusterResponse_clusterType,
    createKxClusterResponse_code,
    createKxClusterResponse_commandLineArguments,
    createKxClusterResponse_createdTimestamp,
    createKxClusterResponse_databases,
    createKxClusterResponse_environmentId,
    createKxClusterResponse_executionRole,
    createKxClusterResponse_initializationScript,
    createKxClusterResponse_lastModifiedTimestamp,
    createKxClusterResponse_releaseLabel,
    createKxClusterResponse_savedownStorageConfiguration,
    createKxClusterResponse_status,
    createKxClusterResponse_statusReason,
    createKxClusterResponse_vpcConfiguration,
    createKxClusterResponse_httpStatus,

    -- ** CreateKxDatabase
    createKxDatabase_description,
    createKxDatabase_tags,
    createKxDatabase_environmentId,
    createKxDatabase_databaseName,
    createKxDatabase_clientToken,
    createKxDatabaseResponse_createdTimestamp,
    createKxDatabaseResponse_databaseArn,
    createKxDatabaseResponse_databaseName,
    createKxDatabaseResponse_description,
    createKxDatabaseResponse_environmentId,
    createKxDatabaseResponse_lastModifiedTimestamp,
    createKxDatabaseResponse_httpStatus,

    -- ** CreateKxEnvironment
    createKxEnvironment_clientToken,
    createKxEnvironment_description,
    createKxEnvironment_tags,
    createKxEnvironment_name,
    createKxEnvironment_kmsKeyId,
    createKxEnvironmentResponse_creationTimestamp,
    createKxEnvironmentResponse_description,
    createKxEnvironmentResponse_environmentArn,
    createKxEnvironmentResponse_environmentId,
    createKxEnvironmentResponse_kmsKeyId,
    createKxEnvironmentResponse_name,
    createKxEnvironmentResponse_status,
    createKxEnvironmentResponse_httpStatus,

    -- ** CreateKxUser
    createKxUser_clientToken,
    createKxUser_tags,
    createKxUser_environmentId,
    createKxUser_userName,
    createKxUser_iamRole,
    createKxUserResponse_environmentId,
    createKxUserResponse_iamRole,
    createKxUserResponse_userArn,
    createKxUserResponse_userName,
    createKxUserResponse_httpStatus,

    -- ** DeleteEnvironment
    deleteEnvironment_environmentId,
    deleteEnvironmentResponse_httpStatus,

    -- ** DeleteKxCluster
    deleteKxCluster_clientToken,
    deleteKxCluster_environmentId,
    deleteKxCluster_clusterName,
    deleteKxClusterResponse_httpStatus,

    -- ** DeleteKxDatabase
    deleteKxDatabase_environmentId,
    deleteKxDatabase_databaseName,
    deleteKxDatabase_clientToken,
    deleteKxDatabaseResponse_httpStatus,

    -- ** DeleteKxEnvironment
    deleteKxEnvironment_environmentId,
    deleteKxEnvironmentResponse_httpStatus,

    -- ** DeleteKxUser
    deleteKxUser_userName,
    deleteKxUser_environmentId,
    deleteKxUserResponse_httpStatus,

    -- ** GetEnvironment
    getEnvironment_environmentId,
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,

    -- ** GetKxChangeset
    getKxChangeset_environmentId,
    getKxChangeset_databaseName,
    getKxChangeset_changesetId,
    getKxChangesetResponse_activeFromTimestamp,
    getKxChangesetResponse_changeRequests,
    getKxChangesetResponse_changesetId,
    getKxChangesetResponse_createdTimestamp,
    getKxChangesetResponse_databaseName,
    getKxChangesetResponse_environmentId,
    getKxChangesetResponse_errorInfo,
    getKxChangesetResponse_lastModifiedTimestamp,
    getKxChangesetResponse_status,
    getKxChangesetResponse_httpStatus,

    -- ** GetKxCluster
    getKxCluster_environmentId,
    getKxCluster_clusterName,
    getKxClusterResponse_autoScalingConfiguration,
    getKxClusterResponse_availabilityZoneId,
    getKxClusterResponse_azMode,
    getKxClusterResponse_cacheStorageConfigurations,
    getKxClusterResponse_capacityConfiguration,
    getKxClusterResponse_clusterDescription,
    getKxClusterResponse_clusterName,
    getKxClusterResponse_clusterType,
    getKxClusterResponse_code,
    getKxClusterResponse_commandLineArguments,
    getKxClusterResponse_createdTimestamp,
    getKxClusterResponse_databases,
    getKxClusterResponse_executionRole,
    getKxClusterResponse_initializationScript,
    getKxClusterResponse_lastModifiedTimestamp,
    getKxClusterResponse_releaseLabel,
    getKxClusterResponse_savedownStorageConfiguration,
    getKxClusterResponse_status,
    getKxClusterResponse_statusReason,
    getKxClusterResponse_vpcConfiguration,
    getKxClusterResponse_httpStatus,

    -- ** GetKxConnectionString
    getKxConnectionString_userArn,
    getKxConnectionString_environmentId,
    getKxConnectionString_clusterName,
    getKxConnectionStringResponse_signedConnectionString,
    getKxConnectionStringResponse_httpStatus,

    -- ** GetKxDatabase
    getKxDatabase_environmentId,
    getKxDatabase_databaseName,
    getKxDatabaseResponse_createdTimestamp,
    getKxDatabaseResponse_databaseArn,
    getKxDatabaseResponse_databaseName,
    getKxDatabaseResponse_description,
    getKxDatabaseResponse_environmentId,
    getKxDatabaseResponse_lastCompletedChangesetId,
    getKxDatabaseResponse_lastModifiedTimestamp,
    getKxDatabaseResponse_numBytes,
    getKxDatabaseResponse_numChangesets,
    getKxDatabaseResponse_numFiles,
    getKxDatabaseResponse_httpStatus,

    -- ** GetKxEnvironment
    getKxEnvironment_environmentId,
    getKxEnvironmentResponse_availabilityZoneIds,
    getKxEnvironmentResponse_awsAccountId,
    getKxEnvironmentResponse_certificateAuthorityArn,
    getKxEnvironmentResponse_creationTimestamp,
    getKxEnvironmentResponse_customDNSConfiguration,
    getKxEnvironmentResponse_dedicatedServiceAccountId,
    getKxEnvironmentResponse_description,
    getKxEnvironmentResponse_dnsStatus,
    getKxEnvironmentResponse_environmentArn,
    getKxEnvironmentResponse_environmentId,
    getKxEnvironmentResponse_errorMessage,
    getKxEnvironmentResponse_kmsKeyId,
    getKxEnvironmentResponse_name,
    getKxEnvironmentResponse_status,
    getKxEnvironmentResponse_tgwStatus,
    getKxEnvironmentResponse_transitGatewayConfiguration,
    getKxEnvironmentResponse_updateTimestamp,
    getKxEnvironmentResponse_httpStatus,

    -- ** GetKxUser
    getKxUser_userName,
    getKxUser_environmentId,
    getKxUserResponse_environmentId,
    getKxUserResponse_iamRole,
    getKxUserResponse_userArn,
    getKxUserResponse_userName,
    getKxUserResponse_httpStatus,

    -- ** ListEnvironments
    listEnvironments_maxResults,
    listEnvironments_nextToken,
    listEnvironmentsResponse_environments,
    listEnvironmentsResponse_nextToken,
    listEnvironmentsResponse_httpStatus,

    -- ** ListKxChangesets
    listKxChangesets_maxResults,
    listKxChangesets_nextToken,
    listKxChangesets_environmentId,
    listKxChangesets_databaseName,
    listKxChangesetsResponse_kxChangesets,
    listKxChangesetsResponse_nextToken,
    listKxChangesetsResponse_httpStatus,

    -- ** ListKxClusterNodes
    listKxClusterNodes_maxResults,
    listKxClusterNodes_nextToken,
    listKxClusterNodes_clusterName,
    listKxClusterNodes_environmentId,
    listKxClusterNodesResponse_nextToken,
    listKxClusterNodesResponse_nodes,
    listKxClusterNodesResponse_httpStatus,

    -- ** ListKxClusters
    listKxClusters_clusterType,
    listKxClusters_maxResults,
    listKxClusters_nextToken,
    listKxClusters_environmentId,
    listKxClustersResponse_kxClusterSummaries,
    listKxClustersResponse_nextToken,
    listKxClustersResponse_httpStatus,

    -- ** ListKxDatabases
    listKxDatabases_maxResults,
    listKxDatabases_nextToken,
    listKxDatabases_environmentId,
    listKxDatabasesResponse_kxDatabases,
    listKxDatabasesResponse_nextToken,
    listKxDatabasesResponse_httpStatus,

    -- ** ListKxEnvironments
    listKxEnvironments_maxResults,
    listKxEnvironments_nextToken,
    listKxEnvironmentsResponse_environments,
    listKxEnvironmentsResponse_nextToken,
    listKxEnvironmentsResponse_httpStatus,

    -- ** ListKxUsers
    listKxUsers_maxResults,
    listKxUsers_nextToken,
    listKxUsers_environmentId,
    listKxUsersResponse_nextToken,
    listKxUsersResponse_users,
    listKxUsersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEnvironment
    updateEnvironment_description,
    updateEnvironment_federationMode,
    updateEnvironment_federationParameters,
    updateEnvironment_name,
    updateEnvironment_environmentId,
    updateEnvironmentResponse_environment,
    updateEnvironmentResponse_httpStatus,

    -- ** UpdateKxClusterDatabases
    updateKxClusterDatabases_clientToken,
    updateKxClusterDatabases_environmentId,
    updateKxClusterDatabases_clusterName,
    updateKxClusterDatabases_databases,
    updateKxClusterDatabasesResponse_httpStatus,

    -- ** UpdateKxDatabase
    updateKxDatabase_description,
    updateKxDatabase_environmentId,
    updateKxDatabase_databaseName,
    updateKxDatabase_clientToken,
    updateKxDatabaseResponse_databaseName,
    updateKxDatabaseResponse_description,
    updateKxDatabaseResponse_environmentId,
    updateKxDatabaseResponse_lastModifiedTimestamp,
    updateKxDatabaseResponse_httpStatus,

    -- ** UpdateKxEnvironment
    updateKxEnvironment_clientToken,
    updateKxEnvironment_description,
    updateKxEnvironment_name,
    updateKxEnvironment_environmentId,
    updateKxEnvironmentResponse_availabilityZoneIds,
    updateKxEnvironmentResponse_awsAccountId,
    updateKxEnvironmentResponse_creationTimestamp,
    updateKxEnvironmentResponse_customDNSConfiguration,
    updateKxEnvironmentResponse_dedicatedServiceAccountId,
    updateKxEnvironmentResponse_description,
    updateKxEnvironmentResponse_dnsStatus,
    updateKxEnvironmentResponse_environmentArn,
    updateKxEnvironmentResponse_environmentId,
    updateKxEnvironmentResponse_errorMessage,
    updateKxEnvironmentResponse_kmsKeyId,
    updateKxEnvironmentResponse_name,
    updateKxEnvironmentResponse_status,
    updateKxEnvironmentResponse_tgwStatus,
    updateKxEnvironmentResponse_transitGatewayConfiguration,
    updateKxEnvironmentResponse_updateTimestamp,
    updateKxEnvironmentResponse_httpStatus,

    -- ** UpdateKxEnvironmentNetwork
    updateKxEnvironmentNetwork_clientToken,
    updateKxEnvironmentNetwork_customDNSConfiguration,
    updateKxEnvironmentNetwork_transitGatewayConfiguration,
    updateKxEnvironmentNetwork_environmentId,
    updateKxEnvironmentNetworkResponse_availabilityZoneIds,
    updateKxEnvironmentNetworkResponse_awsAccountId,
    updateKxEnvironmentNetworkResponse_creationTimestamp,
    updateKxEnvironmentNetworkResponse_customDNSConfiguration,
    updateKxEnvironmentNetworkResponse_dedicatedServiceAccountId,
    updateKxEnvironmentNetworkResponse_description,
    updateKxEnvironmentNetworkResponse_dnsStatus,
    updateKxEnvironmentNetworkResponse_environmentArn,
    updateKxEnvironmentNetworkResponse_environmentId,
    updateKxEnvironmentNetworkResponse_errorMessage,
    updateKxEnvironmentNetworkResponse_kmsKeyId,
    updateKxEnvironmentNetworkResponse_name,
    updateKxEnvironmentNetworkResponse_status,
    updateKxEnvironmentNetworkResponse_tgwStatus,
    updateKxEnvironmentNetworkResponse_transitGatewayConfiguration,
    updateKxEnvironmentNetworkResponse_updateTimestamp,
    updateKxEnvironmentNetworkResponse_httpStatus,

    -- ** UpdateKxUser
    updateKxUser_clientToken,
    updateKxUser_environmentId,
    updateKxUser_userName,
    updateKxUser_iamRole,
    updateKxUserResponse_environmentId,
    updateKxUserResponse_iamRole,
    updateKxUserResponse_userArn,
    updateKxUserResponse_userName,
    updateKxUserResponse_httpStatus,

    -- * Types

    -- ** AutoScalingConfiguration
    autoScalingConfiguration_autoScalingMetric,
    autoScalingConfiguration_maxNodeCount,
    autoScalingConfiguration_metricTarget,
    autoScalingConfiguration_minNodeCount,
    autoScalingConfiguration_scaleInCooldownSeconds,
    autoScalingConfiguration_scaleOutCooldownSeconds,

    -- ** CapacityConfiguration
    capacityConfiguration_nodeCount,
    capacityConfiguration_nodeType,

    -- ** ChangeRequest
    changeRequest_s3Path,
    changeRequest_changeType,
    changeRequest_dbPath,

    -- ** CodeConfiguration
    codeConfiguration_s3Bucket,
    codeConfiguration_s3Key,
    codeConfiguration_s3ObjectVersion,

    -- ** CustomDNSServer
    customDNSServer_customDNSServerName,
    customDNSServer_customDNSServerIP,

    -- ** Environment
    environment_awsAccountId,
    environment_dedicatedServiceAccountId,
    environment_description,
    environment_environmentArn,
    environment_environmentId,
    environment_environmentUrl,
    environment_federationMode,
    environment_federationParameters,
    environment_kmsKeyId,
    environment_name,
    environment_sageMakerStudioDomainUrl,
    environment_status,

    -- ** ErrorInfo
    errorInfo_errorMessage,
    errorInfo_errorType,

    -- ** FederationParameters
    federationParameters_applicationCallBackURL,
    federationParameters_attributeMap,
    federationParameters_federationProviderName,
    federationParameters_federationURN,
    federationParameters_samlMetadataDocument,
    federationParameters_samlMetadataURL,

    -- ** KxCacheStorageConfiguration
    kxCacheStorageConfiguration_type,
    kxCacheStorageConfiguration_size,

    -- ** KxChangesetListEntry
    kxChangesetListEntry_activeFromTimestamp,
    kxChangesetListEntry_changesetId,
    kxChangesetListEntry_createdTimestamp,
    kxChangesetListEntry_lastModifiedTimestamp,
    kxChangesetListEntry_status,

    -- ** KxCluster
    kxCluster_availabilityZoneId,
    kxCluster_azMode,
    kxCluster_clusterDescription,
    kxCluster_clusterName,
    kxCluster_clusterType,
    kxCluster_createdTimestamp,
    kxCluster_executionRole,
    kxCluster_initializationScript,
    kxCluster_lastModifiedTimestamp,
    kxCluster_releaseLabel,
    kxCluster_status,
    kxCluster_statusReason,

    -- ** KxCommandLineArgument
    kxCommandLineArgument_key,
    kxCommandLineArgument_value,

    -- ** KxDatabaseCacheConfiguration
    kxDatabaseCacheConfiguration_cacheType,
    kxDatabaseCacheConfiguration_dbPaths,

    -- ** KxDatabaseConfiguration
    kxDatabaseConfiguration_cacheConfigurations,
    kxDatabaseConfiguration_changesetId,
    kxDatabaseConfiguration_databaseName,

    -- ** KxDatabaseListEntry
    kxDatabaseListEntry_createdTimestamp,
    kxDatabaseListEntry_databaseName,
    kxDatabaseListEntry_lastModifiedTimestamp,

    -- ** KxEnvironment
    kxEnvironment_availabilityZoneIds,
    kxEnvironment_awsAccountId,
    kxEnvironment_certificateAuthorityArn,
    kxEnvironment_creationTimestamp,
    kxEnvironment_customDNSConfiguration,
    kxEnvironment_dedicatedServiceAccountId,
    kxEnvironment_description,
    kxEnvironment_dnsStatus,
    kxEnvironment_environmentArn,
    kxEnvironment_environmentId,
    kxEnvironment_errorMessage,
    kxEnvironment_kmsKeyId,
    kxEnvironment_name,
    kxEnvironment_status,
    kxEnvironment_tgwStatus,
    kxEnvironment_transitGatewayConfiguration,
    kxEnvironment_updateTimestamp,

    -- ** KxNode
    kxNode_availabilityZoneId,
    kxNode_launchTime,
    kxNode_nodeId,

    -- ** KxSavedownStorageConfiguration
    kxSavedownStorageConfiguration_type,
    kxSavedownStorageConfiguration_size,

    -- ** KxUser
    kxUser_createTimestamp,
    kxUser_iamRole,
    kxUser_updateTimestamp,
    kxUser_userArn,
    kxUser_userName,

    -- ** SuperuserParameters
    superuserParameters_emailAddress,
    superuserParameters_firstName,
    superuserParameters_lastName,

    -- ** TransitGatewayConfiguration
    transitGatewayConfiguration_transitGatewayID,
    transitGatewayConfiguration_routableCIDRSpace,

    -- ** VpcConfiguration
    vpcConfiguration_ipAddressType,
    vpcConfiguration_securityGroupIds,
    vpcConfiguration_subnetIds,
    vpcConfiguration_vpcId,
  )
where

import Amazonka.FinSpace.CreateEnvironment
import Amazonka.FinSpace.CreateKxChangeset
import Amazonka.FinSpace.CreateKxCluster
import Amazonka.FinSpace.CreateKxDatabase
import Amazonka.FinSpace.CreateKxEnvironment
import Amazonka.FinSpace.CreateKxUser
import Amazonka.FinSpace.DeleteEnvironment
import Amazonka.FinSpace.DeleteKxCluster
import Amazonka.FinSpace.DeleteKxDatabase
import Amazonka.FinSpace.DeleteKxEnvironment
import Amazonka.FinSpace.DeleteKxUser
import Amazonka.FinSpace.GetEnvironment
import Amazonka.FinSpace.GetKxChangeset
import Amazonka.FinSpace.GetKxCluster
import Amazonka.FinSpace.GetKxConnectionString
import Amazonka.FinSpace.GetKxDatabase
import Amazonka.FinSpace.GetKxEnvironment
import Amazonka.FinSpace.GetKxUser
import Amazonka.FinSpace.ListEnvironments
import Amazonka.FinSpace.ListKxChangesets
import Amazonka.FinSpace.ListKxClusterNodes
import Amazonka.FinSpace.ListKxClusters
import Amazonka.FinSpace.ListKxDatabases
import Amazonka.FinSpace.ListKxEnvironments
import Amazonka.FinSpace.ListKxUsers
import Amazonka.FinSpace.ListTagsForResource
import Amazonka.FinSpace.TagResource
import Amazonka.FinSpace.Types.AutoScalingConfiguration
import Amazonka.FinSpace.Types.CapacityConfiguration
import Amazonka.FinSpace.Types.ChangeRequest
import Amazonka.FinSpace.Types.CodeConfiguration
import Amazonka.FinSpace.Types.CustomDNSServer
import Amazonka.FinSpace.Types.Environment
import Amazonka.FinSpace.Types.ErrorInfo
import Amazonka.FinSpace.Types.FederationParameters
import Amazonka.FinSpace.Types.KxCacheStorageConfiguration
import Amazonka.FinSpace.Types.KxChangesetListEntry
import Amazonka.FinSpace.Types.KxCluster
import Amazonka.FinSpace.Types.KxCommandLineArgument
import Amazonka.FinSpace.Types.KxDatabaseCacheConfiguration
import Amazonka.FinSpace.Types.KxDatabaseConfiguration
import Amazonka.FinSpace.Types.KxDatabaseListEntry
import Amazonka.FinSpace.Types.KxEnvironment
import Amazonka.FinSpace.Types.KxNode
import Amazonka.FinSpace.Types.KxSavedownStorageConfiguration
import Amazonka.FinSpace.Types.KxUser
import Amazonka.FinSpace.Types.SuperuserParameters
import Amazonka.FinSpace.Types.TransitGatewayConfiguration
import Amazonka.FinSpace.Types.VpcConfiguration
import Amazonka.FinSpace.UntagResource
import Amazonka.FinSpace.UpdateEnvironment
import Amazonka.FinSpace.UpdateKxClusterDatabases
import Amazonka.FinSpace.UpdateKxDatabase
import Amazonka.FinSpace.UpdateKxEnvironment
import Amazonka.FinSpace.UpdateKxEnvironmentNetwork
import Amazonka.FinSpace.UpdateKxUser
