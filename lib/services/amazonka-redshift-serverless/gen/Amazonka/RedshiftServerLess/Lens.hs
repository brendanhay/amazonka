{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftServerLess.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Lens
  ( -- * Operations

    -- ** ConvertRecoveryPointToSnapshot
    convertRecoveryPointToSnapshot_retentionPeriod,
    convertRecoveryPointToSnapshot_tags,
    convertRecoveryPointToSnapshot_recoveryPointId,
    convertRecoveryPointToSnapshot_snapshotName,
    convertRecoveryPointToSnapshotResponse_snapshot,
    convertRecoveryPointToSnapshotResponse_httpStatus,

    -- ** CreateEndpointAccess
    createEndpointAccess_vpcSecurityGroupIds,
    createEndpointAccess_endpointName,
    createEndpointAccess_subnetIds,
    createEndpointAccess_workgroupName,
    createEndpointAccessResponse_endpoint,
    createEndpointAccessResponse_httpStatus,

    -- ** CreateNamespace
    createNamespace_adminUserPassword,
    createNamespace_adminUsername,
    createNamespace_dbName,
    createNamespace_defaultIamRoleArn,
    createNamespace_iamRoles,
    createNamespace_kmsKeyId,
    createNamespace_logExports,
    createNamespace_tags,
    createNamespace_namespaceName,
    createNamespaceResponse_namespace,
    createNamespaceResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_retentionPeriod,
    createSnapshot_tags,
    createSnapshot_namespaceName,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateUsageLimit
    createUsageLimit_breachAction,
    createUsageLimit_period,
    createUsageLimit_amount,
    createUsageLimit_resourceArn,
    createUsageLimit_usageType,
    createUsageLimitResponse_usageLimit,
    createUsageLimitResponse_httpStatus,

    -- ** CreateWorkgroup
    createWorkgroup_baseCapacity,
    createWorkgroup_configParameters,
    createWorkgroup_enhancedVpcRouting,
    createWorkgroup_port,
    createWorkgroup_publiclyAccessible,
    createWorkgroup_securityGroupIds,
    createWorkgroup_subnetIds,
    createWorkgroup_tags,
    createWorkgroup_namespaceName,
    createWorkgroup_workgroupName,
    createWorkgroupResponse_workgroup,
    createWorkgroupResponse_httpStatus,

    -- ** DeleteEndpointAccess
    deleteEndpointAccess_endpointName,
    deleteEndpointAccessResponse_endpoint,
    deleteEndpointAccessResponse_httpStatus,

    -- ** DeleteNamespace
    deleteNamespace_finalSnapshotName,
    deleteNamespace_finalSnapshotRetentionPeriod,
    deleteNamespace_namespaceName,
    deleteNamespaceResponse_httpStatus,
    deleteNamespaceResponse_namespace,

    -- ** DeleteResourcePolicy
    deleteResourcePolicy_resourceArn,
    deleteResourcePolicyResponse_httpStatus,

    -- ** DeleteSnapshot
    deleteSnapshot_snapshotName,
    deleteSnapshotResponse_snapshot,
    deleteSnapshotResponse_httpStatus,

    -- ** DeleteUsageLimit
    deleteUsageLimit_usageLimitId,
    deleteUsageLimitResponse_usageLimit,
    deleteUsageLimitResponse_httpStatus,

    -- ** DeleteWorkgroup
    deleteWorkgroup_workgroupName,
    deleteWorkgroupResponse_httpStatus,
    deleteWorkgroupResponse_workgroup,

    -- ** GetCredentials
    getCredentials_dbName,
    getCredentials_durationSeconds,
    getCredentials_workgroupName,
    getCredentialsResponse_dbPassword,
    getCredentialsResponse_dbUser,
    getCredentialsResponse_expiration,
    getCredentialsResponse_nextRefreshTime,
    getCredentialsResponse_httpStatus,

    -- ** GetEndpointAccess
    getEndpointAccess_endpointName,
    getEndpointAccessResponse_endpoint,
    getEndpointAccessResponse_httpStatus,

    -- ** GetNamespace
    getNamespace_namespaceName,
    getNamespaceResponse_httpStatus,
    getNamespaceResponse_namespace,

    -- ** GetRecoveryPoint
    getRecoveryPoint_recoveryPointId,
    getRecoveryPointResponse_recoveryPoint,
    getRecoveryPointResponse_httpStatus,

    -- ** GetResourcePolicy
    getResourcePolicy_resourceArn,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_httpStatus,

    -- ** GetSnapshot
    getSnapshot_ownerAccount,
    getSnapshot_snapshotArn,
    getSnapshot_snapshotName,
    getSnapshotResponse_snapshot,
    getSnapshotResponse_httpStatus,

    -- ** GetTableRestoreStatus
    getTableRestoreStatus_tableRestoreRequestId,
    getTableRestoreStatusResponse_tableRestoreStatus,
    getTableRestoreStatusResponse_httpStatus,

    -- ** GetUsageLimit
    getUsageLimit_usageLimitId,
    getUsageLimitResponse_usageLimit,
    getUsageLimitResponse_httpStatus,

    -- ** GetWorkgroup
    getWorkgroup_workgroupName,
    getWorkgroupResponse_httpStatus,
    getWorkgroupResponse_workgroup,

    -- ** ListEndpointAccess
    listEndpointAccess_maxResults,
    listEndpointAccess_nextToken,
    listEndpointAccess_vpcId,
    listEndpointAccess_workgroupName,
    listEndpointAccessResponse_nextToken,
    listEndpointAccessResponse_httpStatus,
    listEndpointAccessResponse_endpoints,

    -- ** ListNamespaces
    listNamespaces_maxResults,
    listNamespaces_nextToken,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_httpStatus,
    listNamespacesResponse_namespaces,

    -- ** ListRecoveryPoints
    listRecoveryPoints_endTime,
    listRecoveryPoints_maxResults,
    listRecoveryPoints_namespaceArn,
    listRecoveryPoints_namespaceName,
    listRecoveryPoints_nextToken,
    listRecoveryPoints_startTime,
    listRecoveryPointsResponse_nextToken,
    listRecoveryPointsResponse_recoveryPoints,
    listRecoveryPointsResponse_httpStatus,

    -- ** ListSnapshots
    listSnapshots_endTime,
    listSnapshots_maxResults,
    listSnapshots_namespaceArn,
    listSnapshots_namespaceName,
    listSnapshots_nextToken,
    listSnapshots_ownerAccount,
    listSnapshots_startTime,
    listSnapshotsResponse_nextToken,
    listSnapshotsResponse_snapshots,
    listSnapshotsResponse_httpStatus,

    -- ** ListTableRestoreStatus
    listTableRestoreStatus_maxResults,
    listTableRestoreStatus_namespaceName,
    listTableRestoreStatus_nextToken,
    listTableRestoreStatus_workgroupName,
    listTableRestoreStatusResponse_nextToken,
    listTableRestoreStatusResponse_tableRestoreStatuses,
    listTableRestoreStatusResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsageLimits
    listUsageLimits_maxResults,
    listUsageLimits_nextToken,
    listUsageLimits_resourceArn,
    listUsageLimits_usageType,
    listUsageLimitsResponse_nextToken,
    listUsageLimitsResponse_usageLimits,
    listUsageLimitsResponse_httpStatus,

    -- ** ListWorkgroups
    listWorkgroups_maxResults,
    listWorkgroups_nextToken,
    listWorkgroupsResponse_nextToken,
    listWorkgroupsResponse_httpStatus,
    listWorkgroupsResponse_workgroups,

    -- ** PutResourcePolicy
    putResourcePolicy_policy,
    putResourcePolicy_resourceArn,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,

    -- ** RestoreFromRecoveryPoint
    restoreFromRecoveryPoint_namespaceName,
    restoreFromRecoveryPoint_recoveryPointId,
    restoreFromRecoveryPoint_workgroupName,
    restoreFromRecoveryPointResponse_namespace,
    restoreFromRecoveryPointResponse_recoveryPointId,
    restoreFromRecoveryPointResponse_httpStatus,

    -- ** RestoreFromSnapshot
    restoreFromSnapshot_ownerAccount,
    restoreFromSnapshot_snapshotArn,
    restoreFromSnapshot_snapshotName,
    restoreFromSnapshot_namespaceName,
    restoreFromSnapshot_workgroupName,
    restoreFromSnapshotResponse_namespace,
    restoreFromSnapshotResponse_ownerAccount,
    restoreFromSnapshotResponse_snapshotName,
    restoreFromSnapshotResponse_httpStatus,

    -- ** RestoreTableFromSnapshot
    restoreTableFromSnapshot_activateCaseSensitiveIdentifier,
    restoreTableFromSnapshot_sourceSchemaName,
    restoreTableFromSnapshot_targetDatabaseName,
    restoreTableFromSnapshot_targetSchemaName,
    restoreTableFromSnapshot_namespaceName,
    restoreTableFromSnapshot_newTableName,
    restoreTableFromSnapshot_snapshotName,
    restoreTableFromSnapshot_sourceDatabaseName,
    restoreTableFromSnapshot_sourceTableName,
    restoreTableFromSnapshot_workgroupName,
    restoreTableFromSnapshotResponse_tableRestoreStatus,
    restoreTableFromSnapshotResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEndpointAccess
    updateEndpointAccess_vpcSecurityGroupIds,
    updateEndpointAccess_endpointName,
    updateEndpointAccessResponse_endpoint,
    updateEndpointAccessResponse_httpStatus,

    -- ** UpdateNamespace
    updateNamespace_adminUserPassword,
    updateNamespace_adminUsername,
    updateNamespace_defaultIamRoleArn,
    updateNamespace_iamRoles,
    updateNamespace_kmsKeyId,
    updateNamespace_logExports,
    updateNamespace_namespaceName,
    updateNamespaceResponse_httpStatus,
    updateNamespaceResponse_namespace,

    -- ** UpdateSnapshot
    updateSnapshot_retentionPeriod,
    updateSnapshot_snapshotName,
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,

    -- ** UpdateUsageLimit
    updateUsageLimit_amount,
    updateUsageLimit_breachAction,
    updateUsageLimit_usageLimitId,
    updateUsageLimitResponse_usageLimit,
    updateUsageLimitResponse_httpStatus,

    -- ** UpdateWorkgroup
    updateWorkgroup_baseCapacity,
    updateWorkgroup_configParameters,
    updateWorkgroup_enhancedVpcRouting,
    updateWorkgroup_port,
    updateWorkgroup_publiclyAccessible,
    updateWorkgroup_securityGroupIds,
    updateWorkgroup_subnetIds,
    updateWorkgroup_workgroupName,
    updateWorkgroupResponse_httpStatus,
    updateWorkgroupResponse_workgroup,

    -- * Types

    -- ** ConfigParameter
    configParameter_parameterKey,
    configParameter_parameterValue,

    -- ** Endpoint
    endpoint_address,
    endpoint_port,
    endpoint_vpcEndpoints,

    -- ** EndpointAccess
    endpointAccess_address,
    endpointAccess_endpointArn,
    endpointAccess_endpointCreateTime,
    endpointAccess_endpointName,
    endpointAccess_endpointStatus,
    endpointAccess_port,
    endpointAccess_subnetIds,
    endpointAccess_vpcEndpoint,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_workgroupName,

    -- ** Namespace
    namespace_adminUsername,
    namespace_creationDate,
    namespace_dbName,
    namespace_defaultIamRoleArn,
    namespace_iamRoles,
    namespace_kmsKeyId,
    namespace_logExports,
    namespace_namespaceArn,
    namespace_namespaceId,
    namespace_namespaceName,
    namespace_status,

    -- ** NetworkInterface
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,
    networkInterface_subnetId,

    -- ** RecoveryPoint
    recoveryPoint_namespaceArn,
    recoveryPoint_namespaceName,
    recoveryPoint_recoveryPointCreateTime,
    recoveryPoint_recoveryPointId,
    recoveryPoint_totalSizeInMegaBytes,
    recoveryPoint_workgroupName,

    -- ** ResourcePolicy
    resourcePolicy_policy,
    resourcePolicy_resourceArn,

    -- ** Snapshot
    snapshot_accountsWithProvisionedRestoreAccess,
    snapshot_accountsWithRestoreAccess,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_adminUsername,
    snapshot_backupProgressInMegaBytes,
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_elapsedTimeInSeconds,
    snapshot_estimatedSecondsToCompletion,
    snapshot_kmsKeyId,
    snapshot_namespaceArn,
    snapshot_namespaceName,
    snapshot_ownerAccount,
    snapshot_snapshotArn,
    snapshot_snapshotCreateTime,
    snapshot_snapshotName,
    snapshot_snapshotRemainingDays,
    snapshot_snapshotRetentionPeriod,
    snapshot_snapshotRetentionStartTime,
    snapshot_status,
    snapshot_totalBackupSizeInMegaBytes,

    -- ** TableRestoreStatus
    tableRestoreStatus_message,
    tableRestoreStatus_namespaceName,
    tableRestoreStatus_newTableName,
    tableRestoreStatus_progressInMegaBytes,
    tableRestoreStatus_requestTime,
    tableRestoreStatus_snapshotName,
    tableRestoreStatus_sourceDatabaseName,
    tableRestoreStatus_sourceSchemaName,
    tableRestoreStatus_sourceTableName,
    tableRestoreStatus_status,
    tableRestoreStatus_tableRestoreRequestId,
    tableRestoreStatus_targetDatabaseName,
    tableRestoreStatus_targetSchemaName,
    tableRestoreStatus_totalDataInMegaBytes,
    tableRestoreStatus_workgroupName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UsageLimit
    usageLimit_amount,
    usageLimit_breachAction,
    usageLimit_period,
    usageLimit_resourceArn,
    usageLimit_usageLimitArn,
    usageLimit_usageLimitId,
    usageLimit_usageType,

    -- ** VpcEndpoint
    vpcEndpoint_networkInterfaces,
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- ** Workgroup
    workgroup_baseCapacity,
    workgroup_configParameters,
    workgroup_creationDate,
    workgroup_endpoint,
    workgroup_enhancedVpcRouting,
    workgroup_namespaceName,
    workgroup_port,
    workgroup_publiclyAccessible,
    workgroup_securityGroupIds,
    workgroup_status,
    workgroup_subnetIds,
    workgroup_workgroupArn,
    workgroup_workgroupId,
    workgroup_workgroupName,
  )
where

import Amazonka.RedshiftServerLess.ConvertRecoveryPointToSnapshot
import Amazonka.RedshiftServerLess.CreateEndpointAccess
import Amazonka.RedshiftServerLess.CreateNamespace
import Amazonka.RedshiftServerLess.CreateSnapshot
import Amazonka.RedshiftServerLess.CreateUsageLimit
import Amazonka.RedshiftServerLess.CreateWorkgroup
import Amazonka.RedshiftServerLess.DeleteEndpointAccess
import Amazonka.RedshiftServerLess.DeleteNamespace
import Amazonka.RedshiftServerLess.DeleteResourcePolicy
import Amazonka.RedshiftServerLess.DeleteSnapshot
import Amazonka.RedshiftServerLess.DeleteUsageLimit
import Amazonka.RedshiftServerLess.DeleteWorkgroup
import Amazonka.RedshiftServerLess.GetCredentials
import Amazonka.RedshiftServerLess.GetEndpointAccess
import Amazonka.RedshiftServerLess.GetNamespace
import Amazonka.RedshiftServerLess.GetRecoveryPoint
import Amazonka.RedshiftServerLess.GetResourcePolicy
import Amazonka.RedshiftServerLess.GetSnapshot
import Amazonka.RedshiftServerLess.GetTableRestoreStatus
import Amazonka.RedshiftServerLess.GetUsageLimit
import Amazonka.RedshiftServerLess.GetWorkgroup
import Amazonka.RedshiftServerLess.ListEndpointAccess
import Amazonka.RedshiftServerLess.ListNamespaces
import Amazonka.RedshiftServerLess.ListRecoveryPoints
import Amazonka.RedshiftServerLess.ListSnapshots
import Amazonka.RedshiftServerLess.ListTableRestoreStatus
import Amazonka.RedshiftServerLess.ListTagsForResource
import Amazonka.RedshiftServerLess.ListUsageLimits
import Amazonka.RedshiftServerLess.ListWorkgroups
import Amazonka.RedshiftServerLess.PutResourcePolicy
import Amazonka.RedshiftServerLess.RestoreFromRecoveryPoint
import Amazonka.RedshiftServerLess.RestoreFromSnapshot
import Amazonka.RedshiftServerLess.RestoreTableFromSnapshot
import Amazonka.RedshiftServerLess.TagResource
import Amazonka.RedshiftServerLess.Types.ConfigParameter
import Amazonka.RedshiftServerLess.Types.Endpoint
import Amazonka.RedshiftServerLess.Types.EndpointAccess
import Amazonka.RedshiftServerLess.Types.Namespace
import Amazonka.RedshiftServerLess.Types.NetworkInterface
import Amazonka.RedshiftServerLess.Types.RecoveryPoint
import Amazonka.RedshiftServerLess.Types.ResourcePolicy
import Amazonka.RedshiftServerLess.Types.Snapshot
import Amazonka.RedshiftServerLess.Types.TableRestoreStatus
import Amazonka.RedshiftServerLess.Types.Tag
import Amazonka.RedshiftServerLess.Types.UsageLimit
import Amazonka.RedshiftServerLess.Types.VpcEndpoint
import Amazonka.RedshiftServerLess.Types.VpcSecurityGroupMembership
import Amazonka.RedshiftServerLess.Types.Workgroup
import Amazonka.RedshiftServerLess.UntagResource
import Amazonka.RedshiftServerLess.UpdateEndpointAccess
import Amazonka.RedshiftServerLess.UpdateNamespace
import Amazonka.RedshiftServerLess.UpdateSnapshot
import Amazonka.RedshiftServerLess.UpdateUsageLimit
import Amazonka.RedshiftServerLess.UpdateWorkgroup
