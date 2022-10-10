{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RedshiftServerLess.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Lens
  ( -- * Operations

    -- ** ConvertRecoveryPointToSnapshot
    convertRecoveryPointToSnapshot_retentionPeriod,
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
    createNamespace_tags,
    createNamespace_logExports,
    createNamespace_iamRoles,
    createNamespace_kmsKeyId,
    createNamespace_defaultIamRoleArn,
    createNamespace_adminUserPassword,
    createNamespace_adminUsername,
    createNamespace_dbName,
    createNamespace_namespaceName,
    createNamespaceResponse_namespace,
    createNamespaceResponse_httpStatus,

    -- ** CreateSnapshot
    createSnapshot_retentionPeriod,
    createSnapshot_namespaceName,
    createSnapshot_snapshotName,
    createSnapshotResponse_snapshot,
    createSnapshotResponse_httpStatus,

    -- ** CreateUsageLimit
    createUsageLimit_period,
    createUsageLimit_breachAction,
    createUsageLimit_amount,
    createUsageLimit_resourceArn,
    createUsageLimit_usageType,
    createUsageLimitResponse_usageLimit,
    createUsageLimitResponse_httpStatus,

    -- ** CreateWorkgroup
    createWorkgroup_tags,
    createWorkgroup_securityGroupIds,
    createWorkgroup_baseCapacity,
    createWorkgroup_publiclyAccessible,
    createWorkgroup_configParameters,
    createWorkgroup_enhancedVpcRouting,
    createWorkgroup_subnetIds,
    createWorkgroup_namespaceName,
    createWorkgroup_workgroupName,
    createWorkgroupResponse_workgroup,
    createWorkgroupResponse_httpStatus,

    -- ** DeleteEndpointAccess
    deleteEndpointAccess_endpointName,
    deleteEndpointAccessResponse_endpoint,
    deleteEndpointAccessResponse_httpStatus,

    -- ** DeleteNamespace
    deleteNamespace_finalSnapshotRetentionPeriod,
    deleteNamespace_finalSnapshotName,
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
    getCredentials_durationSeconds,
    getCredentials_dbName,
    getCredentials_workgroupName,
    getCredentialsResponse_expiration,
    getCredentialsResponse_dbPassword,
    getCredentialsResponse_nextRefreshTime,
    getCredentialsResponse_dbUser,
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
    getSnapshot_snapshotName,
    getSnapshot_snapshotArn,
    getSnapshot_ownerAccount,
    getSnapshotResponse_snapshot,
    getSnapshotResponse_httpStatus,

    -- ** GetUsageLimit
    getUsageLimit_usageLimitId,
    getUsageLimitResponse_usageLimit,
    getUsageLimitResponse_httpStatus,

    -- ** GetWorkgroup
    getWorkgroup_workgroupName,
    getWorkgroupResponse_httpStatus,
    getWorkgroupResponse_workgroup,

    -- ** ListEndpointAccess
    listEndpointAccess_nextToken,
    listEndpointAccess_workgroupName,
    listEndpointAccess_maxResults,
    listEndpointAccess_vpcId,
    listEndpointAccessResponse_nextToken,
    listEndpointAccessResponse_httpStatus,
    listEndpointAccessResponse_endpoints,

    -- ** ListNamespaces
    listNamespaces_nextToken,
    listNamespaces_maxResults,
    listNamespacesResponse_nextToken,
    listNamespacesResponse_httpStatus,
    listNamespacesResponse_namespaces,

    -- ** ListRecoveryPoints
    listRecoveryPoints_nextToken,
    listRecoveryPoints_namespaceName,
    listRecoveryPoints_endTime,
    listRecoveryPoints_maxResults,
    listRecoveryPoints_startTime,
    listRecoveryPointsResponse_nextToken,
    listRecoveryPointsResponse_recoveryPoints,
    listRecoveryPointsResponse_httpStatus,

    -- ** ListSnapshots
    listSnapshots_nextToken,
    listSnapshots_namespaceName,
    listSnapshots_namespaceArn,
    listSnapshots_endTime,
    listSnapshots_maxResults,
    listSnapshots_ownerAccount,
    listSnapshots_startTime,
    listSnapshotsResponse_nextToken,
    listSnapshotsResponse_snapshots,
    listSnapshotsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsageLimits
    listUsageLimits_nextToken,
    listUsageLimits_usageType,
    listUsageLimits_maxResults,
    listUsageLimits_resourceArn,
    listUsageLimitsResponse_nextToken,
    listUsageLimitsResponse_usageLimits,
    listUsageLimitsResponse_httpStatus,

    -- ** ListWorkgroups
    listWorkgroups_nextToken,
    listWorkgroups_maxResults,
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
    restoreFromRecoveryPointResponse_recoveryPointId,
    restoreFromRecoveryPointResponse_namespace,
    restoreFromRecoveryPointResponse_httpStatus,

    -- ** RestoreFromSnapshot
    restoreFromSnapshot_snapshotName,
    restoreFromSnapshot_snapshotArn,
    restoreFromSnapshot_ownerAccount,
    restoreFromSnapshot_namespaceName,
    restoreFromSnapshot_workgroupName,
    restoreFromSnapshotResponse_snapshotName,
    restoreFromSnapshotResponse_ownerAccount,
    restoreFromSnapshotResponse_namespace,
    restoreFromSnapshotResponse_httpStatus,

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
    updateNamespace_logExports,
    updateNamespace_iamRoles,
    updateNamespace_kmsKeyId,
    updateNamespace_defaultIamRoleArn,
    updateNamespace_adminUserPassword,
    updateNamespace_adminUsername,
    updateNamespace_namespaceName,
    updateNamespaceResponse_httpStatus,
    updateNamespaceResponse_namespace,

    -- ** UpdateSnapshot
    updateSnapshot_retentionPeriod,
    updateSnapshot_snapshotName,
    updateSnapshotResponse_snapshot,
    updateSnapshotResponse_httpStatus,

    -- ** UpdateUsageLimit
    updateUsageLimit_breachAction,
    updateUsageLimit_amount,
    updateUsageLimit_usageLimitId,
    updateUsageLimitResponse_usageLimit,
    updateUsageLimitResponse_httpStatus,

    -- ** UpdateWorkgroup
    updateWorkgroup_securityGroupIds,
    updateWorkgroup_baseCapacity,
    updateWorkgroup_publiclyAccessible,
    updateWorkgroup_configParameters,
    updateWorkgroup_enhancedVpcRouting,
    updateWorkgroup_subnetIds,
    updateWorkgroup_workgroupName,
    updateWorkgroupResponse_httpStatus,
    updateWorkgroupResponse_workgroup,

    -- * Types

    -- ** ConfigParameter
    configParameter_parameterValue,
    configParameter_parameterKey,

    -- ** Endpoint
    endpoint_port,
    endpoint_address,
    endpoint_vpcEndpoints,

    -- ** EndpointAccess
    endpointAccess_port,
    endpointAccess_endpointName,
    endpointAccess_workgroupName,
    endpointAccess_address,
    endpointAccess_endpointStatus,
    endpointAccess_subnetIds,
    endpointAccess_endpointArn,
    endpointAccess_vpcEndpoint,
    endpointAccess_vpcSecurityGroups,
    endpointAccess_endpointCreateTime,

    -- ** Namespace
    namespace_namespaceName,
    namespace_creationDate,
    namespace_logExports,
    namespace_namespaceArn,
    namespace_iamRoles,
    namespace_status,
    namespace_namespaceId,
    namespace_kmsKeyId,
    namespace_defaultIamRoleArn,
    namespace_adminUsername,
    namespace_dbName,

    -- ** NetworkInterface
    networkInterface_subnetId,
    networkInterface_availabilityZone,
    networkInterface_networkInterfaceId,
    networkInterface_privateIpAddress,

    -- ** RecoveryPoint
    recoveryPoint_namespaceName,
    recoveryPoint_recoveryPointId,
    recoveryPoint_totalSizeInMegaBytes,
    recoveryPoint_workgroupName,
    recoveryPoint_recoveryPointCreateTime,

    -- ** ResourcePolicy
    resourcePolicy_policy,
    resourcePolicy_resourceArn,

    -- ** Snapshot
    snapshot_currentBackupRateInMegaBytesPerSecond,
    snapshot_namespaceName,
    snapshot_snapshotName,
    snapshot_snapshotArn,
    snapshot_snapshotRemainingDays,
    snapshot_namespaceArn,
    snapshot_status,
    snapshot_elapsedTimeInSeconds,
    snapshot_snapshotRetentionStartTime,
    snapshot_snapshotCreateTime,
    snapshot_snapshotRetentionPeriod,
    snapshot_estimatedSecondsToCompletion,
    snapshot_kmsKeyId,
    snapshot_totalBackupSizeInMegaBytes,
    snapshot_accountsWithRestoreAccess,
    snapshot_ownerAccount,
    snapshot_backupProgressInMegaBytes,
    snapshot_adminUsername,
    snapshot_actualIncrementalBackupSizeInMegaBytes,
    snapshot_accountsWithProvisionedRestoreAccess,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** UsageLimit
    usageLimit_usageLimitId,
    usageLimit_usageLimitArn,
    usageLimit_period,
    usageLimit_usageType,
    usageLimit_breachAction,
    usageLimit_resourceArn,
    usageLimit_amount,

    -- ** VpcEndpoint
    vpcEndpoint_vpcEndpointId,
    vpcEndpoint_vpcId,
    vpcEndpoint_networkInterfaces,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,

    -- ** Workgroup
    workgroup_securityGroupIds,
    workgroup_namespaceName,
    workgroup_baseCapacity,
    workgroup_creationDate,
    workgroup_workgroupName,
    workgroup_status,
    workgroup_publiclyAccessible,
    workgroup_configParameters,
    workgroup_enhancedVpcRouting,
    workgroup_endpoint,
    workgroup_workgroupId,
    workgroup_subnetIds,
    workgroup_workgroupArn,
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
import Amazonka.RedshiftServerLess.GetUsageLimit
import Amazonka.RedshiftServerLess.GetWorkgroup
import Amazonka.RedshiftServerLess.ListEndpointAccess
import Amazonka.RedshiftServerLess.ListNamespaces
import Amazonka.RedshiftServerLess.ListRecoveryPoints
import Amazonka.RedshiftServerLess.ListSnapshots
import Amazonka.RedshiftServerLess.ListTagsForResource
import Amazonka.RedshiftServerLess.ListUsageLimits
import Amazonka.RedshiftServerLess.ListWorkgroups
import Amazonka.RedshiftServerLess.PutResourcePolicy
import Amazonka.RedshiftServerLess.RestoreFromRecoveryPoint
import Amazonka.RedshiftServerLess.RestoreFromSnapshot
import Amazonka.RedshiftServerLess.TagResource
import Amazonka.RedshiftServerLess.Types.ConfigParameter
import Amazonka.RedshiftServerLess.Types.Endpoint
import Amazonka.RedshiftServerLess.Types.EndpointAccess
import Amazonka.RedshiftServerLess.Types.Namespace
import Amazonka.RedshiftServerLess.Types.NetworkInterface
import Amazonka.RedshiftServerLess.Types.RecoveryPoint
import Amazonka.RedshiftServerLess.Types.ResourcePolicy
import Amazonka.RedshiftServerLess.Types.Snapshot
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
