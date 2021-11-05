{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Lens
  ( -- * Operations

    -- ** StartDBCluster
    startDBCluster_dbClusterIdentifier,
    startDBClusterResponse_dbCluster,
    startDBClusterResponse_httpStatus,

    -- ** DescribeDBClusterParameterGroups
    describeDBClusterParameterGroups_filters,
    describeDBClusterParameterGroups_marker,
    describeDBClusterParameterGroups_maxRecords,
    describeDBClusterParameterGroups_dbClusterParameterGroupName,
    describeDBClusterParameterGroupsResponse_marker,
    describeDBClusterParameterGroupsResponse_dbClusterParameterGroups,
    describeDBClusterParameterGroupsResponse_httpStatus,

    -- ** DescribeDBEngineVersions
    describeDBEngineVersions_engineVersion,
    describeDBEngineVersions_listSupportedTimezones,
    describeDBEngineVersions_defaultOnly,
    describeDBEngineVersions_filters,
    describeDBEngineVersions_engine,
    describeDBEngineVersions_dbParameterGroupFamily,
    describeDBEngineVersions_listSupportedCharacterSets,
    describeDBEngineVersions_marker,
    describeDBEngineVersions_maxRecords,
    describeDBEngineVersionsResponse_marker,
    describeDBEngineVersionsResponse_dbEngineVersions,
    describeDBEngineVersionsResponse_httpStatus,

    -- ** AddSourceIdentifierToSubscription
    addSourceIdentifierToSubscription_subscriptionName,
    addSourceIdentifierToSubscription_sourceIdentifier,
    addSourceIdentifierToSubscriptionResponse_eventSubscription,
    addSourceIdentifierToSubscriptionResponse_httpStatus,

    -- ** ModifyDBInstance
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_promotionTier,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_dbInstanceIdentifier,
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,

    -- ** ModifyEventSubscription
    modifyEventSubscription_snsTopicArn,
    modifyEventSubscription_enabled,
    modifyEventSubscription_sourceType,
    modifyEventSubscription_eventCategories,
    modifyEventSubscription_subscriptionName,
    modifyEventSubscriptionResponse_eventSubscription,
    modifyEventSubscriptionResponse_httpStatus,

    -- ** ResetDBClusterParameterGroup
    resetDBClusterParameterGroup_resetAllParameters,
    resetDBClusterParameterGroup_parameters,
    resetDBClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeEvents
    describeEvents_startTime,
    describeEvents_sourceType,
    describeEvents_filters,
    describeEvents_sourceIdentifier,
    describeEvents_eventCategories,
    describeEvents_marker,
    describeEvents_maxRecords,
    describeEvents_endTime,
    describeEvents_duration,
    describeEventsResponse_events,
    describeEventsResponse_marker,
    describeEventsResponse_httpStatus,

    -- ** DescribeDBClusters
    describeDBClusters_dbClusterIdentifier,
    describeDBClusters_filters,
    describeDBClusters_marker,
    describeDBClusters_maxRecords,
    describeDBClustersResponse_dbClusters,
    describeDBClustersResponse_marker,
    describeDBClustersResponse_httpStatus,

    -- ** ModifyDBSubnetGroup
    modifyDBSubnetGroup_dbSubnetGroupDescription,
    modifyDBSubnetGroup_dbSubnetGroupName,
    modifyDBSubnetGroup_subnetIds,
    modifyDBSubnetGroupResponse_dbSubnetGroup,
    modifyDBSubnetGroupResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_filters,
    listTagsForResource_resourceName,
    listTagsForResourceResponse_tagList,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteDBCluster
    deleteDBCluster_finalDBSnapshotIdentifier,
    deleteDBCluster_skipFinalSnapshot,
    deleteDBCluster_dbClusterIdentifier,
    deleteDBClusterResponse_dbCluster,
    deleteDBClusterResponse_httpStatus,

    -- ** RemoveSourceIdentifierFromSubscription
    removeSourceIdentifierFromSubscription_subscriptionName,
    removeSourceIdentifierFromSubscription_sourceIdentifier,
    removeSourceIdentifierFromSubscriptionResponse_eventSubscription,
    removeSourceIdentifierFromSubscriptionResponse_httpStatus,

    -- ** DescribeEngineDefaultClusterParameters
    describeEngineDefaultClusterParameters_filters,
    describeEngineDefaultClusterParameters_marker,
    describeEngineDefaultClusterParameters_maxRecords,
    describeEngineDefaultClusterParameters_dbParameterGroupFamily,
    describeEngineDefaultClusterParametersResponse_engineDefaults,
    describeEngineDefaultClusterParametersResponse_httpStatus,

    -- ** DeleteGlobalCluster
    deleteGlobalCluster_globalClusterIdentifier,
    deleteGlobalClusterResponse_globalCluster,
    deleteGlobalClusterResponse_httpStatus,

    -- ** RemoveTagsFromResource
    removeTagsFromResource_resourceName,
    removeTagsFromResource_tagKeys,

    -- ** CreateEventSubscription
    createEventSubscription_enabled,
    createEventSubscription_sourceType,
    createEventSubscription_eventCategories,
    createEventSubscription_sourceIds,
    createEventSubscription_tags,
    createEventSubscription_subscriptionName,
    createEventSubscription_snsTopicArn,
    createEventSubscriptionResponse_eventSubscription,
    createEventSubscriptionResponse_httpStatus,

    -- ** CreateDBInstance
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_promotionTier,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_availabilityZone,
    createDBInstance_tags,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstance_dbClusterIdentifier,
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,

    -- ** DeleteDBClusterParameterGroup
    deleteDBClusterParameterGroup_dbClusterParameterGroupName,

    -- ** DescribeCertificates
    describeCertificates_filters,
    describeCertificates_certificateIdentifier,
    describeCertificates_marker,
    describeCertificates_maxRecords,
    describeCertificatesResponse_certificates,
    describeCertificatesResponse_marker,
    describeCertificatesResponse_httpStatus,

    -- ** RestoreDBClusterFromSnapshot
    restoreDBClusterFromSnapshot_engineVersion,
    restoreDBClusterFromSnapshot_deletionProtection,
    restoreDBClusterFromSnapshot_dbSubnetGroupName,
    restoreDBClusterFromSnapshot_availabilityZones,
    restoreDBClusterFromSnapshot_kmsKeyId,
    restoreDBClusterFromSnapshot_vpcSecurityGroupIds,
    restoreDBClusterFromSnapshot_tags,
    restoreDBClusterFromSnapshot_port,
    restoreDBClusterFromSnapshot_enableCloudwatchLogsExports,
    restoreDBClusterFromSnapshot_dbClusterIdentifier,
    restoreDBClusterFromSnapshot_snapshotIdentifier,
    restoreDBClusterFromSnapshot_engine,
    restoreDBClusterFromSnapshotResponse_dbCluster,
    restoreDBClusterFromSnapshotResponse_httpStatus,

    -- ** DescribeOrderableDBInstanceOptions
    describeOrderableDBInstanceOptions_engineVersion,
    describeOrderableDBInstanceOptions_filters,
    describeOrderableDBInstanceOptions_dbInstanceClass,
    describeOrderableDBInstanceOptions_licenseModel,
    describeOrderableDBInstanceOptions_marker,
    describeOrderableDBInstanceOptions_maxRecords,
    describeOrderableDBInstanceOptions_vpc,
    describeOrderableDBInstanceOptions_engine,
    describeOrderableDBInstanceOptionsResponse_orderableDBInstanceOptions,
    describeOrderableDBInstanceOptionsResponse_marker,
    describeOrderableDBInstanceOptionsResponse_httpStatus,

    -- ** CreateDBClusterParameterGroup
    createDBClusterParameterGroup_tags,
    createDBClusterParameterGroup_dbClusterParameterGroupName,
    createDBClusterParameterGroup_dbParameterGroupFamily,
    createDBClusterParameterGroup_description,
    createDBClusterParameterGroupResponse_dbClusterParameterGroup,
    createDBClusterParameterGroupResponse_httpStatus,

    -- ** DeleteEventSubscription
    deleteEventSubscription_subscriptionName,
    deleteEventSubscriptionResponse_eventSubscription,
    deleteEventSubscriptionResponse_httpStatus,

    -- ** RemoveFromGlobalCluster
    removeFromGlobalCluster_globalClusterIdentifier,
    removeFromGlobalCluster_dbClusterIdentifier,
    removeFromGlobalClusterResponse_globalCluster,
    removeFromGlobalClusterResponse_httpStatus,

    -- ** DeleteDBClusterSnapshot
    deleteDBClusterSnapshot_dbClusterSnapshotIdentifier,
    deleteDBClusterSnapshotResponse_dbClusterSnapshot,
    deleteDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeEventSubscriptions
    describeEventSubscriptions_subscriptionName,
    describeEventSubscriptions_filters,
    describeEventSubscriptions_marker,
    describeEventSubscriptions_maxRecords,
    describeEventSubscriptionsResponse_eventSubscriptionsList,
    describeEventSubscriptionsResponse_marker,
    describeEventSubscriptionsResponse_httpStatus,

    -- ** AddTagsToResource
    addTagsToResource_resourceName,
    addTagsToResource_tags,

    -- ** CreateDBClusterSnapshot
    createDBClusterSnapshot_tags,
    createDBClusterSnapshot_dbClusterSnapshotIdentifier,
    createDBClusterSnapshot_dbClusterIdentifier,
    createDBClusterSnapshotResponse_dbClusterSnapshot,
    createDBClusterSnapshotResponse_httpStatus,

    -- ** DescribeDBSubnetGroups
    describeDBSubnetGroups_dbSubnetGroupName,
    describeDBSubnetGroups_filters,
    describeDBSubnetGroups_marker,
    describeDBSubnetGroups_maxRecords,
    describeDBSubnetGroupsResponse_dbSubnetGroups,
    describeDBSubnetGroupsResponse_marker,
    describeDBSubnetGroupsResponse_httpStatus,

    -- ** StopDBCluster
    stopDBCluster_dbClusterIdentifier,
    stopDBClusterResponse_dbCluster,
    stopDBClusterResponse_httpStatus,

    -- ** ModifyDBClusterSnapshotAttribute
    modifyDBClusterSnapshotAttribute_valuesToAdd,
    modifyDBClusterSnapshotAttribute_valuesToRemove,
    modifyDBClusterSnapshotAttribute_dbClusterSnapshotIdentifier,
    modifyDBClusterSnapshotAttribute_attributeName,
    modifyDBClusterSnapshotAttributeResponse_dbClusterSnapshotAttributesResult,
    modifyDBClusterSnapshotAttributeResponse_httpStatus,

    -- ** ModifyDBCluster
    modifyDBCluster_engineVersion,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_port,
    modifyDBCluster_dbClusterIdentifier,
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,

    -- ** CopyDBClusterParameterGroup
    copyDBClusterParameterGroup_tags,
    copyDBClusterParameterGroup_sourceDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupIdentifier,
    copyDBClusterParameterGroup_targetDBClusterParameterGroupDescription,
    copyDBClusterParameterGroupResponse_dbClusterParameterGroup,
    copyDBClusterParameterGroupResponse_httpStatus,

    -- ** DescribeEventCategories
    describeEventCategories_sourceType,
    describeEventCategories_filters,
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,

    -- ** DescribeGlobalClusters
    describeGlobalClusters_globalClusterIdentifier,
    describeGlobalClusters_filters,
    describeGlobalClusters_marker,
    describeGlobalClusters_maxRecords,
    describeGlobalClustersResponse_globalClusters,
    describeGlobalClustersResponse_marker,
    describeGlobalClustersResponse_httpStatus,

    -- ** ModifyDBClusterParameterGroup
    modifyDBClusterParameterGroup_dbClusterParameterGroupName,
    modifyDBClusterParameterGroup_parameters,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DescribeDBClusterSnapshotAttributes
    describeDBClusterSnapshotAttributes_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshotAttributesResponse_dbClusterSnapshotAttributesResult,
    describeDBClusterSnapshotAttributesResponse_httpStatus,

    -- ** DescribePendingMaintenanceActions
    describePendingMaintenanceActions_filters,
    describePendingMaintenanceActions_marker,
    describePendingMaintenanceActions_maxRecords,
    describePendingMaintenanceActions_resourceIdentifier,
    describePendingMaintenanceActionsResponse_pendingMaintenanceActions,
    describePendingMaintenanceActionsResponse_marker,
    describePendingMaintenanceActionsResponse_httpStatus,

    -- ** CopyDBClusterSnapshot
    copyDBClusterSnapshot_preSignedUrl,
    copyDBClusterSnapshot_copyTags,
    copyDBClusterSnapshot_kmsKeyId,
    copyDBClusterSnapshot_tags,
    copyDBClusterSnapshot_sourceDBClusterSnapshotIdentifier,
    copyDBClusterSnapshot_targetDBClusterSnapshotIdentifier,
    copyDBClusterSnapshotResponse_dbClusterSnapshot,
    copyDBClusterSnapshotResponse_httpStatus,

    -- ** CreateGlobalCluster
    createGlobalCluster_engineVersion,
    createGlobalCluster_deletionProtection,
    createGlobalCluster_storageEncrypted,
    createGlobalCluster_sourceDBClusterIdentifier,
    createGlobalCluster_engine,
    createGlobalCluster_databaseName,
    createGlobalCluster_globalClusterIdentifier,
    createGlobalClusterResponse_globalCluster,
    createGlobalClusterResponse_httpStatus,

    -- ** CreateDBCluster
    createDBCluster_engineVersion,
    createDBCluster_deletionProtection,
    createDBCluster_storageEncrypted,
    createDBCluster_masterUserPassword,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_masterUsername,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_availabilityZones,
    createDBCluster_kmsKeyId,
    createDBCluster_preferredBackupWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_tags,
    createDBCluster_port,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,

    -- ** FailoverDBCluster
    failoverDBCluster_dbClusterIdentifier,
    failoverDBCluster_targetDBInstanceIdentifier,
    failoverDBClusterResponse_dbCluster,
    failoverDBClusterResponse_httpStatus,

    -- ** ApplyPendingMaintenanceAction
    applyPendingMaintenanceAction_resourceIdentifier,
    applyPendingMaintenanceAction_applyAction,
    applyPendingMaintenanceAction_optInType,
    applyPendingMaintenanceActionResponse_resourcePendingMaintenanceActions,
    applyPendingMaintenanceActionResponse_httpStatus,

    -- ** DescribeDBClusterParameters
    describeDBClusterParameters_filters,
    describeDBClusterParameters_marker,
    describeDBClusterParameters_maxRecords,
    describeDBClusterParameters_source,
    describeDBClusterParameters_dbClusterParameterGroupName,
    describeDBClusterParametersResponse_marker,
    describeDBClusterParametersResponse_parameters,
    describeDBClusterParametersResponse_httpStatus,

    -- ** DeleteDBSubnetGroup
    deleteDBSubnetGroup_dbSubnetGroupName,

    -- ** DescribeDBClusterSnapshots
    describeDBClusterSnapshots_dbClusterIdentifier,
    describeDBClusterSnapshots_includeShared,
    describeDBClusterSnapshots_dbClusterSnapshotIdentifier,
    describeDBClusterSnapshots_filters,
    describeDBClusterSnapshots_snapshotType,
    describeDBClusterSnapshots_marker,
    describeDBClusterSnapshots_maxRecords,
    describeDBClusterSnapshots_includePublic,
    describeDBClusterSnapshotsResponse_marker,
    describeDBClusterSnapshotsResponse_dbClusterSnapshots,
    describeDBClusterSnapshotsResponse_httpStatus,

    -- ** RebootDBInstance
    rebootDBInstance_forceFailover,
    rebootDBInstance_dbInstanceIdentifier,
    rebootDBInstanceResponse_dbInstance,
    rebootDBInstanceResponse_httpStatus,

    -- ** CreateDBSubnetGroup
    createDBSubnetGroup_tags,
    createDBSubnetGroup_dbSubnetGroupName,
    createDBSubnetGroup_dbSubnetGroupDescription,
    createDBSubnetGroup_subnetIds,
    createDBSubnetGroupResponse_dbSubnetGroup,
    createDBSubnetGroupResponse_httpStatus,

    -- ** DeleteDBInstance
    deleteDBInstance_dbInstanceIdentifier,
    deleteDBInstanceResponse_dbInstance,
    deleteDBInstanceResponse_httpStatus,

    -- ** ModifyGlobalCluster
    modifyGlobalCluster_deletionProtection,
    modifyGlobalCluster_newGlobalClusterIdentifier,
    modifyGlobalCluster_globalClusterIdentifier,
    modifyGlobalClusterResponse_globalCluster,
    modifyGlobalClusterResponse_httpStatus,

    -- ** RestoreDBClusterToPointInTime
    restoreDBClusterToPointInTime_deletionProtection,
    restoreDBClusterToPointInTime_useLatestRestorableTime,
    restoreDBClusterToPointInTime_dbSubnetGroupName,
    restoreDBClusterToPointInTime_kmsKeyId,
    restoreDBClusterToPointInTime_vpcSecurityGroupIds,
    restoreDBClusterToPointInTime_restoreToTime,
    restoreDBClusterToPointInTime_tags,
    restoreDBClusterToPointInTime_port,
    restoreDBClusterToPointInTime_enableCloudwatchLogsExports,
    restoreDBClusterToPointInTime_dbClusterIdentifier,
    restoreDBClusterToPointInTime_sourceDBClusterIdentifier,
    restoreDBClusterToPointInTimeResponse_dbCluster,
    restoreDBClusterToPointInTimeResponse_httpStatus,

    -- ** DescribeDBInstances
    describeDBInstances_filters,
    describeDBInstances_dbInstanceIdentifier,
    describeDBInstances_marker,
    describeDBInstances_maxRecords,
    describeDBInstancesResponse_dbInstances,
    describeDBInstancesResponse_marker,
    describeDBInstancesResponse_httpStatus,

    -- * Types

    -- ** AvailabilityZone
    availabilityZone_name,

    -- ** Certificate
    certificate_certificateType,
    certificate_certificateArn,
    certificate_validTill,
    certificate_certificateIdentifier,
    certificate_thumbprint,
    certificate_validFrom,

    -- ** CloudwatchLogsExportConfiguration
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- ** DBCluster
    dbCluster_engineVersion,
    dbCluster_status,
    dbCluster_deletionProtection,
    dbCluster_storageEncrypted,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_readReplicaIdentifiers,
    dbCluster_replicationSourceIdentifier,
    dbCluster_hostedZoneId,
    dbCluster_dbClusterParameterGroup,
    dbCluster_masterUsername,
    dbCluster_dbClusterResourceId,
    dbCluster_earliestRestorableTime,
    dbCluster_engine,
    dbCluster_dbClusterArn,
    dbCluster_latestRestorableTime,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_availabilityZones,
    dbCluster_kmsKeyId,
    dbCluster_preferredBackupWindow,
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_backupRetentionPeriod,
    dbCluster_dbSubnetGroup,
    dbCluster_multiAZ,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_clusterCreateTime,
    dbCluster_endpoint,
    dbCluster_percentProgress,
    dbCluster_readerEndpoint,
    dbCluster_port,

    -- ** DBClusterMember
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbClusterParameterGroupStatus,

    -- ** DBClusterParameterGroup
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_description,

    -- ** DBClusterParameterGroupNameMessage
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- ** DBClusterRole
    dbClusterRole_status,
    dbClusterRole_roleArn,

    -- ** DBClusterSnapshot
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_status,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_port,

    -- ** DBClusterSnapshotAttribute
    dbClusterSnapshotAttribute_attributeValues,
    dbClusterSnapshotAttribute_attributeName,

    -- ** DBClusterSnapshotAttributesResult
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,

    -- ** DBEngineVersion
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_exportableLogTypes,

    -- ** DBInstance
    dbInstance_engineVersion,
    dbInstance_storageEncrypted,
    dbInstance_dbClusterIdentifier,
    dbInstance_publiclyAccessible,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceArn,
    dbInstance_instanceCreateTime,
    dbInstance_engine,
    dbInstance_latestRestorableTime,
    dbInstance_dbInstanceClass,
    dbInstance_promotionTier,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbInstanceIdentifier,
    dbInstance_kmsKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_availabilityZone,
    dbInstance_vpcSecurityGroups,
    dbInstance_backupRetentionPeriod,
    dbInstance_dbSubnetGroup,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_dbiResourceId,
    dbInstance_endpoint,
    dbInstance_dbInstanceStatus,
    dbInstance_pendingModifiedValues,
    dbInstance_statusInfos,

    -- ** DBInstanceStatusInfo
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,
    dbInstanceStatusInfo_message,

    -- ** DBSubnetGroup
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

    -- ** Endpoint
    endpoint_hostedZoneId,
    endpoint_address,
    endpoint_port,

    -- ** EngineDefaults
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- ** Event
    event_sourceType,
    event_sourceArn,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- ** EventCategoriesMap
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- ** EventSubscription
    eventSubscription_status,
    eventSubscription_customerAwsId,
    eventSubscription_custSubscriptionId,
    eventSubscription_snsTopicArn,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_enabled,
    eventSubscription_sourceType,
    eventSubscription_subscriptionCreationTime,
    eventSubscription_eventCategoriesList,
    eventSubscription_sourceIdsList,

    -- ** Filter
    filter_name,
    filter_values,

    -- ** GlobalCluster
    globalCluster_engineVersion,
    globalCluster_status,
    globalCluster_deletionProtection,
    globalCluster_storageEncrypted,
    globalCluster_globalClusterIdentifier,
    globalCluster_engine,
    globalCluster_globalClusterArn,
    globalCluster_databaseName,
    globalCluster_globalClusterMembers,
    globalCluster_globalClusterResourceId,

    -- ** GlobalClusterMember
    globalClusterMember_readers,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,

    -- ** OrderableDBInstanceOption
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_vpc,

    -- ** Parameter
    parameter_applyType,
    parameter_parameterValue,
    parameter_applyMethod,
    parameter_minimumEngineVersion,
    parameter_source,
    parameter_isModifiable,
    parameter_dataType,
    parameter_allowedValues,
    parameter_parameterName,
    parameter_description,

    -- ** PendingCloudwatchLogsExports
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- ** PendingMaintenanceAction
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- ** PendingModifiedValues
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_iops,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_port,
    pendingModifiedValues_storageType,

    -- ** ResourcePendingMaintenanceActions
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- ** Subnet
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** UpgradeTarget
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,

    -- ** VpcSecurityGroupMembership
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import Amazonka.DocumentDB.AddSourceIdentifierToSubscription
import Amazonka.DocumentDB.AddTagsToResource
import Amazonka.DocumentDB.ApplyPendingMaintenanceAction
import Amazonka.DocumentDB.CopyDBClusterParameterGroup
import Amazonka.DocumentDB.CopyDBClusterSnapshot
import Amazonka.DocumentDB.CreateDBCluster
import Amazonka.DocumentDB.CreateDBClusterParameterGroup
import Amazonka.DocumentDB.CreateDBClusterSnapshot
import Amazonka.DocumentDB.CreateDBInstance
import Amazonka.DocumentDB.CreateDBSubnetGroup
import Amazonka.DocumentDB.CreateEventSubscription
import Amazonka.DocumentDB.CreateGlobalCluster
import Amazonka.DocumentDB.DeleteDBCluster
import Amazonka.DocumentDB.DeleteDBClusterParameterGroup
import Amazonka.DocumentDB.DeleteDBClusterSnapshot
import Amazonka.DocumentDB.DeleteDBInstance
import Amazonka.DocumentDB.DeleteDBSubnetGroup
import Amazonka.DocumentDB.DeleteEventSubscription
import Amazonka.DocumentDB.DeleteGlobalCluster
import Amazonka.DocumentDB.DescribeCertificates
import Amazonka.DocumentDB.DescribeDBClusterParameterGroups
import Amazonka.DocumentDB.DescribeDBClusterParameters
import Amazonka.DocumentDB.DescribeDBClusterSnapshotAttributes
import Amazonka.DocumentDB.DescribeDBClusterSnapshots
import Amazonka.DocumentDB.DescribeDBClusters
import Amazonka.DocumentDB.DescribeDBEngineVersions
import Amazonka.DocumentDB.DescribeDBInstances
import Amazonka.DocumentDB.DescribeDBSubnetGroups
import Amazonka.DocumentDB.DescribeEngineDefaultClusterParameters
import Amazonka.DocumentDB.DescribeEventCategories
import Amazonka.DocumentDB.DescribeEventSubscriptions
import Amazonka.DocumentDB.DescribeEvents
import Amazonka.DocumentDB.DescribeGlobalClusters
import Amazonka.DocumentDB.DescribeOrderableDBInstanceOptions
import Amazonka.DocumentDB.DescribePendingMaintenanceActions
import Amazonka.DocumentDB.FailoverDBCluster
import Amazonka.DocumentDB.ListTagsForResource
import Amazonka.DocumentDB.ModifyDBCluster
import Amazonka.DocumentDB.ModifyDBClusterParameterGroup
import Amazonka.DocumentDB.ModifyDBClusterSnapshotAttribute
import Amazonka.DocumentDB.ModifyDBInstance
import Amazonka.DocumentDB.ModifyDBSubnetGroup
import Amazonka.DocumentDB.ModifyEventSubscription
import Amazonka.DocumentDB.ModifyGlobalCluster
import Amazonka.DocumentDB.RebootDBInstance
import Amazonka.DocumentDB.RemoveFromGlobalCluster
import Amazonka.DocumentDB.RemoveSourceIdentifierFromSubscription
import Amazonka.DocumentDB.RemoveTagsFromResource
import Amazonka.DocumentDB.ResetDBClusterParameterGroup
import Amazonka.DocumentDB.RestoreDBClusterFromSnapshot
import Amazonka.DocumentDB.RestoreDBClusterToPointInTime
import Amazonka.DocumentDB.StartDBCluster
import Amazonka.DocumentDB.StopDBCluster
import Amazonka.DocumentDB.Types.AvailabilityZone
import Amazonka.DocumentDB.Types.Certificate
import Amazonka.DocumentDB.Types.CloudwatchLogsExportConfiguration
import Amazonka.DocumentDB.Types.DBCluster
import Amazonka.DocumentDB.Types.DBClusterMember
import Amazonka.DocumentDB.Types.DBClusterParameterGroup
import Amazonka.DocumentDB.Types.DBClusterParameterGroupNameMessage
import Amazonka.DocumentDB.Types.DBClusterRole
import Amazonka.DocumentDB.Types.DBClusterSnapshot
import Amazonka.DocumentDB.Types.DBClusterSnapshotAttribute
import Amazonka.DocumentDB.Types.DBClusterSnapshotAttributesResult
import Amazonka.DocumentDB.Types.DBEngineVersion
import Amazonka.DocumentDB.Types.DBInstance
import Amazonka.DocumentDB.Types.DBInstanceStatusInfo
import Amazonka.DocumentDB.Types.DBSubnetGroup
import Amazonka.DocumentDB.Types.Endpoint
import Amazonka.DocumentDB.Types.EngineDefaults
import Amazonka.DocumentDB.Types.Event
import Amazonka.DocumentDB.Types.EventCategoriesMap
import Amazonka.DocumentDB.Types.EventSubscription
import Amazonka.DocumentDB.Types.Filter
import Amazonka.DocumentDB.Types.GlobalCluster
import Amazonka.DocumentDB.Types.GlobalClusterMember
import Amazonka.DocumentDB.Types.OrderableDBInstanceOption
import Amazonka.DocumentDB.Types.Parameter
import Amazonka.DocumentDB.Types.PendingCloudwatchLogsExports
import Amazonka.DocumentDB.Types.PendingMaintenanceAction
import Amazonka.DocumentDB.Types.PendingModifiedValues
import Amazonka.DocumentDB.Types.ResourcePendingMaintenanceActions
import Amazonka.DocumentDB.Types.Subnet
import Amazonka.DocumentDB.Types.Tag
import Amazonka.DocumentDB.Types.UpgradeTarget
import Amazonka.DocumentDB.Types.VpcSecurityGroupMembership
