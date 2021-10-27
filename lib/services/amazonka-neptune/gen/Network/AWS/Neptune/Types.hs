{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Neptune.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Neptune.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidDBParameterGroupStateFault,
    _SourceNotFoundFault,
    _CertificateNotFoundFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _DBParameterGroupAlreadyExistsFault,
    _DBParameterGroupQuotaExceededFault,
    _InsufficientDBClusterCapacityFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _SubscriptionCategoryNotFoundFault,
    _SubscriptionNotFoundFault,
    _InvalidSubnet,
    _SharedSnapshotQuotaExceededFault,
    _DBSubnetQuotaExceededFault,
    _OptionGroupNotFoundFault,
    _DBClusterNotFoundFault,
    _DBClusterAlreadyExistsFault,
    _StorageTypeNotSupportedFault,
    _SNSTopicArnNotFoundFault,
    _InvalidDBClusterEndpointStateFault,
    _InvalidEventSubscriptionStateFault,
    _KMSKeyNotAccessibleFault,
    _DBSnapshotNotFoundFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
    _SnapshotQuotaExceededFault,
    _DBSubnetGroupAlreadyExistsFault,
    _SNSNoAuthorizationFault,
    _DBSecurityGroupNotFoundFault,
    _InstanceQuotaExceededFault,
    _DomainNotFoundFault,
    _DBParameterGroupNotFoundFault,
    _InvalidDBSubnetStateFault,
    _DBClusterSnapshotNotFoundFault,
    _SNSInvalidTopicFault,
    _InsufficientDBInstanceCapacityFault,
    _InvalidDBClusterSnapshotStateFault,
    _SubscriptionAlreadyExistFault,
    _DBClusterRoleAlreadyExistsFault,
    _DBClusterRoleQuotaExceededFault,
    _InvalidVPCNetworkStateFault,
    _AuthorizationNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _EventSubscriptionQuotaExceededFault,
    _InsufficientStorageClusterCapacityFault,
    _DBClusterEndpointQuotaExceededFault,
    _InvalidDBClusterStateFault,
    _DBInstanceAlreadyExistsFault,
    _InvalidRestoreFault,
    _InvalidDBSecurityGroupStateFault,
    _ResourceNotFoundFault,
    _DBSubnetGroupNotFoundFault,
    _DBUpgradeDependencyFailureFault,
    _InvalidDBInstanceStateFault,
    _DBClusterEndpointAlreadyExistsFault,
    _DBSnapshotAlreadyExistsFault,
    _DBInstanceNotFoundFault,
    _StorageQuotaExceededFault,
    _InvalidDBSnapshotStateFault,
    _InvalidDBSubnetGroupStateFault,
    _DBClusterEndpointNotFoundFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _SubnetAlreadyInUse,
    _DBClusterRoleNotFoundFault,

    -- * ApplyMethod
    ApplyMethod (..),

    -- * SourceType
    SourceType (..),

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * CharacterSet
    CharacterSet (..),
    newCharacterSet,
    characterSet_characterSetName,
    characterSet_characterSetDescription,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
    dbCluster_engineVersion,
    dbCluster_status,
    dbCluster_deletionProtection,
    dbCluster_automaticRestartTime,
    dbCluster_storageEncrypted,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_readReplicaIdentifiers,
    dbCluster_replicationSourceIdentifier,
    dbCluster_hostedZoneId,
    dbCluster_dbClusterParameterGroup,
    dbCluster_masterUsername,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_dbClusterResourceId,
    dbCluster_earliestRestorableTime,
    dbCluster_engine,
    dbCluster_dbClusterArn,
    dbCluster_cloneGroupId,
    dbCluster_latestRestorableTime,
    dbCluster_crossAccountClone,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_availabilityZones,
    dbCluster_characterSetName,
    dbCluster_kmsKeyId,
    dbCluster_preferredBackupWindow,
    dbCluster_associatedRoles,
    dbCluster_vpcSecurityGroups,
    dbCluster_backupRetentionPeriod,
    dbCluster_dbSubnetGroup,
    dbCluster_databaseName,
    dbCluster_multiAZ,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_allocatedStorage,
    dbCluster_copyTagsToSnapshot,
    dbCluster_clusterCreateTime,
    dbCluster_endpoint,
    dbCluster_percentProgress,
    dbCluster_readerEndpoint,
    dbCluster_port,
    dbCluster_dbClusterOptionGroupMemberships,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
    dbClusterEndpoint_status,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_excludedMembers,

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbClusterParameterGroupStatus,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    newDBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus_status,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    newDBClusterParameterGroup,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_description,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_status,
    dbClusterRole_featureName,
    dbClusterRole_roleArn,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_status,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_vpcId,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_port,

    -- * DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    newDBClusterSnapshotAttribute,
    dbClusterSnapshotAttribute_attributeValues,
    dbClusterSnapshotAttribute_attributeName,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    newDBClusterSnapshotAttributesResult,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,

    -- * DBEngineVersion
    DBEngineVersion (..),
    newDBEngineVersion,
    dbEngineVersion_engineVersion,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_exportableLogTypes,

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
    dbInstance_engineVersion,
    dbInstance_dbSecurityGroups,
    dbInstance_deletionProtection,
    dbInstance_storageEncrypted,
    dbInstance_dbClusterIdentifier,
    dbInstance_publiclyAccessible,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_dbInstanceArn,
    dbInstance_masterUsername,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_monitoringRoleArn,
    dbInstance_iops,
    dbInstance_instanceCreateTime,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_monitoringInterval,
    dbInstance_engine,
    dbInstance_latestRestorableTime,
    dbInstance_dbInstanceClass,
    dbInstance_promotionTier,
    dbInstance_licenseModel,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_cACertificateIdentifier,
    dbInstance_dbInstanceIdentifier,
    dbInstance_characterSetName,
    dbInstance_kmsKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_availabilityZone,
    dbInstance_vpcSecurityGroups,
    dbInstance_backupRetentionPeriod,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_dbSubnetGroup,
    dbInstance_multiAZ,
    dbInstance_optionGroupMemberships,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_performanceInsightsEnabled,
    dbInstance_allocatedStorage,
    dbInstance_dbiResourceId,
    dbInstance_dbParameterGroups,
    dbInstance_copyTagsToSnapshot,
    dbInstance_timezone,
    dbInstance_tdeCredentialArn,
    dbInstance_endpoint,
    dbInstance_dbInstanceStatus,
    dbInstance_dbInstancePort,
    dbInstance_pendingModifiedValues,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_storageType,
    dbInstance_statusInfos,
    dbInstance_domainMemberships,
    dbInstance_dbName,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,
    dbInstanceStatusInfo_message,

    -- * DBParameterGroup
    DBParameterGroup (..),
    newDBParameterGroup,
    dbParameterGroup_dbParameterGroupArn,
    dbParameterGroup_dbParameterGroupFamily,
    dbParameterGroup_dbParameterGroupName,
    dbParameterGroup_description,

    -- * DBParameterGroupNameMessage
    DBParameterGroupNameMessage (..),
    newDBParameterGroupNameMessage,
    dbParameterGroupNameMessage_dbParameterGroupName,

    -- * DBParameterGroupStatus
    DBParameterGroupStatus (..),
    newDBParameterGroupStatus,
    dbParameterGroupStatus_dbParameterGroupName,
    dbParameterGroupStatus_parameterApplyStatus,

    -- * DBSecurityGroupMembership
    DBSecurityGroupMembership (..),
    newDBSecurityGroupMembership,
    dbSecurityGroupMembership_status,
    dbSecurityGroupMembership_dbSecurityGroupName,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

    -- * DomainMembership
    DomainMembership (..),
    newDomainMembership,
    domainMembership_status,
    domainMembership_fqdn,
    domainMembership_domain,
    domainMembership_iAMRoleName,

    -- * DoubleRange
    DoubleRange (..),
    newDoubleRange,
    doubleRange_to,
    doubleRange_from,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_hostedZoneId,
    endpoint_address,
    endpoint_port,

    -- * EngineDefaults
    EngineDefaults (..),
    newEngineDefaults,
    engineDefaults_dbParameterGroupFamily,
    engineDefaults_marker,
    engineDefaults_parameters,

    -- * Event
    Event (..),
    newEvent,
    event_sourceType,
    event_sourceArn,
    event_sourceIdentifier,
    event_date,
    event_eventCategories,
    event_message,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_sourceType,
    eventCategoriesMap_eventCategories,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
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

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_status,
    optionGroupMembership_optionGroupName,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_vpc,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_storageType,

    -- * Parameter
    Parameter (..),
    newParameter,
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

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToEnable,
    pendingCloudwatchLogsExports_logTypesToDisable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_optInStatus,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_currentApplyDate,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
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

    -- * Range
    Range (..),
    newRange,
    range_to,
    range_from,
    range_step,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetStatus,
    subnet_subnetIdentifier,
    subnet_subnetAvailabilityZone,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,

    -- * Timezone
    Timezone (..),
    newTimezone,
    timezone_timezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_storage,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_storageSize,
    validStorageOptions_provisionedIops,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_storageType,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Neptune.Types.ApplyMethod
import Network.AWS.Neptune.Types.AvailabilityZone
import Network.AWS.Neptune.Types.CharacterSet
import Network.AWS.Neptune.Types.CloudwatchLogsExportConfiguration
import Network.AWS.Neptune.Types.DBCluster
import Network.AWS.Neptune.Types.DBClusterEndpoint
import Network.AWS.Neptune.Types.DBClusterMember
import Network.AWS.Neptune.Types.DBClusterOptionGroupStatus
import Network.AWS.Neptune.Types.DBClusterParameterGroup
import Network.AWS.Neptune.Types.DBClusterParameterGroupNameMessage
import Network.AWS.Neptune.Types.DBClusterRole
import Network.AWS.Neptune.Types.DBClusterSnapshot
import Network.AWS.Neptune.Types.DBClusterSnapshotAttribute
import Network.AWS.Neptune.Types.DBClusterSnapshotAttributesResult
import Network.AWS.Neptune.Types.DBEngineVersion
import Network.AWS.Neptune.Types.DBInstance
import Network.AWS.Neptune.Types.DBInstanceStatusInfo
import Network.AWS.Neptune.Types.DBParameterGroup
import Network.AWS.Neptune.Types.DBParameterGroupNameMessage
import Network.AWS.Neptune.Types.DBParameterGroupStatus
import Network.AWS.Neptune.Types.DBSecurityGroupMembership
import Network.AWS.Neptune.Types.DBSubnetGroup
import Network.AWS.Neptune.Types.DomainMembership
import Network.AWS.Neptune.Types.DoubleRange
import Network.AWS.Neptune.Types.Endpoint
import Network.AWS.Neptune.Types.EngineDefaults
import Network.AWS.Neptune.Types.Event
import Network.AWS.Neptune.Types.EventCategoriesMap
import Network.AWS.Neptune.Types.EventSubscription
import Network.AWS.Neptune.Types.Filter
import Network.AWS.Neptune.Types.OptionGroupMembership
import Network.AWS.Neptune.Types.OrderableDBInstanceOption
import Network.AWS.Neptune.Types.Parameter
import Network.AWS.Neptune.Types.PendingCloudwatchLogsExports
import Network.AWS.Neptune.Types.PendingMaintenanceAction
import Network.AWS.Neptune.Types.PendingModifiedValues
import Network.AWS.Neptune.Types.Range
import Network.AWS.Neptune.Types.ResourcePendingMaintenanceActions
import Network.AWS.Neptune.Types.SourceType
import Network.AWS.Neptune.Types.Subnet
import Network.AWS.Neptune.Types.Tag
import Network.AWS.Neptune.Types.Timezone
import Network.AWS.Neptune.Types.UpgradeTarget
import Network.AWS.Neptune.Types.ValidDBInstanceModificationsMessage
import Network.AWS.Neptune.Types.ValidStorageOptions
import Network.AWS.Neptune.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon Neptune SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Neptune",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "rds",
      Core._serviceSigningName = "rds",
      Core._serviceVersion = "2014-10-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "Neptune",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The DB parameter group is in use or is in an invalid state. If you are
-- attempting to delete the parameter group, you cannot delete it when the
-- parameter group is in this state.
_InvalidDBParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | /CertificateIdentifier/ does not refer to an existing certificate.
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | User already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Request would result in user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The DB cluster does not have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Prelude.. Core.hasStatus 400

-- | The designated subscription category could not be found.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The designated subscription could not be found.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Request would result in user exceeding the allowed number of subnets in
-- a DB subnet groups.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The designated option group could not be found.
_OptionGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | /DBClusterIdentifier/ does not refer to an existing DB cluster.
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | User already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | /StorageType/ specified cannot be associated with the DB Instance.
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The ARN of the SNS topic could not be found.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested operation cannot be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The event subscription is in an invalid state.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | Error accessing KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | /DBSnapshotIdentifier/ does not refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | /DBClusterParameterGroupName/ does not refer to an existing DB Cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | User attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | Request would result in user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | There is no SNS authorization.
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | /DBSecurityGroupName/ does not refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | /Domain/ does not refer to an existing Active Directory Domain.
_DomainNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | /DBParameterGroupName/ does not refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The DB subnet is not in the /available/ state.
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | /DBClusterSnapshotIdentifier/ does not refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The SNS topic is invalid.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | Specified DB instance class is not available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | The supplied value is not a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | This subscription already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | DB subnet group does not cover all Availability Zones after it is
-- created because users\' change.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | Specified CIDRIP or EC2 security group is not authorized for the
-- specified DB security group.
--
-- Neptune may not also be authorized via IAM to perform necessary actions
-- on your behalf.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | You have exceeded the number of events you can subscribe to.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | There is insufficient storage available for the current action. You may
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The DB cluster is not in a valid state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | User already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Cannot restore from vpc backup to non-vpc DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The state of the DB security group does not allow deletion.
_InvalidDBSecurityGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | /DBSubnetGroupName/ does not refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | The specified DB instance is not in the /available/ state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint cannot be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | /DBInstanceIdentifier/ does not refer to an existing DB instance.
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The state of the DB snapshot does not allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it is in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) is not associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Prelude.. Core.hasStatus 404
