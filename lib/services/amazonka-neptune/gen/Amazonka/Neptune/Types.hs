{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Neptune.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AuthorizationNotFoundFault,
    _CertificateNotFoundFault,
    _DBClusterAlreadyExistsFault,
    _DBClusterEndpointAlreadyExistsFault,
    _DBClusterEndpointNotFoundFault,
    _DBClusterEndpointQuotaExceededFault,
    _DBClusterNotFoundFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
    _DBClusterRoleAlreadyExistsFault,
    _DBClusterRoleNotFoundFault,
    _DBClusterRoleQuotaExceededFault,
    _DBClusterSnapshotAlreadyExistsFault,
    _DBClusterSnapshotNotFoundFault,
    _DBInstanceAlreadyExistsFault,
    _DBInstanceNotFoundFault,
    _DBParameterGroupAlreadyExistsFault,
    _DBParameterGroupNotFoundFault,
    _DBParameterGroupQuotaExceededFault,
    _DBSecurityGroupNotFoundFault,
    _DBSnapshotAlreadyExistsFault,
    _DBSnapshotNotFoundFault,
    _DBSubnetGroupAlreadyExistsFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _DBSubnetGroupNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _DBSubnetQuotaExceededFault,
    _DBUpgradeDependencyFailureFault,
    _DomainNotFoundFault,
    _EventSubscriptionQuotaExceededFault,
    _GlobalClusterAlreadyExistsFault,
    _GlobalClusterNotFoundFault,
    _GlobalClusterQuotaExceededFault,
    _InstanceQuotaExceededFault,
    _InsufficientDBClusterCapacityFault,
    _InsufficientDBInstanceCapacityFault,
    _InsufficientStorageClusterCapacityFault,
    _InvalidDBClusterEndpointStateFault,
    _InvalidDBClusterSnapshotStateFault,
    _InvalidDBClusterStateFault,
    _InvalidDBInstanceStateFault,
    _InvalidDBParameterGroupStateFault,
    _InvalidDBSecurityGroupStateFault,
    _InvalidDBSnapshotStateFault,
    _InvalidDBSubnetGroupStateFault,
    _InvalidDBSubnetStateFault,
    _InvalidEventSubscriptionStateFault,
    _InvalidGlobalClusterStateFault,
    _InvalidRestoreFault,
    _InvalidSubnet,
    _InvalidVPCNetworkStateFault,
    _KMSKeyNotAccessibleFault,
    _OptionGroupNotFoundFault,
    _ProvisionedIopsNotAvailableInAZFault,
    _ResourceNotFoundFault,
    _SNSInvalidTopicFault,
    _SNSNoAuthorizationFault,
    _SNSTopicArnNotFoundFault,
    _SharedSnapshotQuotaExceededFault,
    _SnapshotQuotaExceededFault,
    _SourceNotFoundFault,
    _StorageQuotaExceededFault,
    _StorageTypeNotSupportedFault,
    _SubnetAlreadyInUse,
    _SubscriptionAlreadyExistFault,
    _SubscriptionCategoryNotFoundFault,
    _SubscriptionNotFoundFault,

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
    characterSet_characterSetDescription,
    characterSet_characterSetName,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
    dbCluster_allocatedStorage,
    dbCluster_associatedRoles,
    dbCluster_automaticRestartTime,
    dbCluster_availabilityZones,
    dbCluster_backupRetentionPeriod,
    dbCluster_characterSetName,
    dbCluster_cloneGroupId,
    dbCluster_clusterCreateTime,
    dbCluster_copyTagsToSnapshot,
    dbCluster_crossAccountClone,
    dbCluster_dbClusterArn,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterOptionGroupMemberships,
    dbCluster_dbClusterParameterGroup,
    dbCluster_dbSubnetGroup,
    dbCluster_databaseName,
    dbCluster_dbClusterResourceId,
    dbCluster_deletionProtection,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_endpoint,
    dbCluster_engine,
    dbCluster_engineVersion,
    dbCluster_hostedZoneId,
    dbCluster_iAMDatabaseAuthenticationEnabled,
    dbCluster_kmsKeyId,
    dbCluster_latestRestorableTime,
    dbCluster_masterUsername,
    dbCluster_multiAZ,
    dbCluster_percentProgress,
    dbCluster_port,
    dbCluster_preferredBackupWindow,
    dbCluster_preferredMaintenanceWindow,
    dbCluster_readReplicaIdentifiers,
    dbCluster_readerEndpoint,
    dbCluster_replicationSourceIdentifier,
    dbCluster_serverlessV2ScalingConfiguration,
    dbCluster_status,
    dbCluster_storageEncrypted,
    dbCluster_vpcSecurityGroups,

    -- * DBClusterEndpoint
    DBClusterEndpoint (..),
    newDBClusterEndpoint,
    dbClusterEndpoint_customEndpointType,
    dbClusterEndpoint_dbClusterEndpointArn,
    dbClusterEndpoint_dbClusterEndpointIdentifier,
    dbClusterEndpoint_dbClusterEndpointResourceIdentifier,
    dbClusterEndpoint_dbClusterIdentifier,
    dbClusterEndpoint_endpoint,
    dbClusterEndpoint_endpointType,
    dbClusterEndpoint_excludedMembers,
    dbClusterEndpoint_staticMembers,
    dbClusterEndpoint_status,

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_promotionTier,

    -- * DBClusterOptionGroupStatus
    DBClusterOptionGroupStatus (..),
    newDBClusterOptionGroupStatus,
    dbClusterOptionGroupStatus_dbClusterOptionGroupName,
    dbClusterOptionGroupStatus_status,

    -- * DBClusterParameterGroup
    DBClusterParameterGroup (..),
    newDBClusterParameterGroup,
    dbClusterParameterGroup_dbClusterParameterGroupArn,
    dbClusterParameterGroup_dbClusterParameterGroupName,
    dbClusterParameterGroup_dbParameterGroupFamily,
    dbClusterParameterGroup_description,

    -- * DBClusterParameterGroupNameMessage
    DBClusterParameterGroupNameMessage (..),
    newDBClusterParameterGroupNameMessage,
    dbClusterParameterGroupNameMessage_dbClusterParameterGroupName,

    -- * DBClusterRole
    DBClusterRole (..),
    newDBClusterRole,
    dbClusterRole_featureName,
    dbClusterRole_roleArn,
    dbClusterRole_status,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
    dbClusterSnapshot_allocatedStorage,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_iAMDatabaseAuthenticationEnabled,
    dbClusterSnapshot_kmsKeyId,
    dbClusterSnapshot_licenseModel,
    dbClusterSnapshot_masterUsername,
    dbClusterSnapshot_percentProgress,
    dbClusterSnapshot_port,
    dbClusterSnapshot_snapshotCreateTime,
    dbClusterSnapshot_snapshotType,
    dbClusterSnapshot_sourceDBClusterSnapshotArn,
    dbClusterSnapshot_status,
    dbClusterSnapshot_storageEncrypted,
    dbClusterSnapshot_vpcId,

    -- * DBClusterSnapshotAttribute
    DBClusterSnapshotAttribute (..),
    newDBClusterSnapshotAttribute,
    dbClusterSnapshotAttribute_attributeName,
    dbClusterSnapshotAttribute_attributeValues,

    -- * DBClusterSnapshotAttributesResult
    DBClusterSnapshotAttributesResult (..),
    newDBClusterSnapshotAttributesResult,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotAttributes,
    dbClusterSnapshotAttributesResult_dbClusterSnapshotIdentifier,

    -- * DBEngineVersion
    DBEngineVersion (..),
    newDBEngineVersion,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_dbEngineVersionDescription,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_defaultCharacterSet,
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportedCharacterSets,
    dbEngineVersion_supportedTimezones,
    dbEngineVersion_supportsGlobalDatabases,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_supportsReadReplica,
    dbEngineVersion_validUpgradeTarget,

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
    dbInstance_allocatedStorage,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_availabilityZone,
    dbInstance_backupRetentionPeriod,
    dbInstance_cACertificateIdentifier,
    dbInstance_characterSetName,
    dbInstance_copyTagsToSnapshot,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbInstanceArn,
    dbInstance_dbInstanceClass,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstanceStatus,
    dbInstance_dbName,
    dbInstance_dbParameterGroups,
    dbInstance_dbSecurityGroups,
    dbInstance_dbSubnetGroup,
    dbInstance_dbInstancePort,
    dbInstance_dbiResourceId,
    dbInstance_deletionProtection,
    dbInstance_domainMemberships,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_endpoint,
    dbInstance_engine,
    dbInstance_engineVersion,
    dbInstance_enhancedMonitoringResourceArn,
    dbInstance_iAMDatabaseAuthenticationEnabled,
    dbInstance_instanceCreateTime,
    dbInstance_iops,
    dbInstance_kmsKeyId,
    dbInstance_latestRestorableTime,
    dbInstance_licenseModel,
    dbInstance_masterUsername,
    dbInstance_monitoringInterval,
    dbInstance_monitoringRoleArn,
    dbInstance_multiAZ,
    dbInstance_optionGroupMemberships,
    dbInstance_pendingModifiedValues,
    dbInstance_performanceInsightsEnabled,
    dbInstance_performanceInsightsKMSKeyId,
    dbInstance_preferredBackupWindow,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_promotionTier,
    dbInstance_publiclyAccessible,
    dbInstance_readReplicaDBClusterIdentifiers,
    dbInstance_readReplicaDBInstanceIdentifiers,
    dbInstance_readReplicaSourceDBInstanceIdentifier,
    dbInstance_secondaryAvailabilityZone,
    dbInstance_statusInfos,
    dbInstance_storageEncrypted,
    dbInstance_storageType,
    dbInstance_tdeCredentialArn,
    dbInstance_timezone,
    dbInstance_vpcSecurityGroups,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_statusType,

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
    dbSecurityGroupMembership_dbSecurityGroupName,
    dbSecurityGroupMembership_status,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_vpcId,

    -- * DomainMembership
    DomainMembership (..),
    newDomainMembership,
    domainMembership_domain,
    domainMembership_fqdn,
    domainMembership_iAMRoleName,
    domainMembership_status,

    -- * DoubleRange
    DoubleRange (..),
    newDoubleRange,
    doubleRange_from,
    doubleRange_to,

    -- * Endpoint
    Endpoint (..),
    newEndpoint,
    endpoint_address,
    endpoint_hostedZoneId,
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
    event_date,
    event_eventCategories,
    event_message,
    event_sourceArn,
    event_sourceIdentifier,
    event_sourceType,

    -- * EventCategoriesMap
    EventCategoriesMap (..),
    newEventCategoriesMap,
    eventCategoriesMap_eventCategories,
    eventCategoriesMap_sourceType,

    -- * EventSubscription
    EventSubscription (..),
    newEventSubscription,
    eventSubscription_custSubscriptionId,
    eventSubscription_customerAwsId,
    eventSubscription_enabled,
    eventSubscription_eventCategoriesList,
    eventSubscription_eventSubscriptionArn,
    eventSubscription_snsTopicArn,
    eventSubscription_sourceIdsList,
    eventSubscription_sourceType,
    eventSubscription_status,
    eventSubscription_subscriptionCreationTime,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_values,

    -- * GlobalCluster
    GlobalCluster (..),
    newGlobalCluster,
    globalCluster_deletionProtection,
    globalCluster_engine,
    globalCluster_engineVersion,
    globalCluster_globalClusterArn,
    globalCluster_globalClusterIdentifier,
    globalCluster_globalClusterMembers,
    globalCluster_globalClusterResourceId,
    globalCluster_status,
    globalCluster_storageEncrypted,

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,
    globalClusterMember_readers,

    -- * OptionGroupMembership
    OptionGroupMembership (..),
    newOptionGroupMembership,
    optionGroupMembership_optionGroupName,
    optionGroupMembership_status,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_maxIopsPerDbInstance,
    orderableDBInstanceOption_maxIopsPerGib,
    orderableDBInstanceOption_maxStorageSize,
    orderableDBInstanceOption_minIopsPerDbInstance,
    orderableDBInstanceOption_minIopsPerGib,
    orderableDBInstanceOption_minStorageSize,
    orderableDBInstanceOption_multiAZCapable,
    orderableDBInstanceOption_readReplicaCapable,
    orderableDBInstanceOption_storageType,
    orderableDBInstanceOption_supportsEnhancedMonitoring,
    orderableDBInstanceOption_supportsGlobalDatabases,
    orderableDBInstanceOption_supportsIAMDatabaseAuthentication,
    orderableDBInstanceOption_supportsIops,
    orderableDBInstanceOption_supportsPerformanceInsights,
    orderableDBInstanceOption_supportsStorageEncryption,
    orderableDBInstanceOption_vpc,

    -- * Parameter
    Parameter (..),
    newParameter,
    parameter_allowedValues,
    parameter_applyMethod,
    parameter_applyType,
    parameter_dataType,
    parameter_description,
    parameter_isModifiable,
    parameter_minimumEngineVersion,
    parameter_parameterName,
    parameter_parameterValue,
    parameter_source,

    -- * PendingCloudwatchLogsExports
    PendingCloudwatchLogsExports (..),
    newPendingCloudwatchLogsExports,
    pendingCloudwatchLogsExports_logTypesToDisable,
    pendingCloudwatchLogsExports_logTypesToEnable,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_autoAppliedAfterDate,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_forcedApplyDate,
    pendingMaintenanceAction_optInStatus,

    -- * PendingModifiedValues
    PendingModifiedValues (..),
    newPendingModifiedValues,
    pendingModifiedValues_allocatedStorage,
    pendingModifiedValues_backupRetentionPeriod,
    pendingModifiedValues_cACertificateIdentifier,
    pendingModifiedValues_dbInstanceClass,
    pendingModifiedValues_dbInstanceIdentifier,
    pendingModifiedValues_dbSubnetGroupName,
    pendingModifiedValues_engineVersion,
    pendingModifiedValues_iops,
    pendingModifiedValues_licenseModel,
    pendingModifiedValues_masterUserPassword,
    pendingModifiedValues_multiAZ,
    pendingModifiedValues_pendingCloudwatchLogsExports,
    pendingModifiedValues_port,
    pendingModifiedValues_storageType,

    -- * Range
    Range (..),
    newRange,
    range_from,
    range_step,
    range_to,

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

    -- * ServerlessV2ScalingConfiguration
    ServerlessV2ScalingConfiguration (..),
    newServerlessV2ScalingConfiguration,
    serverlessV2ScalingConfiguration_maxCapacity,
    serverlessV2ScalingConfiguration_minCapacity,

    -- * ServerlessV2ScalingConfigurationInfo
    ServerlessV2ScalingConfigurationInfo (..),
    newServerlessV2ScalingConfigurationInfo,
    serverlessV2ScalingConfigurationInfo_maxCapacity,
    serverlessV2ScalingConfigurationInfo_minCapacity,

    -- * Subnet
    Subnet (..),
    newSubnet,
    subnet_subnetAvailabilityZone,
    subnet_subnetIdentifier,
    subnet_subnetStatus,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Timezone
    Timezone (..),
    newTimezone,
    timezone_timezoneName,

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_supportsGlobalDatabases,

    -- * ValidDBInstanceModificationsMessage
    ValidDBInstanceModificationsMessage (..),
    newValidDBInstanceModificationsMessage,
    validDBInstanceModificationsMessage_storage,

    -- * ValidStorageOptions
    ValidStorageOptions (..),
    newValidStorageOptions,
    validStorageOptions_iopsToStorageRatio,
    validStorageOptions_provisionedIops,
    validStorageOptions_storageSize,
    validStorageOptions_storageType,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Neptune.Types.ApplyMethod
import Amazonka.Neptune.Types.AvailabilityZone
import Amazonka.Neptune.Types.CharacterSet
import Amazonka.Neptune.Types.CloudwatchLogsExportConfiguration
import Amazonka.Neptune.Types.DBCluster
import Amazonka.Neptune.Types.DBClusterEndpoint
import Amazonka.Neptune.Types.DBClusterMember
import Amazonka.Neptune.Types.DBClusterOptionGroupStatus
import Amazonka.Neptune.Types.DBClusterParameterGroup
import Amazonka.Neptune.Types.DBClusterParameterGroupNameMessage
import Amazonka.Neptune.Types.DBClusterRole
import Amazonka.Neptune.Types.DBClusterSnapshot
import Amazonka.Neptune.Types.DBClusterSnapshotAttribute
import Amazonka.Neptune.Types.DBClusterSnapshotAttributesResult
import Amazonka.Neptune.Types.DBEngineVersion
import Amazonka.Neptune.Types.DBInstance
import Amazonka.Neptune.Types.DBInstanceStatusInfo
import Amazonka.Neptune.Types.DBParameterGroup
import Amazonka.Neptune.Types.DBParameterGroupNameMessage
import Amazonka.Neptune.Types.DBParameterGroupStatus
import Amazonka.Neptune.Types.DBSecurityGroupMembership
import Amazonka.Neptune.Types.DBSubnetGroup
import Amazonka.Neptune.Types.DomainMembership
import Amazonka.Neptune.Types.DoubleRange
import Amazonka.Neptune.Types.Endpoint
import Amazonka.Neptune.Types.EngineDefaults
import Amazonka.Neptune.Types.Event
import Amazonka.Neptune.Types.EventCategoriesMap
import Amazonka.Neptune.Types.EventSubscription
import Amazonka.Neptune.Types.Filter
import Amazonka.Neptune.Types.GlobalCluster
import Amazonka.Neptune.Types.GlobalClusterMember
import Amazonka.Neptune.Types.OptionGroupMembership
import Amazonka.Neptune.Types.OrderableDBInstanceOption
import Amazonka.Neptune.Types.Parameter
import Amazonka.Neptune.Types.PendingCloudwatchLogsExports
import Amazonka.Neptune.Types.PendingMaintenanceAction
import Amazonka.Neptune.Types.PendingModifiedValues
import Amazonka.Neptune.Types.Range
import Amazonka.Neptune.Types.ResourcePendingMaintenanceActions
import Amazonka.Neptune.Types.ServerlessV2ScalingConfiguration
import Amazonka.Neptune.Types.ServerlessV2ScalingConfigurationInfo
import Amazonka.Neptune.Types.SourceType
import Amazonka.Neptune.Types.Subnet
import Amazonka.Neptune.Types.Tag
import Amazonka.Neptune.Types.Timezone
import Amazonka.Neptune.Types.UpgradeTarget
import Amazonka.Neptune.Types.ValidDBInstanceModificationsMessage
import Amazonka.Neptune.Types.ValidStorageOptions
import Amazonka.Neptune.Types.VpcSecurityGroupMembership
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon Neptune SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Neptune",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rds",
      Core.signingName = "rds",
      Core.version = "2014-10-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "Neptune",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Specified CIDRIP or EC2 security group is not authorized for the
-- specified DB security group.
--
-- Neptune may not also be authorized via IAM to perform necessary actions
-- on your behalf.
_AuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | /CertificateIdentifier/ does not refer to an existing certificate.
_CertificateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | User already has a DB cluster with the given identifier.
_DBClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint cannot be created because it already
-- exists.
_DBClusterEndpointAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The specified custom endpoint doesn\'t exist.
_DBClusterEndpointNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointNotFoundFault"
    Prelude.. Core.hasStatus 400

-- | The cluster already has the maximum number of custom endpoints.
_DBClusterEndpointQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterEndpointQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterEndpointQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | /DBClusterIdentifier/ does not refer to an existing DB cluster.
_DBClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | /DBClusterParameterGroupName/ does not refer to an existing DB Cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | User attempted to create a new DB cluster and the user has already
-- reached the maximum allowed DB cluster quota.
_DBClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The specified IAM role Amazon Resource Name (ARN) is already associated
-- with the specified DB cluster.
_DBClusterRoleAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | The specified IAM role Amazon Resource Name (ARN) is not associated with
-- the specified DB cluster.
_DBClusterRoleNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the maximum number of IAM roles that can be associated
-- with the specified DB cluster.
_DBClusterRoleQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterRoleQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterRoleQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | User already has a DB cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | /DBClusterSnapshotIdentifier/ does not refer to an existing DB cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | User already has a DB instance with the given identifier.
_DBInstanceAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | /DBInstanceIdentifier/ does not refer to an existing DB instance.
_DBInstanceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | A DB parameter group with the same name exists.
_DBParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | /DBParameterGroupName/ does not refer to an existing DB parameter group.
_DBParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed number of DB
-- parameter groups.
_DBParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | /DBSecurityGroupName/ does not refer to an existing DB security group.
_DBSecurityGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | /DBSnapshotIdentifier/ is already used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | /DBSnapshotIdentifier/ does not refer to an existing DB snapshot.
_DBSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | /DBSubnetGroupName/ is already used by an existing DB subnet group.
_DBSubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Subnets in the DB subnet group should cover at least two Availability
-- Zones unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | /DBSubnetGroupName/ does not refer to an existing DB subnet group.
_DBSubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed number of DB subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Request would result in user exceeding the allowed number of subnets in
-- a DB subnet groups.
_DBSubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The DB upgrade failed because a resource the DB depends on could not be
-- modified.
_DBUpgradeDependencyFailureFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | /Domain/ does not refer to an existing Active Directory Domain.
_DomainNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DomainNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DomainNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the number of events you can subscribe to.
_EventSubscriptionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- database identifier (unique name) to create a new global database
-- cluster.
_GlobalClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- database cluster.
_GlobalClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The number of global database clusters for this account is already at
-- the maximum allowed.
_GlobalClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | Request would result in user exceeding the allowed number of DB
-- instances.
_InstanceQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The DB cluster does not have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | Specified DB instance class is not available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | There is insufficient storage available for the current action. You may
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The requested operation cannot be performed on the endpoint while the
-- endpoint is in this state.
_InvalidDBClusterEndpointStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterEndpointStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterEndpointStateFault"
    Prelude.. Core.hasStatus 400

-- | The supplied value is not a valid DB cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB cluster is not in a valid state.
_InvalidDBClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified DB instance is not in the /available/ state.
_InvalidDBInstanceStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The DB parameter group is in use or is in an invalid state. If you are
-- attempting to delete the parameter group, you cannot delete it when the
-- parameter group is in this state.
_InvalidDBParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the DB security group does not allow deletion.
_InvalidDBSecurityGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the DB snapshot does not allow deletion.
_InvalidDBSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The DB subnet group cannot be deleted because it is in use.
_InvalidDBSubnetGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is not in the /available/ state.
_InvalidDBSubnetStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | The event subscription is in an invalid state.
_InvalidEventSubscriptionStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | The global cluster is in an invalid state and can\'t perform the
-- requested operation.
_InvalidGlobalClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | Cannot restore from vpc backup to non-vpc DB instance.
_InvalidRestoreFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is invalid, or multiple subnets were requested that
-- are not all in a common VPC.
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | DB subnet group does not cover all Availability Zones after it is
-- created because users\' change.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | Error accessing KMS key.
_KMSKeyNotAccessibleFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | The designated option group could not be found.
_OptionGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OptionGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "OptionGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Provisioned IOPS not available in the specified Availability Zone.
_ProvisionedIopsNotAvailableInAZFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ProvisionedIopsNotAvailableInAZFault =
  Core._MatchServiceError
    defaultService
    "ProvisionedIopsNotAvailableInAZFault"
    Prelude.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The SNS topic is invalid.
_SNSInvalidTopicFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | There is no SNS authorization.
_SNSNoAuthorizationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | The ARN of the SNS topic could not be found.
_SNSTopicArnNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | You have exceeded the maximum number of accounts that you can share a
-- manual DB snapshot with.
_SharedSnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SharedSnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SharedSnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Request would result in user exceeding the allowed number of DB
-- snapshots.
_SnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The source could not be found.
_SourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | Request would result in user exceeding the allowed amount of storage
-- available across all DB instances.
_StorageQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | /StorageType/ specified cannot be associated with the DB Instance.
_StorageTypeNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The DB subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | This subscription already exists.
_SubscriptionAlreadyExistFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The designated subscription category could not be found.
_SubscriptionCategoryNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The designated subscription could not be found.
_SubscriptionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404
