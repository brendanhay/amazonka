{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocumentDB.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DocumentDB.Types
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
    _SubscriptionCategoryNotFoundFault,
    _SubscriptionNotFoundFault,
    _InvalidSubnet,
    _SharedSnapshotQuotaExceededFault,
    _DBSubnetQuotaExceededFault,
    _GlobalClusterAlreadyExistsFault,
    _DBClusterNotFoundFault,
    _DBClusterAlreadyExistsFault,
    _StorageTypeNotSupportedFault,
    _SNSTopicArnNotFoundFault,
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
    _DBParameterGroupNotFoundFault,
    _InvalidDBSubnetStateFault,
    _DBClusterSnapshotNotFoundFault,
    _SNSInvalidTopicFault,
    _InsufficientDBInstanceCapacityFault,
    _InvalidDBClusterSnapshotStateFault,
    _SubscriptionAlreadyExistFault,
    _InvalidVPCNetworkStateFault,
    _AuthorizationNotFoundFault,
    _DBSubnetGroupQuotaExceededFault,
    _InvalidGlobalClusterStateFault,
    _EventSubscriptionQuotaExceededFault,
    _InsufficientStorageClusterCapacityFault,
    _InvalidDBClusterStateFault,
    _GlobalClusterNotFoundFault,
    _DBInstanceAlreadyExistsFault,
    _InvalidRestoreFault,
    _InvalidDBSecurityGroupStateFault,
    _ResourceNotFoundFault,
    _DBSubnetGroupNotFoundFault,
    _DBUpgradeDependencyFailureFault,
    _InvalidDBInstanceStateFault,
    _DBSnapshotAlreadyExistsFault,
    _DBInstanceNotFoundFault,
    _StorageQuotaExceededFault,
    _InvalidDBSnapshotStateFault,
    _InvalidDBSubnetGroupStateFault,
    _GlobalClusterQuotaExceededFault,
    _DBSubnetGroupDoesNotCoverEnoughAZs,
    _SubnetAlreadyInUse,

    -- * ApplyMethod
    ApplyMethod (..),

    -- * SourceType
    SourceType (..),

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_name,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateType,
    certificate_certificateArn,
    certificate_validTill,
    certificate_certificateIdentifier,
    certificate_thumbprint,
    certificate_validFrom,

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

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_promotionTier,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_dbClusterParameterGroupStatus,

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
    dbClusterRole_roleArn,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
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
    dbEngineVersion_engine,
    dbEngineVersion_dbParameterGroupFamily,
    dbEngineVersion_dbEngineDescription,
    dbEngineVersion_validUpgradeTarget,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_exportableLogTypes,

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
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

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_statusType,
    dbInstanceStatusInfo_message,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_vpcId,
    dbSubnetGroup_subnets,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_subnetGroupStatus,

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

    -- * GlobalCluster
    GlobalCluster (..),
    newGlobalCluster,
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

    -- * GlobalClusterMember
    GlobalClusterMember (..),
    newGlobalClusterMember,
    globalClusterMember_readers,
    globalClusterMember_dbClusterArn,
    globalClusterMember_isWriter,

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_licenseModel,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_vpc,

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

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,
    upgradeTarget_engine,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DocumentDB.Types.ApplyMethod
import Network.AWS.DocumentDB.Types.AvailabilityZone
import Network.AWS.DocumentDB.Types.Certificate
import Network.AWS.DocumentDB.Types.CloudwatchLogsExportConfiguration
import Network.AWS.DocumentDB.Types.DBCluster
import Network.AWS.DocumentDB.Types.DBClusterMember
import Network.AWS.DocumentDB.Types.DBClusterParameterGroup
import Network.AWS.DocumentDB.Types.DBClusterParameterGroupNameMessage
import Network.AWS.DocumentDB.Types.DBClusterRole
import Network.AWS.DocumentDB.Types.DBClusterSnapshot
import Network.AWS.DocumentDB.Types.DBClusterSnapshotAttribute
import Network.AWS.DocumentDB.Types.DBClusterSnapshotAttributesResult
import Network.AWS.DocumentDB.Types.DBEngineVersion
import Network.AWS.DocumentDB.Types.DBInstance
import Network.AWS.DocumentDB.Types.DBInstanceStatusInfo
import Network.AWS.DocumentDB.Types.DBSubnetGroup
import Network.AWS.DocumentDB.Types.Endpoint
import Network.AWS.DocumentDB.Types.EngineDefaults
import Network.AWS.DocumentDB.Types.Event
import Network.AWS.DocumentDB.Types.EventCategoriesMap
import Network.AWS.DocumentDB.Types.EventSubscription
import Network.AWS.DocumentDB.Types.Filter
import Network.AWS.DocumentDB.Types.GlobalCluster
import Network.AWS.DocumentDB.Types.GlobalClusterMember
import Network.AWS.DocumentDB.Types.OrderableDBInstanceOption
import Network.AWS.DocumentDB.Types.Parameter
import Network.AWS.DocumentDB.Types.PendingCloudwatchLogsExports
import Network.AWS.DocumentDB.Types.PendingMaintenanceAction
import Network.AWS.DocumentDB.Types.PendingModifiedValues
import Network.AWS.DocumentDB.Types.ResourcePendingMaintenanceActions
import Network.AWS.DocumentDB.Types.SourceType
import Network.AWS.DocumentDB.Types.Subnet
import Network.AWS.DocumentDB.Types.Tag
import Network.AWS.DocumentDB.Types.UpgradeTarget
import Network.AWS.DocumentDB.Types.VpcSecurityGroupMembership
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon DocumentDB with MongoDB compatibility SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DocumentDB",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "rds",
      Core._serviceSigningName = "rds",
      Core._serviceVersion = "2014-10-31",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseXMLError "DocumentDB",
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

-- | The parameter group is in use, or it is in a state that is not valid. If
-- you are trying to delete the parameter group, you can\'t delete it when
-- the parameter group is in this state.
_InvalidDBParameterGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The requested source could not be found.
_SourceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | You already have a cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | A parameter group with the same name already exists.
_DBParameterGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | This request would cause you to exceed the allowed number of parameter
-- groups.
_DBParameterGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | The provided category does not exist.
_SubscriptionCategoryNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404

-- | The requested subnet is not valid, or multiple subnets were requested
-- that are not all in a common virtual private cloud (VPC).
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

-- | The request would cause you to exceed the allowed number of subnets in a
-- subnet group.
_DBSubnetQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- cluster identifier (unique name) to create a new global cluster.
_GlobalClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterIdentifier@ doesn\'t refer to an existing cluster.
_DBClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You already have a cluster with the given identifier.
_DBClusterAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | Storage of the specified @StorageType@ can\'t be associated with the DB
-- instance.
_StorageTypeNotSupportedFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The SNS topic Amazon Resource Name (ARN) does not exist.
_SNSTopicArnNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSTopicArnNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SNSTopicArnNotFound"
    Prelude.. Core.hasStatus 404

-- | Someone else might be modifying a subscription. Wait a few seconds, and
-- try again.
_InvalidEventSubscriptionStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | An error occurred when accessing an KMS key.
_KMSKeyNotAccessibleFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing snapshot.
_DBSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The cluster can\'t be created because you have reached the maximum
-- allowed quota of clusters.
_DBClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | The request would cause you to exceed the allowed number of snapshots.
_SnapshotQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @DBSubnetGroupName@ is already being used by an existing subnet group.
_DBSubnetGroupAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the SNS topic Amazon Resource
-- Name (ARN).
_SNSNoAuthorizationFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing security group.
_DBSecurityGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would cause you to exceed the allowed number of instances.
_InstanceQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @DBParameterGroupName@ doesn\'t refer to an existing parameter group.
_DBParameterGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Amazon SNS has responded that there is a problem with the specified
-- topic.
_SNSInvalidTopicFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | The specified instance class isn\'t available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | The provided value isn\'t a valid cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | The provided subscription name already exists.
_SubscriptionAlreadyExistFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The subnet group doesn\'t cover all Availability Zones after it is
-- created because of changes that were made.
_InvalidVPCNetworkStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified CIDR IP or Amazon EC2 security group isn\'t authorized for
-- the specified security group.
--
-- Amazon DocumentDB also might not be authorized to perform necessary
-- actions on your behalf using IAM.
_AuthorizationNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would cause you to exceed the allowed number of subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidGlobalClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | There is not enough storage available for the current action. You might
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The cluster isn\'t in a valid state.
_InvalidDBClusterStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- cluster.
_GlobalClusterNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You already have a instance with the given identifier.
_DBInstanceAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | You cannot restore from a virtual private cloud (VPC) backup to a
-- non-VPC DB instance.
_InvalidRestoreFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The state of the security group doesn\'t allow deletion.
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

-- | @DBSubnetGroupName@ doesn\'t refer to an existing subnet group.
_DBSubnetGroupNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The upgrade failed because a resource that the depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | The specified instance isn\'t in the /available/ state.
_InvalidDBInstanceStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ is already being used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing instance.
_DBInstanceNotFoundFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would cause you to exceed the allowed amount of storage
-- available across all instances.
_StorageQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The state of the snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The subnet group can\'t be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The number of global clusters for this account is already at the maximum
-- allowed.
_GlobalClusterQuotaExceededFault :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | Subnets in the subnet group should cover at least two Availability Zones
-- unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | The subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400
