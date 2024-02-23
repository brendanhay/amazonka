{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DocumentDB.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DocumentDB.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AuthorizationNotFoundFault,
    _CertificateNotFoundFault,
    _DBClusterAlreadyExistsFault,
    _DBClusterNotFoundFault,
    _DBClusterParameterGroupNotFoundFault,
    _DBClusterQuotaExceededFault,
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
    _EventSubscriptionQuotaExceededFault,
    _GlobalClusterAlreadyExistsFault,
    _GlobalClusterNotFoundFault,
    _GlobalClusterQuotaExceededFault,
    _InstanceQuotaExceededFault,
    _InsufficientDBClusterCapacityFault,
    _InsufficientDBInstanceCapacityFault,
    _InsufficientStorageClusterCapacityFault,
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

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_certificateArn,
    certificate_certificateIdentifier,
    certificate_certificateType,
    certificate_thumbprint,
    certificate_validFrom,
    certificate_validTill,

    -- * CloudwatchLogsExportConfiguration
    CloudwatchLogsExportConfiguration (..),
    newCloudwatchLogsExportConfiguration,
    cloudwatchLogsExportConfiguration_disableLogTypes,
    cloudwatchLogsExportConfiguration_enableLogTypes,

    -- * DBCluster
    DBCluster (..),
    newDBCluster,
    dbCluster_associatedRoles,
    dbCluster_availabilityZones,
    dbCluster_backupRetentionPeriod,
    dbCluster_cloneGroupId,
    dbCluster_clusterCreateTime,
    dbCluster_dbClusterArn,
    dbCluster_dbClusterIdentifier,
    dbCluster_dbClusterMembers,
    dbCluster_dbClusterParameterGroup,
    dbCluster_dbSubnetGroup,
    dbCluster_dbClusterResourceId,
    dbCluster_deletionProtection,
    dbCluster_earliestRestorableTime,
    dbCluster_enabledCloudwatchLogsExports,
    dbCluster_endpoint,
    dbCluster_engine,
    dbCluster_engineVersion,
    dbCluster_hostedZoneId,
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
    dbCluster_status,
    dbCluster_storageEncrypted,
    dbCluster_vpcSecurityGroups,

    -- * DBClusterMember
    DBClusterMember (..),
    newDBClusterMember,
    dbClusterMember_dbClusterParameterGroupStatus,
    dbClusterMember_dbInstanceIdentifier,
    dbClusterMember_isClusterWriter,
    dbClusterMember_promotionTier,

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
    dbClusterRole_roleArn,
    dbClusterRole_status,

    -- * DBClusterSnapshot
    DBClusterSnapshot (..),
    newDBClusterSnapshot,
    dbClusterSnapshot_availabilityZones,
    dbClusterSnapshot_clusterCreateTime,
    dbClusterSnapshot_dbClusterIdentifier,
    dbClusterSnapshot_dbClusterSnapshotArn,
    dbClusterSnapshot_dbClusterSnapshotIdentifier,
    dbClusterSnapshot_engine,
    dbClusterSnapshot_engineVersion,
    dbClusterSnapshot_kmsKeyId,
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
    dbEngineVersion_engine,
    dbEngineVersion_engineVersion,
    dbEngineVersion_exportableLogTypes,
    dbEngineVersion_supportsLogExportsToCloudwatchLogs,
    dbEngineVersion_validUpgradeTarget,

    -- * DBInstance
    DBInstance (..),
    newDBInstance,
    dbInstance_autoMinorVersionUpgrade,
    dbInstance_availabilityZone,
    dbInstance_backupRetentionPeriod,
    dbInstance_cACertificateIdentifier,
    dbInstance_copyTagsToSnapshot,
    dbInstance_dbClusterIdentifier,
    dbInstance_dbInstanceArn,
    dbInstance_dbInstanceClass,
    dbInstance_dbInstanceIdentifier,
    dbInstance_dbInstanceStatus,
    dbInstance_dbSubnetGroup,
    dbInstance_dbiResourceId,
    dbInstance_enabledCloudwatchLogsExports,
    dbInstance_endpoint,
    dbInstance_engine,
    dbInstance_engineVersion,
    dbInstance_instanceCreateTime,
    dbInstance_kmsKeyId,
    dbInstance_latestRestorableTime,
    dbInstance_pendingModifiedValues,
    dbInstance_preferredBackupWindow,
    dbInstance_preferredMaintenanceWindow,
    dbInstance_promotionTier,
    dbInstance_publiclyAccessible,
    dbInstance_statusInfos,
    dbInstance_storageEncrypted,
    dbInstance_vpcSecurityGroups,

    -- * DBInstanceStatusInfo
    DBInstanceStatusInfo (..),
    newDBInstanceStatusInfo,
    dbInstanceStatusInfo_message,
    dbInstanceStatusInfo_normal,
    dbInstanceStatusInfo_status,
    dbInstanceStatusInfo_statusType,

    -- * DBSubnetGroup
    DBSubnetGroup (..),
    newDBSubnetGroup,
    dbSubnetGroup_dbSubnetGroupArn,
    dbSubnetGroup_dbSubnetGroupDescription,
    dbSubnetGroup_dbSubnetGroupName,
    dbSubnetGroup_subnetGroupStatus,
    dbSubnetGroup_subnets,
    dbSubnetGroup_vpcId,

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
    globalCluster_databaseName,
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

    -- * OrderableDBInstanceOption
    OrderableDBInstanceOption (..),
    newOrderableDBInstanceOption,
    orderableDBInstanceOption_availabilityZones,
    orderableDBInstanceOption_dbInstanceClass,
    orderableDBInstanceOption_engine,
    orderableDBInstanceOption_engineVersion,
    orderableDBInstanceOption_licenseModel,
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

    -- * ResourcePendingMaintenanceActions
    ResourcePendingMaintenanceActions (..),
    newResourcePendingMaintenanceActions,
    resourcePendingMaintenanceActions_pendingMaintenanceActionDetails,
    resourcePendingMaintenanceActions_resourceIdentifier,

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

    -- * UpgradeTarget
    UpgradeTarget (..),
    newUpgradeTarget,
    upgradeTarget_autoUpgrade,
    upgradeTarget_description,
    upgradeTarget_engine,
    upgradeTarget_engineVersion,
    upgradeTarget_isMajorVersionUpgrade,

    -- * VpcSecurityGroupMembership
    VpcSecurityGroupMembership (..),
    newVpcSecurityGroupMembership,
    vpcSecurityGroupMembership_status,
    vpcSecurityGroupMembership_vpcSecurityGroupId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DocumentDB.Types.ApplyMethod
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
import Amazonka.DocumentDB.Types.SourceType
import Amazonka.DocumentDB.Types.Subnet
import Amazonka.DocumentDB.Types.Tag
import Amazonka.DocumentDB.Types.UpgradeTarget
import Amazonka.DocumentDB.Types.VpcSecurityGroupMembership
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2014-10-31@ of the Amazon DocumentDB with MongoDB compatibility SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DocumentDB",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "rds",
      Core.signingName = "rds",
      Core.version = "2014-10-31",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseXMLError "DocumentDB",
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

-- | The specified CIDR IP or Amazon EC2 security group isn\'t authorized for
-- the specified security group.
--
-- Amazon DocumentDB also might not be authorized to perform necessary
-- actions on your behalf using IAM.
_AuthorizationNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AuthorizationNotFoundFault =
  Core._MatchServiceError
    defaultService
    "AuthorizationNotFound"
    Prelude.. Core.hasStatus 404

-- | @CertificateIdentifier@ doesn\'t refer to an existing certificate.
_CertificateNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_CertificateNotFoundFault =
  Core._MatchServiceError
    defaultService
    "CertificateNotFound"
    Prelude.. Core.hasStatus 404

-- | You already have a cluster with the given identifier.
_DBClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterIdentifier@ doesn\'t refer to an existing cluster.
_DBClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | @DBClusterParameterGroupName@ doesn\'t refer to an existing cluster
-- parameter group.
_DBClusterParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | The cluster can\'t be created because you have reached the maximum
-- allowed quota of clusters.
_DBClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 403

-- | You already have a cluster snapshot with the given identifier.
_DBClusterSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | @DBClusterSnapshotIdentifier@ doesn\'t refer to an existing cluster
-- snapshot.
_DBClusterSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBClusterSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBClusterSnapshotNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | You already have a instance with the given identifier.
_DBInstanceAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBInstanceIdentifier@ doesn\'t refer to an existing instance.
_DBInstanceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBInstanceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBInstanceNotFound"
    Prelude.. Core.hasStatus 404

-- | A parameter group with the same name already exists.
_DBParameterGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBParameterGroupName@ doesn\'t refer to an existing parameter group.
_DBParameterGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | This request would cause you to exceed the allowed number of parameter
-- groups.
_DBParameterGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBParameterGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBParameterGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | @DBSecurityGroupName@ doesn\'t refer to an existing security group.
_DBSecurityGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSecurityGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSecurityGroupNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBSnapshotIdentifier@ is already being used by an existing snapshot.
_DBSnapshotAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | @DBSnapshotIdentifier@ doesn\'t refer to an existing snapshot.
_DBSnapshotNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSnapshotNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSnapshotNotFound"
    Prelude.. Core.hasStatus 404

-- | @DBSubnetGroupName@ is already being used by an existing subnet group.
_DBSubnetGroupAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupAlreadyExists"
    Prelude.. Core.hasStatus 400

-- | Subnets in the subnet group should cover at least two Availability Zones
-- unless there is only one Availability Zone.
_DBSubnetGroupDoesNotCoverEnoughAZs :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupDoesNotCoverEnoughAZs =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupDoesNotCoverEnoughAZs"
    Prelude.. Core.hasStatus 400

-- | @DBSubnetGroupName@ doesn\'t refer to an existing subnet group.
_DBSubnetGroupNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupNotFoundFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The request would cause you to exceed the allowed number of subnet
-- groups.
_DBSubnetGroupQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetGroupQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetGroupQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The request would cause you to exceed the allowed number of subnets in a
-- subnet group.
_DBSubnetQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBSubnetQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "DBSubnetQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The upgrade failed because a resource that the depends on can\'t be
-- modified.
_DBUpgradeDependencyFailureFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_DBUpgradeDependencyFailureFault =
  Core._MatchServiceError
    defaultService
    "DBUpgradeDependencyFailure"
    Prelude.. Core.hasStatus 400

-- | You have reached the maximum number of event subscriptions.
_EventSubscriptionQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_EventSubscriptionQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "EventSubscriptionQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ already exists. Choose a new global
-- cluster identifier (unique name) to create a new global cluster.
_GlobalClusterAlreadyExistsFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterAlreadyExistsFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterAlreadyExistsFault"
    Prelude.. Core.hasStatus 400

-- | The @GlobalClusterIdentifier@ doesn\'t refer to an existing global
-- cluster.
_GlobalClusterNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterNotFoundFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | The number of global clusters for this account is already at the maximum
-- allowed.
_GlobalClusterQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_GlobalClusterQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "GlobalClusterQuotaExceededFault"
    Prelude.. Core.hasStatus 400

-- | The request would cause you to exceed the allowed number of instances.
_InstanceQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InstanceQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "InstanceQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The cluster doesn\'t have enough capacity for the current operation.
_InsufficientDBClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBClusterCapacityFault"
    Prelude.. Core.hasStatus 403

-- | The specified instance class isn\'t available in the specified
-- Availability Zone.
_InsufficientDBInstanceCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientDBInstanceCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientDBInstanceCapacity"
    Prelude.. Core.hasStatus 400

-- | There is not enough storage available for the current action. You might
-- be able to resolve this error by updating your subnet group to use
-- different Availability Zones that have more storage available.
_InsufficientStorageClusterCapacityFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InsufficientStorageClusterCapacityFault =
  Core._MatchServiceError
    defaultService
    "InsufficientStorageClusterCapacity"
    Prelude.. Core.hasStatus 400

-- | The provided value isn\'t a valid cluster snapshot state.
_InvalidDBClusterSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterSnapshotStateFault"
    Prelude.. Core.hasStatus 400

-- | The cluster isn\'t in a valid state.
_InvalidDBClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | The specified instance isn\'t in the /available/ state.
_InvalidDBInstanceStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBInstanceStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBInstanceState"
    Prelude.. Core.hasStatus 400

-- | The parameter group is in use, or it is in a state that is not valid. If
-- you are trying to delete the parameter group, you can\'t delete it when
-- the parameter group is in this state.
_InvalidDBParameterGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBParameterGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBParameterGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the security group doesn\'t allow deletion.
_InvalidDBSecurityGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSecurityGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSecurityGroupState"
    Prelude.. Core.hasStatus 400

-- | The state of the snapshot doesn\'t allow deletion.
_InvalidDBSnapshotStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSnapshotStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSnapshotState"
    Prelude.. Core.hasStatus 400

-- | The subnet group can\'t be deleted because it\'s in use.
_InvalidDBSubnetGroupStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetGroupStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetGroupStateFault"
    Prelude.. Core.hasStatus 400

-- | The subnet isn\'t in the /available/ state.
_InvalidDBSubnetStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidDBSubnetStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidDBSubnetStateFault"
    Prelude.. Core.hasStatus 400

-- | Someone else might be modifying a subscription. Wait a few seconds, and
-- try again.
_InvalidEventSubscriptionStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidEventSubscriptionStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidEventSubscriptionState"
    Prelude.. Core.hasStatus 400

-- | The requested operation can\'t be performed while the cluster is in this
-- state.
_InvalidGlobalClusterStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidGlobalClusterStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidGlobalClusterStateFault"
    Prelude.. Core.hasStatus 400

-- | You cannot restore from a virtual private cloud (VPC) backup to a
-- non-VPC DB instance.
_InvalidRestoreFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRestoreFault =
  Core._MatchServiceError
    defaultService
    "InvalidRestoreFault"
    Prelude.. Core.hasStatus 400

-- | The requested subnet is not valid, or multiple subnets were requested
-- that are not all in a common virtual private cloud (VPC).
_InvalidSubnet :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidSubnet =
  Core._MatchServiceError
    defaultService
    "InvalidSubnet"
    Prelude.. Core.hasStatus 400

-- | The subnet group doesn\'t cover all Availability Zones after it is
-- created because of changes that were made.
_InvalidVPCNetworkStateFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidVPCNetworkStateFault =
  Core._MatchServiceError
    defaultService
    "InvalidVPCNetworkStateFault"
    Prelude.. Core.hasStatus 400

-- | An error occurred when accessing an KMS key.
_KMSKeyNotAccessibleFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_KMSKeyNotAccessibleFault =
  Core._MatchServiceError
    defaultService
    "KMSKeyNotAccessibleFault"
    Prelude.. Core.hasStatus 400

-- | The specified resource ID was not found.
_ResourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundFault"
    Prelude.. Core.hasStatus 404

-- | Amazon SNS has responded that there is a problem with the specified
-- topic.
_SNSInvalidTopicFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSInvalidTopicFault =
  Core._MatchServiceError
    defaultService
    "SNSInvalidTopic"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to publish to the SNS topic Amazon Resource
-- Name (ARN).
_SNSNoAuthorizationFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SNSNoAuthorizationFault =
  Core._MatchServiceError
    defaultService
    "SNSNoAuthorization"
    Prelude.. Core.hasStatus 400

-- | The SNS topic Amazon Resource Name (ARN) does not exist.
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

-- | The request would cause you to exceed the allowed number of snapshots.
_SnapshotQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SnapshotQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "SnapshotQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | The requested source could not be found.
_SourceNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SourceNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SourceNotFound"
    Prelude.. Core.hasStatus 404

-- | The request would cause you to exceed the allowed amount of storage
-- available across all instances.
_StorageQuotaExceededFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageQuotaExceededFault =
  Core._MatchServiceError
    defaultService
    "StorageQuotaExceeded"
    Prelude.. Core.hasStatus 400

-- | Storage of the specified @StorageType@ can\'t be associated with the DB
-- instance.
_StorageTypeNotSupportedFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_StorageTypeNotSupportedFault =
  Core._MatchServiceError
    defaultService
    "StorageTypeNotSupported"
    Prelude.. Core.hasStatus 400

-- | The subnet is already in use in the Availability Zone.
_SubnetAlreadyInUse :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubnetAlreadyInUse =
  Core._MatchServiceError
    defaultService
    "SubnetAlreadyInUse"
    Prelude.. Core.hasStatus 400

-- | The provided subscription name already exists.
_SubscriptionAlreadyExistFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionAlreadyExistFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionAlreadyExist"
    Prelude.. Core.hasStatus 400

-- | The provided category does not exist.
_SubscriptionCategoryNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionCategoryNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionCategoryNotFound"
    Prelude.. Core.hasStatus 404

-- | The subscription name does not exist.
_SubscriptionNotFoundFault :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_SubscriptionNotFoundFault =
  Core._MatchServiceError
    defaultService
    "SubscriptionNotFound"
    Prelude.. Core.hasStatus 404
