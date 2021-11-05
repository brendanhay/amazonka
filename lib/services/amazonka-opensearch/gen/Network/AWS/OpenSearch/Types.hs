{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpenSearch.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ResourceAlreadyExistsException,
    _ConflictException,
    _BaseException,
    _DisabledOperationException,
    _InternalException,
    _InvalidTypeException,
    _ResourceNotFoundException,
    _InvalidPaginationTokenException,
    _LimitExceededException,

    -- * AutoTuneDesiredState
    AutoTuneDesiredState (..),

    -- * AutoTuneState
    AutoTuneState (..),

    -- * AutoTuneType
    AutoTuneType (..),

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DescribePackagesFilterName
    DescribePackagesFilterName (..),

    -- * DomainPackageStatus
    DomainPackageStatus (..),

    -- * EngineType
    EngineType (..),

    -- * InboundConnectionStatusCode
    InboundConnectionStatusCode (..),

    -- * LogType
    LogType (..),

    -- * OpenSearchPartitionInstanceType
    OpenSearchPartitionInstanceType (..),

    -- * OpenSearchWarmPartitionInstanceType
    OpenSearchWarmPartitionInstanceType (..),

    -- * OptionState
    OptionState (..),

    -- * OutboundConnectionStatusCode
    OutboundConnectionStatusCode (..),

    -- * PackageStatus
    PackageStatus (..),

    -- * PackageType
    PackageType (..),

    -- * ReservedInstancePaymentOption
    ReservedInstancePaymentOption (..),

    -- * RollbackOnDisable
    RollbackOnDisable (..),

    -- * ScheduledAutoTuneActionType
    ScheduledAutoTuneActionType (..),

    -- * ScheduledAutoTuneSeverityType
    ScheduledAutoTuneSeverityType (..),

    -- * TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- * TimeUnit
    TimeUnit (..),

    -- * UpgradeStatus
    UpgradeStatus (..),

    -- * UpgradeStep
    UpgradeStep (..),

    -- * VolumeType
    VolumeType (..),

    -- * AWSDomainInformation
    AWSDomainInformation (..),
    newAWSDomainInformation,
    aWSDomainInformation_ownerId,
    aWSDomainInformation_region,
    aWSDomainInformation_domainName,

    -- * AccessPoliciesStatus
    AccessPoliciesStatus (..),
    newAccessPoliciesStatus,
    accessPoliciesStatus_options,
    accessPoliciesStatus_status,

    -- * AdditionalLimit
    AdditionalLimit (..),
    newAdditionalLimit,
    additionalLimit_limitName,
    additionalLimit_limitValues,

    -- * AdvancedOptionsStatus
    AdvancedOptionsStatus (..),
    newAdvancedOptionsStatus,
    advancedOptionsStatus_options,
    advancedOptionsStatus_status,

    -- * AdvancedSecurityOptions
    AdvancedSecurityOptions (..),
    newAdvancedSecurityOptions,
    advancedSecurityOptions_enabled,
    advancedSecurityOptions_internalUserDatabaseEnabled,
    advancedSecurityOptions_sAMLOptions,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (..),
    newAdvancedSecurityOptionsInput,
    advancedSecurityOptionsInput_enabled,
    advancedSecurityOptionsInput_internalUserDatabaseEnabled,
    advancedSecurityOptionsInput_masterUserOptions,
    advancedSecurityOptionsInput_sAMLOptions,

    -- * AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus (..),
    newAdvancedSecurityOptionsStatus,
    advancedSecurityOptionsStatus_options,
    advancedSecurityOptionsStatus_status,

    -- * AutoTune
    AutoTune (..),
    newAutoTune,
    autoTune_autoTuneDetails,
    autoTune_autoTuneType,

    -- * AutoTuneDetails
    AutoTuneDetails (..),
    newAutoTuneDetails,
    autoTuneDetails_scheduledAutoTuneDetails,

    -- * AutoTuneMaintenanceSchedule
    AutoTuneMaintenanceSchedule (..),
    newAutoTuneMaintenanceSchedule,
    autoTuneMaintenanceSchedule_startAt,
    autoTuneMaintenanceSchedule_cronExpressionForRecurrence,
    autoTuneMaintenanceSchedule_duration,

    -- * AutoTuneOptions
    AutoTuneOptions (..),
    newAutoTuneOptions,
    autoTuneOptions_desiredState,
    autoTuneOptions_rollbackOnDisable,
    autoTuneOptions_maintenanceSchedules,

    -- * AutoTuneOptionsInput
    AutoTuneOptionsInput (..),
    newAutoTuneOptionsInput,
    autoTuneOptionsInput_desiredState,
    autoTuneOptionsInput_maintenanceSchedules,

    -- * AutoTuneOptionsOutput
    AutoTuneOptionsOutput (..),
    newAutoTuneOptionsOutput,
    autoTuneOptionsOutput_state,
    autoTuneOptionsOutput_errorMessage,

    -- * AutoTuneOptionsStatus
    AutoTuneOptionsStatus (..),
    newAutoTuneOptionsStatus,
    autoTuneOptionsStatus_status,
    autoTuneOptionsStatus_options,

    -- * AutoTuneStatus
    AutoTuneStatus (..),
    newAutoTuneStatus,
    autoTuneStatus_pendingDeletion,
    autoTuneStatus_errorMessage,
    autoTuneStatus_updateVersion,
    autoTuneStatus_creationDate,
    autoTuneStatus_updateDate,
    autoTuneStatus_state,

    -- * ClusterConfig
    ClusterConfig (..),
    newClusterConfig,
    clusterConfig_dedicatedMasterCount,
    clusterConfig_dedicatedMasterType,
    clusterConfig_dedicatedMasterEnabled,
    clusterConfig_instanceCount,
    clusterConfig_coldStorageOptions,
    clusterConfig_zoneAwarenessEnabled,
    clusterConfig_instanceType,
    clusterConfig_warmEnabled,
    clusterConfig_zoneAwarenessConfig,
    clusterConfig_warmCount,
    clusterConfig_warmType,

    -- * ClusterConfigStatus
    ClusterConfigStatus (..),
    newClusterConfigStatus,
    clusterConfigStatus_options,
    clusterConfigStatus_status,

    -- * CognitoOptions
    CognitoOptions (..),
    newCognitoOptions,
    cognitoOptions_identityPoolId,
    cognitoOptions_enabled,
    cognitoOptions_userPoolId,
    cognitoOptions_roleArn,

    -- * CognitoOptionsStatus
    CognitoOptionsStatus (..),
    newCognitoOptionsStatus,
    cognitoOptionsStatus_options,
    cognitoOptionsStatus_status,

    -- * ColdStorageOptions
    ColdStorageOptions (..),
    newColdStorageOptions,
    coldStorageOptions_enabled,

    -- * CompatibleVersionsMap
    CompatibleVersionsMap (..),
    newCompatibleVersionsMap,
    compatibleVersionsMap_sourceVersion,
    compatibleVersionsMap_targetVersions,

    -- * DescribePackagesFilter
    DescribePackagesFilter (..),
    newDescribePackagesFilter,
    describePackagesFilter_value,
    describePackagesFilter_name,

    -- * DomainConfig
    DomainConfig (..),
    newDomainConfig,
    domainConfig_eBSOptions,
    domainConfig_engineVersion,
    domainConfig_nodeToNodeEncryptionOptions,
    domainConfig_accessPolicies,
    domainConfig_autoTuneOptions,
    domainConfig_logPublishingOptions,
    domainConfig_clusterConfig,
    domainConfig_advancedSecurityOptions,
    domainConfig_snapshotOptions,
    domainConfig_cognitoOptions,
    domainConfig_encryptionAtRestOptions,
    domainConfig_vPCOptions,
    domainConfig_domainEndpointOptions,
    domainConfig_advancedOptions,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    newDomainEndpointOptions,
    domainEndpointOptions_enforceHTTPS,
    domainEndpointOptions_tLSSecurityPolicy,
    domainEndpointOptions_customEndpointEnabled,
    domainEndpointOptions_customEndpoint,
    domainEndpointOptions_customEndpointCertificateArn,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    newDomainEndpointOptionsStatus,
    domainEndpointOptionsStatus_options,
    domainEndpointOptionsStatus_status,

    -- * DomainInfo
    DomainInfo (..),
    newDomainInfo,
    domainInfo_domainName,
    domainInfo_engineType,

    -- * DomainInformationContainer
    DomainInformationContainer (..),
    newDomainInformationContainer,
    domainInformationContainer_aWSDomainInformation,

    -- * DomainPackageDetails
    DomainPackageDetails (..),
    newDomainPackageDetails,
    domainPackageDetails_lastUpdated,
    domainPackageDetails_packageID,
    domainPackageDetails_packageType,
    domainPackageDetails_packageName,
    domainPackageDetails_packageVersion,
    domainPackageDetails_domainPackageStatus,
    domainPackageDetails_domainName,
    domainPackageDetails_errorDetails,
    domainPackageDetails_referencePath,

    -- * DomainStatus
    DomainStatus (..),
    newDomainStatus,
    domainStatus_eBSOptions,
    domainStatus_engineVersion,
    domainStatus_nodeToNodeEncryptionOptions,
    domainStatus_accessPolicies,
    domainStatus_serviceSoftwareOptions,
    domainStatus_autoTuneOptions,
    domainStatus_logPublishingOptions,
    domainStatus_advancedSecurityOptions,
    domainStatus_created,
    domainStatus_snapshotOptions,
    domainStatus_cognitoOptions,
    domainStatus_encryptionAtRestOptions,
    domainStatus_deleted,
    domainStatus_vPCOptions,
    domainStatus_endpoints,
    domainStatus_domainEndpointOptions,
    domainStatus_processing,
    domainStatus_endpoint,
    domainStatus_upgradeProcessing,
    domainStatus_advancedOptions,
    domainStatus_domainId,
    domainStatus_domainName,
    domainStatus_arn,
    domainStatus_clusterConfig,

    -- * Duration
    Duration (..),
    newDuration,
    duration_value,
    duration_unit,

    -- * EBSOptions
    EBSOptions (..),
    newEBSOptions,
    eBSOptions_volumeSize,
    eBSOptions_iops,
    eBSOptions_volumeType,
    eBSOptions_eBSEnabled,

    -- * EBSOptionsStatus
    EBSOptionsStatus (..),
    newEBSOptionsStatus,
    eBSOptionsStatus_options,
    eBSOptionsStatus_status,

    -- * EncryptionAtRestOptions
    EncryptionAtRestOptions (..),
    newEncryptionAtRestOptions,
    encryptionAtRestOptions_enabled,
    encryptionAtRestOptions_kmsKeyId,

    -- * EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus (..),
    newEncryptionAtRestOptionsStatus,
    encryptionAtRestOptionsStatus_options,
    encryptionAtRestOptionsStatus_status,

    -- * ErrorDetails
    ErrorDetails (..),
    newErrorDetails,
    errorDetails_errorType,
    errorDetails_errorMessage,

    -- * Filter
    Filter (..),
    newFilter,
    filter_values,
    filter_name,

    -- * InboundConnection
    InboundConnection (..),
    newInboundConnection,
    inboundConnection_remoteDomainInfo,
    inboundConnection_localDomainInfo,
    inboundConnection_connectionId,
    inboundConnection_connectionStatus,

    -- * InboundConnectionStatus
    InboundConnectionStatus (..),
    newInboundConnectionStatus,
    inboundConnectionStatus_message,
    inboundConnectionStatus_statusCode,

    -- * InstanceCountLimits
    InstanceCountLimits (..),
    newInstanceCountLimits,
    instanceCountLimits_maximumInstanceCount,
    instanceCountLimits_minimumInstanceCount,

    -- * InstanceLimits
    InstanceLimits (..),
    newInstanceLimits,
    instanceLimits_instanceCountLimits,

    -- * InstanceTypeDetails
    InstanceTypeDetails (..),
    newInstanceTypeDetails,
    instanceTypeDetails_encryptionEnabled,
    instanceTypeDetails_cognitoEnabled,
    instanceTypeDetails_instanceRole,
    instanceTypeDetails_instanceType,
    instanceTypeDetails_warmEnabled,
    instanceTypeDetails_advancedSecurityEnabled,
    instanceTypeDetails_appLogsEnabled,

    -- * Limits
    Limits (..),
    newLimits,
    limits_instanceLimits,
    limits_additionalLimits,
    limits_storageTypes,

    -- * LogPublishingOption
    LogPublishingOption (..),
    newLogPublishingOption,
    logPublishingOption_enabled,
    logPublishingOption_cloudWatchLogsLogGroupArn,

    -- * LogPublishingOptionsStatus
    LogPublishingOptionsStatus (..),
    newLogPublishingOptionsStatus,
    logPublishingOptionsStatus_status,
    logPublishingOptionsStatus_options,

    -- * MasterUserOptions
    MasterUserOptions (..),
    newMasterUserOptions,
    masterUserOptions_masterUserPassword,
    masterUserOptions_masterUserName,
    masterUserOptions_masterUserARN,

    -- * NodeToNodeEncryptionOptions
    NodeToNodeEncryptionOptions (..),
    newNodeToNodeEncryptionOptions,
    nodeToNodeEncryptionOptions_enabled,

    -- * NodeToNodeEncryptionOptionsStatus
    NodeToNodeEncryptionOptionsStatus (..),
    newNodeToNodeEncryptionOptionsStatus,
    nodeToNodeEncryptionOptionsStatus_options,
    nodeToNodeEncryptionOptionsStatus_status,

    -- * OptionStatus
    OptionStatus (..),
    newOptionStatus,
    optionStatus_pendingDeletion,
    optionStatus_updateVersion,
    optionStatus_creationDate,
    optionStatus_updateDate,
    optionStatus_state,

    -- * OutboundConnection
    OutboundConnection (..),
    newOutboundConnection,
    outboundConnection_remoteDomainInfo,
    outboundConnection_connectionAlias,
    outboundConnection_localDomainInfo,
    outboundConnection_connectionId,
    outboundConnection_connectionStatus,

    -- * OutboundConnectionStatus
    OutboundConnectionStatus (..),
    newOutboundConnectionStatus,
    outboundConnectionStatus_message,
    outboundConnectionStatus_statusCode,

    -- * PackageDetails
    PackageDetails (..),
    newPackageDetails,
    packageDetails_packageID,
    packageDetails_packageType,
    packageDetails_lastUpdatedAt,
    packageDetails_createdAt,
    packageDetails_packageName,
    packageDetails_packageStatus,
    packageDetails_packageDescription,
    packageDetails_errorDetails,
    packageDetails_availablePackageVersion,

    -- * PackageSource
    PackageSource (..),
    newPackageSource,
    packageSource_s3Key,
    packageSource_s3BucketName,

    -- * PackageVersionHistory
    PackageVersionHistory (..),
    newPackageVersionHistory,
    packageVersionHistory_createdAt,
    packageVersionHistory_packageVersion,
    packageVersionHistory_commitMessage,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_recurringChargeFrequency,
    recurringCharge_recurringChargeAmount,

    -- * ReservedInstance
    ReservedInstance (..),
    newReservedInstance,
    reservedInstance_state,
    reservedInstance_currencyCode,
    reservedInstance_instanceCount,
    reservedInstance_startTime,
    reservedInstance_instanceType,
    reservedInstance_reservationName,
    reservedInstance_billingSubscriptionId,
    reservedInstance_recurringCharges,
    reservedInstance_usagePrice,
    reservedInstance_reservedInstanceId,
    reservedInstance_reservedInstanceOfferingId,
    reservedInstance_fixedPrice,
    reservedInstance_duration,
    reservedInstance_paymentOption,

    -- * ReservedInstanceOffering
    ReservedInstanceOffering (..),
    newReservedInstanceOffering,
    reservedInstanceOffering_currencyCode,
    reservedInstanceOffering_instanceType,
    reservedInstanceOffering_recurringCharges,
    reservedInstanceOffering_usagePrice,
    reservedInstanceOffering_reservedInstanceOfferingId,
    reservedInstanceOffering_fixedPrice,
    reservedInstanceOffering_duration,
    reservedInstanceOffering_paymentOption,

    -- * SAMLIdp
    SAMLIdp (..),
    newSAMLIdp,
    sAMLIdp_metadataContent,
    sAMLIdp_entityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput (..),
    newSAMLOptionsInput,
    sAMLOptionsInput_masterUserName,
    sAMLOptionsInput_enabled,
    sAMLOptionsInput_idp,
    sAMLOptionsInput_rolesKey,
    sAMLOptionsInput_masterBackendRole,
    sAMLOptionsInput_sessionTimeoutMinutes,
    sAMLOptionsInput_subjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput (..),
    newSAMLOptionsOutput,
    sAMLOptionsOutput_enabled,
    sAMLOptionsOutput_idp,
    sAMLOptionsOutput_rolesKey,
    sAMLOptionsOutput_sessionTimeoutMinutes,
    sAMLOptionsOutput_subjectKey,

    -- * ScheduledAutoTuneDetails
    ScheduledAutoTuneDetails (..),
    newScheduledAutoTuneDetails,
    scheduledAutoTuneDetails_severity,
    scheduledAutoTuneDetails_action,
    scheduledAutoTuneDetails_date,
    scheduledAutoTuneDetails_actionType,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions (..),
    newServiceSoftwareOptions,
    serviceSoftwareOptions_automatedUpdateDate,
    serviceSoftwareOptions_currentVersion,
    serviceSoftwareOptions_optionalDeployment,
    serviceSoftwareOptions_updateStatus,
    serviceSoftwareOptions_cancellable,
    serviceSoftwareOptions_updateAvailable,
    serviceSoftwareOptions_description,
    serviceSoftwareOptions_newVersion,

    -- * SnapshotOptions
    SnapshotOptions (..),
    newSnapshotOptions,
    snapshotOptions_automatedSnapshotStartHour,

    -- * SnapshotOptionsStatus
    SnapshotOptionsStatus (..),
    newSnapshotOptionsStatus,
    snapshotOptionsStatus_options,
    snapshotOptionsStatus_status,

    -- * StorageType
    StorageType (..),
    newStorageType,
    storageType_storageTypeLimits,
    storageType_storageSubTypeName,
    storageType_storageTypeName,

    -- * StorageTypeLimit
    StorageTypeLimit (..),
    newStorageTypeLimit,
    storageTypeLimit_limitName,
    storageTypeLimit_limitValues,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * UpgradeHistory
    UpgradeHistory (..),
    newUpgradeHistory,
    upgradeHistory_upgradeStatus,
    upgradeHistory_stepsList,
    upgradeHistory_upgradeName,
    upgradeHistory_startTimestamp,

    -- * UpgradeStepItem
    UpgradeStepItem (..),
    newUpgradeStepItem,
    upgradeStepItem_upgradeStepStatus,
    upgradeStepItem_progressPercent,
    upgradeStepItem_issues,
    upgradeStepItem_upgradeStep,

    -- * VPCDerivedInfo
    VPCDerivedInfo (..),
    newVPCDerivedInfo,
    vPCDerivedInfo_securityGroupIds,
    vPCDerivedInfo_subnetIds,
    vPCDerivedInfo_vPCId,
    vPCDerivedInfo_availabilityZones,

    -- * VPCDerivedInfoStatus
    VPCDerivedInfoStatus (..),
    newVPCDerivedInfoStatus,
    vPCDerivedInfoStatus_options,
    vPCDerivedInfoStatus_status,

    -- * VPCOptions
    VPCOptions (..),
    newVPCOptions,
    vPCOptions_securityGroupIds,
    vPCOptions_subnetIds,

    -- * VersionStatus
    VersionStatus (..),
    newVersionStatus,
    versionStatus_options,
    versionStatus_status,

    -- * ZoneAwarenessConfig
    ZoneAwarenessConfig (..),
    newZoneAwarenessConfig,
    zoneAwarenessConfig_availabilityZoneCount,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpenSearch.Types.AWSDomainInformation
import Network.AWS.OpenSearch.Types.AccessPoliciesStatus
import Network.AWS.OpenSearch.Types.AdditionalLimit
import Network.AWS.OpenSearch.Types.AdvancedOptionsStatus
import Network.AWS.OpenSearch.Types.AdvancedSecurityOptions
import Network.AWS.OpenSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.OpenSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.OpenSearch.Types.AutoTune
import Network.AWS.OpenSearch.Types.AutoTuneDesiredState
import Network.AWS.OpenSearch.Types.AutoTuneDetails
import Network.AWS.OpenSearch.Types.AutoTuneMaintenanceSchedule
import Network.AWS.OpenSearch.Types.AutoTuneOptions
import Network.AWS.OpenSearch.Types.AutoTuneOptionsInput
import Network.AWS.OpenSearch.Types.AutoTuneOptionsOutput
import Network.AWS.OpenSearch.Types.AutoTuneOptionsStatus
import Network.AWS.OpenSearch.Types.AutoTuneState
import Network.AWS.OpenSearch.Types.AutoTuneStatus
import Network.AWS.OpenSearch.Types.AutoTuneType
import Network.AWS.OpenSearch.Types.ClusterConfig
import Network.AWS.OpenSearch.Types.ClusterConfigStatus
import Network.AWS.OpenSearch.Types.CognitoOptions
import Network.AWS.OpenSearch.Types.CognitoOptionsStatus
import Network.AWS.OpenSearch.Types.ColdStorageOptions
import Network.AWS.OpenSearch.Types.CompatibleVersionsMap
import Network.AWS.OpenSearch.Types.DeploymentStatus
import Network.AWS.OpenSearch.Types.DescribePackagesFilter
import Network.AWS.OpenSearch.Types.DescribePackagesFilterName
import Network.AWS.OpenSearch.Types.DomainConfig
import Network.AWS.OpenSearch.Types.DomainEndpointOptions
import Network.AWS.OpenSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.OpenSearch.Types.DomainInfo
import Network.AWS.OpenSearch.Types.DomainInformationContainer
import Network.AWS.OpenSearch.Types.DomainPackageDetails
import Network.AWS.OpenSearch.Types.DomainPackageStatus
import Network.AWS.OpenSearch.Types.DomainStatus
import Network.AWS.OpenSearch.Types.Duration
import Network.AWS.OpenSearch.Types.EBSOptions
import Network.AWS.OpenSearch.Types.EBSOptionsStatus
import Network.AWS.OpenSearch.Types.EncryptionAtRestOptions
import Network.AWS.OpenSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.OpenSearch.Types.EngineType
import Network.AWS.OpenSearch.Types.ErrorDetails
import Network.AWS.OpenSearch.Types.Filter
import Network.AWS.OpenSearch.Types.InboundConnection
import Network.AWS.OpenSearch.Types.InboundConnectionStatus
import Network.AWS.OpenSearch.Types.InboundConnectionStatusCode
import Network.AWS.OpenSearch.Types.InstanceCountLimits
import Network.AWS.OpenSearch.Types.InstanceLimits
import Network.AWS.OpenSearch.Types.InstanceTypeDetails
import Network.AWS.OpenSearch.Types.Limits
import Network.AWS.OpenSearch.Types.LogPublishingOption
import Network.AWS.OpenSearch.Types.LogPublishingOptionsStatus
import Network.AWS.OpenSearch.Types.LogType
import Network.AWS.OpenSearch.Types.MasterUserOptions
import Network.AWS.OpenSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.OpenSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.OpenSearch.Types.OpenSearchPartitionInstanceType
import Network.AWS.OpenSearch.Types.OpenSearchWarmPartitionInstanceType
import Network.AWS.OpenSearch.Types.OptionState
import Network.AWS.OpenSearch.Types.OptionStatus
import Network.AWS.OpenSearch.Types.OutboundConnection
import Network.AWS.OpenSearch.Types.OutboundConnectionStatus
import Network.AWS.OpenSearch.Types.OutboundConnectionStatusCode
import Network.AWS.OpenSearch.Types.PackageDetails
import Network.AWS.OpenSearch.Types.PackageSource
import Network.AWS.OpenSearch.Types.PackageStatus
import Network.AWS.OpenSearch.Types.PackageType
import Network.AWS.OpenSearch.Types.PackageVersionHistory
import Network.AWS.OpenSearch.Types.RecurringCharge
import Network.AWS.OpenSearch.Types.ReservedInstance
import Network.AWS.OpenSearch.Types.ReservedInstanceOffering
import Network.AWS.OpenSearch.Types.ReservedInstancePaymentOption
import Network.AWS.OpenSearch.Types.RollbackOnDisable
import Network.AWS.OpenSearch.Types.SAMLIdp
import Network.AWS.OpenSearch.Types.SAMLOptionsInput
import Network.AWS.OpenSearch.Types.SAMLOptionsOutput
import Network.AWS.OpenSearch.Types.ScheduledAutoTuneActionType
import Network.AWS.OpenSearch.Types.ScheduledAutoTuneDetails
import Network.AWS.OpenSearch.Types.ScheduledAutoTuneSeverityType
import Network.AWS.OpenSearch.Types.ServiceSoftwareOptions
import Network.AWS.OpenSearch.Types.SnapshotOptions
import Network.AWS.OpenSearch.Types.SnapshotOptionsStatus
import Network.AWS.OpenSearch.Types.StorageType
import Network.AWS.OpenSearch.Types.StorageTypeLimit
import Network.AWS.OpenSearch.Types.TLSSecurityPolicy
import Network.AWS.OpenSearch.Types.Tag
import Network.AWS.OpenSearch.Types.TimeUnit
import Network.AWS.OpenSearch.Types.UpgradeHistory
import Network.AWS.OpenSearch.Types.UpgradeStatus
import Network.AWS.OpenSearch.Types.UpgradeStep
import Network.AWS.OpenSearch.Types.UpgradeStepItem
import Network.AWS.OpenSearch.Types.VPCDerivedInfo
import Network.AWS.OpenSearch.Types.VPCDerivedInfoStatus
import Network.AWS.OpenSearch.Types.VPCOptions
import Network.AWS.OpenSearch.Types.VersionStatus
import Network.AWS.OpenSearch.Types.VolumeType
import Network.AWS.OpenSearch.Types.ZoneAwarenessConfig
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2021-01-01@ of the Amazon OpenSearch Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "OpenSearch",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "es",
      Core._serviceSigningName = "es",
      Core._serviceVersion = "2021-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "OpenSearch",
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

-- | An exception for missing \/ invalid input fields. Gives http status code
-- of 400.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | An error occurred because user does not have permissions to access the
-- resource. Returns HTTP status code 403.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An exception for creating a resource that already exists. Gives http
-- status code of 400.
_ResourceAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistsException"
    Prelude.. Core.hasStatus 409

-- | An error occurred because the client attempts to remove a resource that
-- is currently in use. Returns HTTP status code 409.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | An error occurred while processing the request.
_BaseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BaseException =
  Core._MatchServiceError
    defaultService
    "BaseException"

-- | An error occured because the client wanted to access a not supported
-- operation. Gives http status code of 409.
_DisabledOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DisabledOperationException =
  Core._MatchServiceError
    defaultService
    "DisabledOperationException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of an unknown error, exception
-- or failure (the failure is internal to the service) . Gives http status
-- code of 500.
_InternalException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalException =
  Core._MatchServiceError
    defaultService
    "InternalException"
    Prelude.. Core.hasStatus 500

-- | An exception for trying to create or access sub-resource that is either
-- invalid or not supported. Gives http status code of 409.
_InvalidTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidTypeException"
    Prelude.. Core.hasStatus 409

-- | An exception for accessing or deleting a resource that does not exist.
-- Gives http status code of 400.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 409

-- | The request processing has failed because of invalid pagination token
-- provided by customer. Returns an HTTP status code of 400.
_InvalidPaginationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPaginationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidPaginationTokenException"
    Prelude.. Core.hasStatus 400

-- | An exception for trying to create more than allowed resources or
-- sub-resources. Gives http status code of 409.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 409
