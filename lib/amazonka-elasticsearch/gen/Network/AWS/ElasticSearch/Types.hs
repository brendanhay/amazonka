{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types
  ( -- * Service Configuration
    elasticSearch,

    -- * Errors

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DescribePackagesFilterName
    DescribePackagesFilterName (..),

    -- * DomainPackageStatus
    DomainPackageStatus (..),

    -- * ESPartitionInstanceType
    ESPartitionInstanceType (..),

    -- * ESWarmPartitionInstanceType
    ESWarmPartitionInstanceType (..),

    -- * InboundCrossClusterSearchConnectionStatusCode
    InboundCrossClusterSearchConnectionStatusCode (..),

    -- * LogType
    LogType (..),

    -- * OptionState
    OptionState (..),

    -- * OutboundCrossClusterSearchConnectionStatusCode
    OutboundCrossClusterSearchConnectionStatusCode (..),

    -- * PackageStatus
    PackageStatus (..),

    -- * PackageType
    PackageType (..),

    -- * ReservedElasticsearchInstancePaymentOption
    ReservedElasticsearchInstancePaymentOption (..),

    -- * TLSSecurityPolicy
    TLSSecurityPolicy (..),

    -- * UpgradeStatus
    UpgradeStatus (..),

    -- * UpgradeStep
    UpgradeStep (..),

    -- * VolumeType
    VolumeType (..),

    -- * AccessPoliciesStatus
    AccessPoliciesStatus,
    accessPoliciesStatus,
    apsOptions,
    apsStatus,

    -- * AdditionalLimit
    AdditionalLimit,
    additionalLimit,
    alLimitName,
    alLimitValues,

    -- * AdvancedOptionsStatus
    AdvancedOptionsStatus,
    advancedOptionsStatus,
    aosOptions,
    aosStatus,

    -- * AdvancedSecurityOptions
    AdvancedSecurityOptions,
    advancedSecurityOptions,
    asoEnabled,
    asoInternalUserDatabaseEnabled,
    asoSAMLOptions,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput,
    advancedSecurityOptionsInput,
    asoiEnabled,
    asoiInternalUserDatabaseEnabled,
    asoiMasterUserOptions,
    asoiSAMLOptions,

    -- * AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus,
    advancedSecurityOptionsStatus,
    asosOptions,
    asosStatus,

    -- * CognitoOptions
    CognitoOptions,
    cognitoOptions,
    coIdentityPoolId,
    coEnabled,
    coUserPoolId,
    coRoleARN,

    -- * CognitoOptionsStatus
    CognitoOptionsStatus,
    cognitoOptionsStatus,
    cosOptions,
    cosStatus,

    -- * CompatibleVersionsMap
    CompatibleVersionsMap,
    compatibleVersionsMap,
    cvmSourceVersion,
    cvmTargetVersions,

    -- * DescribePackagesFilter
    DescribePackagesFilter,
    describePackagesFilter,
    dpfValue,
    dpfName,

    -- * DomainEndpointOptions
    DomainEndpointOptions,
    domainEndpointOptions,
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,
    deoCustomEndpointEnabled,
    deoCustomEndpoint,
    deoCustomEndpointCertificateARN,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus,
    domainEndpointOptionsStatus,
    deosOptions,
    deosStatus,

    -- * DomainInfo
    DomainInfo,
    domainInfo,
    dDomainName,

    -- * DomainInformation
    DomainInformation,
    domainInformation,
    diOwnerId,
    diRegion,
    diDomainName,

    -- * DomainPackageDetails
    DomainPackageDetails,
    domainPackageDetails,
    dpdLastUpdated,
    dpdPackageId,
    dpdPackageType,
    dpdPackageName,
    dpdPackageVersion,
    dpdDomainPackageStatus,
    dpdDomainName,
    dpdErrorDetails,
    dpdReferencePath,

    -- * EBSOptions
    EBSOptions,
    ebsOptions,
    eoVolumeSize,
    eoIOPS,
    eoVolumeType,
    eoEBSEnabled,

    -- * EBSOptionsStatus
    EBSOptionsStatus,
    ebsOptionsStatus,
    eosOptions,
    eosStatus,

    -- * ElasticsearchClusterConfig
    ElasticsearchClusterConfig,
    elasticsearchClusterConfig,
    eccDedicatedMasterCount,
    eccDedicatedMasterType,
    eccDedicatedMasterEnabled,
    eccInstanceCount,
    eccZoneAwarenessEnabled,
    eccInstanceType,
    eccWarmEnabled,
    eccZoneAwarenessConfig,
    eccWarmCount,
    eccWarmType,

    -- * ElasticsearchClusterConfigStatus
    ElasticsearchClusterConfigStatus,
    elasticsearchClusterConfigStatus,
    eccsOptions,
    eccsStatus,

    -- * ElasticsearchDomainConfig
    ElasticsearchDomainConfig,
    elasticsearchDomainConfig,
    edcEBSOptions,
    edcNodeToNodeEncryptionOptions,
    edcAccessPolicies,
    edcLogPublishingOptions,
    edcAdvancedSecurityOptions,
    edcElasticsearchClusterConfig,
    edcSnapshotOptions,
    edcCognitoOptions,
    edcEncryptionAtRestOptions,
    edcVPCOptions,
    edcDomainEndpointOptions,
    edcAdvancedOptions,
    edcElasticsearchVersion,

    -- * ElasticsearchDomainStatus
    ElasticsearchDomainStatus,
    elasticsearchDomainStatus,
    edsEBSOptions,
    edsNodeToNodeEncryptionOptions,
    edsAccessPolicies,
    edsServiceSoftwareOptions,
    edsLogPublishingOptions,
    edsAdvancedSecurityOptions,
    edsCreated,
    edsSnapshotOptions,
    edsCognitoOptions,
    edsEncryptionAtRestOptions,
    edsDeleted,
    edsVPCOptions,
    edsEndpoints,
    edsDomainEndpointOptions,
    edsProcessing,
    edsEndpoint,
    edsUpgradeProcessing,
    edsAdvancedOptions,
    edsElasticsearchVersion,
    edsDomainId,
    edsDomainName,
    edsARN,
    edsElasticsearchClusterConfig,

    -- * ElasticsearchVersionStatus
    ElasticsearchVersionStatus,
    elasticsearchVersionStatus,
    evsOptions,
    evsStatus,

    -- * EncryptionAtRestOptions
    EncryptionAtRestOptions,
    encryptionAtRestOptions,
    earoEnabled,
    earoKMSKeyId,

    -- * EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus,
    encryptionAtRestOptionsStatus,
    earosOptions,
    earosStatus,

    -- * ErrorDetails
    ErrorDetails,
    errorDetails,
    edErrorType,
    edErrorMessage,

    -- * Filter
    Filter,
    filter',
    fValues,
    fName,

    -- * InboundCrossClusterSearchConnection
    InboundCrossClusterSearchConnection,
    inboundCrossClusterSearchConnection,
    iccscDestinationDomainInfo,
    iccscCrossClusterSearchConnectionId,
    iccscConnectionStatus,
    iccscSourceDomainInfo,

    -- * InboundCrossClusterSearchConnectionStatus
    InboundCrossClusterSearchConnectionStatus,
    inboundCrossClusterSearchConnectionStatus,
    iccscsMessage,
    iccscsStatusCode,

    -- * InstanceCountLimits
    InstanceCountLimits,
    instanceCountLimits,
    iclMaximumInstanceCount,
    iclMinimumInstanceCount,

    -- * InstanceLimits
    InstanceLimits,
    instanceLimits,
    ilInstanceCountLimits,

    -- * Limits
    Limits,
    limits,
    lInstanceLimits,
    lAdditionalLimits,
    lStorageTypes,

    -- * LogPublishingOption
    LogPublishingOption,
    logPublishingOption,
    lpoEnabled,
    lpoCloudWatchLogsLogGroupARN,

    -- * LogPublishingOptionsStatus
    LogPublishingOptionsStatus,
    logPublishingOptionsStatus,
    lposStatus,
    lposOptions,

    -- * MasterUserOptions
    MasterUserOptions,
    masterUserOptions,
    muoMasterUserPassword,
    muoMasterUserName,
    muoMasterUserARN,

    -- * NodeToNodeEncryptionOptions
    NodeToNodeEncryptionOptions,
    nodeToNodeEncryptionOptions,
    ntneoEnabled,

    -- * NodeToNodeEncryptionOptionsStatus
    NodeToNodeEncryptionOptionsStatus,
    nodeToNodeEncryptionOptionsStatus,
    ntneosOptions,
    ntneosStatus,

    -- * OptionStatus
    OptionStatus,
    optionStatus,
    osPendingDeletion,
    osUpdateVersion,
    osCreationDate,
    osUpdateDate,
    osState,

    -- * OutboundCrossClusterSearchConnection
    OutboundCrossClusterSearchConnection,
    outboundCrossClusterSearchConnection,
    occscDestinationDomainInfo,
    occscConnectionAlias,
    occscCrossClusterSearchConnectionId,
    occscConnectionStatus,
    occscSourceDomainInfo,

    -- * OutboundCrossClusterSearchConnectionStatus
    OutboundCrossClusterSearchConnectionStatus,
    outboundCrossClusterSearchConnectionStatus,
    occscsMessage,
    occscsStatusCode,

    -- * PackageDetails
    PackageDetails,
    packageDetails,
    pdPackageId,
    pdPackageType,
    pdLastUpdatedAt,
    pdCreatedAt,
    pdPackageName,
    pdPackageStatus,
    pdPackageDescription,
    pdErrorDetails,
    pdAvailablePackageVersion,

    -- * PackageSource
    PackageSource,
    packageSource,
    psS3Key,
    psS3BucketName,

    -- * PackageVersionHistory
    PackageVersionHistory,
    packageVersionHistory,
    pvhCreatedAt,
    pvhPackageVersion,
    pvhCommitMessage,

    -- * RecurringCharge
    RecurringCharge,
    recurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedElasticsearchInstance
    ReservedElasticsearchInstance,
    reservedElasticsearchInstance,
    reiState,
    reiCurrencyCode,
    reiStartTime,
    reiReservedElasticsearchInstanceOfferingId,
    reiReservedElasticsearchInstanceId,
    reiElasticsearchInstanceCount,
    reiReservationName,
    reiElasticsearchInstanceType,
    reiRecurringCharges,
    reiUsagePrice,
    reiFixedPrice,
    reiDuration,
    reiPaymentOption,

    -- * ReservedElasticsearchInstanceOffering
    ReservedElasticsearchInstanceOffering,
    reservedElasticsearchInstanceOffering,
    reioCurrencyCode,
    reioReservedElasticsearchInstanceOfferingId,
    reioElasticsearchInstanceType,
    reioRecurringCharges,
    reioUsagePrice,
    reioFixedPrice,
    reioDuration,
    reioPaymentOption,

    -- * SAMLIdp
    SAMLIdp,
    sAMLIdp,
    samliMetadataContent,
    samliEntityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput,
    sAMLOptionsInput,
    samloiMasterUserName,
    samloiEnabled,
    samloiIdp,
    samloiRolesKey,
    samloiMasterBackendRole,
    samloiSessionTimeoutMinutes,
    samloiSubjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput,
    sAMLOptionsOutput,
    samlooEnabled,
    samlooIdp,
    samlooRolesKey,
    samlooSessionTimeoutMinutes,
    samlooSubjectKey,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions,
    serviceSoftwareOptions,
    ssoAutomatedUpdateDate,
    ssoCurrentVersion,
    ssoOptionalDeployment,
    ssoUpdateStatus,
    ssoCancellable,
    ssoUpdateAvailable,
    ssoDescription,
    ssoNewVersion,

    -- * SnapshotOptions
    SnapshotOptions,
    snapshotOptions,
    soAutomatedSnapshotStartHour,

    -- * SnapshotOptionsStatus
    SnapshotOptionsStatus,
    snapshotOptionsStatus,
    sosOptions,
    sosStatus,

    -- * StorageType
    StorageType,
    storageType,
    stStorageTypeLimits,
    stStorageSubTypeName,
    stStorageTypeName,

    -- * StorageTypeLimit
    StorageTypeLimit,
    storageTypeLimit,
    stlLimitName,
    stlLimitValues,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * UpgradeHistory
    UpgradeHistory,
    upgradeHistory,
    uhUpgradeStatus,
    uhStepsList,
    uhUpgradeName,
    uhStartTimestamp,

    -- * UpgradeStepItem
    UpgradeStepItem,
    upgradeStepItem,
    usiUpgradeStepStatus,
    usiProgressPercent,
    usiIssues,
    usiUpgradeStep,

    -- * VPCDerivedInfo
    VPCDerivedInfo,
    vpcDerivedInfo,
    vdiSecurityGroupIds,
    vdiSubnetIds,
    vdiVPCId,
    vdiAvailabilityZones,

    -- * VPCDerivedInfoStatus
    VPCDerivedInfoStatus,
    vpcDerivedInfoStatus,
    vdisOptions,
    vdisStatus,

    -- * VPCOptions
    VPCOptions,
    vpcOptions,
    voSecurityGroupIds,
    voSubnetIds,

    -- * ZoneAwarenessConfig
    ZoneAwarenessConfig,
    zoneAwarenessConfig,
    zacAvailabilityZoneCount,
  )
where

import Network.AWS.ElasticSearch.Types.AccessPoliciesStatus
import Network.AWS.ElasticSearch.Types.AdditionalLimit
import Network.AWS.ElasticSearch.Types.AdvancedOptionsStatus
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsInput
import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
import Network.AWS.ElasticSearch.Types.CognitoOptions
import Network.AWS.ElasticSearch.Types.CognitoOptionsStatus
import Network.AWS.ElasticSearch.Types.CompatibleVersionsMap
import Network.AWS.ElasticSearch.Types.DeploymentStatus
import Network.AWS.ElasticSearch.Types.DescribePackagesFilter
import Network.AWS.ElasticSearch.Types.DescribePackagesFilterName
import Network.AWS.ElasticSearch.Types.DomainEndpointOptions
import Network.AWS.ElasticSearch.Types.DomainEndpointOptionsStatus
import Network.AWS.ElasticSearch.Types.DomainInfo
import Network.AWS.ElasticSearch.Types.DomainInformation
import Network.AWS.ElasticSearch.Types.DomainPackageDetails
import Network.AWS.ElasticSearch.Types.DomainPackageStatus
import Network.AWS.ElasticSearch.Types.EBSOptions
import Network.AWS.ElasticSearch.Types.EBSOptionsStatus
import Network.AWS.ElasticSearch.Types.ESPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ESWarmPartitionInstanceType
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainConfig
import Network.AWS.ElasticSearch.Types.ElasticsearchDomainStatus
import Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
import Network.AWS.ElasticSearch.Types.ErrorDetails
import Network.AWS.ElasticSearch.Types.Filter
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.InboundCrossClusterSearchConnectionStatusCode
import Network.AWS.ElasticSearch.Types.InstanceCountLimits
import Network.AWS.ElasticSearch.Types.InstanceLimits
import Network.AWS.ElasticSearch.Types.Limits
import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.MasterUserOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
import Network.AWS.ElasticSearch.Types.OptionState
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnection
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatus
import Network.AWS.ElasticSearch.Types.OutboundCrossClusterSearchConnectionStatusCode
import Network.AWS.ElasticSearch.Types.PackageDetails
import Network.AWS.ElasticSearch.Types.PackageSource
import Network.AWS.ElasticSearch.Types.PackageStatus
import Network.AWS.ElasticSearch.Types.PackageType
import Network.AWS.ElasticSearch.Types.PackageVersionHistory
import Network.AWS.ElasticSearch.Types.RecurringCharge
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstance
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstanceOffering
import Network.AWS.ElasticSearch.Types.ReservedElasticsearchInstancePaymentOption
import Network.AWS.ElasticSearch.Types.SAMLIdp
import Network.AWS.ElasticSearch.Types.SAMLOptionsInput
import Network.AWS.ElasticSearch.Types.SAMLOptionsOutput
import Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptions
import Network.AWS.ElasticSearch.Types.SnapshotOptionsStatus
import Network.AWS.ElasticSearch.Types.StorageType
import Network.AWS.ElasticSearch.Types.StorageTypeLimit
import Network.AWS.ElasticSearch.Types.TLSSecurityPolicy
import Network.AWS.ElasticSearch.Types.Tag
import Network.AWS.ElasticSearch.Types.UpgradeHistory
import Network.AWS.ElasticSearch.Types.UpgradeStatus
import Network.AWS.ElasticSearch.Types.UpgradeStep
import Network.AWS.ElasticSearch.Types.UpgradeStepItem
import Network.AWS.ElasticSearch.Types.VPCDerivedInfo
import Network.AWS.ElasticSearch.Types.VPCDerivedInfoStatus
import Network.AWS.ElasticSearch.Types.VPCOptions
import Network.AWS.ElasticSearch.Types.VolumeType
import Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
elasticSearch :: Service
elasticSearch =
  Service
    { _svcAbbrev = "ElasticSearch",
      _svcSigner = v4,
      _svcPrefix = "es",
      _svcVersion = "2015-01-01",
      _svcEndpoint = defaultEndpoint elasticSearch,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ElasticSearch",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
