-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types
  ( -- * Service configuration
    elasticSearchService,

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
    AccessPoliciesStatus (..),
    mkAccessPoliciesStatus,
    apsStatus,
    apsOptions,

    -- * AdditionalLimit
    AdditionalLimit (..),
    mkAdditionalLimit,
    alLimitName,
    alLimitValues,

    -- * AdvancedOptionsStatus
    AdvancedOptionsStatus (..),
    mkAdvancedOptionsStatus,
    aosStatus,
    aosOptions,

    -- * AdvancedSecurityOptions
    AdvancedSecurityOptions (..),
    mkAdvancedSecurityOptions,
    asoEnabled,
    asoInternalUserDatabaseEnabled,
    asoSAMLOptions,

    -- * AdvancedSecurityOptionsInput
    AdvancedSecurityOptionsInput (..),
    mkAdvancedSecurityOptionsInput,
    asoiEnabled,
    asoiInternalUserDatabaseEnabled,
    asoiMasterUserOptions,
    asoiSAMLOptions,

    -- * AdvancedSecurityOptionsStatus
    AdvancedSecurityOptionsStatus (..),
    mkAdvancedSecurityOptionsStatus,
    asosStatus,
    asosOptions,

    -- * CognitoOptions
    CognitoOptions (..),
    mkCognitoOptions,
    coIdentityPoolId,
    coEnabled,
    coUserPoolId,
    coRoleARN,

    -- * CognitoOptionsStatus
    CognitoOptionsStatus (..),
    mkCognitoOptionsStatus,
    cosStatus,
    cosOptions,

    -- * CompatibleVersionsMap
    CompatibleVersionsMap (..),
    mkCompatibleVersionsMap,
    cvmSourceVersion,
    cvmTargetVersions,

    -- * DescribePackagesFilter
    DescribePackagesFilter (..),
    mkDescribePackagesFilter,
    dpfValue,
    dpfName,

    -- * DomainEndpointOptions
    DomainEndpointOptions (..),
    mkDomainEndpointOptions,
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,
    deoCustomEndpointEnabled,
    deoCustomEndpoint,
    deoCustomEndpointCertificateARN,

    -- * DomainEndpointOptionsStatus
    DomainEndpointOptionsStatus (..),
    mkDomainEndpointOptionsStatus,
    deosStatus,
    deosOptions,

    -- * DomainInfo
    DomainInfo (..),
    mkDomainInfo,
    diDomainName,

    -- * DomainInformation
    DomainInformation (..),
    mkDomainInformation,
    dOwnerId,
    dDomainName,
    dRegion,

    -- * DomainPackageDetails
    DomainPackageDetails (..),
    mkDomainPackageDetails,
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
    EBSOptions (..),
    mkEBSOptions,
    eoVolumeSize,
    eoIOPS,
    eoVolumeType,
    eoEBSEnabled,

    -- * EBSOptionsStatus
    EBSOptionsStatus (..),
    mkEBSOptionsStatus,
    eosStatus,
    eosOptions,

    -- * ElasticsearchClusterConfig
    ElasticsearchClusterConfig (..),
    mkElasticsearchClusterConfig,
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
    ElasticsearchClusterConfigStatus (..),
    mkElasticsearchClusterConfigStatus,
    eccsStatus,
    eccsOptions,

    -- * ElasticsearchDomainConfig
    ElasticsearchDomainConfig (..),
    mkElasticsearchDomainConfig,
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
    ElasticsearchDomainStatus (..),
    mkElasticsearchDomainStatus,
    edsEBSOptions,
    edsNodeToNodeEncryptionOptions,
    edsAccessPolicies,
    edsServiceSoftwareOptions,
    edsARN,
    edsLogPublishingOptions,
    edsAdvancedSecurityOptions,
    edsCreated,
    edsElasticsearchClusterConfig,
    edsSnapshotOptions,
    edsDomainName,
    edsCognitoOptions,
    edsEncryptionAtRestOptions,
    edsDeleted,
    edsVPCOptions,
    edsDomainId,
    edsEndpoints,
    edsDomainEndpointOptions,
    edsProcessing,
    edsEndpoint,
    edsUpgradeProcessing,
    edsAdvancedOptions,
    edsElasticsearchVersion,

    -- * ElasticsearchVersionStatus
    ElasticsearchVersionStatus (..),
    mkElasticsearchVersionStatus,
    evsStatus,
    evsOptions,

    -- * EncryptionAtRestOptions
    EncryptionAtRestOptions (..),
    mkEncryptionAtRestOptions,
    earoEnabled,
    earoKMSKeyId,

    -- * EncryptionAtRestOptionsStatus
    EncryptionAtRestOptionsStatus (..),
    mkEncryptionAtRestOptionsStatus,
    earosStatus,
    earosOptions,

    -- * ErrorDetails
    ErrorDetails (..),
    mkErrorDetails,
    edErrorType,
    edErrorMessage,

    -- * Filter
    Filter (..),
    mkFilter,
    fValues,
    fName,

    -- * InboundCrossClusterSearchConnection
    InboundCrossClusterSearchConnection (..),
    mkInboundCrossClusterSearchConnection,
    iccscDestinationDomainInfo,
    iccscCrossClusterSearchConnectionId,
    iccscConnectionStatus,
    iccscSourceDomainInfo,

    -- * InboundCrossClusterSearchConnectionStatus
    InboundCrossClusterSearchConnectionStatus (..),
    mkInboundCrossClusterSearchConnectionStatus,
    iccscsMessage,
    iccscsStatusCode,

    -- * InstanceCountLimits
    InstanceCountLimits (..),
    mkInstanceCountLimits,
    iclMaximumInstanceCount,
    iclMinimumInstanceCount,

    -- * InstanceLimits
    InstanceLimits (..),
    mkInstanceLimits,
    ilInstanceCountLimits,

    -- * Limits
    Limits (..),
    mkLimits,
    lInstanceLimits,
    lAdditionalLimits,
    lStorageTypes,

    -- * LogPublishingOption
    LogPublishingOption (..),
    mkLogPublishingOption,
    lpoEnabled,
    lpoCloudWatchLogsLogGroupARN,

    -- * LogPublishingOptionsStatus
    LogPublishingOptionsStatus (..),
    mkLogPublishingOptionsStatus,
    lposStatus,
    lposOptions,

    -- * MasterUserOptions
    MasterUserOptions (..),
    mkMasterUserOptions,
    muoMasterUserPassword,
    muoMasterUserName,
    muoMasterUserARN,

    -- * NodeToNodeEncryptionOptions
    NodeToNodeEncryptionOptions (..),
    mkNodeToNodeEncryptionOptions,
    ntneoEnabled,

    -- * NodeToNodeEncryptionOptionsStatus
    NodeToNodeEncryptionOptionsStatus (..),
    mkNodeToNodeEncryptionOptionsStatus,
    ntneosStatus,
    ntneosOptions,

    -- * OptionStatus
    OptionStatus (..),
    mkOptionStatus,
    osState,
    osUpdateDate,
    osPendingDeletion,
    osCreationDate,
    osUpdateVersion,

    -- * OutboundCrossClusterSearchConnection
    OutboundCrossClusterSearchConnection (..),
    mkOutboundCrossClusterSearchConnection,
    occscDestinationDomainInfo,
    occscConnectionAlias,
    occscCrossClusterSearchConnectionId,
    occscConnectionStatus,
    occscSourceDomainInfo,

    -- * OutboundCrossClusterSearchConnectionStatus
    OutboundCrossClusterSearchConnectionStatus (..),
    mkOutboundCrossClusterSearchConnectionStatus,
    occscsMessage,
    occscsStatusCode,

    -- * PackageDetails
    PackageDetails (..),
    mkPackageDetails,
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
    PackageSource (..),
    mkPackageSource,
    psS3Key,
    psS3BucketName,

    -- * PackageVersionHistory
    PackageVersionHistory (..),
    mkPackageVersionHistory,
    pvhCreatedAt,
    pvhPackageVersion,
    pvhCommitMessage,

    -- * RecurringCharge
    RecurringCharge (..),
    mkRecurringCharge,
    rcRecurringChargeFrequency,
    rcRecurringChargeAmount,

    -- * ReservedElasticsearchInstance
    ReservedElasticsearchInstance (..),
    mkReservedElasticsearchInstance,
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
    ReservedElasticsearchInstanceOffering (..),
    mkReservedElasticsearchInstanceOffering,
    reioCurrencyCode,
    reioReservedElasticsearchInstanceOfferingId,
    reioElasticsearchInstanceType,
    reioRecurringCharges,
    reioUsagePrice,
    reioFixedPrice,
    reioDuration,
    reioPaymentOption,

    -- * SAMLIdp
    SAMLIdp (..),
    mkSAMLIdp,
    samliMetadataContent,
    samliEntityId,

    -- * SAMLOptionsInput
    SAMLOptionsInput (..),
    mkSAMLOptionsInput,
    samloiMasterUserName,
    samloiEnabled,
    samloiIdp,
    samloiRolesKey,
    samloiMasterBackendRole,
    samloiSessionTimeoutMinutes,
    samloiSubjectKey,

    -- * SAMLOptionsOutput
    SAMLOptionsOutput (..),
    mkSAMLOptionsOutput,
    samlooEnabled,
    samlooIdp,
    samlooRolesKey,
    samlooSessionTimeoutMinutes,
    samlooSubjectKey,

    -- * ServiceSoftwareOptions
    ServiceSoftwareOptions (..),
    mkServiceSoftwareOptions,
    ssoAutomatedUpdateDate,
    ssoCurrentVersion,
    ssoOptionalDeployment,
    ssoUpdateStatus,
    ssoCancellable,
    ssoUpdateAvailable,
    ssoDescription,
    ssoNewVersion,

    -- * SnapshotOptions
    SnapshotOptions (..),
    mkSnapshotOptions,
    soAutomatedSnapshotStartHour,

    -- * SnapshotOptionsStatus
    SnapshotOptionsStatus (..),
    mkSnapshotOptionsStatus,
    sosStatus,
    sosOptions,

    -- * StorageType
    StorageType (..),
    mkStorageType,
    stStorageTypeLimits,
    stStorageSubTypeName,
    stStorageTypeName,

    -- * StorageTypeLimit
    StorageTypeLimit (..),
    mkStorageTypeLimit,
    stlLimitName,
    stlLimitValues,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * UpgradeHistory
    UpgradeHistory (..),
    mkUpgradeHistory,
    uhUpgradeStatus,
    uhStepsList,
    uhUpgradeName,
    uhStartTimestamp,

    -- * UpgradeStepItem
    UpgradeStepItem (..),
    mkUpgradeStepItem,
    usiUpgradeStepStatus,
    usiProgressPercent,
    usiIssues,
    usiUpgradeStep,

    -- * VPCDerivedInfo
    VPCDerivedInfo (..),
    mkVPCDerivedInfo,
    vdiSecurityGroupIds,
    vdiSubnetIds,
    vdiVPCId,
    vdiAvailabilityZones,

    -- * VPCDerivedInfoStatus
    VPCDerivedInfoStatus (..),
    mkVPCDerivedInfoStatus,
    vdisStatus,
    vdisOptions,

    -- * VPCOptions
    VPCOptions (..),
    mkVPCOptions,
    voSecurityGroupIds,
    voSubnetIds,

    -- * ZoneAwarenessConfig
    ZoneAwarenessConfig (..),
    mkZoneAwarenessConfig,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-01-01@ of the Amazon Elasticsearch Service SDK configuration.
elasticSearchService :: Lude.Service
elasticSearchService =
  Lude.Service
    { Lude._svcAbbrev = "ElasticSearch",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "es",
      Lude._svcVersion = "2015-01-01",
      Lude._svcEndpoint = Lude.defaultEndpoint elasticSearchService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ElasticSearch",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
