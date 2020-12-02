{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types
  ( -- * Service Configuration
    lightsail,

    -- * Errors

    -- * AccessDirection
    AccessDirection (..),

    -- * AddOnType
    AddOnType (..),

    -- * AlarmState
    AlarmState (..),

    -- * AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- * BehaviorEnum
    BehaviorEnum (..),

    -- * BlueprintType
    BlueprintType (..),

    -- * CertificateStatus
    CertificateStatus (..),

    -- * CloudFormationStackRecordSourceType
    CloudFormationStackRecordSourceType (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * ContactMethodStatus
    ContactMethodStatus (..),

    -- * ContactMethodVerificationProtocol
    ContactMethodVerificationProtocol (..),

    -- * ContactProtocol
    ContactProtocol (..),

    -- * ContainerServiceDeploymentState
    ContainerServiceDeploymentState (..),

    -- * ContainerServiceMetricName
    ContainerServiceMetricName (..),

    -- * ContainerServicePowerName
    ContainerServicePowerName (..),

    -- * ContainerServiceProtocol
    ContainerServiceProtocol (..),

    -- * ContainerServiceState
    ContainerServiceState (..),

    -- * DiskSnapshotState
    DiskSnapshotState (..),

    -- * DiskState
    DiskState (..),

    -- * DistributionMetricName
    DistributionMetricName (..),

    -- * ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- * ForwardValues
    ForwardValues (..),

    -- * HeaderEnum
    HeaderEnum (..),

    -- * InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- * InstanceHealthReason
    InstanceHealthReason (..),

    -- * InstanceHealthState
    InstanceHealthState (..),

    -- * InstanceMetricName
    InstanceMetricName (..),

    -- * InstancePlatform
    InstancePlatform (..),

    -- * InstanceSnapshotState
    InstanceSnapshotState (..),

    -- * LoadBalancerAttributeName
    LoadBalancerAttributeName (..),

    -- * LoadBalancerMetricName
    LoadBalancerMetricName (..),

    -- * LoadBalancerProtocol
    LoadBalancerProtocol (..),

    -- * LoadBalancerState
    LoadBalancerState (..),

    -- * LoadBalancerTLSCertificateDomainStatus
    LoadBalancerTLSCertificateDomainStatus (..),

    -- * LoadBalancerTLSCertificateFailureReason
    LoadBalancerTLSCertificateFailureReason (..),

    -- * LoadBalancerTLSCertificateRenewalStatus
    LoadBalancerTLSCertificateRenewalStatus (..),

    -- * LoadBalancerTLSCertificateRevocationReason
    LoadBalancerTLSCertificateRevocationReason (..),

    -- * LoadBalancerTLSCertificateStatus
    LoadBalancerTLSCertificateStatus (..),

    -- * MetricName
    MetricName (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricUnit
    MetricUnit (..),

    -- * NetworkProtocol
    NetworkProtocol (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationType
    OperationType (..),

    -- * OriginProtocolPolicyEnum
    OriginProtocolPolicyEnum (..),

    -- * PortAccessType
    PortAccessType (..),

    -- * PortInfoSourceType
    PortInfoSourceType (..),

    -- * PortState
    PortState (..),

    -- * RecordState
    RecordState (..),

    -- * RegionName
    RegionName (..),

    -- * RelationalDatabaseEngine
    RelationalDatabaseEngine (..),

    -- * RelationalDatabaseMetricName
    RelationalDatabaseMetricName (..),

    -- * RelationalDatabasePasswordVersion
    RelationalDatabasePasswordVersion (..),

    -- * RenewalStatus
    RenewalStatus (..),

    -- * ResourceType
    ResourceType (..),

    -- * TreatMissingData
    TreatMissingData (..),

    -- * AddOn
    AddOn,
    addOn,
    aoStatus,
    aoNextSnapshotTimeOfDay,
    aoSnapshotTimeOfDay,
    aoName,

    -- * AddOnRequest
    AddOnRequest,
    addOnRequest,
    aorAutoSnapshotAddOnRequest,
    aorAddOnType,

    -- * Alarm
    Alarm,
    alarm,
    aState,
    aTreatMissingData,
    aResourceType,
    aArn,
    aCreatedAt,
    aLocation,
    aContactProtocols,
    aPeriod,
    aEvaluationPeriods,
    aMetricName,
    aComparisonOperator,
    aName,
    aThreshold,
    aDatapointsToAlarm,
    aSupportCode,
    aNotificationEnabled,
    aNotificationTriggers,
    aStatistic,
    aUnit,
    aMonitoredResourceInfo,

    -- * AttachedDisk
    AttachedDisk,
    attachedDisk,
    adPath,
    adSizeInGb,

    -- * AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest,
    autoSnapshotAddOnRequest,
    asaorSnapshotTimeOfDay,

    -- * AutoSnapshotDetails
    AutoSnapshotDetails,
    autoSnapshotDetails,
    asdStatus,
    asdFromAttachedDisks,
    asdCreatedAt,
    asdDate,

    -- * AvailabilityZone
    AvailabilityZone,
    availabilityZone,
    azState,
    azZoneName,

    -- * Blueprint
    Blueprint,
    blueprint,
    bVersionCode,
    bPlatform,
    bGroup,
    bMinPower,
    bProductURL,
    bLicenseURL,
    bName,
    bVersion,
    bBlueprintId,
    bType,
    bIsActive,
    bDescription,

    -- * Bundle
    Bundle,
    bundle,
    bunCpuCount,
    bunTransferPerMonthInGb,
    bunBundleId,
    bunInstanceType,
    bunName,
    bunPower,
    bunDiskSizeInGb,
    bunSupportedPlatforms,
    bunPrice,
    bunIsActive,
    bunRamSizeInGb,

    -- * CacheBehavior
    CacheBehavior,
    cacheBehavior,
    cbBehavior,

    -- * CacheBehaviorPerPath
    CacheBehaviorPerPath,
    cacheBehaviorPerPath,
    cbppPath,
    cbppBehavior,

    -- * CacheSettings
    CacheSettings,
    cacheSettings,
    csMaximumTTL,
    csCachedHTTPMethods,
    csForwardedCookies,
    csAllowedHTTPMethods,
    csDefaultTTL,
    csMinimumTTL,
    csForwardedHeaders,
    csForwardedQueryStrings,

    -- * Certificate
    Certificate,
    certificate,
    cerStatus,
    cerSubjectAlternativeNames,
    cerArn,
    cerCreatedAt,
    cerEligibleToRenew,
    cerRequestFailureReason,
    cerRevokedAt,
    cerNotBefore,
    cerRevocationReason,
    cerDomainName,
    cerName,
    cerRenewalSummary,
    cerSupportCode,
    cerDomainValidationRecords,
    cerInUseResourceCount,
    cerIssuedAt,
    cerKeyAlgorithm,
    cerSerialNumber,
    cerIssuerCA,
    cerTags,
    cerNotAfter,

    -- * CertificateSummary
    CertificateSummary,
    certificateSummary,
    cCertificateDetail,
    cCertificateName,
    cCertificateARN,
    cDomainName,
    cTags,

    -- * CloudFormationStackRecord
    CloudFormationStackRecord,
    cloudFormationStackRecord,
    cfsrState,
    cfsrDestinationInfo,
    cfsrResourceType,
    cfsrArn,
    cfsrCreatedAt,
    cfsrLocation,
    cfsrName,
    cfsrSourceInfo,

    -- * CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo,
    cloudFormationStackRecordSourceInfo,
    cfsrsiResourceType,
    cfsrsiArn,
    cfsrsiName,

    -- * ContactMethod
    ContactMethod,
    contactMethod,
    cmStatus,
    cmResourceType,
    cmArn,
    cmCreatedAt,
    cmLocation,
    cmProtocol,
    cmName,
    cmSupportCode,
    cmContactEndpoint,

    -- * Container
    Container,
    container,
    cImage,
    cCommand,
    cEnvironment,
    cPorts,

    -- * ContainerImage
    ContainerImage,
    containerImage,
    ciImage,
    ciCreatedAt,
    ciDigest,

    -- * ContainerService
    ContainerService,
    containerService,
    csState,
    csPowerId,
    csResourceType,
    csArn,
    csCreatedAt,
    csLocation,
    csScale,
    csUrl,
    csNextDeployment,
    csPrincipalARN,
    csPower,
    csPrivateDomainName,
    csIsDisabled,
    csPublicDomainNames,
    csContainerServiceName,
    csCurrentDeployment,
    csTags,

    -- * ContainerServiceDeployment
    ContainerServiceDeployment,
    containerServiceDeployment,
    csdState,
    csdPublicEndpoint,
    csdCreatedAt,
    csdContainers,
    csdVersion,

    -- * ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest,
    containerServiceDeploymentRequest,
    csdrPublicEndpoint,
    csdrContainers,

    -- * ContainerServiceEndpoint
    ContainerServiceEndpoint,
    containerServiceEndpoint,
    cseHealthCheck,
    cseContainerName,
    cseContainerPort,

    -- * ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig,
    containerServiceHealthCheckConfig,
    cshccHealthyThreshold,
    cshccPath,
    cshccSuccessCodes,
    cshccIntervalSeconds,
    cshccTimeoutSeconds,
    cshccUnhealthyThreshold,

    -- * ContainerServiceLogEvent
    ContainerServiceLogEvent,
    containerServiceLogEvent,
    csleCreatedAt,
    csleMessage,

    -- * ContainerServicePower
    ContainerServicePower,
    containerServicePower,
    cspPowerId,
    cspCpuCount,
    cspName,
    cspPrice,
    cspIsActive,
    cspRamSizeInGb,

    -- * ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin,
    containerServiceRegistryLogin,
    csrlExpiresAt,
    csrlUsername,
    csrlPassword,
    csrlRegistry,

    -- * CookieObject
    CookieObject,
    cookieObject,
    coCookiesAllowList,
    coOption,

    -- * DestinationInfo
    DestinationInfo,
    destinationInfo,
    diService,
    diId,

    -- * Disk
    Disk,
    disk,
    dState,
    dResourceType,
    dArn,
    dPath,
    dCreatedAt,
    dLocation,
    dIops,
    dIsAttached,
    dAddOns,
    dAttachmentState,
    dName,
    dSizeInGb,
    dSupportCode,
    dIsSystemDisk,
    dAttachedTo,
    dGbInUse,
    dTags,

    -- * DiskInfo
    DiskInfo,
    diskInfo,
    diPath,
    diName,
    diSizeInGb,
    diIsSystemDisk,

    -- * DiskMap
    DiskMap,
    diskMap,
    dmNewDiskName,
    dmOriginalDiskPath,

    -- * DiskSnapshot
    DiskSnapshot,
    diskSnapshot,
    dsFromDiskName,
    dsIsFromAutoSnapshot,
    dsState,
    dsResourceType,
    dsArn,
    dsCreatedAt,
    dsLocation,
    dsProgress,
    dsName,
    dsSizeInGb,
    dsSupportCode,
    dsFromInstanceARN,
    dsFromInstanceName,
    dsFromDiskARN,
    dsTags,

    -- * DiskSnapshotInfo
    DiskSnapshotInfo,
    diskSnapshotInfo,
    dsiSizeInGb,

    -- * DistributionBundle
    DistributionBundle,
    distributionBundle,
    dbTransferPerMonthInGb,
    dbBundleId,
    dbName,
    dbPrice,
    dbIsActive,

    -- * Domain
    Domain,
    domain,
    domResourceType,
    domDomainEntries,
    domArn,
    domCreatedAt,
    domLocation,
    domName,
    domSupportCode,
    domTags,

    -- * DomainEntry
    DomainEntry,
    domainEntry,
    deIsAlias,
    deName,
    deId,
    deOptions,
    deType,
    deTarget,

    -- * DomainValidationRecord
    DomainValidationRecord,
    domainValidationRecord,
    dvrResourceRecord,
    dvrDomainName,

    -- * EndpointRequest
    EndpointRequest,
    endpointRequest,
    erHealthCheck,
    erContainerName,
    erContainerPort,

    -- * ExportSnapshotRecord
    ExportSnapshotRecord,
    exportSnapshotRecord,
    esrState,
    esrDestinationInfo,
    esrResourceType,
    esrArn,
    esrCreatedAt,
    esrLocation,
    esrName,
    esrSourceInfo,

    -- * ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo,
    exportSnapshotRecordSourceInfo,
    esrsiDiskSnapshotInfo,
    esrsiResourceType,
    esrsiArn,
    esrsiCreatedAt,
    esrsiFromResourceARN,
    esrsiName,
    esrsiInstanceSnapshotInfo,
    esrsiFromResourceName,

    -- * HeaderObject
    HeaderObject,
    headerObject,
    hoHeadersAllowList,
    hoOption,

    -- * HostKeyAttributes
    HostKeyAttributes,
    hostKeyAttributes,
    hkaNotValidAfter,
    hkaNotValidBefore,
    hkaFingerprintSHA1,
    hkaPublicKey,
    hkaAlgorithm,
    hkaWitnessedAt,
    hkaFingerprintSHA256,

    -- * InputOrigin
    InputOrigin,
    inputOrigin,
    ioRegionName,
    ioName,
    ioProtocolPolicy,

    -- * Instance
    Instance,
    instance',
    iState,
    iIpv6Address,
    iResourceType,
    iArn,
    iCreatedAt,
    iLocation,
    iSshKeyName,
    iAddOns,
    iUsername,
    iNetworking,
    iBundleId,
    iName,
    iSupportCode,
    iBlueprintId,
    iPrivateIPAddress,
    iBlueprintName,
    iIsStaticIP,
    iPublicIPAddress,
    iHardware,
    iTags,

    -- * InstanceAccessDetails
    InstanceAccessDetails,
    instanceAccessDetails,
    iadHostKeys,
    iadCertKey,
    iadIpAddress,
    iadPrivateKey,
    iadExpiresAt,
    iadUsername,
    iadProtocol,
    iadPasswordData,
    iadPassword,
    iadInstanceName,

    -- * InstanceEntry
    InstanceEntry,
    instanceEntry,
    ieUserData,
    ieSourceName,
    ieInstanceType,
    iePortInfoSource,
    ieAvailabilityZone,

    -- * InstanceHardware
    InstanceHardware,
    instanceHardware,
    ihCpuCount,
    ihDisks,
    ihRamSizeInGb,

    -- * InstanceHealthSummary
    InstanceHealthSummary,
    instanceHealthSummary,
    ihsInstanceHealth,
    ihsInstanceName,
    ihsInstanceHealthReason,

    -- * InstanceNetworking
    InstanceNetworking,
    instanceNetworking,
    inMonthlyTransfer,
    inPorts,

    -- * InstancePortInfo
    InstancePortInfo,
    instancePortInfo,
    ipiFromPort,
    ipiCidrs,
    ipiCommonName,
    ipiProtocol,
    ipiCidrListAliases,
    ipiAccessDirection,
    ipiAccessType,
    ipiToPort,
    ipiAccessFrom,

    -- * InstancePortState
    InstancePortState,
    instancePortState,
    ipsFromPort,
    ipsCidrs,
    ipsState,
    ipsProtocol,
    ipsCidrListAliases,
    ipsToPort,

    -- * InstanceSnapshot
    InstanceSnapshot,
    instanceSnapshot,
    insFromBlueprintId,
    insIsFromAutoSnapshot,
    insState,
    insResourceType,
    insFromAttachedDisks,
    insArn,
    insCreatedAt,
    insLocation,
    insProgress,
    insName,
    insFromBundleId,
    insSizeInGb,
    insSupportCode,
    insFromInstanceARN,
    insFromInstanceName,
    insTags,

    -- * InstanceSnapshotInfo
    InstanceSnapshotInfo,
    instanceSnapshotInfo,
    isiFromBlueprintId,
    isiFromBundleId,
    isiFromDiskInfo,

    -- * InstanceState
    InstanceState,
    instanceState,
    isName,
    isCode,

    -- * KeyPair
    KeyPair,
    keyPair,
    kpResourceType,
    kpArn,
    kpCreatedAt,
    kpLocation,
    kpFingerprint,
    kpName,
    kpSupportCode,
    kpTags,

    -- * LightsailDistribution
    LightsailDistribution,
    lightsailDistribution,
    ldStatus,
    ldOrigin,
    ldCertificateName,
    ldResourceType,
    ldArn,
    ldCreatedAt,
    ldLocation,
    ldCacheBehaviorSettings,
    ldAlternativeDomainNames,
    ldBundleId,
    ldAbleToUpdateBundle,
    ldOriginPublicDNS,
    ldDomainName,
    ldName,
    ldIsEnabled,
    ldSupportCode,
    ldDefaultCacheBehavior,
    ldCacheBehaviors,
    ldTags,

    -- * LoadBalancer
    LoadBalancer,
    loadBalancer,
    lbHealthCheckPath,
    lbState,
    lbResourceType,
    lbArn,
    lbCreatedAt,
    lbLocation,
    lbInstancePort,
    lbConfigurationOptions,
    lbProtocol,
    lbTlsCertificateSummaries,
    lbName,
    lbSupportCode,
    lbPublicPorts,
    lbDnsName,
    lbInstanceHealthSummary,
    lbTags,

    -- * LoadBalancerTLSCertificate
    LoadBalancerTLSCertificate,
    loadBalancerTLSCertificate,
    lbtcFailureReason,
    lbtcSubject,
    lbtcStatus,
    lbtcSubjectAlternativeNames,
    lbtcResourceType,
    lbtcArn,
    lbtcCreatedAt,
    lbtcLocation,
    lbtcLoadBalancerName,
    lbtcSerial,
    lbtcIsAttached,
    lbtcRevokedAt,
    lbtcNotBefore,
    lbtcRevocationReason,
    lbtcDomainName,
    lbtcName,
    lbtcRenewalSummary,
    lbtcSupportCode,
    lbtcDomainValidationRecords,
    lbtcIssuedAt,
    lbtcKeyAlgorithm,
    lbtcSignatureAlgorithm,
    lbtcIssuer,
    lbtcTags,
    lbtcNotAfter,

    -- * LoadBalancerTLSCertificateDomainValidationOption
    LoadBalancerTLSCertificateDomainValidationOption,
    loadBalancerTLSCertificateDomainValidationOption,
    lbtcdvoDomainName,
    lbtcdvoValidationStatus,

    -- * LoadBalancerTLSCertificateDomainValidationRecord
    LoadBalancerTLSCertificateDomainValidationRecord,
    loadBalancerTLSCertificateDomainValidationRecord,
    lbtcdvrValue,
    lbtcdvrDomainName,
    lbtcdvrName,
    lbtcdvrValidationStatus,
    lbtcdvrType,

    -- * LoadBalancerTLSCertificateRenewalSummary
    LoadBalancerTLSCertificateRenewalSummary,
    loadBalancerTLSCertificateRenewalSummary,
    lbtcrsRenewalStatus,
    lbtcrsDomainValidationOptions,

    -- * LoadBalancerTLSCertificateSummary
    LoadBalancerTLSCertificateSummary,
    loadBalancerTLSCertificateSummary,
    lbtcsIsAttached,
    lbtcsName,

    -- * LogEvent
    LogEvent,
    logEvent,
    leCreatedAt,
    leMessage,

    -- * MetricDatapoint
    MetricDatapoint,
    metricDatapoint,
    mdSampleCount,
    mdMaximum,
    mdAverage,
    mdMinimum,
    mdSum,
    mdTimestamp,
    mdUnit,

    -- * MonitoredResourceInfo
    MonitoredResourceInfo,
    monitoredResourceInfo,
    mriResourceType,
    mriArn,
    mriName,

    -- * MonthlyTransfer
    MonthlyTransfer,
    monthlyTransfer,
    mtGbPerMonthAllocated,

    -- * Operation
    Operation,
    operation,
    opeStatus,
    opeOperationDetails,
    opeResourceType,
    opeCreatedAt,
    opeResourceName,
    opeLocation,
    opeStatusChangedAt,
    opeErrorDetails,
    opeErrorCode,
    opeId,
    opeOperationType,
    opeIsTerminal,

    -- * Origin
    Origin,
    origin,
    oRegionName,
    oResourceType,
    oName,
    oProtocolPolicy,

    -- * PasswordData
    PasswordData,
    passwordData,
    pdKeyPairName,
    pdCiphertext,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction,
    pendingMaintenanceAction,
    pmaAction,
    pmaDescription,
    pmaCurrentApplyDate,

    -- * PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues,
    pendingModifiedRelationalDatabaseValues,
    pmrdvEngineVersion,
    pmrdvMasterUserPassword,
    pmrdvBackupRetentionEnabled,

    -- * PortInfo
    PortInfo,
    portInfo,
    piFromPort,
    piCidrs,
    piProtocol,
    piCidrListAliases,
    piToPort,

    -- * QueryStringObject
    QueryStringObject,
    queryStringObject,
    qsoQueryStringsAllowList,
    qsoOption,

    -- * RegionInfo
    RegionInfo,
    regionInfo,
    riAvailabilityZones,
    riName,
    riRelationalDatabaseAvailabilityZones,
    riDisplayName,
    riContinentCode,
    riDescription,

    -- * RelationalDatabase
    RelationalDatabase,
    relationalDatabase,
    rdEngineVersion,
    rdRelationalDatabaseBundleId,
    rdMasterEndpoint,
    rdState,
    rdResourceType,
    rdPubliclyAccessible,
    rdMasterUsername,
    rdArn,
    rdCreatedAt,
    rdLocation,
    rdEngine,
    rdLatestRestorableTime,
    rdPreferredMaintenanceWindow,
    rdRelationalDatabaseBlueprintId,
    rdCaCertificateIdentifier,
    rdName,
    rdBackupRetentionEnabled,
    rdPreferredBackupWindow,
    rdPendingMaintenanceActions,
    rdSupportCode,
    rdSecondaryAvailabilityZone,
    rdPendingModifiedValues,
    rdMasterDatabaseName,
    rdHardware,
    rdParameterApplyStatus,
    rdTags,

    -- * RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint,
    relationalDatabaseBlueprint,
    rdbEngineVersion,
    rdbIsEngineDefault,
    rdbEngineVersionDescription,
    rdbEngine,
    rdbBlueprintId,
    rdbEngineDescription,

    -- * RelationalDatabaseBundle
    RelationalDatabaseBundle,
    relationalDatabaseBundle,
    rdbIsEncrypted,
    rdbCpuCount,
    rdbTransferPerMonthInGb,
    rdbBundleId,
    rdbName,
    rdbDiskSizeInGb,
    rdbPrice,
    rdbIsActive,
    rdbRamSizeInGb,

    -- * RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint,
    relationalDatabaseEndpoint,
    rdeAddress,
    rdePort,

    -- * RelationalDatabaseEvent
    RelationalDatabaseEvent,
    relationalDatabaseEvent,
    rdeCreatedAt,
    rdeEventCategories,
    rdeResource,
    rdeMessage,

    -- * RelationalDatabaseHardware
    RelationalDatabaseHardware,
    relationalDatabaseHardware,
    rdhCpuCount,
    rdhDiskSizeInGb,
    rdhRamSizeInGb,

    -- * RelationalDatabaseParameter
    RelationalDatabaseParameter,
    relationalDatabaseParameter,
    rdpApplyType,
    rdpParameterValue,
    rdpApplyMethod,
    rdpDataType,
    rdpIsModifiable,
    rdpAllowedValues,
    rdpParameterName,
    rdpDescription,

    -- * RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot,
    relationalDatabaseSnapshot,
    rdsEngineVersion,
    rdsState,
    rdsFromRelationalDatabaseName,
    rdsResourceType,
    rdsFromRelationalDatabaseBlueprintId,
    rdsArn,
    rdsCreatedAt,
    rdsLocation,
    rdsEngine,
    rdsName,
    rdsSizeInGb,
    rdsSupportCode,
    rdsFromRelationalDatabaseARN,
    rdsFromRelationalDatabaseBundleId,
    rdsTags,

    -- * RenewalSummary
    RenewalSummary,
    renewalSummary,
    rsRenewalStatus,
    rsDomainValidationRecords,
    rsUpdatedAt,
    rsRenewalStatusReason,

    -- * ResourceLocation
    ResourceLocation,
    resourceLocation,
    rlRegionName,
    rlAvailabilityZone,

    -- * ResourceRecord
    ResourceRecord,
    resourceRecord,
    rrValue,
    rrName,
    rrType,

    -- * StaticIP
    StaticIP,
    staticIP,
    siIpAddress,
    siResourceType,
    siArn,
    siCreatedAt,
    siLocation,
    siIsAttached,
    siName,
    siSupportCode,
    siAttachedTo,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.AddOnRequest
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.Alarm
import Network.AWS.Lightsail.Types.AlarmState
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.BehaviorEnum
import Network.AWS.Lightsail.Types.Blueprint
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.Bundle
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.CertificateStatus
import Network.AWS.Lightsail.Types.CertificateSummary
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import Network.AWS.Lightsail.Types.ComparisonOperator
import Network.AWS.Lightsail.Types.ContactMethod
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerImage
import Network.AWS.Lightsail.Types.ContainerService
import Network.AWS.Lightsail.Types.ContainerServiceDeployment
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import Network.AWS.Lightsail.Types.ContainerServiceLogEvent
import Network.AWS.Lightsail.Types.ContainerServiceMetricName
import Network.AWS.Lightsail.Types.ContainerServicePower
import Network.AWS.Lightsail.Types.ContainerServicePowerName
import Network.AWS.Lightsail.Types.ContainerServiceProtocol
import Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
import Network.AWS.Lightsail.Types.ContainerServiceState
import Network.AWS.Lightsail.Types.CookieObject
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.DiskInfo
import Network.AWS.Lightsail.Types.DiskMap
import Network.AWS.Lightsail.Types.DiskSnapshot
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.DistributionBundle
import Network.AWS.Lightsail.Types.DistributionMetricName
import Network.AWS.Lightsail.Types.Domain
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.EndpointRequest
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
import Network.AWS.Lightsail.Types.ForwardValues
import Network.AWS.Lightsail.Types.HeaderEnum
import Network.AWS.Lightsail.Types.HeaderObject
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.InputOrigin
import Network.AWS.Lightsail.Types.Instance
import Network.AWS.Lightsail.Types.InstanceAccessDetails
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
import Network.AWS.Lightsail.Types.InstanceEntry
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceHealthReason
import Network.AWS.Lightsail.Types.InstanceHealthState
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.InstanceMetricName
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstancePlatform
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.InstancePortState
import Network.AWS.Lightsail.Types.InstanceSnapshot
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.KeyPair
import Network.AWS.Lightsail.Types.LightsailDistribution
import Network.AWS.Lightsail.Types.LoadBalancer
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerMetricName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificate
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainStatus
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalStatus
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateStatus
import Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
import Network.AWS.Lightsail.Types.LogEvent
import Network.AWS.Lightsail.Types.MetricDatapoint
import Network.AWS.Lightsail.Types.MetricName
import Network.AWS.Lightsail.Types.MetricStatistic
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.Operation
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.PortAccessType
import Network.AWS.Lightsail.Types.PortInfo
import Network.AWS.Lightsail.Types.PortInfoSourceType
import Network.AWS.Lightsail.Types.PortState
import Network.AWS.Lightsail.Types.QueryStringObject
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.RegionInfo
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.RelationalDatabase
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseEngine
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
import Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
import Network.AWS.Lightsail.Types.RenewalStatus
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceRecord
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.StaticIP
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Lightsail.Types.TreatMissingData
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
lightsail :: Service
lightsail =
  Service
    { _svcAbbrev = "Lightsail",
      _svcSigner = v4,
      _svcPrefix = "lightsail",
      _svcVersion = "2016-11-28",
      _svcEndpoint = defaultEndpoint lightsail,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Lightsail",
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
