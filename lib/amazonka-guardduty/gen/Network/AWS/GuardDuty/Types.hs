{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types
  ( -- * Service Configuration
    guardDuty,

    -- * Errors

    -- * AdminStatus
    AdminStatus (..),

    -- * DataSource
    DataSource (..),

    -- * DataSourceStatus
    DataSourceStatus (..),

    -- * DestinationType
    DestinationType (..),

    -- * DetectorStatus
    DetectorStatus (..),

    -- * Feedback
    Feedback (..),

    -- * FilterAction
    FilterAction (..),

    -- * FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- * FindingStatisticType
    FindingStatisticType (..),

    -- * IPSetFormat
    IPSetFormat (..),

    -- * IPSetStatus
    IPSetStatus (..),

    -- * OrderBy
    OrderBy (..),

    -- * PublishingStatus
    PublishingStatus (..),

    -- * ThreatIntelSetFormat
    ThreatIntelSetFormat (..),

    -- * ThreatIntelSetStatus
    ThreatIntelSetStatus (..),

    -- * UsageStatisticType
    UsageStatisticType (..),

    -- * AWSAPICallAction
    AWSAPICallAction,
    awsAPICallAction,
    aacaRemoteIPDetails,
    aacaCallerType,
    aacaDomainDetails,
    aacaServiceName,
    aacaErrorCode,
    aacaAPI,

    -- * AccessControlList
    AccessControlList,
    accessControlList,
    aclAllowsPublicWriteAccess,
    aclAllowsPublicReadAccess,

    -- * AccessKeyDetails
    AccessKeyDetails,
    accessKeyDetails,
    akdPrincipalId,
    akdUserName,
    akdAccessKeyId,
    akdUserType,

    -- * AccountDetail
    AccountDetail,
    accountDetail,
    adAccountId,
    adEmail,

    -- * AccountLevelPermissions
    AccountLevelPermissions,
    accountLevelPermissions,
    alpBlockPublicAccess,

    -- * Action
    Action,
    action,
    aNetworkConnectionAction,
    aPortProbeAction,
    aActionType,
    aDNSRequestAction,
    aAWSAPICallAction,

    -- * AdminAccount
    AdminAccount,
    adminAccount,
    aaAdminAccountId,
    aaAdminStatus,

    -- * BlockPublicAccess
    BlockPublicAccess,
    blockPublicAccess,
    bpaIgnorePublicACLs,
    bpaBlockPublicACLs,
    bpaRestrictPublicBuckets,
    bpaBlockPublicPolicy,

    -- * BucketLevelPermissions
    BucketLevelPermissions,
    bucketLevelPermissions,
    blpAccessControlList,
    blpBlockPublicAccess,
    blpBucketPolicy,

    -- * BucketPolicy
    BucketPolicy,
    bucketPolicy,
    bpAllowsPublicWriteAccess,
    bpAllowsPublicReadAccess,

    -- * City
    City,
    city,
    cCityName,

    -- * CloudTrailConfigurationResult
    CloudTrailConfigurationResult,
    cloudTrailConfigurationResult,
    ctcrStatus,

    -- * Condition
    Condition,
    condition,
    cEQ,
    cLessThan,
    cLte,
    cGreaterThanOrEqual,
    cLessThanOrEqual,
    cGT,
    cEquals,
    cNeq,
    cNotEquals,
    cLT,
    cGte,
    cGreaterThan,

    -- * Country
    Country,
    country,
    cCountryName,
    cCountryCode,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult,
    dnsLogsConfigurationResult,
    dlcrStatus,

    -- * DNSRequestAction
    DNSRequestAction,
    dnsRequestAction,
    draDomain,

    -- * DataSourceConfigurations
    DataSourceConfigurations,
    dataSourceConfigurations,
    dscS3Logs,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult,
    dataSourceConfigurationsResult,
    dscrCloudTrail,
    dscrDNSLogs,
    dscrFlowLogs,
    dscrS3Logs,

    -- * DefaultServerSideEncryption
    DefaultServerSideEncryption,
    defaultServerSideEncryption,
    dsseEncryptionType,
    dsseKMSMasterKeyARN,

    -- * Destination
    Destination,
    destination,
    dDestinationId,
    dDestinationType,
    dStatus,

    -- * DestinationProperties
    DestinationProperties,
    destinationProperties,
    dpKMSKeyARN,
    dpDestinationARN,

    -- * DomainDetails
    DomainDetails,
    domainDetails,
    ddDomain,

    -- * Evidence
    Evidence,
    evidence,
    eThreatIntelligenceDetails,

    -- * Finding
    Finding,
    finding,
    fService,
    fConfidence,
    fPartition,
    fTitle,
    fDescription,
    fAccountId,
    fARN,
    fCreatedAt,
    fId,
    fRegion,
    fResource,
    fSchemaVersion,
    fSeverity,
    fType,
    fUpdatedAt,

    -- * FindingCriteria
    FindingCriteria,
    findingCriteria,
    fcCriterion,

    -- * FindingStatistics
    FindingStatistics,
    findingStatistics,
    fsCountBySeverity,

    -- * FlowLogsConfigurationResult
    FlowLogsConfigurationResult,
    flowLogsConfigurationResult,
    flcrStatus,

    -- * GeoLocation
    GeoLocation,
    geoLocation,
    glLat,
    glLon,

    -- * IAMInstanceProfile
    IAMInstanceProfile,
    iamInstanceProfile,
    iapARN,
    iapId,

    -- * InstanceDetails
    InstanceDetails,
    instanceDetails,
    idInstanceId,
    idPlatform,
    idLaunchTime,
    idNetworkInterfaces,
    idOutpostARN,
    idInstanceType,
    idAvailabilityZone,
    idIAMInstanceProfile,
    idImageId,
    idProductCodes,
    idInstanceState,
    idTags,
    idImageDescription,

    -- * Invitation
    Invitation,
    invitation,
    iInvitedAt,
    iRelationshipStatus,
    iInvitationId,
    iAccountId,

    -- * LocalIPDetails
    LocalIPDetails,
    localIPDetails,
    lidIPAddressV4,

    -- * LocalPortDetails
    LocalPortDetails,
    localPortDetails,
    lpdPortName,
    lpdPort,

    -- * Master
    Master,
    master,
    masInvitedAt,
    masRelationshipStatus,
    masInvitationId,
    masAccountId,

    -- * Member
    Member,
    member,
    mInvitedAt,
    mDetectorId,
    mAccountId,
    mMasterId,
    mEmail,
    mRelationshipStatus,
    mUpdatedAt,

    -- * MemberDataSourceConfiguration
    MemberDataSourceConfiguration,
    memberDataSourceConfiguration,
    mdscAccountId,
    mdscDataSources,

    -- * NetworkConnectionAction
    NetworkConnectionAction,
    networkConnectionAction,
    ncaRemoteIPDetails,
    ncaProtocol,
    ncaLocalIPDetails,
    ncaRemotePortDetails,
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalPortDetails,

    -- * NetworkInterface
    NetworkInterface,
    networkInterface,
    niPrivateIPAddresses,
    niPublicDNSName,
    niSecurityGroups,
    niVPCId,
    niNetworkInterfaceId,
    niSubnetId,
    niPrivateIPAddress,
    niPublicIP,
    niPrivateDNSName,
    niIPv6Addresses,

    -- * Organization
    Organization,
    organization,
    oOrg,
    oASNOrg,
    oASN,
    oIsp,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations,
    organizationDataSourceConfigurations,
    odscS3Logs,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult,
    organizationDataSourceConfigurationsResult,
    odscrS3Logs,

    -- * OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration,
    organizationS3LogsConfiguration,
    oslcAutoEnable,

    -- * OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult,
    organizationS3LogsConfigurationResult,
    oslcrAutoEnable,

    -- * Owner
    Owner,
    owner,
    oId,

    -- * PermissionConfiguration
    PermissionConfiguration,
    permissionConfiguration,
    pcBucketLevelPermissions,
    pcAccountLevelPermissions,

    -- * PortProbeAction
    PortProbeAction,
    portProbeAction,
    ppaPortProbeDetails,
    ppaBlocked,

    -- * PortProbeDetail
    PortProbeDetail,
    portProbeDetail,
    ppdRemoteIPDetails,
    ppdLocalIPDetails,
    ppdLocalPortDetails,

    -- * PrivateIPAddressDetails
    PrivateIPAddressDetails,
    privateIPAddressDetails,
    piadPrivateIPAddress,
    piadPrivateDNSName,

    -- * ProductCode
    ProductCode,
    productCode,
    pcProductType,
    pcCode,

    -- * PublicAccess
    PublicAccess,
    publicAccess,
    paPermissionConfiguration,
    paEffectivePermission,

    -- * RemoteIPDetails
    RemoteIPDetails,
    remoteIPDetails,
    ridCountry,
    ridCity,
    ridIPAddressV4,
    ridGeoLocation,
    ridOrganization,

    -- * RemotePortDetails
    RemotePortDetails,
    remotePortDetails,
    rpdPortName,
    rpdPort,

    -- * Resource
    Resource,
    resource,
    rResourceType,
    rS3BucketDetails,
    rInstanceDetails,
    rAccessKeyDetails,

    -- * S3BucketDetail
    S3BucketDetail,
    s3BucketDetail,
    sbdARN,
    sbdCreatedAt,
    sbdOwner,
    sbdName,
    sbdDefaultServerSideEncryption,
    sbdPublicAccess,
    sbdType,
    sbdTags,

    -- * S3LogsConfiguration
    S3LogsConfiguration,
    s3LogsConfiguration,
    slcEnable,

    -- * S3LogsConfigurationResult
    S3LogsConfigurationResult,
    s3LogsConfigurationResult,
    slcrStatus,

    -- * SecurityGroup
    SecurityGroup,
    securityGroup,
    sgGroupId,
    sgGroupName,

    -- * ServiceInfo
    ServiceInfo,
    serviceInfo,
    siCount,
    siEventFirstSeen,
    siAction,
    siDetectorId,
    siServiceName,
    siUserFeedback,
    siEvidence,
    siEventLastSeen,
    siResourceRole,
    siArchived,

    -- * SortCriteria
    SortCriteria,
    sortCriteria,
    scOrderBy,
    scAttributeName,

    -- * Tag
    Tag,
    tag,
    tagValue,
    tagKey,

    -- * ThreatIntelligenceDetail
    ThreatIntelligenceDetail,
    threatIntelligenceDetail,
    tidThreatNames,
    tidThreatListName,

    -- * Total
    Total,
    total,
    tAmount,
    tUnit,

    -- * UnprocessedAccount
    UnprocessedAccount,
    unprocessedAccount,
    uaAccountId,
    uaResult,

    -- * UsageAccountResult
    UsageAccountResult,
    usageAccountResult,
    uarAccountId,
    uarTotal,

    -- * UsageCriteria
    UsageCriteria,
    usageCriteria,
    ucAccountIds,
    ucResources,
    ucDataSources,

    -- * UsageDataSourceResult
    UsageDataSourceResult,
    usageDataSourceResult,
    udsrTotal,
    udsrDataSource,

    -- * UsageResourceResult
    UsageResourceResult,
    usageResourceResult,
    urrTotal,
    urrResource,

    -- * UsageStatistics
    UsageStatistics,
    usageStatistics,
    usTopResources,
    usSumByResource,
    usSumByDataSource,
    usSumByAccount,
  )
where

import Network.AWS.GuardDuty.Types.AWSAPICallAction
import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.AccountDetail
import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.AdminAccount
import Network.AWS.GuardDuty.Types.AdminStatus
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import Network.AWS.GuardDuty.Types.BucketPolicy
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.DNSRequestAction
import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.DataSourceConfigurations
import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Destination
import Network.AWS.GuardDuty.Types.DestinationProperties
import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.DetectorStatus
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.GuardDuty.Types.Feedback
import Network.AWS.GuardDuty.Types.FilterAction
import Network.AWS.GuardDuty.Types.Finding
import Network.AWS.GuardDuty.Types.FindingCriteria
import Network.AWS.GuardDuty.Types.FindingPublishingFrequency
import Network.AWS.GuardDuty.Types.FindingStatisticType
import Network.AWS.GuardDuty.Types.FindingStatistics
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.IAMInstanceProfile
import Network.AWS.GuardDuty.Types.IPSetFormat
import Network.AWS.GuardDuty.Types.IPSetStatus
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.Invitation
import Network.AWS.GuardDuty.Types.LocalIPDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.Master
import Network.AWS.GuardDuty.Types.Member
import Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.OrderBy
import Network.AWS.GuardDuty.Types.Organization
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.Owner
import Network.AWS.GuardDuty.Types.PermissionConfiguration
import Network.AWS.GuardDuty.Types.PortProbeAction
import Network.AWS.GuardDuty.Types.PortProbeDetail
import Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.PublishingStatus
import Network.AWS.GuardDuty.Types.RemoteIPDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.S3BucketDetail
import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.GuardDuty.Types.SortCriteria
import Network.AWS.GuardDuty.Types.Tag
import Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
import Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.GuardDuty.Types.Total
import Network.AWS.GuardDuty.Types.UnprocessedAccount
import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageCriteria
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import Network.AWS.GuardDuty.Types.UsageStatisticType
import Network.AWS.GuardDuty.Types.UsageStatistics
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
guardDuty :: Service
guardDuty =
  Service
    { _svcAbbrev = "GuardDuty",
      _svcSigner = v4,
      _svcPrefix = "guardduty",
      _svcVersion = "2017-11-28",
      _svcEndpoint = defaultEndpoint guardDuty,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "GuardDuty",
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
