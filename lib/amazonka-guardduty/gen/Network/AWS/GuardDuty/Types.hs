-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InternalServerErrorException,
    _BadRequestException,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    mkOrganizationDataSourceConfigurationsResult,
    odscrS3Logs,

    -- * OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (..),
    mkOrganizationS3LogsConfiguration,
    oslcAutoEnable,

    -- * Destination
    Destination (..),
    mkDestination,
    dDestinationId,
    dDestinationType,
    dStatus,

    -- * Email
    Email (..),

    -- * RemoteIpDetails
    RemoteIpDetails (..),
    mkRemoteIpDetails,
    ridCity,
    ridCountry,
    ridGeoLocation,
    ridIpAddressV4,
    ridOrganization,

    -- * FilterName
    FilterName (..),

    -- * S3LogsConfiguration
    S3LogsConfiguration (..),
    mkS3LogsConfiguration,
    slcEnable,

    -- * CloudTrailConfigurationResult
    CloudTrailConfigurationResult (..),
    mkCloudTrailConfigurationResult,
    ctcrStatus,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * ThreatIntelligenceDetail
    ThreatIntelligenceDetail (..),
    mkThreatIntelligenceDetail,
    tidThreatListName,
    tidThreatNames,

    -- * ClientToken
    ClientToken (..),

    -- * Location
    Location (..),

    -- * OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (..),
    mkOrganizationS3LogsConfigurationResult,
    oslcrAutoEnable,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    mkOrganizationDataSourceConfigurations,
    odscS3Logs,

    -- * UsageAccountResult
    UsageAccountResult (..),
    mkUsageAccountResult,
    uarAccountId,
    uarTotal,

    -- * MemberDataSourceConfiguration
    MemberDataSourceConfiguration (..),
    mkMemberDataSourceConfiguration,
    mdscAccountId,
    mdscDataSources,

    -- * PublishingStatus
    PublishingStatus (..),

    -- * String
    String (..),

    -- * ServiceInfo
    ServiceInfo (..),
    mkServiceInfo,
    siAction,
    siArchived,
    siCount,
    siDetectorId,
    siEventFirstSeen,
    siEventLastSeen,
    siEvidence,
    siResourceRole,
    siServiceName,
    siUserFeedback,

    -- * OrderBy
    OrderBy (..),

    -- * Finding
    Finding (..),
    mkFinding,
    fAccountId,
    fArn,
    fCreatedAt,
    fId,
    fRegion,
    fResource,
    fSchemaVersion,
    fSeverity,
    fType,
    fUpdatedAt,
    fConfidence,
    fDescription,
    fPartition,
    fService,
    fTitle,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    mkUnprocessedAccount,
    uaAccountId,
    uaResult,

    -- * Country
    Country (..),
    mkCountry,
    cCountryCode,
    cCountryName,

    -- * FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    mkBucketLevelPermissions,
    blpAccessControlList,
    blpBlockPublicAccess,
    blpBucketPolicy,

    -- * UsageStatistics
    UsageStatistics (..),
    mkUsageStatistics,
    usSumByAccount,
    usSumByDataSource,
    usSumByResource,
    usTopResources,

    -- * PrivateIpAddressDetails
    PrivateIpAddressDetails (..),
    mkPrivateIpAddressDetails,
    piadPrivateDnsName,
    piadPrivateIpAddress,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
    niIpv6Addresses,
    niNetworkInterfaceId,
    niPrivateDnsName,
    niPrivateIpAddress,
    niPrivateIpAddresses,
    niPublicDnsName,
    niPublicIp,
    niSecurityGroups,
    niSubnetId,
    niVpcId,

    -- * FindingStatisticType
    FindingStatisticType (..),

    -- * AdminStatus
    AdminStatus (..),

    -- * FindingCriteria
    FindingCriteria (..),
    mkFindingCriteria,
    fcCriterion,

    -- * Action
    Action (..),
    mkAction,
    aActionType,
    aAwsApiCallAction,
    aDnsRequestAction,
    aNetworkConnectionAction,
    aPortProbeAction,

    -- * AdminAccount
    AdminAccount (..),
    mkAdminAccount,
    aaAdminAccountId,
    aaAdminStatus,

    -- * DataSourceConfigurations
    DataSourceConfigurations (..),
    mkDataSourceConfigurations,
    dscS3Logs,

    -- * FilterDescription
    FilterDescription (..),

    -- * LocalIpDetails
    LocalIpDetails (..),
    mkLocalIpDetails,
    lidIpAddressV4,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    mkNetworkConnectionAction,
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalIpDetails,
    ncaLocalPortDetails,
    ncaProtocol,
    ncaRemoteIpDetails,
    ncaRemotePortDetails,

    -- * TagValue
    TagValue (..),

    -- * UsageDataSourceResult
    UsageDataSourceResult (..),
    mkUsageDataSourceResult,
    udsrDataSource,
    udsrTotal,

    -- * SortCriteria
    SortCriteria (..),
    mkSortCriteria,
    scAttributeName,
    scOrderBy,

    -- * Owner
    Owner (..),
    mkOwner,
    oId,

    -- * Invitation
    Invitation (..),
    mkInvitation,
    iAccountId,
    iInvitationId,
    iInvitedAt,
    iRelationshipStatus,

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgGroupId,
    sgGroupName,

    -- * AccessControlList
    AccessControlList (..),
    mkAccessControlList,
    aclAllowsPublicReadAccess,
    aclAllowsPublicWriteAccess,

    -- * AccountId
    AccountId (..),

    -- * GuardDutyArn
    GuardDutyArn (..),

    -- * FlowLogsConfigurationResult
    FlowLogsConfigurationResult (..),
    mkFlowLogsConfigurationResult,
    flcrStatus,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    mkBlockPublicAccess,
    bpaBlockPublicAcls,
    bpaBlockPublicPolicy,
    bpaIgnorePublicAcls,
    bpaRestrictPublicBuckets,

    -- * DestinationType
    DestinationType (..),

    -- * Master
    Master (..),
    mkMaster,
    mfAccountId,
    mfInvitationId,
    mfInvitedAt,
    mfRelationshipStatus,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    mkAccountLevelPermissions,
    alpBlockPublicAccess,

    -- * DetectorId
    DetectorId (..),

    -- * City
    City (..),
    mkCity,
    cCityName,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    mkDNSLogsConfigurationResult,
    dnslcrStatus,

    -- * S3BucketDetail
    S3BucketDetail (..),
    mkS3BucketDetail,
    sbdArn,
    sbdCreatedAt,
    sbdDefaultServerSideEncryption,
    sbdName,
    sbdOwner,
    sbdPublicAccess,
    sbdTags,
    sbdType,

    -- * RemotePortDetails
    RemotePortDetails (..),
    mkRemotePortDetails,
    rpdPort,
    rpdPortName,

    -- * Name
    Name (..),

    -- * IpSetStatus
    IpSetStatus (..),

    -- * IamInstanceProfile
    IamInstanceProfile (..),
    mkIamInstanceProfile,
    iipArn,
    iipId,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    mkDataSourceConfigurationsResult,
    dscrCloudTrail,
    dscrDNSLogs,
    dscrFlowLogs,
    dscrS3Logs,

    -- * Total
    Total (..),
    mkTotal,
    tAmount,
    tUnit,

    -- * DomainDetails
    DomainDetails (..),
    mkDomainDetails,
    ddDomain,

    -- * PortProbeDetail
    PortProbeDetail (..),
    mkPortProbeDetail,
    ppdLocalIpDetails,
    ppdLocalPortDetails,
    ppdRemoteIpDetails,

    -- * DefaultServerSideEncryption
    DefaultServerSideEncryption (..),
    mkDefaultServerSideEncryption,
    dsseEncryptionType,
    dsseKmsMasterKeyArn,

    -- * Resource
    Resource (..),
    mkResource,
    rAccessKeyDetails,
    rInstanceDetails,
    rResourceType,
    rS3BucketDetails,

    -- * ThreatIntelSetStatus
    ThreatIntelSetStatus (..),

    -- * FindingType
    FindingType (..),

    -- * DataSource
    DataSource (..),

    -- * DestinationProperties
    DestinationProperties (..),
    mkDestinationProperties,
    dpDestinationArn,
    dpKmsKeyArn,

    -- * PublicAccess
    PublicAccess (..),
    mkPublicAccess,
    paEffectivePermission,
    paPermissionConfiguration,

    -- * ThreatIntelSetFormat
    ThreatIntelSetFormat (..),

    -- * PortProbeAction
    PortProbeAction (..),
    mkPortProbeAction,
    ppaBlocked,
    ppaPortProbeDetails,

    -- * TagKey
    TagKey (..),

    -- * Member
    Member (..),
    mkMember,
    mAccountId,
    mMasterId,
    mEmail,
    mRelationshipStatus,
    mUpdatedAt,
    mDetectorId,
    mInvitedAt,

    -- * AccountDetail
    AccountDetail (..),
    mkAccountDetail,
    adAccountId,
    adEmail,

    -- * IpSetFormat
    IpSetFormat (..),

    -- * FindingId
    FindingId (..),

    -- * GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glLat,
    glLon,

    -- * FindingStatistics
    FindingStatistics (..),
    mkFindingStatistics,
    fsCountBySeverity,

    -- * UsageStatisticType
    UsageStatisticType (..),

    -- * Condition
    Condition (..),
    mkCondition,
    cEq,
    cEquals,
    cGreaterThan,
    cGreaterThanOrEqual,
    cGt,
    cGte,
    cLessThan,
    cLessThanOrEqual,
    cLt,
    cLte,
    cNeq,
    cNotEquals,

    -- * PermissionConfiguration
    PermissionConfiguration (..),
    mkPermissionConfiguration,
    pcAccountLevelPermissions,
    pcBucketLevelPermissions,

    -- * Organization
    Organization (..),
    mkOrganization,
    oAsn,
    oAsnOrg,
    oIsp,
    oOrg,

    -- * InstanceDetails
    InstanceDetails (..),
    mkInstanceDetails,
    idAvailabilityZone,
    idIamInstanceProfile,
    idImageDescription,
    idImageId,
    idInstanceId,
    idInstanceState,
    idInstanceType,
    idLaunchTime,
    idNetworkInterfaces,
    idOutpostArn,
    idPlatform,
    idProductCodes,
    idTags,

    -- * Evidence
    Evidence (..),
    mkEvidence,
    eThreatIntelligenceDetails,

    -- * BucketPolicy
    BucketPolicy (..),
    mkBucketPolicy,
    bpAllowsPublicReadAccess,
    bpAllowsPublicWriteAccess,

    -- * ProductCode
    ProductCode (..),
    mkProductCode,
    pcCode,
    pcProductType,

    -- * FilterAction
    FilterAction (..),

    -- * UsageCriteria
    UsageCriteria (..),
    mkUsageCriteria,
    ucDataSources,
    ucAccountIds,
    ucResources,

    -- * DetectorStatus
    DetectorStatus (..),

    -- * DnsRequestAction
    DnsRequestAction (..),
    mkDnsRequestAction,
    draDomain,

    -- * UsageResourceResult
    UsageResourceResult (..),
    mkUsageResourceResult,
    urrResource,
    urrTotal,

    -- * S3LogsConfigurationResult
    S3LogsConfigurationResult (..),
    mkS3LogsConfigurationResult,
    slcrStatus,

    -- * LocalPortDetails
    LocalPortDetails (..),
    mkLocalPortDetails,
    lpdPort,
    lpdPortName,

    -- * Feedback
    Feedback (..),

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    mkAwsApiCallAction,
    aacaApi,
    aacaCallerType,
    aacaDomainDetails,
    aacaErrorCode,
    aacaRemoteIpDetails,
    aacaServiceName,

    -- * AccessKeyDetails
    AccessKeyDetails (..),
    mkAccessKeyDetails,
    akdAccessKeyId,
    akdPrincipalId,
    akdUserName,
    akdUserType,

    -- * DataSourceStatus
    DataSourceStatus (..),

    -- * NextToken
    NextToken (..),

    -- * IpSetId
    IpSetId (..),

    -- * DestinationId
    DestinationId (..),

    -- * IpAddressV4
    IpAddressV4 (..),

    -- * Unit
    Unit (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * ThreatListName
    ThreatListName (..),

    -- * Description
    Description (..),

    -- * Type
    Type (..),

    -- * ResourceArn
    ResourceArn (..),
  )
where

import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.AccountDetail
import Network.AWS.GuardDuty.Types.AccountId
import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.AdminAccount
import Network.AWS.GuardDuty.Types.AdminStatus
import Network.AWS.GuardDuty.Types.AwsApiCallAction
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import Network.AWS.GuardDuty.Types.BucketPolicy
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.ClientToken
import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.DataSource
import Network.AWS.GuardDuty.Types.DataSourceConfigurations
import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.DataSourceStatus
import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Description
import Network.AWS.GuardDuty.Types.Destination
import Network.AWS.GuardDuty.Types.DestinationId
import Network.AWS.GuardDuty.Types.DestinationProperties
import Network.AWS.GuardDuty.Types.DestinationType
import Network.AWS.GuardDuty.Types.DetectorId
import Network.AWS.GuardDuty.Types.DetectorStatus
import Network.AWS.GuardDuty.Types.DnsRequestAction
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.Email
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.GuardDuty.Types.Feedback
import Network.AWS.GuardDuty.Types.FilterAction
import Network.AWS.GuardDuty.Types.FilterDescription
import Network.AWS.GuardDuty.Types.FilterName
import Network.AWS.GuardDuty.Types.Finding
import Network.AWS.GuardDuty.Types.FindingCriteria
import Network.AWS.GuardDuty.Types.FindingId
import Network.AWS.GuardDuty.Types.FindingPublishingFrequency
import Network.AWS.GuardDuty.Types.FindingStatisticType
import Network.AWS.GuardDuty.Types.FindingStatistics
import Network.AWS.GuardDuty.Types.FindingType
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.GuardDutyArn
import Network.AWS.GuardDuty.Types.IamInstanceProfile
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.Invitation
import Network.AWS.GuardDuty.Types.IpAddressV4
import Network.AWS.GuardDuty.Types.IpSetFormat
import Network.AWS.GuardDuty.Types.IpSetId
import Network.AWS.GuardDuty.Types.IpSetStatus
import Network.AWS.GuardDuty.Types.Key
import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.Location
import Network.AWS.GuardDuty.Types.Master
import Network.AWS.GuardDuty.Types.Member
import Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
import Network.AWS.GuardDuty.Types.Name
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.NextToken
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
import Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.PublishingStatus
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.ResourceArn
import Network.AWS.GuardDuty.Types.S3BucketDetail
import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.GuardDuty.Types.SortCriteria
import Network.AWS.GuardDuty.Types.String
import Network.AWS.GuardDuty.Types.Tag
import Network.AWS.GuardDuty.Types.TagKey
import Network.AWS.GuardDuty.Types.TagValue
import Network.AWS.GuardDuty.Types.ThreatIntelSetFormat
import Network.AWS.GuardDuty.Types.ThreatIntelSetStatus
import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.GuardDuty.Types.ThreatListName
import Network.AWS.GuardDuty.Types.Total
import Network.AWS.GuardDuty.Types.Type
import Network.AWS.GuardDuty.Types.Unit
import Network.AWS.GuardDuty.Types.UnprocessedAccount
import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageCriteria
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import Network.AWS.GuardDuty.Types.UsageStatisticType
import Network.AWS.GuardDuty.Types.UsageStatistics
import Network.AWS.GuardDuty.Types.Value
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "GuardDuty",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "guardduty",
      Core._svcVersion = "2017-11-28",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "GuardDuty",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | An internal server error exception object.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | A bad request exception object.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}
