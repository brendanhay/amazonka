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
    guardDutyService,

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
    AWSAPICallAction (..),
    mkAWSAPICallAction,
    aacaRemoteIPDetails,
    aacaCallerType,
    aacaDomainDetails,
    aacaServiceName,
    aacaErrorCode,
    aacaAPI,

    -- * AccessControlList
    AccessControlList (..),
    mkAccessControlList,
    aclAllowsPublicWriteAccess,
    aclAllowsPublicReadAccess,

    -- * AccessKeyDetails
    AccessKeyDetails (..),
    mkAccessKeyDetails,
    akdPrincipalId,
    akdUserName,
    akdAccessKeyId,
    akdUserType,

    -- * AccountDetail
    AccountDetail (..),
    mkAccountDetail,
    adAccountId,
    adEmail,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    mkAccountLevelPermissions,
    alpBlockPublicAccess,

    -- * Action
    Action (..),
    mkAction,
    aNetworkConnectionAction,
    aPortProbeAction,
    aActionType,
    aDNSRequestAction,
    aAWSAPICallAction,

    -- * AdminAccount
    AdminAccount (..),
    mkAdminAccount,
    aaAdminAccountId,
    aaAdminStatus,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    mkBlockPublicAccess,
    bpaIgnorePublicACLs,
    bpaBlockPublicACLs,
    bpaRestrictPublicBuckets,
    bpaBlockPublicPolicy,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    mkBucketLevelPermissions,
    blpAccessControlList,
    blpBlockPublicAccess,
    blpBucketPolicy,

    -- * BucketPolicy
    BucketPolicy (..),
    mkBucketPolicy,
    bpAllowsPublicWriteAccess,
    bpAllowsPublicReadAccess,

    -- * City
    City (..),
    mkCity,
    cCityName,

    -- * CloudTrailConfigurationResult
    CloudTrailConfigurationResult (..),
    mkCloudTrailConfigurationResult,
    ctcrStatus,

    -- * Condition
    Condition (..),
    mkCondition,
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
    Country (..),
    mkCountry,
    cCountryName,
    cCountryCode,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    mkDNSLogsConfigurationResult,
    dlcrStatus,

    -- * DNSRequestAction
    DNSRequestAction (..),
    mkDNSRequestAction,
    draDomain,

    -- * DataSourceConfigurations
    DataSourceConfigurations (..),
    mkDataSourceConfigurations,
    dscS3Logs,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    mkDataSourceConfigurationsResult,
    dscrCloudTrail,
    dscrDNSLogs,
    dscrFlowLogs,
    dscrS3Logs,

    -- * DefaultServerSideEncryption
    DefaultServerSideEncryption (..),
    mkDefaultServerSideEncryption,
    dsseEncryptionType,
    dsseKMSMasterKeyARN,

    -- * Destination
    Destination (..),
    mkDestination,
    dDestinationId,
    dDestinationType,
    dStatus,

    -- * DestinationProperties
    DestinationProperties (..),
    mkDestinationProperties,
    dpKMSKeyARN,
    dpDestinationARN,

    -- * DomainDetails
    DomainDetails (..),
    mkDomainDetails,
    ddDomain,

    -- * Evidence
    Evidence (..),
    mkEvidence,
    eThreatIntelligenceDetails,

    -- * Finding
    Finding (..),
    mkFinding,
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
    FindingCriteria (..),
    mkFindingCriteria,
    fcCriterion,

    -- * FindingStatistics
    FindingStatistics (..),
    mkFindingStatistics,
    fsCountBySeverity,

    -- * FlowLogsConfigurationResult
    FlowLogsConfigurationResult (..),
    mkFlowLogsConfigurationResult,
    flcrStatus,

    -- * GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glLat,
    glLon,

    -- * IAMInstanceProfile
    IAMInstanceProfile (..),
    mkIAMInstanceProfile,
    iapARN,
    iapId,

    -- * InstanceDetails
    InstanceDetails (..),
    mkInstanceDetails,
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
    Invitation (..),
    mkInvitation,
    iInvitedAt,
    iRelationshipStatus,
    iInvitationId,
    iAccountId,

    -- * LocalIPDetails
    LocalIPDetails (..),
    mkLocalIPDetails,
    lidIPAddressV4,

    -- * LocalPortDetails
    LocalPortDetails (..),
    mkLocalPortDetails,
    lpdPortName,
    lpdPort,

    -- * Master
    Master (..),
    mkMaster,
    masInvitedAt,
    masRelationshipStatus,
    masInvitationId,
    masAccountId,

    -- * Member
    Member (..),
    mkMember,
    mInvitedAt,
    mDetectorId,
    mAccountId,
    mMasterId,
    mEmail,
    mRelationshipStatus,
    mUpdatedAt,

    -- * MemberDataSourceConfiguration
    MemberDataSourceConfiguration (..),
    mkMemberDataSourceConfiguration,
    mdscAccountId,
    mdscDataSources,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    mkNetworkConnectionAction,
    ncaRemoteIPDetails,
    ncaProtocol,
    ncaLocalIPDetails,
    ncaRemotePortDetails,
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalPortDetails,

    -- * NetworkInterface
    NetworkInterface (..),
    mkNetworkInterface,
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
    Organization (..),
    mkOrganization,
    oOrg,
    oASNOrg,
    oASN,
    oIsp,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    mkOrganizationDataSourceConfigurations,
    odscS3Logs,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    mkOrganizationDataSourceConfigurationsResult,
    odscrS3Logs,

    -- * OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (..),
    mkOrganizationS3LogsConfiguration,
    oslcAutoEnable,

    -- * OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (..),
    mkOrganizationS3LogsConfigurationResult,
    oslcrAutoEnable,

    -- * Owner
    Owner (..),
    mkOwner,
    oId,

    -- * PermissionConfiguration
    PermissionConfiguration (..),
    mkPermissionConfiguration,
    pcBucketLevelPermissions,
    pcAccountLevelPermissions,

    -- * PortProbeAction
    PortProbeAction (..),
    mkPortProbeAction,
    ppaPortProbeDetails,
    ppaBlocked,

    -- * PortProbeDetail
    PortProbeDetail (..),
    mkPortProbeDetail,
    ppdRemoteIPDetails,
    ppdLocalIPDetails,
    ppdLocalPortDetails,

    -- * PrivateIPAddressDetails
    PrivateIPAddressDetails (..),
    mkPrivateIPAddressDetails,
    piadPrivateIPAddress,
    piadPrivateDNSName,

    -- * ProductCode
    ProductCode (..),
    mkProductCode,
    pcProductType,
    pcCode,

    -- * PublicAccess
    PublicAccess (..),
    mkPublicAccess,
    paPermissionConfiguration,
    paEffectivePermission,

    -- * RemoteIPDetails
    RemoteIPDetails (..),
    mkRemoteIPDetails,
    ridCountry,
    ridCity,
    ridIPAddressV4,
    ridGeoLocation,
    ridOrganization,

    -- * RemotePortDetails
    RemotePortDetails (..),
    mkRemotePortDetails,
    rpdPortName,
    rpdPort,

    -- * Resource
    Resource (..),
    mkResource,
    rResourceType,
    rS3BucketDetails,
    rInstanceDetails,
    rAccessKeyDetails,

    -- * S3BucketDetail
    S3BucketDetail (..),
    mkS3BucketDetail,
    sbdARN,
    sbdCreatedAt,
    sbdOwner,
    sbdName,
    sbdDefaultServerSideEncryption,
    sbdPublicAccess,
    sbdType,
    sbdTags,

    -- * S3LogsConfiguration
    S3LogsConfiguration (..),
    mkS3LogsConfiguration,
    slcEnable,

    -- * S3LogsConfigurationResult
    S3LogsConfigurationResult (..),
    mkS3LogsConfigurationResult,
    slcrStatus,

    -- * SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgGroupId,
    sgGroupName,

    -- * ServiceInfo
    ServiceInfo (..),
    mkServiceInfo,
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
    SortCriteria (..),
    mkSortCriteria,
    scOrderBy,
    scAttributeName,

    -- * Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * ThreatIntelligenceDetail
    ThreatIntelligenceDetail (..),
    mkThreatIntelligenceDetail,
    tidThreatNames,
    tidThreatListName,

    -- * Total
    Total (..),
    mkTotal,
    tAmount,
    tUnit,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    mkUnprocessedAccount,
    uaAccountId,
    uaResult,

    -- * UsageAccountResult
    UsageAccountResult (..),
    mkUsageAccountResult,
    uarAccountId,
    uarTotal,

    -- * UsageCriteria
    UsageCriteria (..),
    mkUsageCriteria,
    ucAccountIds,
    ucResources,
    ucDataSources,

    -- * UsageDataSourceResult
    UsageDataSourceResult (..),
    mkUsageDataSourceResult,
    udsrTotal,
    udsrDataSource,

    -- * UsageResourceResult
    UsageResourceResult (..),
    mkUsageResourceResult,
    urrTotal,
    urrResource,

    -- * UsageStatistics
    UsageStatistics (..),
    mkUsageStatistics,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
guardDutyService :: Lude.Service
guardDutyService =
  Lude.Service
    { Lude._svcAbbrev = "GuardDuty",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "guardduty",
      Lude._svcVersion = "2017-11-28",
      Lude._svcEndpoint = Lude.defaultEndpoint guardDutyService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "GuardDuty",
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
