{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon GuardDuty is a continuous security monitoring service that analyzes and processes the following data sources: VPC Flow Logs, AWS CloudTrail event logs, and DNS logs. It uses threat intelligence feeds (such as lists of malicious IPs and domains) and machine learning to identify unexpected, potentially unauthorized, and malicious activity within your AWS environment. This can include issues like escalations of privileges, uses of exposed credentials, or communication with malicious IPs, URLs, or domains. For example, GuardDuty can detect compromised EC2 instances that serve malware or mine bitcoin.
--
-- GuardDuty also monitors AWS account access behavior for signs of compromise. Some examples of this are unauthorized infrastructure deployments such as EC2 instances deployed in a Region that has never been used, or unusual API calls like a password policy change to reduce password strength.
-- GuardDuty informs you of the status of your AWS environment by producing security findings that you can view in the GuardDuty console or through Amazon CloudWatch events. For more information, see the /<https:\/\/docs.aws.amazon.com\/guardduty\/latest\/ug\/what-is-guardduty.html Amazon GuardDuty User Guide> / .
module Network.AWS.GuardDuty
  ( -- * Service configuration
    guardDutyService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateFilter
    module Network.AWS.GuardDuty.CreateFilter,

    -- ** EnableOrganizationAdminAccount
    module Network.AWS.GuardDuty.EnableOrganizationAdminAccount,

    -- ** ListFindings (Paginated)
    module Network.AWS.GuardDuty.ListFindings,

    -- ** ListOrganizationAdminAccounts (Paginated)
    module Network.AWS.GuardDuty.ListOrganizationAdminAccounts,

    -- ** CreateIPSet
    module Network.AWS.GuardDuty.CreateIPSet,

    -- ** DeleteThreatIntelSet
    module Network.AWS.GuardDuty.DeleteThreatIntelSet,

    -- ** UpdateThreatIntelSet
    module Network.AWS.GuardDuty.UpdateThreatIntelSet,

    -- ** StopMonitoringMembers
    module Network.AWS.GuardDuty.StopMonitoringMembers,

    -- ** ListThreatIntelSets (Paginated)
    module Network.AWS.GuardDuty.ListThreatIntelSets,

    -- ** CreateThreatIntelSet
    module Network.AWS.GuardDuty.CreateThreatIntelSet,

    -- ** DeleteMembers
    module Network.AWS.GuardDuty.DeleteMembers,

    -- ** ListTagsForResource
    module Network.AWS.GuardDuty.ListTagsForResource,

    -- ** GetFindingsStatistics
    module Network.AWS.GuardDuty.GetFindingsStatistics,

    -- ** GetIPSet
    module Network.AWS.GuardDuty.GetIPSet,

    -- ** ListInvitations (Paginated)
    module Network.AWS.GuardDuty.ListInvitations,

    -- ** UpdateMemberDetectors
    module Network.AWS.GuardDuty.UpdateMemberDetectors,

    -- ** GetThreatIntelSet
    module Network.AWS.GuardDuty.GetThreatIntelSet,

    -- ** DeleteInvitations
    module Network.AWS.GuardDuty.DeleteInvitations,

    -- ** GetMasterAccount
    module Network.AWS.GuardDuty.GetMasterAccount,

    -- ** GetUsageStatistics
    module Network.AWS.GuardDuty.GetUsageStatistics,

    -- ** CreateDetector
    module Network.AWS.GuardDuty.CreateDetector,

    -- ** DeclineInvitations
    module Network.AWS.GuardDuty.DeclineInvitations,

    -- ** DescribeOrganizationConfiguration
    module Network.AWS.GuardDuty.DescribeOrganizationConfiguration,

    -- ** CreatePublishingDestination
    module Network.AWS.GuardDuty.CreatePublishingDestination,

    -- ** UpdateFilter
    module Network.AWS.GuardDuty.UpdateFilter,

    -- ** DeleteFilter
    module Network.AWS.GuardDuty.DeleteFilter,

    -- ** DisassociateMembers
    module Network.AWS.GuardDuty.DisassociateMembers,

    -- ** DisassociateFromMasterAccount
    module Network.AWS.GuardDuty.DisassociateFromMasterAccount,

    -- ** AcceptInvitation
    module Network.AWS.GuardDuty.AcceptInvitation,

    -- ** ListFilters (Paginated)
    module Network.AWS.GuardDuty.ListFilters,

    -- ** ListMembers (Paginated)
    module Network.AWS.GuardDuty.ListMembers,

    -- ** ListPublishingDestinations
    module Network.AWS.GuardDuty.ListPublishingDestinations,

    -- ** DeletePublishingDestination
    module Network.AWS.GuardDuty.DeletePublishingDestination,

    -- ** UpdatePublishingDestination
    module Network.AWS.GuardDuty.UpdatePublishingDestination,

    -- ** GetDetector
    module Network.AWS.GuardDuty.GetDetector,

    -- ** CreateSampleFindings
    module Network.AWS.GuardDuty.CreateSampleFindings,

    -- ** ArchiveFindings
    module Network.AWS.GuardDuty.ArchiveFindings,

    -- ** CreateMembers
    module Network.AWS.GuardDuty.CreateMembers,

    -- ** UnarchiveFindings
    module Network.AWS.GuardDuty.UnarchiveFindings,

    -- ** GetMemberDetectors
    module Network.AWS.GuardDuty.GetMemberDetectors,

    -- ** GetInvitationsCount
    module Network.AWS.GuardDuty.GetInvitationsCount,

    -- ** StartMonitoringMembers
    module Network.AWS.GuardDuty.StartMonitoringMembers,

    -- ** UpdateOrganizationConfiguration
    module Network.AWS.GuardDuty.UpdateOrganizationConfiguration,

    -- ** InviteMembers
    module Network.AWS.GuardDuty.InviteMembers,

    -- ** DeleteIPSet
    module Network.AWS.GuardDuty.DeleteIPSet,

    -- ** UpdateIPSet
    module Network.AWS.GuardDuty.UpdateIPSet,

    -- ** ListIPSets (Paginated)
    module Network.AWS.GuardDuty.ListIPSets,

    -- ** GetMembers
    module Network.AWS.GuardDuty.GetMembers,

    -- ** DescribePublishingDestination
    module Network.AWS.GuardDuty.DescribePublishingDestination,

    -- ** TagResource
    module Network.AWS.GuardDuty.TagResource,

    -- ** GetFindings
    module Network.AWS.GuardDuty.GetFindings,

    -- ** ListDetectors (Paginated)
    module Network.AWS.GuardDuty.ListDetectors,

    -- ** UntagResource
    module Network.AWS.GuardDuty.UntagResource,

    -- ** UpdateDetector
    module Network.AWS.GuardDuty.UpdateDetector,

    -- ** DeleteDetector
    module Network.AWS.GuardDuty.DeleteDetector,

    -- ** UpdateFindingsFeedback
    module Network.AWS.GuardDuty.UpdateFindingsFeedback,

    -- ** GetFilter
    module Network.AWS.GuardDuty.GetFilter,

    -- ** DisableOrganizationAdminAccount
    module Network.AWS.GuardDuty.DisableOrganizationAdminAccount,

    -- * Types

    -- ** AdminStatus
    AdminStatus (..),

    -- ** DataSource
    DataSource (..),

    -- ** DataSourceStatus
    DataSourceStatus (..),

    -- ** DestinationType
    DestinationType (..),

    -- ** DetectorStatus
    DetectorStatus (..),

    -- ** Feedback
    Feedback (..),

    -- ** FilterAction
    FilterAction (..),

    -- ** FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- ** FindingStatisticType
    FindingStatisticType (..),

    -- ** IPSetFormat
    IPSetFormat (..),

    -- ** IPSetStatus
    IPSetStatus (..),

    -- ** OrderBy
    OrderBy (..),

    -- ** PublishingStatus
    PublishingStatus (..),

    -- ** ThreatIntelSetFormat
    ThreatIntelSetFormat (..),

    -- ** ThreatIntelSetStatus
    ThreatIntelSetStatus (..),

    -- ** UsageStatisticType
    UsageStatisticType (..),

    -- ** AWSAPICallAction
    AWSAPICallAction (..),
    mkAWSAPICallAction,
    aacaRemoteIPDetails,
    aacaCallerType,
    aacaDomainDetails,
    aacaServiceName,
    aacaErrorCode,
    aacaAPI,

    -- ** AccessControlList
    AccessControlList (..),
    mkAccessControlList,
    aclAllowsPublicWriteAccess,
    aclAllowsPublicReadAccess,

    -- ** AccessKeyDetails
    AccessKeyDetails (..),
    mkAccessKeyDetails,
    akdPrincipalId,
    akdUserName,
    akdAccessKeyId,
    akdUserType,

    -- ** AccountDetail
    AccountDetail (..),
    mkAccountDetail,
    adAccountId,
    adEmail,

    -- ** AccountLevelPermissions
    AccountLevelPermissions (..),
    mkAccountLevelPermissions,
    alpBlockPublicAccess,

    -- ** Action
    Action (..),
    mkAction,
    aNetworkConnectionAction,
    aPortProbeAction,
    aActionType,
    aDNSRequestAction,
    aAWSAPICallAction,

    -- ** AdminAccount
    AdminAccount (..),
    mkAdminAccount,
    aaAdminAccountId,
    aaAdminStatus,

    -- ** BlockPublicAccess
    BlockPublicAccess (..),
    mkBlockPublicAccess,
    bpaIgnorePublicACLs,
    bpaBlockPublicACLs,
    bpaRestrictPublicBuckets,
    bpaBlockPublicPolicy,

    -- ** BucketLevelPermissions
    BucketLevelPermissions (..),
    mkBucketLevelPermissions,
    blpAccessControlList,
    blpBlockPublicAccess,
    blpBucketPolicy,

    -- ** BucketPolicy
    BucketPolicy (..),
    mkBucketPolicy,
    bpAllowsPublicWriteAccess,
    bpAllowsPublicReadAccess,

    -- ** City
    City (..),
    mkCity,
    cCityName,

    -- ** CloudTrailConfigurationResult
    CloudTrailConfigurationResult (..),
    mkCloudTrailConfigurationResult,
    ctcrStatus,

    -- ** Condition
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

    -- ** Country
    Country (..),
    mkCountry,
    cCountryName,
    cCountryCode,

    -- ** DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    mkDNSLogsConfigurationResult,
    dlcrStatus,

    -- ** DNSRequestAction
    DNSRequestAction (..),
    mkDNSRequestAction,
    draDomain,

    -- ** DataSourceConfigurations
    DataSourceConfigurations (..),
    mkDataSourceConfigurations,
    dscS3Logs,

    -- ** DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    mkDataSourceConfigurationsResult,
    dscrCloudTrail,
    dscrDNSLogs,
    dscrFlowLogs,
    dscrS3Logs,

    -- ** DefaultServerSideEncryption
    DefaultServerSideEncryption (..),
    mkDefaultServerSideEncryption,
    dsseEncryptionType,
    dsseKMSMasterKeyARN,

    -- ** Destination
    Destination (..),
    mkDestination,
    dDestinationId,
    dDestinationType,
    dStatus,

    -- ** DestinationProperties
    DestinationProperties (..),
    mkDestinationProperties,
    dpKMSKeyARN,
    dpDestinationARN,

    -- ** DomainDetails
    DomainDetails (..),
    mkDomainDetails,
    ddDomain,

    -- ** Evidence
    Evidence (..),
    mkEvidence,
    eThreatIntelligenceDetails,

    -- ** Finding
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

    -- ** FindingCriteria
    FindingCriteria (..),
    mkFindingCriteria,
    fcCriterion,

    -- ** FindingStatistics
    FindingStatistics (..),
    mkFindingStatistics,
    fsCountBySeverity,

    -- ** FlowLogsConfigurationResult
    FlowLogsConfigurationResult (..),
    mkFlowLogsConfigurationResult,
    flcrStatus,

    -- ** GeoLocation
    GeoLocation (..),
    mkGeoLocation,
    glLat,
    glLon,

    -- ** IAMInstanceProfile
    IAMInstanceProfile (..),
    mkIAMInstanceProfile,
    iapARN,
    iapId,

    -- ** InstanceDetails
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

    -- ** Invitation
    Invitation (..),
    mkInvitation,
    iInvitedAt,
    iRelationshipStatus,
    iInvitationId,
    iAccountId,

    -- ** LocalIPDetails
    LocalIPDetails (..),
    mkLocalIPDetails,
    lidIPAddressV4,

    -- ** LocalPortDetails
    LocalPortDetails (..),
    mkLocalPortDetails,
    lpdPortName,
    lpdPort,

    -- ** Master
    Master (..),
    mkMaster,
    masInvitedAt,
    masRelationshipStatus,
    masInvitationId,
    masAccountId,

    -- ** Member
    Member (..),
    mkMember,
    mInvitedAt,
    mDetectorId,
    mAccountId,
    mMasterId,
    mEmail,
    mRelationshipStatus,
    mUpdatedAt,

    -- ** MemberDataSourceConfiguration
    MemberDataSourceConfiguration (..),
    mkMemberDataSourceConfiguration,
    mdscAccountId,
    mdscDataSources,

    -- ** NetworkConnectionAction
    NetworkConnectionAction (..),
    mkNetworkConnectionAction,
    ncaRemoteIPDetails,
    ncaProtocol,
    ncaLocalIPDetails,
    ncaRemotePortDetails,
    ncaBlocked,
    ncaConnectionDirection,
    ncaLocalPortDetails,

    -- ** NetworkInterface
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

    -- ** Organization
    Organization (..),
    mkOrganization,
    oOrg,
    oASNOrg,
    oASN,
    oIsp,

    -- ** OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    mkOrganizationDataSourceConfigurations,
    odscS3Logs,

    -- ** OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    mkOrganizationDataSourceConfigurationsResult,
    odscrS3Logs,

    -- ** OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (..),
    mkOrganizationS3LogsConfiguration,
    oslcAutoEnable,

    -- ** OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (..),
    mkOrganizationS3LogsConfigurationResult,
    oslcrAutoEnable,

    -- ** Owner
    Owner (..),
    mkOwner,
    oId,

    -- ** PermissionConfiguration
    PermissionConfiguration (..),
    mkPermissionConfiguration,
    pcBucketLevelPermissions,
    pcAccountLevelPermissions,

    -- ** PortProbeAction
    PortProbeAction (..),
    mkPortProbeAction,
    ppaPortProbeDetails,
    ppaBlocked,

    -- ** PortProbeDetail
    PortProbeDetail (..),
    mkPortProbeDetail,
    ppdRemoteIPDetails,
    ppdLocalIPDetails,
    ppdLocalPortDetails,

    -- ** PrivateIPAddressDetails
    PrivateIPAddressDetails (..),
    mkPrivateIPAddressDetails,
    piadPrivateIPAddress,
    piadPrivateDNSName,

    -- ** ProductCode
    ProductCode (..),
    mkProductCode,
    pcProductType,
    pcCode,

    -- ** PublicAccess
    PublicAccess (..),
    mkPublicAccess,
    paPermissionConfiguration,
    paEffectivePermission,

    -- ** RemoteIPDetails
    RemoteIPDetails (..),
    mkRemoteIPDetails,
    ridCountry,
    ridCity,
    ridIPAddressV4,
    ridGeoLocation,
    ridOrganization,

    -- ** RemotePortDetails
    RemotePortDetails (..),
    mkRemotePortDetails,
    rpdPortName,
    rpdPort,

    -- ** Resource
    Resource (..),
    mkResource,
    rResourceType,
    rS3BucketDetails,
    rInstanceDetails,
    rAccessKeyDetails,

    -- ** S3BucketDetail
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

    -- ** S3LogsConfiguration
    S3LogsConfiguration (..),
    mkS3LogsConfiguration,
    slcEnable,

    -- ** S3LogsConfigurationResult
    S3LogsConfigurationResult (..),
    mkS3LogsConfigurationResult,
    slcrStatus,

    -- ** SecurityGroup
    SecurityGroup (..),
    mkSecurityGroup,
    sgGroupId,
    sgGroupName,

    -- ** ServiceInfo
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

    -- ** SortCriteria
    SortCriteria (..),
    mkSortCriteria,
    scOrderBy,
    scAttributeName,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- ** ThreatIntelligenceDetail
    ThreatIntelligenceDetail (..),
    mkThreatIntelligenceDetail,
    tidThreatNames,
    tidThreatListName,

    -- ** Total
    Total (..),
    mkTotal,
    tAmount,
    tUnit,

    -- ** UnprocessedAccount
    UnprocessedAccount (..),
    mkUnprocessedAccount,
    uaAccountId,
    uaResult,

    -- ** UsageAccountResult
    UsageAccountResult (..),
    mkUsageAccountResult,
    uarAccountId,
    uarTotal,

    -- ** UsageCriteria
    UsageCriteria (..),
    mkUsageCriteria,
    ucAccountIds,
    ucResources,
    ucDataSources,

    -- ** UsageDataSourceResult
    UsageDataSourceResult (..),
    mkUsageDataSourceResult,
    udsrTotal,
    udsrDataSource,

    -- ** UsageResourceResult
    UsageResourceResult (..),
    mkUsageResourceResult,
    urrTotal,
    urrResource,

    -- ** UsageStatistics
    UsageStatistics (..),
    mkUsageStatistics,
    usTopResources,
    usSumByResource,
    usSumByDataSource,
    usSumByAccount,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.ISO8601,
    Lude.Timestamp,
    Lude.UTCTime,
  )
where

import Network.AWS.GuardDuty.AcceptInvitation
import Network.AWS.GuardDuty.ArchiveFindings
import Network.AWS.GuardDuty.CreateDetector
import Network.AWS.GuardDuty.CreateFilter
import Network.AWS.GuardDuty.CreateIPSet
import Network.AWS.GuardDuty.CreateMembers
import Network.AWS.GuardDuty.CreatePublishingDestination
import Network.AWS.GuardDuty.CreateSampleFindings
import Network.AWS.GuardDuty.CreateThreatIntelSet
import Network.AWS.GuardDuty.DeclineInvitations
import Network.AWS.GuardDuty.DeleteDetector
import Network.AWS.GuardDuty.DeleteFilter
import Network.AWS.GuardDuty.DeleteIPSet
import Network.AWS.GuardDuty.DeleteInvitations
import Network.AWS.GuardDuty.DeleteMembers
import Network.AWS.GuardDuty.DeletePublishingDestination
import Network.AWS.GuardDuty.DeleteThreatIntelSet
import Network.AWS.GuardDuty.DescribeOrganizationConfiguration
import Network.AWS.GuardDuty.DescribePublishingDestination
import Network.AWS.GuardDuty.DisableOrganizationAdminAccount
import Network.AWS.GuardDuty.DisassociateFromMasterAccount
import Network.AWS.GuardDuty.DisassociateMembers
import Network.AWS.GuardDuty.EnableOrganizationAdminAccount
import Network.AWS.GuardDuty.GetDetector
import Network.AWS.GuardDuty.GetFilter
import Network.AWS.GuardDuty.GetFindings
import Network.AWS.GuardDuty.GetFindingsStatistics
import Network.AWS.GuardDuty.GetIPSet
import Network.AWS.GuardDuty.GetInvitationsCount
import Network.AWS.GuardDuty.GetMasterAccount
import Network.AWS.GuardDuty.GetMemberDetectors
import Network.AWS.GuardDuty.GetMembers
import Network.AWS.GuardDuty.GetThreatIntelSet
import Network.AWS.GuardDuty.GetUsageStatistics
import Network.AWS.GuardDuty.InviteMembers
import Network.AWS.GuardDuty.ListDetectors
import Network.AWS.GuardDuty.ListFilters
import Network.AWS.GuardDuty.ListFindings
import Network.AWS.GuardDuty.ListIPSets
import Network.AWS.GuardDuty.ListInvitations
import Network.AWS.GuardDuty.ListMembers
import Network.AWS.GuardDuty.ListOrganizationAdminAccounts
import Network.AWS.GuardDuty.ListPublishingDestinations
import Network.AWS.GuardDuty.ListTagsForResource
import Network.AWS.GuardDuty.ListThreatIntelSets
import Network.AWS.GuardDuty.StartMonitoringMembers
import Network.AWS.GuardDuty.StopMonitoringMembers
import Network.AWS.GuardDuty.TagResource
import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.UnarchiveFindings
import Network.AWS.GuardDuty.UntagResource
import Network.AWS.GuardDuty.UpdateDetector
import Network.AWS.GuardDuty.UpdateFilter
import Network.AWS.GuardDuty.UpdateFindingsFeedback
import Network.AWS.GuardDuty.UpdateIPSet
import Network.AWS.GuardDuty.UpdateMemberDetectors
import Network.AWS.GuardDuty.UpdateOrganizationConfiguration
import Network.AWS.GuardDuty.UpdatePublishingDestination
import Network.AWS.GuardDuty.UpdateThreatIntelSet
import Network.AWS.GuardDuty.Waiters
import qualified Network.AWS.Prelude as Lude

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'GuardDuty'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
