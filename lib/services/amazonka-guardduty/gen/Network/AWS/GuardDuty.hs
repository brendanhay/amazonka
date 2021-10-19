{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.GuardDuty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon GuardDuty is a continuous security monitoring service that
-- analyzes and processes the following data sources: VPC Flow Logs, AWS
-- CloudTrail event logs, and DNS logs. It uses threat intelligence feeds
-- (such as lists of malicious IPs and domains) and machine learning to
-- identify unexpected, potentially unauthorized, and malicious activity
-- within your AWS environment. This can include issues like escalations of
-- privileges, uses of exposed credentials, or communication with malicious
-- IPs, URLs, or domains. For example, GuardDuty can detect compromised EC2
-- instances that serve malware or mine bitcoin.
--
-- GuardDuty also monitors AWS account access behavior for signs of
-- compromise. Some examples of this are unauthorized infrastructure
-- deployments such as EC2 instances deployed in a Region that has never
-- been used, or unusual API calls like a password policy change to reduce
-- password strength.
--
-- GuardDuty informs you of the status of your AWS environment by producing
-- security findings that you can view in the GuardDuty console or through
-- Amazon CloudWatch events. For more information, see the
-- /<https://docs.aws.amazon.com/guardduty/latest/ug/what-is-guardduty.html Amazon GuardDuty User Guide>/
-- .
module Network.AWS.GuardDuty
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- ** BadRequestException
    _BadRequestException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateFilter
    CreateFilter (CreateFilter'),
    newCreateFilter,
    CreateFilterResponse (CreateFilterResponse'),
    newCreateFilterResponse,

    -- ** EnableOrganizationAdminAccount
    EnableOrganizationAdminAccount (EnableOrganizationAdminAccount'),
    newEnableOrganizationAdminAccount,
    EnableOrganizationAdminAccountResponse (EnableOrganizationAdminAccountResponse'),
    newEnableOrganizationAdminAccountResponse,

    -- ** ListFindings (Paginated)
    ListFindings (ListFindings'),
    newListFindings,
    ListFindingsResponse (ListFindingsResponse'),
    newListFindingsResponse,

    -- ** ListOrganizationAdminAccounts (Paginated)
    ListOrganizationAdminAccounts (ListOrganizationAdminAccounts'),
    newListOrganizationAdminAccounts,
    ListOrganizationAdminAccountsResponse (ListOrganizationAdminAccountsResponse'),
    newListOrganizationAdminAccountsResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** DeleteThreatIntelSet
    DeleteThreatIntelSet (DeleteThreatIntelSet'),
    newDeleteThreatIntelSet,
    DeleteThreatIntelSetResponse (DeleteThreatIntelSetResponse'),
    newDeleteThreatIntelSetResponse,

    -- ** UpdateThreatIntelSet
    UpdateThreatIntelSet (UpdateThreatIntelSet'),
    newUpdateThreatIntelSet,
    UpdateThreatIntelSetResponse (UpdateThreatIntelSetResponse'),
    newUpdateThreatIntelSetResponse,

    -- ** StopMonitoringMembers
    StopMonitoringMembers (StopMonitoringMembers'),
    newStopMonitoringMembers,
    StopMonitoringMembersResponse (StopMonitoringMembersResponse'),
    newStopMonitoringMembersResponse,

    -- ** ListThreatIntelSets (Paginated)
    ListThreatIntelSets (ListThreatIntelSets'),
    newListThreatIntelSets,
    ListThreatIntelSetsResponse (ListThreatIntelSetsResponse'),
    newListThreatIntelSetsResponse,

    -- ** CreateThreatIntelSet
    CreateThreatIntelSet (CreateThreatIntelSet'),
    newCreateThreatIntelSet,
    CreateThreatIntelSetResponse (CreateThreatIntelSetResponse'),
    newCreateThreatIntelSetResponse,

    -- ** DeleteMembers
    DeleteMembers (DeleteMembers'),
    newDeleteMembers,
    DeleteMembersResponse (DeleteMembersResponse'),
    newDeleteMembersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** GetFindingsStatistics
    GetFindingsStatistics (GetFindingsStatistics'),
    newGetFindingsStatistics,
    GetFindingsStatisticsResponse (GetFindingsStatisticsResponse'),
    newGetFindingsStatisticsResponse,

    -- ** GetIPSet
    GetIPSet (GetIPSet'),
    newGetIPSet,
    GetIPSetResponse (GetIPSetResponse'),
    newGetIPSetResponse,

    -- ** ListInvitations (Paginated)
    ListInvitations (ListInvitations'),
    newListInvitations,
    ListInvitationsResponse (ListInvitationsResponse'),
    newListInvitationsResponse,

    -- ** UpdateMemberDetectors
    UpdateMemberDetectors (UpdateMemberDetectors'),
    newUpdateMemberDetectors,
    UpdateMemberDetectorsResponse (UpdateMemberDetectorsResponse'),
    newUpdateMemberDetectorsResponse,

    -- ** GetThreatIntelSet
    GetThreatIntelSet (GetThreatIntelSet'),
    newGetThreatIntelSet,
    GetThreatIntelSetResponse (GetThreatIntelSetResponse'),
    newGetThreatIntelSetResponse,

    -- ** DeleteInvitations
    DeleteInvitations (DeleteInvitations'),
    newDeleteInvitations,
    DeleteInvitationsResponse (DeleteInvitationsResponse'),
    newDeleteInvitationsResponse,

    -- ** GetMasterAccount
    GetMasterAccount (GetMasterAccount'),
    newGetMasterAccount,
    GetMasterAccountResponse (GetMasterAccountResponse'),
    newGetMasterAccountResponse,

    -- ** GetUsageStatistics
    GetUsageStatistics (GetUsageStatistics'),
    newGetUsageStatistics,
    GetUsageStatisticsResponse (GetUsageStatisticsResponse'),
    newGetUsageStatisticsResponse,

    -- ** CreateDetector
    CreateDetector (CreateDetector'),
    newCreateDetector,
    CreateDetectorResponse (CreateDetectorResponse'),
    newCreateDetectorResponse,

    -- ** DeclineInvitations
    DeclineInvitations (DeclineInvitations'),
    newDeclineInvitations,
    DeclineInvitationsResponse (DeclineInvitationsResponse'),
    newDeclineInvitationsResponse,

    -- ** DescribeOrganizationConfiguration
    DescribeOrganizationConfiguration (DescribeOrganizationConfiguration'),
    newDescribeOrganizationConfiguration,
    DescribeOrganizationConfigurationResponse (DescribeOrganizationConfigurationResponse'),
    newDescribeOrganizationConfigurationResponse,

    -- ** CreatePublishingDestination
    CreatePublishingDestination (CreatePublishingDestination'),
    newCreatePublishingDestination,
    CreatePublishingDestinationResponse (CreatePublishingDestinationResponse'),
    newCreatePublishingDestinationResponse,

    -- ** UpdateFilter
    UpdateFilter (UpdateFilter'),
    newUpdateFilter,
    UpdateFilterResponse (UpdateFilterResponse'),
    newUpdateFilterResponse,

    -- ** DeleteFilter
    DeleteFilter (DeleteFilter'),
    newDeleteFilter,
    DeleteFilterResponse (DeleteFilterResponse'),
    newDeleteFilterResponse,

    -- ** DisassociateMembers
    DisassociateMembers (DisassociateMembers'),
    newDisassociateMembers,
    DisassociateMembersResponse (DisassociateMembersResponse'),
    newDisassociateMembersResponse,

    -- ** DisassociateFromMasterAccount
    DisassociateFromMasterAccount (DisassociateFromMasterAccount'),
    newDisassociateFromMasterAccount,
    DisassociateFromMasterAccountResponse (DisassociateFromMasterAccountResponse'),
    newDisassociateFromMasterAccountResponse,

    -- ** AcceptInvitation
    AcceptInvitation (AcceptInvitation'),
    newAcceptInvitation,
    AcceptInvitationResponse (AcceptInvitationResponse'),
    newAcceptInvitationResponse,

    -- ** ListFilters (Paginated)
    ListFilters (ListFilters'),
    newListFilters,
    ListFiltersResponse (ListFiltersResponse'),
    newListFiltersResponse,

    -- ** ListMembers (Paginated)
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** ListPublishingDestinations
    ListPublishingDestinations (ListPublishingDestinations'),
    newListPublishingDestinations,
    ListPublishingDestinationsResponse (ListPublishingDestinationsResponse'),
    newListPublishingDestinationsResponse,

    -- ** DeletePublishingDestination
    DeletePublishingDestination (DeletePublishingDestination'),
    newDeletePublishingDestination,
    DeletePublishingDestinationResponse (DeletePublishingDestinationResponse'),
    newDeletePublishingDestinationResponse,

    -- ** UpdatePublishingDestination
    UpdatePublishingDestination (UpdatePublishingDestination'),
    newUpdatePublishingDestination,
    UpdatePublishingDestinationResponse (UpdatePublishingDestinationResponse'),
    newUpdatePublishingDestinationResponse,

    -- ** GetDetector
    GetDetector (GetDetector'),
    newGetDetector,
    GetDetectorResponse (GetDetectorResponse'),
    newGetDetectorResponse,

    -- ** CreateSampleFindings
    CreateSampleFindings (CreateSampleFindings'),
    newCreateSampleFindings,
    CreateSampleFindingsResponse (CreateSampleFindingsResponse'),
    newCreateSampleFindingsResponse,

    -- ** ArchiveFindings
    ArchiveFindings (ArchiveFindings'),
    newArchiveFindings,
    ArchiveFindingsResponse (ArchiveFindingsResponse'),
    newArchiveFindingsResponse,

    -- ** CreateMembers
    CreateMembers (CreateMembers'),
    newCreateMembers,
    CreateMembersResponse (CreateMembersResponse'),
    newCreateMembersResponse,

    -- ** UnarchiveFindings
    UnarchiveFindings (UnarchiveFindings'),
    newUnarchiveFindings,
    UnarchiveFindingsResponse (UnarchiveFindingsResponse'),
    newUnarchiveFindingsResponse,

    -- ** GetMemberDetectors
    GetMemberDetectors (GetMemberDetectors'),
    newGetMemberDetectors,
    GetMemberDetectorsResponse (GetMemberDetectorsResponse'),
    newGetMemberDetectorsResponse,

    -- ** GetInvitationsCount
    GetInvitationsCount (GetInvitationsCount'),
    newGetInvitationsCount,
    GetInvitationsCountResponse (GetInvitationsCountResponse'),
    newGetInvitationsCountResponse,

    -- ** StartMonitoringMembers
    StartMonitoringMembers (StartMonitoringMembers'),
    newStartMonitoringMembers,
    StartMonitoringMembersResponse (StartMonitoringMembersResponse'),
    newStartMonitoringMembersResponse,

    -- ** UpdateOrganizationConfiguration
    UpdateOrganizationConfiguration (UpdateOrganizationConfiguration'),
    newUpdateOrganizationConfiguration,
    UpdateOrganizationConfigurationResponse (UpdateOrganizationConfigurationResponse'),
    newUpdateOrganizationConfigurationResponse,

    -- ** InviteMembers
    InviteMembers (InviteMembers'),
    newInviteMembers,
    InviteMembersResponse (InviteMembersResponse'),
    newInviteMembersResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** ListIPSets (Paginated)
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** GetMembers
    GetMembers (GetMembers'),
    newGetMembers,
    GetMembersResponse (GetMembersResponse'),
    newGetMembersResponse,

    -- ** DescribePublishingDestination
    DescribePublishingDestination (DescribePublishingDestination'),
    newDescribePublishingDestination,
    DescribePublishingDestinationResponse (DescribePublishingDestinationResponse'),
    newDescribePublishingDestinationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetFindings
    GetFindings (GetFindings'),
    newGetFindings,
    GetFindingsResponse (GetFindingsResponse'),
    newGetFindingsResponse,

    -- ** ListDetectors (Paginated)
    ListDetectors (ListDetectors'),
    newListDetectors,
    ListDetectorsResponse (ListDetectorsResponse'),
    newListDetectorsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateDetector
    UpdateDetector (UpdateDetector'),
    newUpdateDetector,
    UpdateDetectorResponse (UpdateDetectorResponse'),
    newUpdateDetectorResponse,

    -- ** DeleteDetector
    DeleteDetector (DeleteDetector'),
    newDeleteDetector,
    DeleteDetectorResponse (DeleteDetectorResponse'),
    newDeleteDetectorResponse,

    -- ** UpdateFindingsFeedback
    UpdateFindingsFeedback (UpdateFindingsFeedback'),
    newUpdateFindingsFeedback,
    UpdateFindingsFeedbackResponse (UpdateFindingsFeedbackResponse'),
    newUpdateFindingsFeedbackResponse,

    -- ** GetFilter
    GetFilter (GetFilter'),
    newGetFilter,
    GetFilterResponse (GetFilterResponse'),
    newGetFilterResponse,

    -- ** DisableOrganizationAdminAccount
    DisableOrganizationAdminAccount (DisableOrganizationAdminAccount'),
    newDisableOrganizationAdminAccount,
    DisableOrganizationAdminAccountResponse (DisableOrganizationAdminAccountResponse'),
    newDisableOrganizationAdminAccountResponse,

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

    -- ** IpSetFormat
    IpSetFormat (..),

    -- ** IpSetStatus
    IpSetStatus (..),

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

    -- ** AccessControlList
    AccessControlList (AccessControlList'),
    newAccessControlList,

    -- ** AccessKeyDetails
    AccessKeyDetails (AccessKeyDetails'),
    newAccessKeyDetails,

    -- ** AccountDetail
    AccountDetail (AccountDetail'),
    newAccountDetail,

    -- ** AccountLevelPermissions
    AccountLevelPermissions (AccountLevelPermissions'),
    newAccountLevelPermissions,

    -- ** Action
    Action (Action'),
    newAction,

    -- ** AdminAccount
    AdminAccount (AdminAccount'),
    newAdminAccount,

    -- ** AwsApiCallAction
    AwsApiCallAction (AwsApiCallAction'),
    newAwsApiCallAction,

    -- ** BlockPublicAccess
    BlockPublicAccess (BlockPublicAccess'),
    newBlockPublicAccess,

    -- ** BucketLevelPermissions
    BucketLevelPermissions (BucketLevelPermissions'),
    newBucketLevelPermissions,

    -- ** BucketPolicy
    BucketPolicy (BucketPolicy'),
    newBucketPolicy,

    -- ** City
    City (City'),
    newCity,

    -- ** CloudTrailConfigurationResult
    CloudTrailConfigurationResult (CloudTrailConfigurationResult'),
    newCloudTrailConfigurationResult,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** Country
    Country (Country'),
    newCountry,

    -- ** DNSLogsConfigurationResult
    DNSLogsConfigurationResult (DNSLogsConfigurationResult'),
    newDNSLogsConfigurationResult,

    -- ** DataSourceConfigurations
    DataSourceConfigurations (DataSourceConfigurations'),
    newDataSourceConfigurations,

    -- ** DataSourceConfigurationsResult
    DataSourceConfigurationsResult (DataSourceConfigurationsResult'),
    newDataSourceConfigurationsResult,

    -- ** DefaultServerSideEncryption
    DefaultServerSideEncryption (DefaultServerSideEncryption'),
    newDefaultServerSideEncryption,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** DestinationProperties
    DestinationProperties (DestinationProperties'),
    newDestinationProperties,

    -- ** DnsRequestAction
    DnsRequestAction (DnsRequestAction'),
    newDnsRequestAction,

    -- ** DomainDetails
    DomainDetails (DomainDetails'),
    newDomainDetails,

    -- ** Evidence
    Evidence (Evidence'),
    newEvidence,

    -- ** Finding
    Finding (Finding'),
    newFinding,

    -- ** FindingCriteria
    FindingCriteria (FindingCriteria'),
    newFindingCriteria,

    -- ** FindingStatistics
    FindingStatistics (FindingStatistics'),
    newFindingStatistics,

    -- ** FlowLogsConfigurationResult
    FlowLogsConfigurationResult (FlowLogsConfigurationResult'),
    newFlowLogsConfigurationResult,

    -- ** GeoLocation
    GeoLocation (GeoLocation'),
    newGeoLocation,

    -- ** IamInstanceProfile
    IamInstanceProfile (IamInstanceProfile'),
    newIamInstanceProfile,

    -- ** InstanceDetails
    InstanceDetails (InstanceDetails'),
    newInstanceDetails,

    -- ** Invitation
    Invitation (Invitation'),
    newInvitation,

    -- ** LocalIpDetails
    LocalIpDetails (LocalIpDetails'),
    newLocalIpDetails,

    -- ** LocalPortDetails
    LocalPortDetails (LocalPortDetails'),
    newLocalPortDetails,

    -- ** Master
    Master (Master'),
    newMaster,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** MemberDataSourceConfiguration
    MemberDataSourceConfiguration (MemberDataSourceConfiguration'),
    newMemberDataSourceConfiguration,

    -- ** NetworkConnectionAction
    NetworkConnectionAction (NetworkConnectionAction'),
    newNetworkConnectionAction,

    -- ** NetworkInterface
    NetworkInterface (NetworkInterface'),
    newNetworkInterface,

    -- ** Organization
    Organization (Organization'),
    newOrganization,

    -- ** OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (OrganizationDataSourceConfigurations'),
    newOrganizationDataSourceConfigurations,

    -- ** OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (OrganizationDataSourceConfigurationsResult'),
    newOrganizationDataSourceConfigurationsResult,

    -- ** OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (OrganizationS3LogsConfiguration'),
    newOrganizationS3LogsConfiguration,

    -- ** OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (OrganizationS3LogsConfigurationResult'),
    newOrganizationS3LogsConfigurationResult,

    -- ** Owner
    Owner (Owner'),
    newOwner,

    -- ** PermissionConfiguration
    PermissionConfiguration (PermissionConfiguration'),
    newPermissionConfiguration,

    -- ** PortProbeAction
    PortProbeAction (PortProbeAction'),
    newPortProbeAction,

    -- ** PortProbeDetail
    PortProbeDetail (PortProbeDetail'),
    newPortProbeDetail,

    -- ** PrivateIpAddressDetails
    PrivateIpAddressDetails (PrivateIpAddressDetails'),
    newPrivateIpAddressDetails,

    -- ** ProductCode
    ProductCode (ProductCode'),
    newProductCode,

    -- ** PublicAccess
    PublicAccess (PublicAccess'),
    newPublicAccess,

    -- ** RemoteIpDetails
    RemoteIpDetails (RemoteIpDetails'),
    newRemoteIpDetails,

    -- ** RemotePortDetails
    RemotePortDetails (RemotePortDetails'),
    newRemotePortDetails,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** S3BucketDetail
    S3BucketDetail (S3BucketDetail'),
    newS3BucketDetail,

    -- ** S3LogsConfiguration
    S3LogsConfiguration (S3LogsConfiguration'),
    newS3LogsConfiguration,

    -- ** S3LogsConfigurationResult
    S3LogsConfigurationResult (S3LogsConfigurationResult'),
    newS3LogsConfigurationResult,

    -- ** SecurityGroup
    SecurityGroup (SecurityGroup'),
    newSecurityGroup,

    -- ** ServiceInfo
    ServiceInfo (ServiceInfo'),
    newServiceInfo,

    -- ** SortCriteria
    SortCriteria (SortCriteria'),
    newSortCriteria,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ThreatIntelligenceDetail
    ThreatIntelligenceDetail (ThreatIntelligenceDetail'),
    newThreatIntelligenceDetail,

    -- ** Total
    Total (Total'),
    newTotal,

    -- ** UnprocessedAccount
    UnprocessedAccount (UnprocessedAccount'),
    newUnprocessedAccount,

    -- ** UsageAccountResult
    UsageAccountResult (UsageAccountResult'),
    newUsageAccountResult,

    -- ** UsageCriteria
    UsageCriteria (UsageCriteria'),
    newUsageCriteria,

    -- ** UsageDataSourceResult
    UsageDataSourceResult (UsageDataSourceResult'),
    newUsageDataSourceResult,

    -- ** UsageResourceResult
    UsageResourceResult (UsageResourceResult'),
    newUsageResourceResult,

    -- ** UsageStatistics
    UsageStatistics (UsageStatistics'),
    newUsageStatistics,
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
import Network.AWS.GuardDuty.Lens
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
