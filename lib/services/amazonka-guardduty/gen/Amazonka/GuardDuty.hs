{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.GuardDuty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon GuardDuty is a continuous security monitoring service that
-- analyzes and processes the following data sources: VPC flow logs, Amazon
-- Web Services CloudTrail management event logs, CloudTrail S3 data event
-- logs, EKS audit logs, DNS logs, and Amazon EBS volume data. It uses
-- threat intelligence feeds, such as lists of malicious IPs and domains,
-- and machine learning to identify unexpected, potentially unauthorized,
-- and malicious activity within your Amazon Web Services environment. This
-- can include issues like escalations of privileges, uses of exposed
-- credentials, or communication with malicious IPs, domains, or presence
-- of malware on your Amazon EC2 instances and container workloads. For
-- example, GuardDuty can detect compromised EC2 instances and container
-- workloads serving malware, or mining bitcoin.
--
-- GuardDuty also monitors Amazon Web Services account access behavior for
-- signs of compromise, such as unauthorized infrastructure deployments
-- like EC2 instances deployed in a Region that has never been used, or
-- unusual API calls like a password policy change to reduce password
-- strength.
--
-- GuardDuty informs you about the status of your Amazon Web Services
-- environment by producing security findings that you can view in the
-- GuardDuty console or through Amazon EventBridge. For more information,
-- see the
-- /<https://docs.aws.amazon.com/guardduty/latest/ug/what-is-guardduty.html Amazon GuardDuty User Guide>/
-- .
module Amazonka.GuardDuty
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServerErrorException
    _InternalServerErrorException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptAdministratorInvitation
    AcceptAdministratorInvitation (AcceptAdministratorInvitation'),
    newAcceptAdministratorInvitation,
    AcceptAdministratorInvitationResponse (AcceptAdministratorInvitationResponse'),
    newAcceptAdministratorInvitationResponse,

    -- ** ArchiveFindings
    ArchiveFindings (ArchiveFindings'),
    newArchiveFindings,
    ArchiveFindingsResponse (ArchiveFindingsResponse'),
    newArchiveFindingsResponse,

    -- ** CreateDetector
    CreateDetector (CreateDetector'),
    newCreateDetector,
    CreateDetectorResponse (CreateDetectorResponse'),
    newCreateDetectorResponse,

    -- ** CreateFilter
    CreateFilter (CreateFilter'),
    newCreateFilter,
    CreateFilterResponse (CreateFilterResponse'),
    newCreateFilterResponse,

    -- ** CreateIPSet
    CreateIPSet (CreateIPSet'),
    newCreateIPSet,
    CreateIPSetResponse (CreateIPSetResponse'),
    newCreateIPSetResponse,

    -- ** CreateMembers
    CreateMembers (CreateMembers'),
    newCreateMembers,
    CreateMembersResponse (CreateMembersResponse'),
    newCreateMembersResponse,

    -- ** CreatePublishingDestination
    CreatePublishingDestination (CreatePublishingDestination'),
    newCreatePublishingDestination,
    CreatePublishingDestinationResponse (CreatePublishingDestinationResponse'),
    newCreatePublishingDestinationResponse,

    -- ** CreateSampleFindings
    CreateSampleFindings (CreateSampleFindings'),
    newCreateSampleFindings,
    CreateSampleFindingsResponse (CreateSampleFindingsResponse'),
    newCreateSampleFindingsResponse,

    -- ** CreateThreatIntelSet
    CreateThreatIntelSet (CreateThreatIntelSet'),
    newCreateThreatIntelSet,
    CreateThreatIntelSetResponse (CreateThreatIntelSetResponse'),
    newCreateThreatIntelSetResponse,

    -- ** DeclineInvitations
    DeclineInvitations (DeclineInvitations'),
    newDeclineInvitations,
    DeclineInvitationsResponse (DeclineInvitationsResponse'),
    newDeclineInvitationsResponse,

    -- ** DeleteDetector
    DeleteDetector (DeleteDetector'),
    newDeleteDetector,
    DeleteDetectorResponse (DeleteDetectorResponse'),
    newDeleteDetectorResponse,

    -- ** DeleteFilter
    DeleteFilter (DeleteFilter'),
    newDeleteFilter,
    DeleteFilterResponse (DeleteFilterResponse'),
    newDeleteFilterResponse,

    -- ** DeleteIPSet
    DeleteIPSet (DeleteIPSet'),
    newDeleteIPSet,
    DeleteIPSetResponse (DeleteIPSetResponse'),
    newDeleteIPSetResponse,

    -- ** DeleteInvitations
    DeleteInvitations (DeleteInvitations'),
    newDeleteInvitations,
    DeleteInvitationsResponse (DeleteInvitationsResponse'),
    newDeleteInvitationsResponse,

    -- ** DeleteMembers
    DeleteMembers (DeleteMembers'),
    newDeleteMembers,
    DeleteMembersResponse (DeleteMembersResponse'),
    newDeleteMembersResponse,

    -- ** DeletePublishingDestination
    DeletePublishingDestination (DeletePublishingDestination'),
    newDeletePublishingDestination,
    DeletePublishingDestinationResponse (DeletePublishingDestinationResponse'),
    newDeletePublishingDestinationResponse,

    -- ** DeleteThreatIntelSet
    DeleteThreatIntelSet (DeleteThreatIntelSet'),
    newDeleteThreatIntelSet,
    DeleteThreatIntelSetResponse (DeleteThreatIntelSetResponse'),
    newDeleteThreatIntelSetResponse,

    -- ** DescribeMalwareScans (Paginated)
    DescribeMalwareScans (DescribeMalwareScans'),
    newDescribeMalwareScans,
    DescribeMalwareScansResponse (DescribeMalwareScansResponse'),
    newDescribeMalwareScansResponse,

    -- ** DescribeOrganizationConfiguration
    DescribeOrganizationConfiguration (DescribeOrganizationConfiguration'),
    newDescribeOrganizationConfiguration,
    DescribeOrganizationConfigurationResponse (DescribeOrganizationConfigurationResponse'),
    newDescribeOrganizationConfigurationResponse,

    -- ** DescribePublishingDestination
    DescribePublishingDestination (DescribePublishingDestination'),
    newDescribePublishingDestination,
    DescribePublishingDestinationResponse (DescribePublishingDestinationResponse'),
    newDescribePublishingDestinationResponse,

    -- ** DisableOrganizationAdminAccount
    DisableOrganizationAdminAccount (DisableOrganizationAdminAccount'),
    newDisableOrganizationAdminAccount,
    DisableOrganizationAdminAccountResponse (DisableOrganizationAdminAccountResponse'),
    newDisableOrganizationAdminAccountResponse,

    -- ** DisassociateFromAdministratorAccount
    DisassociateFromAdministratorAccount (DisassociateFromAdministratorAccount'),
    newDisassociateFromAdministratorAccount,
    DisassociateFromAdministratorAccountResponse (DisassociateFromAdministratorAccountResponse'),
    newDisassociateFromAdministratorAccountResponse,

    -- ** DisassociateMembers
    DisassociateMembers (DisassociateMembers'),
    newDisassociateMembers,
    DisassociateMembersResponse (DisassociateMembersResponse'),
    newDisassociateMembersResponse,

    -- ** EnableOrganizationAdminAccount
    EnableOrganizationAdminAccount (EnableOrganizationAdminAccount'),
    newEnableOrganizationAdminAccount,
    EnableOrganizationAdminAccountResponse (EnableOrganizationAdminAccountResponse'),
    newEnableOrganizationAdminAccountResponse,

    -- ** GetAdministratorAccount
    GetAdministratorAccount (GetAdministratorAccount'),
    newGetAdministratorAccount,
    GetAdministratorAccountResponse (GetAdministratorAccountResponse'),
    newGetAdministratorAccountResponse,

    -- ** GetDetector
    GetDetector (GetDetector'),
    newGetDetector,
    GetDetectorResponse (GetDetectorResponse'),
    newGetDetectorResponse,

    -- ** GetFilter
    GetFilter (GetFilter'),
    newGetFilter,
    GetFilterResponse (GetFilterResponse'),
    newGetFilterResponse,

    -- ** GetFindings
    GetFindings (GetFindings'),
    newGetFindings,
    GetFindingsResponse (GetFindingsResponse'),
    newGetFindingsResponse,

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

    -- ** GetInvitationsCount
    GetInvitationsCount (GetInvitationsCount'),
    newGetInvitationsCount,
    GetInvitationsCountResponse (GetInvitationsCountResponse'),
    newGetInvitationsCountResponse,

    -- ** GetMalwareScanSettings
    GetMalwareScanSettings (GetMalwareScanSettings'),
    newGetMalwareScanSettings,
    GetMalwareScanSettingsResponse (GetMalwareScanSettingsResponse'),
    newGetMalwareScanSettingsResponse,

    -- ** GetMemberDetectors
    GetMemberDetectors (GetMemberDetectors'),
    newGetMemberDetectors,
    GetMemberDetectorsResponse (GetMemberDetectorsResponse'),
    newGetMemberDetectorsResponse,

    -- ** GetMembers
    GetMembers (GetMembers'),
    newGetMembers,
    GetMembersResponse (GetMembersResponse'),
    newGetMembersResponse,

    -- ** GetRemainingFreeTrialDays
    GetRemainingFreeTrialDays (GetRemainingFreeTrialDays'),
    newGetRemainingFreeTrialDays,
    GetRemainingFreeTrialDaysResponse (GetRemainingFreeTrialDaysResponse'),
    newGetRemainingFreeTrialDaysResponse,

    -- ** GetThreatIntelSet
    GetThreatIntelSet (GetThreatIntelSet'),
    newGetThreatIntelSet,
    GetThreatIntelSetResponse (GetThreatIntelSetResponse'),
    newGetThreatIntelSetResponse,

    -- ** GetUsageStatistics
    GetUsageStatistics (GetUsageStatistics'),
    newGetUsageStatistics,
    GetUsageStatisticsResponse (GetUsageStatisticsResponse'),
    newGetUsageStatisticsResponse,

    -- ** InviteMembers
    InviteMembers (InviteMembers'),
    newInviteMembers,
    InviteMembersResponse (InviteMembersResponse'),
    newInviteMembersResponse,

    -- ** ListDetectors (Paginated)
    ListDetectors (ListDetectors'),
    newListDetectors,
    ListDetectorsResponse (ListDetectorsResponse'),
    newListDetectorsResponse,

    -- ** ListFilters (Paginated)
    ListFilters (ListFilters'),
    newListFilters,
    ListFiltersResponse (ListFiltersResponse'),
    newListFiltersResponse,

    -- ** ListFindings (Paginated)
    ListFindings (ListFindings'),
    newListFindings,
    ListFindingsResponse (ListFindingsResponse'),
    newListFindingsResponse,

    -- ** ListIPSets (Paginated)
    ListIPSets (ListIPSets'),
    newListIPSets,
    ListIPSetsResponse (ListIPSetsResponse'),
    newListIPSetsResponse,

    -- ** ListInvitations (Paginated)
    ListInvitations (ListInvitations'),
    newListInvitations,
    ListInvitationsResponse (ListInvitationsResponse'),
    newListInvitationsResponse,

    -- ** ListMembers (Paginated)
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** ListOrganizationAdminAccounts (Paginated)
    ListOrganizationAdminAccounts (ListOrganizationAdminAccounts'),
    newListOrganizationAdminAccounts,
    ListOrganizationAdminAccountsResponse (ListOrganizationAdminAccountsResponse'),
    newListOrganizationAdminAccountsResponse,

    -- ** ListPublishingDestinations
    ListPublishingDestinations (ListPublishingDestinations'),
    newListPublishingDestinations,
    ListPublishingDestinationsResponse (ListPublishingDestinationsResponse'),
    newListPublishingDestinationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListThreatIntelSets (Paginated)
    ListThreatIntelSets (ListThreatIntelSets'),
    newListThreatIntelSets,
    ListThreatIntelSetsResponse (ListThreatIntelSetsResponse'),
    newListThreatIntelSetsResponse,

    -- ** StartMonitoringMembers
    StartMonitoringMembers (StartMonitoringMembers'),
    newStartMonitoringMembers,
    StartMonitoringMembersResponse (StartMonitoringMembersResponse'),
    newStartMonitoringMembersResponse,

    -- ** StopMonitoringMembers
    StopMonitoringMembers (StopMonitoringMembers'),
    newStopMonitoringMembers,
    StopMonitoringMembersResponse (StopMonitoringMembersResponse'),
    newStopMonitoringMembersResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UnarchiveFindings
    UnarchiveFindings (UnarchiveFindings'),
    newUnarchiveFindings,
    UnarchiveFindingsResponse (UnarchiveFindingsResponse'),
    newUnarchiveFindingsResponse,

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

    -- ** UpdateFilter
    UpdateFilter (UpdateFilter'),
    newUpdateFilter,
    UpdateFilterResponse (UpdateFilterResponse'),
    newUpdateFilterResponse,

    -- ** UpdateFindingsFeedback
    UpdateFindingsFeedback (UpdateFindingsFeedback'),
    newUpdateFindingsFeedback,
    UpdateFindingsFeedbackResponse (UpdateFindingsFeedbackResponse'),
    newUpdateFindingsFeedbackResponse,

    -- ** UpdateIPSet
    UpdateIPSet (UpdateIPSet'),
    newUpdateIPSet,
    UpdateIPSetResponse (UpdateIPSetResponse'),
    newUpdateIPSetResponse,

    -- ** UpdateMalwareScanSettings
    UpdateMalwareScanSettings (UpdateMalwareScanSettings'),
    newUpdateMalwareScanSettings,
    UpdateMalwareScanSettingsResponse (UpdateMalwareScanSettingsResponse'),
    newUpdateMalwareScanSettingsResponse,

    -- ** UpdateMemberDetectors
    UpdateMemberDetectors (UpdateMemberDetectors'),
    newUpdateMemberDetectors,
    UpdateMemberDetectorsResponse (UpdateMemberDetectorsResponse'),
    newUpdateMemberDetectorsResponse,

    -- ** UpdateOrganizationConfiguration
    UpdateOrganizationConfiguration (UpdateOrganizationConfiguration'),
    newUpdateOrganizationConfiguration,
    UpdateOrganizationConfigurationResponse (UpdateOrganizationConfigurationResponse'),
    newUpdateOrganizationConfigurationResponse,

    -- ** UpdatePublishingDestination
    UpdatePublishingDestination (UpdatePublishingDestination'),
    newUpdatePublishingDestination,
    UpdatePublishingDestinationResponse (UpdatePublishingDestinationResponse'),
    newUpdatePublishingDestinationResponse,

    -- ** UpdateThreatIntelSet
    UpdateThreatIntelSet (UpdateThreatIntelSet'),
    newUpdateThreatIntelSet,
    UpdateThreatIntelSetResponse (UpdateThreatIntelSetResponse'),
    newUpdateThreatIntelSetResponse,

    -- * Types

    -- ** AdminStatus
    AdminStatus (..),

    -- ** CriterionKey
    CriterionKey (..),

    -- ** DataSource
    DataSource (..),

    -- ** DataSourceStatus
    DataSourceStatus (..),

    -- ** DestinationType
    DestinationType (..),

    -- ** DetectorStatus
    DetectorStatus (..),

    -- ** EbsSnapshotPreservation
    EbsSnapshotPreservation (..),

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

    -- ** ScanCriterionKey
    ScanCriterionKey (..),

    -- ** ScanResult
    ScanResult (..),

    -- ** ScanStatus
    ScanStatus (..),

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

    -- ** AccountFreeTrialInfo
    AccountFreeTrialInfo (AccountFreeTrialInfo'),
    newAccountFreeTrialInfo,

    -- ** AccountLevelPermissions
    AccountLevelPermissions (AccountLevelPermissions'),
    newAccountLevelPermissions,

    -- ** Action
    Action (Action'),
    newAction,

    -- ** AdminAccount
    AdminAccount (AdminAccount'),
    newAdminAccount,

    -- ** Administrator
    Administrator (Administrator'),
    newAdministrator,

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

    -- ** Container
    Container (Container'),
    newContainer,

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

    -- ** DataSourceFreeTrial
    DataSourceFreeTrial (DataSourceFreeTrial'),
    newDataSourceFreeTrial,

    -- ** DataSourcesFreeTrial
    DataSourcesFreeTrial (DataSourcesFreeTrial'),
    newDataSourcesFreeTrial,

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

    -- ** EbsVolumeDetails
    EbsVolumeDetails (EbsVolumeDetails'),
    newEbsVolumeDetails,

    -- ** EbsVolumeScanDetails
    EbsVolumeScanDetails (EbsVolumeScanDetails'),
    newEbsVolumeScanDetails,

    -- ** EbsVolumesResult
    EbsVolumesResult (EbsVolumesResult'),
    newEbsVolumesResult,

    -- ** EcsClusterDetails
    EcsClusterDetails (EcsClusterDetails'),
    newEcsClusterDetails,

    -- ** EcsTaskDetails
    EcsTaskDetails (EcsTaskDetails'),
    newEcsTaskDetails,

    -- ** EksClusterDetails
    EksClusterDetails (EksClusterDetails'),
    newEksClusterDetails,

    -- ** Evidence
    Evidence (Evidence'),
    newEvidence,

    -- ** FilterCondition
    FilterCondition (FilterCondition'),
    newFilterCondition,

    -- ** FilterCriteria
    FilterCriteria (FilterCriteria'),
    newFilterCriteria,

    -- ** FilterCriterion
    FilterCriterion (FilterCriterion'),
    newFilterCriterion,

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

    -- ** HighestSeverityThreatDetails
    HighestSeverityThreatDetails (HighestSeverityThreatDetails'),
    newHighestSeverityThreatDetails,

    -- ** HostPath
    HostPath (HostPath'),
    newHostPath,

    -- ** IamInstanceProfile
    IamInstanceProfile (IamInstanceProfile'),
    newIamInstanceProfile,

    -- ** InstanceDetails
    InstanceDetails (InstanceDetails'),
    newInstanceDetails,

    -- ** Invitation
    Invitation (Invitation'),
    newInvitation,

    -- ** KubernetesApiCallAction
    KubernetesApiCallAction (KubernetesApiCallAction'),
    newKubernetesApiCallAction,

    -- ** KubernetesAuditLogsConfiguration
    KubernetesAuditLogsConfiguration (KubernetesAuditLogsConfiguration'),
    newKubernetesAuditLogsConfiguration,

    -- ** KubernetesAuditLogsConfigurationResult
    KubernetesAuditLogsConfigurationResult (KubernetesAuditLogsConfigurationResult'),
    newKubernetesAuditLogsConfigurationResult,

    -- ** KubernetesConfiguration
    KubernetesConfiguration (KubernetesConfiguration'),
    newKubernetesConfiguration,

    -- ** KubernetesConfigurationResult
    KubernetesConfigurationResult (KubernetesConfigurationResult'),
    newKubernetesConfigurationResult,

    -- ** KubernetesDataSourceFreeTrial
    KubernetesDataSourceFreeTrial (KubernetesDataSourceFreeTrial'),
    newKubernetesDataSourceFreeTrial,

    -- ** KubernetesDetails
    KubernetesDetails (KubernetesDetails'),
    newKubernetesDetails,

    -- ** KubernetesUserDetails
    KubernetesUserDetails (KubernetesUserDetails'),
    newKubernetesUserDetails,

    -- ** KubernetesWorkloadDetails
    KubernetesWorkloadDetails (KubernetesWorkloadDetails'),
    newKubernetesWorkloadDetails,

    -- ** LocalIpDetails
    LocalIpDetails (LocalIpDetails'),
    newLocalIpDetails,

    -- ** LocalPortDetails
    LocalPortDetails (LocalPortDetails'),
    newLocalPortDetails,

    -- ** MalwareProtectionConfiguration
    MalwareProtectionConfiguration (MalwareProtectionConfiguration'),
    newMalwareProtectionConfiguration,

    -- ** MalwareProtectionConfigurationResult
    MalwareProtectionConfigurationResult (MalwareProtectionConfigurationResult'),
    newMalwareProtectionConfigurationResult,

    -- ** MalwareProtectionDataSourceFreeTrial
    MalwareProtectionDataSourceFreeTrial (MalwareProtectionDataSourceFreeTrial'),
    newMalwareProtectionDataSourceFreeTrial,

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

    -- ** OrganizationEbsVolumes
    OrganizationEbsVolumes (OrganizationEbsVolumes'),
    newOrganizationEbsVolumes,

    -- ** OrganizationEbsVolumesResult
    OrganizationEbsVolumesResult (OrganizationEbsVolumesResult'),
    newOrganizationEbsVolumesResult,

    -- ** OrganizationKubernetesAuditLogsConfiguration
    OrganizationKubernetesAuditLogsConfiguration (OrganizationKubernetesAuditLogsConfiguration'),
    newOrganizationKubernetesAuditLogsConfiguration,

    -- ** OrganizationKubernetesAuditLogsConfigurationResult
    OrganizationKubernetesAuditLogsConfigurationResult (OrganizationKubernetesAuditLogsConfigurationResult'),
    newOrganizationKubernetesAuditLogsConfigurationResult,

    -- ** OrganizationKubernetesConfiguration
    OrganizationKubernetesConfiguration (OrganizationKubernetesConfiguration'),
    newOrganizationKubernetesConfiguration,

    -- ** OrganizationKubernetesConfigurationResult
    OrganizationKubernetesConfigurationResult (OrganizationKubernetesConfigurationResult'),
    newOrganizationKubernetesConfigurationResult,

    -- ** OrganizationMalwareProtectionConfiguration
    OrganizationMalwareProtectionConfiguration (OrganizationMalwareProtectionConfiguration'),
    newOrganizationMalwareProtectionConfiguration,

    -- ** OrganizationMalwareProtectionConfigurationResult
    OrganizationMalwareProtectionConfigurationResult (OrganizationMalwareProtectionConfigurationResult'),
    newOrganizationMalwareProtectionConfigurationResult,

    -- ** OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (OrganizationS3LogsConfiguration'),
    newOrganizationS3LogsConfiguration,

    -- ** OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (OrganizationS3LogsConfigurationResult'),
    newOrganizationS3LogsConfigurationResult,

    -- ** OrganizationScanEc2InstanceWithFindings
    OrganizationScanEc2InstanceWithFindings (OrganizationScanEc2InstanceWithFindings'),
    newOrganizationScanEc2InstanceWithFindings,

    -- ** OrganizationScanEc2InstanceWithFindingsResult
    OrganizationScanEc2InstanceWithFindingsResult (OrganizationScanEc2InstanceWithFindingsResult'),
    newOrganizationScanEc2InstanceWithFindingsResult,

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

    -- ** RemoteAccountDetails
    RemoteAccountDetails (RemoteAccountDetails'),
    newRemoteAccountDetails,

    -- ** RemoteIpDetails
    RemoteIpDetails (RemoteIpDetails'),
    newRemoteIpDetails,

    -- ** RemotePortDetails
    RemotePortDetails (RemotePortDetails'),
    newRemotePortDetails,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** S3BucketDetail
    S3BucketDetail (S3BucketDetail'),
    newS3BucketDetail,

    -- ** S3LogsConfiguration
    S3LogsConfiguration (S3LogsConfiguration'),
    newS3LogsConfiguration,

    -- ** S3LogsConfigurationResult
    S3LogsConfigurationResult (S3LogsConfigurationResult'),
    newS3LogsConfigurationResult,

    -- ** Scan
    Scan (Scan'),
    newScan,

    -- ** ScanCondition
    ScanCondition (ScanCondition'),
    newScanCondition,

    -- ** ScanConditionPair
    ScanConditionPair (ScanConditionPair'),
    newScanConditionPair,

    -- ** ScanDetections
    ScanDetections (ScanDetections'),
    newScanDetections,

    -- ** ScanEc2InstanceWithFindings
    ScanEc2InstanceWithFindings (ScanEc2InstanceWithFindings'),
    newScanEc2InstanceWithFindings,

    -- ** ScanEc2InstanceWithFindingsResult
    ScanEc2InstanceWithFindingsResult (ScanEc2InstanceWithFindingsResult'),
    newScanEc2InstanceWithFindingsResult,

    -- ** ScanFilePath
    ScanFilePath (ScanFilePath'),
    newScanFilePath,

    -- ** ScanResourceCriteria
    ScanResourceCriteria (ScanResourceCriteria'),
    newScanResourceCriteria,

    -- ** ScanResultDetails
    ScanResultDetails (ScanResultDetails'),
    newScanResultDetails,

    -- ** ScanThreatName
    ScanThreatName (ScanThreatName'),
    newScanThreatName,

    -- ** ScannedItemCount
    ScannedItemCount (ScannedItemCount'),
    newScannedItemCount,

    -- ** SecurityContext
    SecurityContext (SecurityContext'),
    newSecurityContext,

    -- ** SecurityGroup
    SecurityGroup (SecurityGroup'),
    newSecurityGroup,

    -- ** ServiceAdditionalInfo
    ServiceAdditionalInfo (ServiceAdditionalInfo'),
    newServiceAdditionalInfo,

    -- ** ServiceInfo
    ServiceInfo (ServiceInfo'),
    newServiceInfo,

    -- ** SortCriteria
    SortCriteria (SortCriteria'),
    newSortCriteria,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** ThreatDetectedByName
    ThreatDetectedByName (ThreatDetectedByName'),
    newThreatDetectedByName,

    -- ** ThreatIntelligenceDetail
    ThreatIntelligenceDetail (ThreatIntelligenceDetail'),
    newThreatIntelligenceDetail,

    -- ** ThreatsDetectedItemCount
    ThreatsDetectedItemCount (ThreatsDetectedItemCount'),
    newThreatsDetectedItemCount,

    -- ** Total
    Total (Total'),
    newTotal,

    -- ** TriggerDetails
    TriggerDetails (TriggerDetails'),
    newTriggerDetails,

    -- ** UnprocessedAccount
    UnprocessedAccount (UnprocessedAccount'),
    newUnprocessedAccount,

    -- ** UnprocessedDataSourcesResult
    UnprocessedDataSourcesResult (UnprocessedDataSourcesResult'),
    newUnprocessedDataSourcesResult,

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

    -- ** Volume
    Volume (Volume'),
    newVolume,

    -- ** VolumeDetail
    VolumeDetail (VolumeDetail'),
    newVolumeDetail,

    -- ** VolumeMount
    VolumeMount (VolumeMount'),
    newVolumeMount,
  )
where

import Amazonka.GuardDuty.AcceptAdministratorInvitation
import Amazonka.GuardDuty.ArchiveFindings
import Amazonka.GuardDuty.CreateDetector
import Amazonka.GuardDuty.CreateFilter
import Amazonka.GuardDuty.CreateIPSet
import Amazonka.GuardDuty.CreateMembers
import Amazonka.GuardDuty.CreatePublishingDestination
import Amazonka.GuardDuty.CreateSampleFindings
import Amazonka.GuardDuty.CreateThreatIntelSet
import Amazonka.GuardDuty.DeclineInvitations
import Amazonka.GuardDuty.DeleteDetector
import Amazonka.GuardDuty.DeleteFilter
import Amazonka.GuardDuty.DeleteIPSet
import Amazonka.GuardDuty.DeleteInvitations
import Amazonka.GuardDuty.DeleteMembers
import Amazonka.GuardDuty.DeletePublishingDestination
import Amazonka.GuardDuty.DeleteThreatIntelSet
import Amazonka.GuardDuty.DescribeMalwareScans
import Amazonka.GuardDuty.DescribeOrganizationConfiguration
import Amazonka.GuardDuty.DescribePublishingDestination
import Amazonka.GuardDuty.DisableOrganizationAdminAccount
import Amazonka.GuardDuty.DisassociateFromAdministratorAccount
import Amazonka.GuardDuty.DisassociateMembers
import Amazonka.GuardDuty.EnableOrganizationAdminAccount
import Amazonka.GuardDuty.GetAdministratorAccount
import Amazonka.GuardDuty.GetDetector
import Amazonka.GuardDuty.GetFilter
import Amazonka.GuardDuty.GetFindings
import Amazonka.GuardDuty.GetFindingsStatistics
import Amazonka.GuardDuty.GetIPSet
import Amazonka.GuardDuty.GetInvitationsCount
import Amazonka.GuardDuty.GetMalwareScanSettings
import Amazonka.GuardDuty.GetMemberDetectors
import Amazonka.GuardDuty.GetMembers
import Amazonka.GuardDuty.GetRemainingFreeTrialDays
import Amazonka.GuardDuty.GetThreatIntelSet
import Amazonka.GuardDuty.GetUsageStatistics
import Amazonka.GuardDuty.InviteMembers
import Amazonka.GuardDuty.Lens
import Amazonka.GuardDuty.ListDetectors
import Amazonka.GuardDuty.ListFilters
import Amazonka.GuardDuty.ListFindings
import Amazonka.GuardDuty.ListIPSets
import Amazonka.GuardDuty.ListInvitations
import Amazonka.GuardDuty.ListMembers
import Amazonka.GuardDuty.ListOrganizationAdminAccounts
import Amazonka.GuardDuty.ListPublishingDestinations
import Amazonka.GuardDuty.ListTagsForResource
import Amazonka.GuardDuty.ListThreatIntelSets
import Amazonka.GuardDuty.StartMonitoringMembers
import Amazonka.GuardDuty.StopMonitoringMembers
import Amazonka.GuardDuty.TagResource
import Amazonka.GuardDuty.Types
import Amazonka.GuardDuty.UnarchiveFindings
import Amazonka.GuardDuty.UntagResource
import Amazonka.GuardDuty.UpdateDetector
import Amazonka.GuardDuty.UpdateFilter
import Amazonka.GuardDuty.UpdateFindingsFeedback
import Amazonka.GuardDuty.UpdateIPSet
import Amazonka.GuardDuty.UpdateMalwareScanSettings
import Amazonka.GuardDuty.UpdateMemberDetectors
import Amazonka.GuardDuty.UpdateOrganizationConfiguration
import Amazonka.GuardDuty.UpdatePublishingDestination
import Amazonka.GuardDuty.UpdateThreatIntelSet
import Amazonka.GuardDuty.Waiters

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
