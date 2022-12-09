{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Inspector2
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-06-08@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Inspector is a vulnerability discovery service that automates
-- continuous scanning for security vulnerabilities within your Amazon EC2
-- and Amazon ECR environments.
module Amazonka.Inspector2
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateMember
    AssociateMember (AssociateMember'),
    newAssociateMember,
    AssociateMemberResponse (AssociateMemberResponse'),
    newAssociateMemberResponse,

    -- ** BatchGetAccountStatus
    BatchGetAccountStatus (BatchGetAccountStatus'),
    newBatchGetAccountStatus,
    BatchGetAccountStatusResponse (BatchGetAccountStatusResponse'),
    newBatchGetAccountStatusResponse,

    -- ** BatchGetFreeTrialInfo
    BatchGetFreeTrialInfo (BatchGetFreeTrialInfo'),
    newBatchGetFreeTrialInfo,
    BatchGetFreeTrialInfoResponse (BatchGetFreeTrialInfoResponse'),
    newBatchGetFreeTrialInfoResponse,

    -- ** CancelFindingsReport
    CancelFindingsReport (CancelFindingsReport'),
    newCancelFindingsReport,
    CancelFindingsReportResponse (CancelFindingsReportResponse'),
    newCancelFindingsReportResponse,

    -- ** CreateFilter
    CreateFilter (CreateFilter'),
    newCreateFilter,
    CreateFilterResponse (CreateFilterResponse'),
    newCreateFilterResponse,

    -- ** CreateFindingsReport
    CreateFindingsReport (CreateFindingsReport'),
    newCreateFindingsReport,
    CreateFindingsReportResponse (CreateFindingsReportResponse'),
    newCreateFindingsReportResponse,

    -- ** DeleteFilter
    DeleteFilter (DeleteFilter'),
    newDeleteFilter,
    DeleteFilterResponse (DeleteFilterResponse'),
    newDeleteFilterResponse,

    -- ** DescribeOrganizationConfiguration
    DescribeOrganizationConfiguration (DescribeOrganizationConfiguration'),
    newDescribeOrganizationConfiguration,
    DescribeOrganizationConfigurationResponse (DescribeOrganizationConfigurationResponse'),
    newDescribeOrganizationConfigurationResponse,

    -- ** Disable
    Disable (Disable'),
    newDisable,
    DisableResponse (DisableResponse'),
    newDisableResponse,

    -- ** DisableDelegatedAdminAccount
    DisableDelegatedAdminAccount (DisableDelegatedAdminAccount'),
    newDisableDelegatedAdminAccount,
    DisableDelegatedAdminAccountResponse (DisableDelegatedAdminAccountResponse'),
    newDisableDelegatedAdminAccountResponse,

    -- ** DisassociateMember
    DisassociateMember (DisassociateMember'),
    newDisassociateMember,
    DisassociateMemberResponse (DisassociateMemberResponse'),
    newDisassociateMemberResponse,

    -- ** Enable
    Enable (Enable'),
    newEnable,
    EnableResponse (EnableResponse'),
    newEnableResponse,

    -- ** EnableDelegatedAdminAccount
    EnableDelegatedAdminAccount (EnableDelegatedAdminAccount'),
    newEnableDelegatedAdminAccount,
    EnableDelegatedAdminAccountResponse (EnableDelegatedAdminAccountResponse'),
    newEnableDelegatedAdminAccountResponse,

    -- ** GetConfiguration
    GetConfiguration (GetConfiguration'),
    newGetConfiguration,
    GetConfigurationResponse (GetConfigurationResponse'),
    newGetConfigurationResponse,

    -- ** GetDelegatedAdminAccount
    GetDelegatedAdminAccount (GetDelegatedAdminAccount'),
    newGetDelegatedAdminAccount,
    GetDelegatedAdminAccountResponse (GetDelegatedAdminAccountResponse'),
    newGetDelegatedAdminAccountResponse,

    -- ** GetFindingsReportStatus
    GetFindingsReportStatus (GetFindingsReportStatus'),
    newGetFindingsReportStatus,
    GetFindingsReportStatusResponse (GetFindingsReportStatusResponse'),
    newGetFindingsReportStatusResponse,

    -- ** GetMember
    GetMember (GetMember'),
    newGetMember,
    GetMemberResponse (GetMemberResponse'),
    newGetMemberResponse,

    -- ** ListAccountPermissions (Paginated)
    ListAccountPermissions (ListAccountPermissions'),
    newListAccountPermissions,
    ListAccountPermissionsResponse (ListAccountPermissionsResponse'),
    newListAccountPermissionsResponse,

    -- ** ListCoverage (Paginated)
    ListCoverage (ListCoverage'),
    newListCoverage,
    ListCoverageResponse (ListCoverageResponse'),
    newListCoverageResponse,

    -- ** ListCoverageStatistics (Paginated)
    ListCoverageStatistics (ListCoverageStatistics'),
    newListCoverageStatistics,
    ListCoverageStatisticsResponse (ListCoverageStatisticsResponse'),
    newListCoverageStatisticsResponse,

    -- ** ListDelegatedAdminAccounts (Paginated)
    ListDelegatedAdminAccounts (ListDelegatedAdminAccounts'),
    newListDelegatedAdminAccounts,
    ListDelegatedAdminAccountsResponse (ListDelegatedAdminAccountsResponse'),
    newListDelegatedAdminAccountsResponse,

    -- ** ListFilters (Paginated)
    ListFilters (ListFilters'),
    newListFilters,
    ListFiltersResponse (ListFiltersResponse'),
    newListFiltersResponse,

    -- ** ListFindingAggregations (Paginated)
    ListFindingAggregations (ListFindingAggregations'),
    newListFindingAggregations,
    ListFindingAggregationsResponse (ListFindingAggregationsResponse'),
    newListFindingAggregationsResponse,

    -- ** ListFindings (Paginated)
    ListFindings (ListFindings'),
    newListFindings,
    ListFindingsResponse (ListFindingsResponse'),
    newListFindingsResponse,

    -- ** ListMembers (Paginated)
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListUsageTotals (Paginated)
    ListUsageTotals (ListUsageTotals'),
    newListUsageTotals,
    ListUsageTotalsResponse (ListUsageTotalsResponse'),
    newListUsageTotalsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateConfiguration
    UpdateConfiguration (UpdateConfiguration'),
    newUpdateConfiguration,
    UpdateConfigurationResponse (UpdateConfigurationResponse'),
    newUpdateConfigurationResponse,

    -- ** UpdateFilter
    UpdateFilter (UpdateFilter'),
    newUpdateFilter,
    UpdateFilterResponse (UpdateFilterResponse'),
    newUpdateFilterResponse,

    -- ** UpdateOrganizationConfiguration
    UpdateOrganizationConfiguration (UpdateOrganizationConfiguration'),
    newUpdateOrganizationConfiguration,
    UpdateOrganizationConfigurationResponse (UpdateOrganizationConfigurationResponse'),
    newUpdateOrganizationConfigurationResponse,

    -- * Types

    -- ** AccountSortBy
    AccountSortBy (..),

    -- ** AggregationFindingType
    AggregationFindingType (..),

    -- ** AggregationResourceType
    AggregationResourceType (..),

    -- ** AggregationType
    AggregationType (..),

    -- ** AmiSortBy
    AmiSortBy (..),

    -- ** Architecture
    Architecture (..),

    -- ** AwsEcrContainerSortBy
    AwsEcrContainerSortBy (..),

    -- ** CoverageMapComparison
    CoverageMapComparison (..),

    -- ** CoverageResourceType
    CoverageResourceType (..),

    -- ** CoverageStringComparison
    CoverageStringComparison (..),

    -- ** Currency
    Currency (..),

    -- ** DelegatedAdminStatus
    DelegatedAdminStatus (..),

    -- ** Ec2InstanceSortBy
    Ec2InstanceSortBy (..),

    -- ** Ec2Platform
    Ec2Platform (..),

    -- ** EcrRescanDuration
    EcrRescanDuration (..),

    -- ** EcrRescanDurationStatus
    EcrRescanDurationStatus (..),

    -- ** EcrScanFrequency
    EcrScanFrequency (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** ExploitAvailable
    ExploitAvailable (..),

    -- ** ExternalReportStatus
    ExternalReportStatus (..),

    -- ** FilterAction
    FilterAction (..),

    -- ** FindingStatus
    FindingStatus (..),

    -- ** FindingType
    FindingType (..),

    -- ** FindingTypeSortBy
    FindingTypeSortBy (..),

    -- ** FixAvailable
    FixAvailable (..),

    -- ** FreeTrialInfoErrorCode
    FreeTrialInfoErrorCode (..),

    -- ** FreeTrialStatus
    FreeTrialStatus (..),

    -- ** FreeTrialType
    FreeTrialType (..),

    -- ** GroupKey
    GroupKey (..),

    -- ** ImageLayerSortBy
    ImageLayerSortBy (..),

    -- ** LambdaFunctionSortBy
    LambdaFunctionSortBy (..),

    -- ** LambdaLayerSortBy
    LambdaLayerSortBy (..),

    -- ** MapComparison
    MapComparison (..),

    -- ** NetworkProtocol
    NetworkProtocol (..),

    -- ** Operation
    Operation (..),

    -- ** PackageManager
    PackageManager (..),

    -- ** PackageSortBy
    PackageSortBy (..),

    -- ** PackageType
    PackageType (..),

    -- ** RelationshipStatus
    RelationshipStatus (..),

    -- ** ReportFormat
    ReportFormat (..),

    -- ** ReportingErrorCode
    ReportingErrorCode (..),

    -- ** RepositorySortBy
    RepositorySortBy (..),

    -- ** ResourceScanType
    ResourceScanType (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** Runtime
    Runtime (..),

    -- ** ScanStatusCode
    ScanStatusCode (..),

    -- ** ScanStatusReason
    ScanStatusReason (..),

    -- ** ScanType
    ScanType (..),

    -- ** Service
    Service (..),

    -- ** Severity
    Severity (..),

    -- ** SortField
    SortField (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** Status
    Status (..),

    -- ** StringComparison
    StringComparison (..),

    -- ** TitleSortBy
    TitleSortBy (..),

    -- ** UsageType
    UsageType (..),

    -- ** Account
    Account (Account'),
    newAccount,

    -- ** AccountAggregation
    AccountAggregation (AccountAggregation'),
    newAccountAggregation,

    -- ** AccountAggregationResponse
    AccountAggregationResponse (AccountAggregationResponse'),
    newAccountAggregationResponse,

    -- ** AccountState
    AccountState (AccountState'),
    newAccountState,

    -- ** AggregationRequest
    AggregationRequest (AggregationRequest'),
    newAggregationRequest,

    -- ** AggregationResponse
    AggregationResponse (AggregationResponse'),
    newAggregationResponse,

    -- ** AmiAggregation
    AmiAggregation (AmiAggregation'),
    newAmiAggregation,

    -- ** AmiAggregationResponse
    AmiAggregationResponse (AmiAggregationResponse'),
    newAmiAggregationResponse,

    -- ** AutoEnable
    AutoEnable (AutoEnable'),
    newAutoEnable,

    -- ** AwsEc2InstanceDetails
    AwsEc2InstanceDetails (AwsEc2InstanceDetails'),
    newAwsEc2InstanceDetails,

    -- ** AwsEcrContainerAggregation
    AwsEcrContainerAggregation (AwsEcrContainerAggregation'),
    newAwsEcrContainerAggregation,

    -- ** AwsEcrContainerAggregationResponse
    AwsEcrContainerAggregationResponse (AwsEcrContainerAggregationResponse'),
    newAwsEcrContainerAggregationResponse,

    -- ** AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (AwsEcrContainerImageDetails'),
    newAwsEcrContainerImageDetails,

    -- ** AwsLambdaFunctionDetails
    AwsLambdaFunctionDetails (AwsLambdaFunctionDetails'),
    newAwsLambdaFunctionDetails,

    -- ** Counts
    Counts (Counts'),
    newCounts,

    -- ** CoverageFilterCriteria
    CoverageFilterCriteria (CoverageFilterCriteria'),
    newCoverageFilterCriteria,

    -- ** CoverageMapFilter
    CoverageMapFilter (CoverageMapFilter'),
    newCoverageMapFilter,

    -- ** CoverageStringFilter
    CoverageStringFilter (CoverageStringFilter'),
    newCoverageStringFilter,

    -- ** CoveredResource
    CoveredResource (CoveredResource'),
    newCoveredResource,

    -- ** CvssScore
    CvssScore (CvssScore'),
    newCvssScore,

    -- ** CvssScoreAdjustment
    CvssScoreAdjustment (CvssScoreAdjustment'),
    newCvssScoreAdjustment,

    -- ** CvssScoreDetails
    CvssScoreDetails (CvssScoreDetails'),
    newCvssScoreDetails,

    -- ** DateFilter
    DateFilter (DateFilter'),
    newDateFilter,

    -- ** DelegatedAdmin
    DelegatedAdmin (DelegatedAdmin'),
    newDelegatedAdmin,

    -- ** DelegatedAdminAccount
    DelegatedAdminAccount (DelegatedAdminAccount'),
    newDelegatedAdminAccount,

    -- ** Destination
    Destination (Destination'),
    newDestination,

    -- ** Ec2InstanceAggregation
    Ec2InstanceAggregation (Ec2InstanceAggregation'),
    newEc2InstanceAggregation,

    -- ** Ec2InstanceAggregationResponse
    Ec2InstanceAggregationResponse (Ec2InstanceAggregationResponse'),
    newEc2InstanceAggregationResponse,

    -- ** Ec2Metadata
    Ec2Metadata (Ec2Metadata'),
    newEc2Metadata,

    -- ** EcrConfiguration
    EcrConfiguration (EcrConfiguration'),
    newEcrConfiguration,

    -- ** EcrConfigurationState
    EcrConfigurationState (EcrConfigurationState'),
    newEcrConfigurationState,

    -- ** EcrContainerImageMetadata
    EcrContainerImageMetadata (EcrContainerImageMetadata'),
    newEcrContainerImageMetadata,

    -- ** EcrRepositoryMetadata
    EcrRepositoryMetadata (EcrRepositoryMetadata'),
    newEcrRepositoryMetadata,

    -- ** EcrRescanDurationState
    EcrRescanDurationState (EcrRescanDurationState'),
    newEcrRescanDurationState,

    -- ** ExploitabilityDetails
    ExploitabilityDetails (ExploitabilityDetails'),
    newExploitabilityDetails,

    -- ** FailedAccount
    FailedAccount (FailedAccount'),
    newFailedAccount,

    -- ** Filter
    Filter (Filter'),
    newFilter,

    -- ** FilterCriteria
    FilterCriteria (FilterCriteria'),
    newFilterCriteria,

    -- ** Finding
    Finding (Finding'),
    newFinding,

    -- ** FindingTypeAggregation
    FindingTypeAggregation (FindingTypeAggregation'),
    newFindingTypeAggregation,

    -- ** FindingTypeAggregationResponse
    FindingTypeAggregationResponse (FindingTypeAggregationResponse'),
    newFindingTypeAggregationResponse,

    -- ** FreeTrialAccountInfo
    FreeTrialAccountInfo (FreeTrialAccountInfo'),
    newFreeTrialAccountInfo,

    -- ** FreeTrialInfo
    FreeTrialInfo (FreeTrialInfo'),
    newFreeTrialInfo,

    -- ** FreeTrialInfoError
    FreeTrialInfoError (FreeTrialInfoError'),
    newFreeTrialInfoError,

    -- ** ImageLayerAggregation
    ImageLayerAggregation (ImageLayerAggregation'),
    newImageLayerAggregation,

    -- ** ImageLayerAggregationResponse
    ImageLayerAggregationResponse (ImageLayerAggregationResponse'),
    newImageLayerAggregationResponse,

    -- ** InspectorScoreDetails
    InspectorScoreDetails (InspectorScoreDetails'),
    newInspectorScoreDetails,

    -- ** LambdaFunctionAggregation
    LambdaFunctionAggregation (LambdaFunctionAggregation'),
    newLambdaFunctionAggregation,

    -- ** LambdaFunctionAggregationResponse
    LambdaFunctionAggregationResponse (LambdaFunctionAggregationResponse'),
    newLambdaFunctionAggregationResponse,

    -- ** LambdaFunctionMetadata
    LambdaFunctionMetadata (LambdaFunctionMetadata'),
    newLambdaFunctionMetadata,

    -- ** LambdaLayerAggregation
    LambdaLayerAggregation (LambdaLayerAggregation'),
    newLambdaLayerAggregation,

    -- ** LambdaLayerAggregationResponse
    LambdaLayerAggregationResponse (LambdaLayerAggregationResponse'),
    newLambdaLayerAggregationResponse,

    -- ** LambdaVpcConfig
    LambdaVpcConfig (LambdaVpcConfig'),
    newLambdaVpcConfig,

    -- ** MapFilter
    MapFilter (MapFilter'),
    newMapFilter,

    -- ** Member
    Member (Member'),
    newMember,

    -- ** NetworkPath
    NetworkPath (NetworkPath'),
    newNetworkPath,

    -- ** NetworkReachabilityDetails
    NetworkReachabilityDetails (NetworkReachabilityDetails'),
    newNetworkReachabilityDetails,

    -- ** NumberFilter
    NumberFilter (NumberFilter'),
    newNumberFilter,

    -- ** PackageAggregation
    PackageAggregation (PackageAggregation'),
    newPackageAggregation,

    -- ** PackageAggregationResponse
    PackageAggregationResponse (PackageAggregationResponse'),
    newPackageAggregationResponse,

    -- ** PackageFilter
    PackageFilter (PackageFilter'),
    newPackageFilter,

    -- ** PackageVulnerabilityDetails
    PackageVulnerabilityDetails (PackageVulnerabilityDetails'),
    newPackageVulnerabilityDetails,

    -- ** Permission
    Permission (Permission'),
    newPermission,

    -- ** PortRange
    PortRange (PortRange'),
    newPortRange,

    -- ** PortRangeFilter
    PortRangeFilter (PortRangeFilter'),
    newPortRangeFilter,

    -- ** Recommendation
    Recommendation (Recommendation'),
    newRecommendation,

    -- ** Remediation
    Remediation (Remediation'),
    newRemediation,

    -- ** RepositoryAggregation
    RepositoryAggregation (RepositoryAggregation'),
    newRepositoryAggregation,

    -- ** RepositoryAggregationResponse
    RepositoryAggregationResponse (RepositoryAggregationResponse'),
    newRepositoryAggregationResponse,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceDetails
    ResourceDetails (ResourceDetails'),
    newResourceDetails,

    -- ** ResourceScanMetadata
    ResourceScanMetadata (ResourceScanMetadata'),
    newResourceScanMetadata,

    -- ** ResourceState
    ResourceState (ResourceState'),
    newResourceState,

    -- ** ResourceStatus
    ResourceStatus (ResourceStatus'),
    newResourceStatus,

    -- ** ScanStatus
    ScanStatus (ScanStatus'),
    newScanStatus,

    -- ** SeverityCounts
    SeverityCounts (SeverityCounts'),
    newSeverityCounts,

    -- ** SortCriteria
    SortCriteria (SortCriteria'),
    newSortCriteria,

    -- ** State
    State (State'),
    newState,

    -- ** Step
    Step (Step'),
    newStep,

    -- ** StringFilter
    StringFilter (StringFilter'),
    newStringFilter,

    -- ** TitleAggregation
    TitleAggregation (TitleAggregation'),
    newTitleAggregation,

    -- ** TitleAggregationResponse
    TitleAggregationResponse (TitleAggregationResponse'),
    newTitleAggregationResponse,

    -- ** Usage
    Usage (Usage'),
    newUsage,

    -- ** UsageTotal
    UsageTotal (UsageTotal'),
    newUsageTotal,

    -- ** VulnerablePackage
    VulnerablePackage (VulnerablePackage'),
    newVulnerablePackage,
  )
where

import Amazonka.Inspector2.AssociateMember
import Amazonka.Inspector2.BatchGetAccountStatus
import Amazonka.Inspector2.BatchGetFreeTrialInfo
import Amazonka.Inspector2.CancelFindingsReport
import Amazonka.Inspector2.CreateFilter
import Amazonka.Inspector2.CreateFindingsReport
import Amazonka.Inspector2.DeleteFilter
import Amazonka.Inspector2.DescribeOrganizationConfiguration
import Amazonka.Inspector2.Disable
import Amazonka.Inspector2.DisableDelegatedAdminAccount
import Amazonka.Inspector2.DisassociateMember
import Amazonka.Inspector2.Enable
import Amazonka.Inspector2.EnableDelegatedAdminAccount
import Amazonka.Inspector2.GetConfiguration
import Amazonka.Inspector2.GetDelegatedAdminAccount
import Amazonka.Inspector2.GetFindingsReportStatus
import Amazonka.Inspector2.GetMember
import Amazonka.Inspector2.Lens
import Amazonka.Inspector2.ListAccountPermissions
import Amazonka.Inspector2.ListCoverage
import Amazonka.Inspector2.ListCoverageStatistics
import Amazonka.Inspector2.ListDelegatedAdminAccounts
import Amazonka.Inspector2.ListFilters
import Amazonka.Inspector2.ListFindingAggregations
import Amazonka.Inspector2.ListFindings
import Amazonka.Inspector2.ListMembers
import Amazonka.Inspector2.ListTagsForResource
import Amazonka.Inspector2.ListUsageTotals
import Amazonka.Inspector2.TagResource
import Amazonka.Inspector2.Types
import Amazonka.Inspector2.UntagResource
import Amazonka.Inspector2.UpdateConfiguration
import Amazonka.Inspector2.UpdateFilter
import Amazonka.Inspector2.UpdateOrganizationConfiguration
import Amazonka.Inspector2.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Inspector2'.

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
