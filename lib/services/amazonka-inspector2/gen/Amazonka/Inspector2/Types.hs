{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ThrottlingException,
    _BadRequestException,
    _ValidationException,

    -- * AccountSortBy
    AccountSortBy (..),

    -- * AggregationFindingType
    AggregationFindingType (..),

    -- * AggregationResourceType
    AggregationResourceType (..),

    -- * AggregationType
    AggregationType (..),

    -- * AmiSortBy
    AmiSortBy (..),

    -- * AwsEcrContainerSortBy
    AwsEcrContainerSortBy (..),

    -- * CoverageMapComparison
    CoverageMapComparison (..),

    -- * CoverageResourceType
    CoverageResourceType (..),

    -- * CoverageStringComparison
    CoverageStringComparison (..),

    -- * Currency
    Currency (..),

    -- * DelegatedAdminStatus
    DelegatedAdminStatus (..),

    -- * Ec2InstanceSortBy
    Ec2InstanceSortBy (..),

    -- * Ec2Platform
    Ec2Platform (..),

    -- * EcrRescanDuration
    EcrRescanDuration (..),

    -- * EcrRescanDurationStatus
    EcrRescanDurationStatus (..),

    -- * EcrScanFrequency
    EcrScanFrequency (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ExternalReportStatus
    ExternalReportStatus (..),

    -- * FilterAction
    FilterAction (..),

    -- * FindingStatus
    FindingStatus (..),

    -- * FindingType
    FindingType (..),

    -- * FindingTypeSortBy
    FindingTypeSortBy (..),

    -- * FixAvailable
    FixAvailable (..),

    -- * FreeTrialInfoErrorCode
    FreeTrialInfoErrorCode (..),

    -- * FreeTrialStatus
    FreeTrialStatus (..),

    -- * FreeTrialType
    FreeTrialType (..),

    -- * GroupKey
    GroupKey (..),

    -- * ImageLayerSortBy
    ImageLayerSortBy (..),

    -- * MapComparison
    MapComparison (..),

    -- * NetworkProtocol
    NetworkProtocol (..),

    -- * Operation
    Operation (..),

    -- * PackageManager
    PackageManager (..),

    -- * PackageSortBy
    PackageSortBy (..),

    -- * RelationshipStatus
    RelationshipStatus (..),

    -- * ReportFormat
    ReportFormat (..),

    -- * ReportingErrorCode
    ReportingErrorCode (..),

    -- * RepositorySortBy
    RepositorySortBy (..),

    -- * ResourceScanType
    ResourceScanType (..),

    -- * ResourceType
    ResourceType (..),

    -- * ScanStatusCode
    ScanStatusCode (..),

    -- * ScanStatusReason
    ScanStatusReason (..),

    -- * ScanType
    ScanType (..),

    -- * Service
    Service (..),

    -- * Severity
    Severity (..),

    -- * SortField
    SortField (..),

    -- * SortOrder
    SortOrder (..),

    -- * Status
    Status (..),

    -- * StringComparison
    StringComparison (..),

    -- * TitleSortBy
    TitleSortBy (..),

    -- * UsageType
    UsageType (..),

    -- * Account
    Account (..),
    newAccount,
    account_accountId,
    account_resourceStatus,
    account_status,

    -- * AccountAggregation
    AccountAggregation (..),
    newAccountAggregation,
    accountAggregation_resourceType,
    accountAggregation_sortOrder,
    accountAggregation_sortBy,
    accountAggregation_findingType,

    -- * AccountAggregationResponse
    AccountAggregationResponse (..),
    newAccountAggregationResponse,
    accountAggregationResponse_severityCounts,
    accountAggregationResponse_accountId,

    -- * AccountState
    AccountState (..),
    newAccountState,
    accountState_accountId,
    accountState_resourceState,
    accountState_state,

    -- * AggregationRequest
    AggregationRequest (..),
    newAggregationRequest,
    aggregationRequest_repositoryAggregation,
    aggregationRequest_imageLayerAggregation,
    aggregationRequest_accountAggregation,
    aggregationRequest_awsEcrContainerAggregation,
    aggregationRequest_ec2InstanceAggregation,
    aggregationRequest_findingTypeAggregation,
    aggregationRequest_packageAggregation,
    aggregationRequest_titleAggregation,
    aggregationRequest_amiAggregation,

    -- * AggregationResponse
    AggregationResponse (..),
    newAggregationResponse,
    aggregationResponse_repositoryAggregation,
    aggregationResponse_imageLayerAggregation,
    aggregationResponse_accountAggregation,
    aggregationResponse_awsEcrContainerAggregation,
    aggregationResponse_ec2InstanceAggregation,
    aggregationResponse_findingTypeAggregation,
    aggregationResponse_packageAggregation,
    aggregationResponse_titleAggregation,
    aggregationResponse_amiAggregation,

    -- * AmiAggregation
    AmiAggregation (..),
    newAmiAggregation,
    amiAggregation_sortOrder,
    amiAggregation_sortBy,
    amiAggregation_amis,

    -- * AmiAggregationResponse
    AmiAggregationResponse (..),
    newAmiAggregationResponse,
    amiAggregationResponse_severityCounts,
    amiAggregationResponse_accountId,
    amiAggregationResponse_affectedInstances,
    amiAggregationResponse_ami,

    -- * AutoEnable
    AutoEnable (..),
    newAutoEnable,
    autoEnable_ec2,
    autoEnable_ecr,

    -- * AwsEc2InstanceDetails
    AwsEc2InstanceDetails (..),
    newAwsEc2InstanceDetails,
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_platform,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_vpcId,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_imageId,

    -- * AwsEcrContainerAggregation
    AwsEcrContainerAggregation (..),
    newAwsEcrContainerAggregation,
    awsEcrContainerAggregation_sortOrder,
    awsEcrContainerAggregation_imageShas,
    awsEcrContainerAggregation_sortBy,
    awsEcrContainerAggregation_repositories,
    awsEcrContainerAggregation_resourceIds,
    awsEcrContainerAggregation_imageTags,
    awsEcrContainerAggregation_architectures,

    -- * AwsEcrContainerAggregationResponse
    AwsEcrContainerAggregationResponse (..),
    newAwsEcrContainerAggregationResponse,
    awsEcrContainerAggregationResponse_severityCounts,
    awsEcrContainerAggregationResponse_repository,
    awsEcrContainerAggregationResponse_imageSha,
    awsEcrContainerAggregationResponse_accountId,
    awsEcrContainerAggregationResponse_imageTags,
    awsEcrContainerAggregationResponse_architecture,
    awsEcrContainerAggregationResponse_resourceId,

    -- * AwsEcrContainerImageDetails
    AwsEcrContainerImageDetails (..),
    newAwsEcrContainerImageDetails,
    awsEcrContainerImageDetails_author,
    awsEcrContainerImageDetails_platform,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_pushedAt,
    awsEcrContainerImageDetails_imageHash,
    awsEcrContainerImageDetails_registry,
    awsEcrContainerImageDetails_repositoryName,

    -- * Counts
    Counts (..),
    newCounts,
    counts_groupKey,
    counts_count,

    -- * CoverageFilterCriteria
    CoverageFilterCriteria (..),
    newCoverageFilterCriteria,
    coverageFilterCriteria_resourceId,
    coverageFilterCriteria_resourceType,
    coverageFilterCriteria_ecrImageTags,
    coverageFilterCriteria_scanType,
    coverageFilterCriteria_accountId,
    coverageFilterCriteria_ec2InstanceTags,
    coverageFilterCriteria_scanStatusReason,
    coverageFilterCriteria_scanStatusCode,
    coverageFilterCriteria_ecrRepositoryName,

    -- * CoverageMapFilter
    CoverageMapFilter (..),
    newCoverageMapFilter,
    coverageMapFilter_value,
    coverageMapFilter_comparison,
    coverageMapFilter_key,

    -- * CoverageStringFilter
    CoverageStringFilter (..),
    newCoverageStringFilter,
    coverageStringFilter_comparison,
    coverageStringFilter_value,

    -- * CoveredResource
    CoveredResource (..),
    newCoveredResource,
    coveredResource_resourceMetadata,
    coveredResource_scanStatus,
    coveredResource_accountId,
    coveredResource_resourceId,
    coveredResource_resourceType,
    coveredResource_scanType,

    -- * CvssScore
    CvssScore (..),
    newCvssScore,
    cvssScore_baseScore,
    cvssScore_scoringVector,
    cvssScore_source,
    cvssScore_version,

    -- * CvssScoreAdjustment
    CvssScoreAdjustment (..),
    newCvssScoreAdjustment,
    cvssScoreAdjustment_metric,
    cvssScoreAdjustment_reason,

    -- * CvssScoreDetails
    CvssScoreDetails (..),
    newCvssScoreDetails,
    cvssScoreDetails_adjustments,
    cvssScoreDetails_cvssSource,
    cvssScoreDetails_score,
    cvssScoreDetails_scoreSource,
    cvssScoreDetails_scoringVector,
    cvssScoreDetails_version,

    -- * DateFilter
    DateFilter (..),
    newDateFilter,
    dateFilter_startInclusive,
    dateFilter_endInclusive,

    -- * DelegatedAdmin
    DelegatedAdmin (..),
    newDelegatedAdmin,
    delegatedAdmin_accountId,
    delegatedAdmin_relationshipStatus,

    -- * DelegatedAdminAccount
    DelegatedAdminAccount (..),
    newDelegatedAdminAccount,
    delegatedAdminAccount_status,
    delegatedAdminAccount_accountId,

    -- * Destination
    Destination (..),
    newDestination,
    destination_keyPrefix,
    destination_bucketName,
    destination_kmsKeyArn,

    -- * Ec2InstanceAggregation
    Ec2InstanceAggregation (..),
    newEc2InstanceAggregation,
    ec2InstanceAggregation_sortOrder,
    ec2InstanceAggregation_instanceTags,
    ec2InstanceAggregation_sortBy,
    ec2InstanceAggregation_operatingSystems,
    ec2InstanceAggregation_instanceIds,
    ec2InstanceAggregation_amis,

    -- * Ec2InstanceAggregationResponse
    Ec2InstanceAggregationResponse (..),
    newEc2InstanceAggregationResponse,
    ec2InstanceAggregationResponse_severityCounts,
    ec2InstanceAggregationResponse_operatingSystem,
    ec2InstanceAggregationResponse_instanceTags,
    ec2InstanceAggregationResponse_networkFindings,
    ec2InstanceAggregationResponse_accountId,
    ec2InstanceAggregationResponse_ami,
    ec2InstanceAggregationResponse_instanceId,

    -- * Ec2Metadata
    Ec2Metadata (..),
    newEc2Metadata,
    ec2Metadata_tags,
    ec2Metadata_amiId,
    ec2Metadata_platform,

    -- * EcrConfiguration
    EcrConfiguration (..),
    newEcrConfiguration,
    ecrConfiguration_rescanDuration,

    -- * EcrConfigurationState
    EcrConfigurationState (..),
    newEcrConfigurationState,
    ecrConfigurationState_rescanDurationState,

    -- * EcrContainerImageMetadata
    EcrContainerImageMetadata (..),
    newEcrContainerImageMetadata,
    ecrContainerImageMetadata_tags,

    -- * EcrRepositoryMetadata
    EcrRepositoryMetadata (..),
    newEcrRepositoryMetadata,
    ecrRepositoryMetadata_name,
    ecrRepositoryMetadata_scanFrequency,

    -- * EcrRescanDurationState
    EcrRescanDurationState (..),
    newEcrRescanDurationState,
    ecrRescanDurationState_status,
    ecrRescanDurationState_rescanDuration,
    ecrRescanDurationState_updatedAt,

    -- * FailedAccount
    FailedAccount (..),
    newFailedAccount,
    failedAccount_status,
    failedAccount_resourceStatus,
    failedAccount_accountId,
    failedAccount_errorCode,
    failedAccount_errorMessage,

    -- * Filter
    Filter (..),
    newFilter,
    filter_tags,
    filter_description,
    filter_reason,
    filter_action,
    filter_arn,
    filter_createdAt,
    filter_criteria,
    filter_name,
    filter_ownerId,
    filter_updatedAt,

    -- * FilterCriteria
    FilterCriteria (..),
    newFilterCriteria,
    filterCriteria_awsAccountId,
    filterCriteria_ec2InstanceImageId,
    filterCriteria_networkProtocol,
    filterCriteria_resourceId,
    filterCriteria_resourceType,
    filterCriteria_ecrImageRegistry,
    filterCriteria_severity,
    filterCriteria_portRange,
    filterCriteria_ecrImageArchitecture,
    filterCriteria_findingStatus,
    filterCriteria_vulnerablePackages,
    filterCriteria_vulnerabilitySource,
    filterCriteria_ecrImageRepositoryName,
    filterCriteria_inspectorScore,
    filterCriteria_ecrImageTags,
    filterCriteria_resourceTags,
    filterCriteria_ecrImageHash,
    filterCriteria_title,
    filterCriteria_firstObservedAt,
    filterCriteria_ecrImagePushedAt,
    filterCriteria_vendorSeverity,
    filterCriteria_lastObservedAt,
    filterCriteria_ec2InstanceVpcId,
    filterCriteria_componentId,
    filterCriteria_relatedVulnerabilities,
    filterCriteria_findingType,
    filterCriteria_componentType,
    filterCriteria_vulnerabilityId,
    filterCriteria_fixAvailable,
    filterCriteria_findingArn,
    filterCriteria_ec2InstanceSubnetId,
    filterCriteria_updatedAt,

    -- * Finding
    Finding (..),
    newFinding,
    finding_inspectorScore,
    finding_packageVulnerabilityDetails,
    finding_title,
    finding_inspectorScoreDetails,
    finding_networkReachabilityDetails,
    finding_fixAvailable,
    finding_updatedAt,
    finding_awsAccountId,
    finding_description,
    finding_findingArn,
    finding_firstObservedAt,
    finding_lastObservedAt,
    finding_remediation,
    finding_resources,
    finding_severity,
    finding_status,
    finding_type,

    -- * FindingTypeAggregation
    FindingTypeAggregation (..),
    newFindingTypeAggregation,
    findingTypeAggregation_resourceType,
    findingTypeAggregation_sortOrder,
    findingTypeAggregation_sortBy,
    findingTypeAggregation_findingType,

    -- * FindingTypeAggregationResponse
    FindingTypeAggregationResponse (..),
    newFindingTypeAggregationResponse,
    findingTypeAggregationResponse_severityCounts,
    findingTypeAggregationResponse_accountId,

    -- * FreeTrialAccountInfo
    FreeTrialAccountInfo (..),
    newFreeTrialAccountInfo,
    freeTrialAccountInfo_accountId,
    freeTrialAccountInfo_freeTrialInfo,

    -- * FreeTrialInfo
    FreeTrialInfo (..),
    newFreeTrialInfo,
    freeTrialInfo_end,
    freeTrialInfo_start,
    freeTrialInfo_status,
    freeTrialInfo_type,

    -- * FreeTrialInfoError
    FreeTrialInfoError (..),
    newFreeTrialInfoError,
    freeTrialInfoError_accountId,
    freeTrialInfoError_code,
    freeTrialInfoError_message,

    -- * ImageLayerAggregation
    ImageLayerAggregation (..),
    newImageLayerAggregation,
    imageLayerAggregation_sortOrder,
    imageLayerAggregation_sortBy,
    imageLayerAggregation_repositories,
    imageLayerAggregation_resourceIds,
    imageLayerAggregation_layerHashes,

    -- * ImageLayerAggregationResponse
    ImageLayerAggregationResponse (..),
    newImageLayerAggregationResponse,
    imageLayerAggregationResponse_severityCounts,
    imageLayerAggregationResponse_accountId,
    imageLayerAggregationResponse_layerHash,
    imageLayerAggregationResponse_repository,
    imageLayerAggregationResponse_resourceId,

    -- * InspectorScoreDetails
    InspectorScoreDetails (..),
    newInspectorScoreDetails,
    inspectorScoreDetails_adjustedCvss,

    -- * MapFilter
    MapFilter (..),
    newMapFilter,
    mapFilter_value,
    mapFilter_comparison,
    mapFilter_key,

    -- * Member
    Member (..),
    newMember,
    member_delegatedAdminAccountId,
    member_accountId,
    member_relationshipStatus,
    member_updatedAt,

    -- * NetworkPath
    NetworkPath (..),
    newNetworkPath,
    networkPath_steps,

    -- * NetworkReachabilityDetails
    NetworkReachabilityDetails (..),
    newNetworkReachabilityDetails,
    networkReachabilityDetails_networkPath,
    networkReachabilityDetails_openPortRange,
    networkReachabilityDetails_protocol,

    -- * NumberFilter
    NumberFilter (..),
    newNumberFilter,
    numberFilter_lowerInclusive,
    numberFilter_upperInclusive,

    -- * PackageAggregation
    PackageAggregation (..),
    newPackageAggregation,
    packageAggregation_sortOrder,
    packageAggregation_sortBy,
    packageAggregation_packageNames,

    -- * PackageAggregationResponse
    PackageAggregationResponse (..),
    newPackageAggregationResponse,
    packageAggregationResponse_severityCounts,
    packageAggregationResponse_accountId,
    packageAggregationResponse_packageName,

    -- * PackageFilter
    PackageFilter (..),
    newPackageFilter,
    packageFilter_name,
    packageFilter_epoch,
    packageFilter_release,
    packageFilter_sourceLayerHash,
    packageFilter_architecture,
    packageFilter_version,

    -- * PackageVulnerabilityDetails
    PackageVulnerabilityDetails (..),
    newPackageVulnerabilityDetails,
    packageVulnerabilityDetails_vulnerablePackages,
    packageVulnerabilityDetails_referenceUrls,
    packageVulnerabilityDetails_vendorUpdatedAt,
    packageVulnerabilityDetails_vendorCreatedAt,
    packageVulnerabilityDetails_cvss,
    packageVulnerabilityDetails_vendorSeverity,
    packageVulnerabilityDetails_relatedVulnerabilities,
    packageVulnerabilityDetails_sourceUrl,
    packageVulnerabilityDetails_source,
    packageVulnerabilityDetails_vulnerabilityId,

    -- * Permission
    Permission (..),
    newPermission,
    permission_operation,
    permission_service,

    -- * PortRange
    PortRange (..),
    newPortRange,
    portRange_begin,
    portRange_end,

    -- * PortRangeFilter
    PortRangeFilter (..),
    newPortRangeFilter,
    portRangeFilter_beginInclusive,
    portRangeFilter_endInclusive,

    -- * Recommendation
    Recommendation (..),
    newRecommendation,
    recommendation_url,
    recommendation_text,

    -- * Remediation
    Remediation (..),
    newRemediation,
    remediation_recommendation,

    -- * RepositoryAggregation
    RepositoryAggregation (..),
    newRepositoryAggregation,
    repositoryAggregation_sortOrder,
    repositoryAggregation_sortBy,
    repositoryAggregation_repositories,

    -- * RepositoryAggregationResponse
    RepositoryAggregationResponse (..),
    newRepositoryAggregationResponse,
    repositoryAggregationResponse_severityCounts,
    repositoryAggregationResponse_affectedImages,
    repositoryAggregationResponse_accountId,
    repositoryAggregationResponse_repository,

    -- * Resource
    Resource (..),
    newResource,
    resource_tags,
    resource_details,
    resource_partition,
    resource_region,
    resource_id,
    resource_type,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_awsEc2Instance,
    resourceDetails_awsEcrContainerImage,

    -- * ResourceScanMetadata
    ResourceScanMetadata (..),
    newResourceScanMetadata,
    resourceScanMetadata_ecrRepository,
    resourceScanMetadata_ec2,
    resourceScanMetadata_ecrImage,

    -- * ResourceState
    ResourceState (..),
    newResourceState,
    resourceState_ec2,
    resourceState_ecr,

    -- * ResourceStatus
    ResourceStatus (..),
    newResourceStatus,
    resourceStatus_ec2,
    resourceStatus_ecr,

    -- * ScanStatus
    ScanStatus (..),
    newScanStatus,
    scanStatus_reason,
    scanStatus_statusCode,

    -- * SeverityCounts
    SeverityCounts (..),
    newSeverityCounts,
    severityCounts_critical,
    severityCounts_high,
    severityCounts_all,
    severityCounts_medium,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_field,
    sortCriteria_sortOrder,

    -- * State
    State (..),
    newState,
    state_errorCode,
    state_errorMessage,
    state_status,

    -- * Step
    Step (..),
    newStep,
    step_componentId,
    step_componentType,

    -- * StringFilter
    StringFilter (..),
    newStringFilter,
    stringFilter_comparison,
    stringFilter_value,

    -- * TitleAggregation
    TitleAggregation (..),
    newTitleAggregation,
    titleAggregation_resourceType,
    titleAggregation_sortOrder,
    titleAggregation_vulnerabilityIds,
    titleAggregation_sortBy,
    titleAggregation_titles,

    -- * TitleAggregationResponse
    TitleAggregationResponse (..),
    newTitleAggregationResponse,
    titleAggregationResponse_severityCounts,
    titleAggregationResponse_accountId,
    titleAggregationResponse_vulnerabilityId,
    titleAggregationResponse_title,

    -- * Usage
    Usage (..),
    newUsage,
    usage_type,
    usage_total,
    usage_estimatedMonthlyCost,
    usage_currency,

    -- * UsageTotal
    UsageTotal (..),
    newUsageTotal,
    usageTotal_usage,
    usageTotal_accountId,

    -- * VulnerablePackage
    VulnerablePackage (..),
    newVulnerablePackage,
    vulnerablePackage_filePath,
    vulnerablePackage_fixedInVersion,
    vulnerablePackage_remediation,
    vulnerablePackage_arch,
    vulnerablePackage_epoch,
    vulnerablePackage_packageManager,
    vulnerablePackage_release,
    vulnerablePackage_sourceLayerHash,
    vulnerablePackage_name,
    vulnerablePackage_version,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.Account
import Amazonka.Inspector2.Types.AccountAggregation
import Amazonka.Inspector2.Types.AccountAggregationResponse
import Amazonka.Inspector2.Types.AccountSortBy
import Amazonka.Inspector2.Types.AccountState
import Amazonka.Inspector2.Types.AggregationFindingType
import Amazonka.Inspector2.Types.AggregationRequest
import Amazonka.Inspector2.Types.AggregationResourceType
import Amazonka.Inspector2.Types.AggregationResponse
import Amazonka.Inspector2.Types.AggregationType
import Amazonka.Inspector2.Types.AmiAggregation
import Amazonka.Inspector2.Types.AmiAggregationResponse
import Amazonka.Inspector2.Types.AmiSortBy
import Amazonka.Inspector2.Types.AutoEnable
import Amazonka.Inspector2.Types.AwsEc2InstanceDetails
import Amazonka.Inspector2.Types.AwsEcrContainerAggregation
import Amazonka.Inspector2.Types.AwsEcrContainerAggregationResponse
import Amazonka.Inspector2.Types.AwsEcrContainerImageDetails
import Amazonka.Inspector2.Types.AwsEcrContainerSortBy
import Amazonka.Inspector2.Types.Counts
import Amazonka.Inspector2.Types.CoverageFilterCriteria
import Amazonka.Inspector2.Types.CoverageMapComparison
import Amazonka.Inspector2.Types.CoverageMapFilter
import Amazonka.Inspector2.Types.CoverageResourceType
import Amazonka.Inspector2.Types.CoverageStringComparison
import Amazonka.Inspector2.Types.CoverageStringFilter
import Amazonka.Inspector2.Types.CoveredResource
import Amazonka.Inspector2.Types.Currency
import Amazonka.Inspector2.Types.CvssScore
import Amazonka.Inspector2.Types.CvssScoreAdjustment
import Amazonka.Inspector2.Types.CvssScoreDetails
import Amazonka.Inspector2.Types.DateFilter
import Amazonka.Inspector2.Types.DelegatedAdmin
import Amazonka.Inspector2.Types.DelegatedAdminAccount
import Amazonka.Inspector2.Types.DelegatedAdminStatus
import Amazonka.Inspector2.Types.Destination
import Amazonka.Inspector2.Types.Ec2InstanceAggregation
import Amazonka.Inspector2.Types.Ec2InstanceAggregationResponse
import Amazonka.Inspector2.Types.Ec2InstanceSortBy
import Amazonka.Inspector2.Types.Ec2Metadata
import Amazonka.Inspector2.Types.Ec2Platform
import Amazonka.Inspector2.Types.EcrConfiguration
import Amazonka.Inspector2.Types.EcrConfigurationState
import Amazonka.Inspector2.Types.EcrContainerImageMetadata
import Amazonka.Inspector2.Types.EcrRepositoryMetadata
import Amazonka.Inspector2.Types.EcrRescanDuration
import Amazonka.Inspector2.Types.EcrRescanDurationState
import Amazonka.Inspector2.Types.EcrRescanDurationStatus
import Amazonka.Inspector2.Types.EcrScanFrequency
import Amazonka.Inspector2.Types.ErrorCode
import Amazonka.Inspector2.Types.ExternalReportStatus
import Amazonka.Inspector2.Types.FailedAccount
import Amazonka.Inspector2.Types.Filter
import Amazonka.Inspector2.Types.FilterAction
import Amazonka.Inspector2.Types.FilterCriteria
import Amazonka.Inspector2.Types.Finding
import Amazonka.Inspector2.Types.FindingStatus
import Amazonka.Inspector2.Types.FindingType
import Amazonka.Inspector2.Types.FindingTypeAggregation
import Amazonka.Inspector2.Types.FindingTypeAggregationResponse
import Amazonka.Inspector2.Types.FindingTypeSortBy
import Amazonka.Inspector2.Types.FixAvailable
import Amazonka.Inspector2.Types.FreeTrialAccountInfo
import Amazonka.Inspector2.Types.FreeTrialInfo
import Amazonka.Inspector2.Types.FreeTrialInfoError
import Amazonka.Inspector2.Types.FreeTrialInfoErrorCode
import Amazonka.Inspector2.Types.FreeTrialStatus
import Amazonka.Inspector2.Types.FreeTrialType
import Amazonka.Inspector2.Types.GroupKey
import Amazonka.Inspector2.Types.ImageLayerAggregation
import Amazonka.Inspector2.Types.ImageLayerAggregationResponse
import Amazonka.Inspector2.Types.ImageLayerSortBy
import Amazonka.Inspector2.Types.InspectorScoreDetails
import Amazonka.Inspector2.Types.MapComparison
import Amazonka.Inspector2.Types.MapFilter
import Amazonka.Inspector2.Types.Member
import Amazonka.Inspector2.Types.NetworkPath
import Amazonka.Inspector2.Types.NetworkProtocol
import Amazonka.Inspector2.Types.NetworkReachabilityDetails
import Amazonka.Inspector2.Types.NumberFilter
import Amazonka.Inspector2.Types.Operation
import Amazonka.Inspector2.Types.PackageAggregation
import Amazonka.Inspector2.Types.PackageAggregationResponse
import Amazonka.Inspector2.Types.PackageFilter
import Amazonka.Inspector2.Types.PackageManager
import Amazonka.Inspector2.Types.PackageSortBy
import Amazonka.Inspector2.Types.PackageVulnerabilityDetails
import Amazonka.Inspector2.Types.Permission
import Amazonka.Inspector2.Types.PortRange
import Amazonka.Inspector2.Types.PortRangeFilter
import Amazonka.Inspector2.Types.Recommendation
import Amazonka.Inspector2.Types.RelationshipStatus
import Amazonka.Inspector2.Types.Remediation
import Amazonka.Inspector2.Types.ReportFormat
import Amazonka.Inspector2.Types.ReportingErrorCode
import Amazonka.Inspector2.Types.RepositoryAggregation
import Amazonka.Inspector2.Types.RepositoryAggregationResponse
import Amazonka.Inspector2.Types.RepositorySortBy
import Amazonka.Inspector2.Types.Resource
import Amazonka.Inspector2.Types.ResourceDetails
import Amazonka.Inspector2.Types.ResourceScanMetadata
import Amazonka.Inspector2.Types.ResourceScanType
import Amazonka.Inspector2.Types.ResourceState
import Amazonka.Inspector2.Types.ResourceStatus
import Amazonka.Inspector2.Types.ResourceType
import Amazonka.Inspector2.Types.ScanStatus
import Amazonka.Inspector2.Types.ScanStatusCode
import Amazonka.Inspector2.Types.ScanStatusReason
import Amazonka.Inspector2.Types.ScanType
import Amazonka.Inspector2.Types.Service
import Amazonka.Inspector2.Types.Severity
import Amazonka.Inspector2.Types.SeverityCounts
import Amazonka.Inspector2.Types.SortCriteria
import Amazonka.Inspector2.Types.SortField
import Amazonka.Inspector2.Types.SortOrder
import Amazonka.Inspector2.Types.State
import Amazonka.Inspector2.Types.Status
import Amazonka.Inspector2.Types.Step
import Amazonka.Inspector2.Types.StringComparison
import Amazonka.Inspector2.Types.StringFilter
import Amazonka.Inspector2.Types.TitleAggregation
import Amazonka.Inspector2.Types.TitleAggregationResponse
import Amazonka.Inspector2.Types.TitleSortBy
import Amazonka.Inspector2.Types.Usage
import Amazonka.Inspector2.Types.UsageTotal
import Amazonka.Inspector2.Types.UsageType
import Amazonka.Inspector2.Types.VulnerablePackage
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-06-08@ of the Amazon Inspector2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Inspector2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "inspector2",
      Core.signingName = "inspector2",
      Core.version = "2020-06-08",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Inspector2",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request has failed due to an internal failure of the Amazon
-- Inspector service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | You have exceeded your service quota. To perform the requested action,
-- remove some of the relevant resources, or use Service Quotas to request
-- a service quota increase.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The operation tried to access an invalid resource. Make sure the
-- resource is specified correctly.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | A conflict occurred.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The limit on the number of requests per second was exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | One or more tags submitted as part of the request is not valid.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | The request has failed validation due to missing required fields or
-- having invalid inputs.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
