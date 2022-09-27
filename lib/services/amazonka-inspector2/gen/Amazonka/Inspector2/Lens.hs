{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Lens
  ( -- * Operations

    -- ** AssociateMember
    associateMember_accountId,
    associateMemberResponse_httpStatus,
    associateMemberResponse_accountId,

    -- ** BatchGetAccountStatus
    batchGetAccountStatus_accountIds,
    batchGetAccountStatusResponse_failedAccounts,
    batchGetAccountStatusResponse_httpStatus,
    batchGetAccountStatusResponse_accounts,

    -- ** BatchGetFreeTrialInfo
    batchGetFreeTrialInfo_accountIds,
    batchGetFreeTrialInfoResponse_httpStatus,
    batchGetFreeTrialInfoResponse_accounts,
    batchGetFreeTrialInfoResponse_failedAccounts,

    -- ** CancelFindingsReport
    cancelFindingsReport_reportId,
    cancelFindingsReportResponse_httpStatus,
    cancelFindingsReportResponse_reportId,

    -- ** CreateFilter
    createFilter_tags,
    createFilter_description,
    createFilter_reason,
    createFilter_action,
    createFilter_filterCriteria,
    createFilter_name,
    createFilterResponse_httpStatus,
    createFilterResponse_arn,

    -- ** CreateFindingsReport
    createFindingsReport_filterCriteria,
    createFindingsReport_reportFormat,
    createFindingsReport_s3Destination,
    createFindingsReportResponse_reportId,
    createFindingsReportResponse_httpStatus,

    -- ** DeleteFilter
    deleteFilter_arn,
    deleteFilterResponse_httpStatus,
    deleteFilterResponse_arn,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_maxAccountLimitReached,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** Disable
    disable_accountIds,
    disable_resourceTypes,
    disableResponse_failedAccounts,
    disableResponse_httpStatus,
    disableResponse_accounts,

    -- ** DisableDelegatedAdminAccount
    disableDelegatedAdminAccount_delegatedAdminAccountId,
    disableDelegatedAdminAccountResponse_httpStatus,
    disableDelegatedAdminAccountResponse_delegatedAdminAccountId,

    -- ** DisassociateMember
    disassociateMember_accountId,
    disassociateMemberResponse_httpStatus,
    disassociateMemberResponse_accountId,

    -- ** Enable
    enable_accountIds,
    enable_clientToken,
    enable_resourceTypes,
    enableResponse_failedAccounts,
    enableResponse_httpStatus,
    enableResponse_accounts,

    -- ** EnableDelegatedAdminAccount
    enableDelegatedAdminAccount_clientToken,
    enableDelegatedAdminAccount_delegatedAdminAccountId,
    enableDelegatedAdminAccountResponse_httpStatus,
    enableDelegatedAdminAccountResponse_delegatedAdminAccountId,

    -- ** GetConfiguration
    getConfigurationResponse_ecrConfiguration,
    getConfigurationResponse_httpStatus,

    -- ** GetDelegatedAdminAccount
    getDelegatedAdminAccountResponse_delegatedAdmin,
    getDelegatedAdminAccountResponse_httpStatus,

    -- ** GetFindingsReportStatus
    getFindingsReportStatus_reportId,
    getFindingsReportStatusResponse_destination,
    getFindingsReportStatusResponse_errorMessage,
    getFindingsReportStatusResponse_status,
    getFindingsReportStatusResponse_filterCriteria,
    getFindingsReportStatusResponse_reportId,
    getFindingsReportStatusResponse_errorCode,
    getFindingsReportStatusResponse_httpStatus,

    -- ** GetMember
    getMember_accountId,
    getMemberResponse_member,
    getMemberResponse_httpStatus,

    -- ** ListAccountPermissions
    listAccountPermissions_nextToken,
    listAccountPermissions_service,
    listAccountPermissions_maxResults,
    listAccountPermissionsResponse_nextToken,
    listAccountPermissionsResponse_httpStatus,
    listAccountPermissionsResponse_permissions,

    -- ** ListCoverage
    listCoverage_nextToken,
    listCoverage_filterCriteria,
    listCoverage_maxResults,
    listCoverageResponse_coveredResources,
    listCoverageResponse_nextToken,
    listCoverageResponse_httpStatus,

    -- ** ListCoverageStatistics
    listCoverageStatistics_nextToken,
    listCoverageStatistics_groupBy,
    listCoverageStatistics_filterCriteria,
    listCoverageStatisticsResponse_nextToken,
    listCoverageStatisticsResponse_countsByGroup,
    listCoverageStatisticsResponse_httpStatus,
    listCoverageStatisticsResponse_totalCounts,

    -- ** ListDelegatedAdminAccounts
    listDelegatedAdminAccounts_nextToken,
    listDelegatedAdminAccounts_maxResults,
    listDelegatedAdminAccountsResponse_nextToken,
    listDelegatedAdminAccountsResponse_delegatedAdminAccounts,
    listDelegatedAdminAccountsResponse_httpStatus,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_arns,
    listFilters_maxResults,
    listFilters_action,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filters,

    -- ** ListFindingAggregations
    listFindingAggregations_accountIds,
    listFindingAggregations_nextToken,
    listFindingAggregations_aggregationRequest,
    listFindingAggregations_maxResults,
    listFindingAggregations_aggregationType,
    listFindingAggregationsResponse_nextToken,
    listFindingAggregationsResponse_responses,
    listFindingAggregationsResponse_httpStatus,
    listFindingAggregationsResponse_aggregationType,

    -- ** ListFindings
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_filterCriteria,
    listFindings_maxResults,
    listFindingsResponse_findings,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_maxResults,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsageTotals
    listUsageTotals_accountIds,
    listUsageTotals_nextToken,
    listUsageTotals_maxResults,
    listUsageTotalsResponse_nextToken,
    listUsageTotalsResponse_totals,
    listUsageTotalsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateConfiguration
    updateConfiguration_ecrConfiguration,
    updateConfigurationResponse_httpStatus,

    -- ** UpdateFilter
    updateFilter_name,
    updateFilter_description,
    updateFilter_filterCriteria,
    updateFilter_reason,
    updateFilter_action,
    updateFilter_filterArn,
    updateFilterResponse_httpStatus,
    updateFilterResponse_arn,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,
    updateOrganizationConfigurationResponse_autoEnable,

    -- * Types

    -- ** Account
    account_accountId,
    account_resourceStatus,
    account_status,

    -- ** AccountAggregation
    accountAggregation_resourceType,
    accountAggregation_sortOrder,
    accountAggregation_sortBy,
    accountAggregation_findingType,

    -- ** AccountAggregationResponse
    accountAggregationResponse_severityCounts,
    accountAggregationResponse_accountId,

    -- ** AccountState
    accountState_accountId,
    accountState_resourceState,
    accountState_state,

    -- ** AggregationRequest
    aggregationRequest_repositoryAggregation,
    aggregationRequest_imageLayerAggregation,
    aggregationRequest_accountAggregation,
    aggregationRequest_awsEcrContainerAggregation,
    aggregationRequest_ec2InstanceAggregation,
    aggregationRequest_findingTypeAggregation,
    aggregationRequest_packageAggregation,
    aggregationRequest_titleAggregation,
    aggregationRequest_amiAggregation,

    -- ** AggregationResponse
    aggregationResponse_repositoryAggregation,
    aggregationResponse_imageLayerAggregation,
    aggregationResponse_accountAggregation,
    aggregationResponse_awsEcrContainerAggregation,
    aggregationResponse_ec2InstanceAggregation,
    aggregationResponse_findingTypeAggregation,
    aggregationResponse_packageAggregation,
    aggregationResponse_titleAggregation,
    aggregationResponse_amiAggregation,

    -- ** AmiAggregation
    amiAggregation_sortOrder,
    amiAggregation_sortBy,
    amiAggregation_amis,

    -- ** AmiAggregationResponse
    amiAggregationResponse_severityCounts,
    amiAggregationResponse_accountId,
    amiAggregationResponse_affectedInstances,
    amiAggregationResponse_ami,

    -- ** AutoEnable
    autoEnable_ec2,
    autoEnable_ecr,

    -- ** AwsEc2InstanceDetails
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

    -- ** AwsEcrContainerAggregation
    awsEcrContainerAggregation_sortOrder,
    awsEcrContainerAggregation_imageShas,
    awsEcrContainerAggregation_sortBy,
    awsEcrContainerAggregation_repositories,
    awsEcrContainerAggregation_resourceIds,
    awsEcrContainerAggregation_imageTags,
    awsEcrContainerAggregation_architectures,

    -- ** AwsEcrContainerAggregationResponse
    awsEcrContainerAggregationResponse_severityCounts,
    awsEcrContainerAggregationResponse_repository,
    awsEcrContainerAggregationResponse_imageSha,
    awsEcrContainerAggregationResponse_accountId,
    awsEcrContainerAggregationResponse_imageTags,
    awsEcrContainerAggregationResponse_architecture,
    awsEcrContainerAggregationResponse_resourceId,

    -- ** AwsEcrContainerImageDetails
    awsEcrContainerImageDetails_author,
    awsEcrContainerImageDetails_platform,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_pushedAt,
    awsEcrContainerImageDetails_imageHash,
    awsEcrContainerImageDetails_registry,
    awsEcrContainerImageDetails_repositoryName,

    -- ** Counts
    counts_groupKey,
    counts_count,

    -- ** CoverageFilterCriteria
    coverageFilterCriteria_resourceId,
    coverageFilterCriteria_resourceType,
    coverageFilterCriteria_ecrImageTags,
    coverageFilterCriteria_scanType,
    coverageFilterCriteria_accountId,
    coverageFilterCriteria_ec2InstanceTags,
    coverageFilterCriteria_scanStatusReason,
    coverageFilterCriteria_scanStatusCode,
    coverageFilterCriteria_ecrRepositoryName,

    -- ** CoverageMapFilter
    coverageMapFilter_value,
    coverageMapFilter_comparison,
    coverageMapFilter_key,

    -- ** CoverageStringFilter
    coverageStringFilter_comparison,
    coverageStringFilter_value,

    -- ** CoveredResource
    coveredResource_resourceMetadata,
    coveredResource_scanStatus,
    coveredResource_accountId,
    coveredResource_resourceId,
    coveredResource_resourceType,
    coveredResource_scanType,

    -- ** CvssScore
    cvssScore_baseScore,
    cvssScore_scoringVector,
    cvssScore_source,
    cvssScore_version,

    -- ** CvssScoreAdjustment
    cvssScoreAdjustment_metric,
    cvssScoreAdjustment_reason,

    -- ** CvssScoreDetails
    cvssScoreDetails_adjustments,
    cvssScoreDetails_cvssSource,
    cvssScoreDetails_score,
    cvssScoreDetails_scoreSource,
    cvssScoreDetails_scoringVector,
    cvssScoreDetails_version,

    -- ** DateFilter
    dateFilter_startInclusive,
    dateFilter_endInclusive,

    -- ** DelegatedAdmin
    delegatedAdmin_accountId,
    delegatedAdmin_relationshipStatus,

    -- ** DelegatedAdminAccount
    delegatedAdminAccount_status,
    delegatedAdminAccount_accountId,

    -- ** Destination
    destination_keyPrefix,
    destination_bucketName,
    destination_kmsKeyArn,

    -- ** Ec2InstanceAggregation
    ec2InstanceAggregation_sortOrder,
    ec2InstanceAggregation_instanceTags,
    ec2InstanceAggregation_sortBy,
    ec2InstanceAggregation_operatingSystems,
    ec2InstanceAggregation_instanceIds,
    ec2InstanceAggregation_amis,

    -- ** Ec2InstanceAggregationResponse
    ec2InstanceAggregationResponse_severityCounts,
    ec2InstanceAggregationResponse_operatingSystem,
    ec2InstanceAggregationResponse_instanceTags,
    ec2InstanceAggregationResponse_networkFindings,
    ec2InstanceAggregationResponse_accountId,
    ec2InstanceAggregationResponse_ami,
    ec2InstanceAggregationResponse_instanceId,

    -- ** Ec2Metadata
    ec2Metadata_tags,
    ec2Metadata_amiId,
    ec2Metadata_platform,

    -- ** EcrConfiguration
    ecrConfiguration_rescanDuration,

    -- ** EcrConfigurationState
    ecrConfigurationState_rescanDurationState,

    -- ** EcrContainerImageMetadata
    ecrContainerImageMetadata_tags,

    -- ** EcrRepositoryMetadata
    ecrRepositoryMetadata_name,
    ecrRepositoryMetadata_scanFrequency,

    -- ** EcrRescanDurationState
    ecrRescanDurationState_status,
    ecrRescanDurationState_rescanDuration,
    ecrRescanDurationState_updatedAt,

    -- ** FailedAccount
    failedAccount_status,
    failedAccount_resourceStatus,
    failedAccount_accountId,
    failedAccount_errorCode,
    failedAccount_errorMessage,

    -- ** Filter
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

    -- ** FilterCriteria
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
    filterCriteria_findingArn,
    filterCriteria_ec2InstanceSubnetId,
    filterCriteria_updatedAt,

    -- ** Finding
    finding_inspectorScore,
    finding_packageVulnerabilityDetails,
    finding_title,
    finding_inspectorScoreDetails,
    finding_networkReachabilityDetails,
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

    -- ** FindingTypeAggregation
    findingTypeAggregation_resourceType,
    findingTypeAggregation_sortOrder,
    findingTypeAggregation_sortBy,
    findingTypeAggregation_findingType,

    -- ** FindingTypeAggregationResponse
    findingTypeAggregationResponse_severityCounts,
    findingTypeAggregationResponse_accountId,

    -- ** FreeTrialAccountInfo
    freeTrialAccountInfo_accountId,
    freeTrialAccountInfo_freeTrialInfo,

    -- ** FreeTrialInfo
    freeTrialInfo_end,
    freeTrialInfo_start,
    freeTrialInfo_status,
    freeTrialInfo_type,

    -- ** FreeTrialInfoError
    freeTrialInfoError_accountId,
    freeTrialInfoError_code,
    freeTrialInfoError_message,

    -- ** ImageLayerAggregation
    imageLayerAggregation_sortOrder,
    imageLayerAggregation_sortBy,
    imageLayerAggregation_repositories,
    imageLayerAggregation_resourceIds,
    imageLayerAggregation_layerHashes,

    -- ** ImageLayerAggregationResponse
    imageLayerAggregationResponse_severityCounts,
    imageLayerAggregationResponse_accountId,
    imageLayerAggregationResponse_layerHash,
    imageLayerAggregationResponse_repository,
    imageLayerAggregationResponse_resourceId,

    -- ** InspectorScoreDetails
    inspectorScoreDetails_adjustedCvss,

    -- ** MapFilter
    mapFilter_value,
    mapFilter_comparison,
    mapFilter_key,

    -- ** Member
    member_delegatedAdminAccountId,
    member_accountId,
    member_relationshipStatus,
    member_updatedAt,

    -- ** NetworkPath
    networkPath_steps,

    -- ** NetworkReachabilityDetails
    networkReachabilityDetails_networkPath,
    networkReachabilityDetails_openPortRange,
    networkReachabilityDetails_protocol,

    -- ** NumberFilter
    numberFilter_lowerInclusive,
    numberFilter_upperInclusive,

    -- ** PackageAggregation
    packageAggregation_sortOrder,
    packageAggregation_sortBy,
    packageAggregation_packageNames,

    -- ** PackageAggregationResponse
    packageAggregationResponse_severityCounts,
    packageAggregationResponse_accountId,
    packageAggregationResponse_packageName,

    -- ** PackageFilter
    packageFilter_name,
    packageFilter_epoch,
    packageFilter_release,
    packageFilter_sourceLayerHash,
    packageFilter_architecture,
    packageFilter_version,

    -- ** PackageVulnerabilityDetails
    packageVulnerabilityDetails_referenceUrls,
    packageVulnerabilityDetails_vendorUpdatedAt,
    packageVulnerabilityDetails_vendorCreatedAt,
    packageVulnerabilityDetails_cvss,
    packageVulnerabilityDetails_vendorSeverity,
    packageVulnerabilityDetails_relatedVulnerabilities,
    packageVulnerabilityDetails_sourceUrl,
    packageVulnerabilityDetails_source,
    packageVulnerabilityDetails_vulnerabilityId,
    packageVulnerabilityDetails_vulnerablePackages,

    -- ** Permission
    permission_operation,
    permission_service,

    -- ** PortRange
    portRange_begin,
    portRange_end,

    -- ** PortRangeFilter
    portRangeFilter_beginInclusive,
    portRangeFilter_endInclusive,

    -- ** Recommendation
    recommendation_url,
    recommendation_text,

    -- ** Remediation
    remediation_recommendation,

    -- ** RepositoryAggregation
    repositoryAggregation_sortOrder,
    repositoryAggregation_sortBy,
    repositoryAggregation_repositories,

    -- ** RepositoryAggregationResponse
    repositoryAggregationResponse_severityCounts,
    repositoryAggregationResponse_affectedImages,
    repositoryAggregationResponse_accountId,
    repositoryAggregationResponse_repository,

    -- ** Resource
    resource_tags,
    resource_details,
    resource_partition,
    resource_region,
    resource_id,
    resource_type,

    -- ** ResourceDetails
    resourceDetails_awsEc2Instance,
    resourceDetails_awsEcrContainerImage,

    -- ** ResourceScanMetadata
    resourceScanMetadata_ecrRepository,
    resourceScanMetadata_ec2,
    resourceScanMetadata_ecrImage,

    -- ** ResourceState
    resourceState_ec2,
    resourceState_ecr,

    -- ** ResourceStatus
    resourceStatus_ec2,
    resourceStatus_ecr,

    -- ** ScanStatus
    scanStatus_reason,
    scanStatus_statusCode,

    -- ** SeverityCounts
    severityCounts_critical,
    severityCounts_high,
    severityCounts_all,
    severityCounts_medium,

    -- ** SortCriteria
    sortCriteria_field,
    sortCriteria_sortOrder,

    -- ** State
    state_errorCode,
    state_errorMessage,
    state_status,

    -- ** Step
    step_componentId,
    step_componentType,

    -- ** StringFilter
    stringFilter_comparison,
    stringFilter_value,

    -- ** TitleAggregation
    titleAggregation_resourceType,
    titleAggregation_sortOrder,
    titleAggregation_vulnerabilityIds,
    titleAggregation_sortBy,
    titleAggregation_titles,

    -- ** TitleAggregationResponse
    titleAggregationResponse_severityCounts,
    titleAggregationResponse_accountId,
    titleAggregationResponse_vulnerabilityId,
    titleAggregationResponse_title,

    -- ** Usage
    usage_type,
    usage_total,
    usage_estimatedMonthlyCost,
    usage_currency,

    -- ** UsageTotal
    usageTotal_usage,
    usageTotal_accountId,

    -- ** VulnerablePackage
    vulnerablePackage_filePath,
    vulnerablePackage_fixedInVersion,
    vulnerablePackage_arch,
    vulnerablePackage_epoch,
    vulnerablePackage_packageManager,
    vulnerablePackage_release,
    vulnerablePackage_sourceLayerHash,
    vulnerablePackage_name,
    vulnerablePackage_version,
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
import Amazonka.Inspector2.Types.Account
import Amazonka.Inspector2.Types.AccountAggregation
import Amazonka.Inspector2.Types.AccountAggregationResponse
import Amazonka.Inspector2.Types.AccountState
import Amazonka.Inspector2.Types.AggregationRequest
import Amazonka.Inspector2.Types.AggregationResponse
import Amazonka.Inspector2.Types.AmiAggregation
import Amazonka.Inspector2.Types.AmiAggregationResponse
import Amazonka.Inspector2.Types.AutoEnable
import Amazonka.Inspector2.Types.AwsEc2InstanceDetails
import Amazonka.Inspector2.Types.AwsEcrContainerAggregation
import Amazonka.Inspector2.Types.AwsEcrContainerAggregationResponse
import Amazonka.Inspector2.Types.AwsEcrContainerImageDetails
import Amazonka.Inspector2.Types.Counts
import Amazonka.Inspector2.Types.CoverageFilterCriteria
import Amazonka.Inspector2.Types.CoverageMapFilter
import Amazonka.Inspector2.Types.CoverageStringFilter
import Amazonka.Inspector2.Types.CoveredResource
import Amazonka.Inspector2.Types.CvssScore
import Amazonka.Inspector2.Types.CvssScoreAdjustment
import Amazonka.Inspector2.Types.CvssScoreDetails
import Amazonka.Inspector2.Types.DateFilter
import Amazonka.Inspector2.Types.DelegatedAdmin
import Amazonka.Inspector2.Types.DelegatedAdminAccount
import Amazonka.Inspector2.Types.Destination
import Amazonka.Inspector2.Types.Ec2InstanceAggregation
import Amazonka.Inspector2.Types.Ec2InstanceAggregationResponse
import Amazonka.Inspector2.Types.Ec2Metadata
import Amazonka.Inspector2.Types.EcrConfiguration
import Amazonka.Inspector2.Types.EcrConfigurationState
import Amazonka.Inspector2.Types.EcrContainerImageMetadata
import Amazonka.Inspector2.Types.EcrRepositoryMetadata
import Amazonka.Inspector2.Types.EcrRescanDurationState
import Amazonka.Inspector2.Types.FailedAccount
import Amazonka.Inspector2.Types.Filter
import Amazonka.Inspector2.Types.FilterCriteria
import Amazonka.Inspector2.Types.Finding
import Amazonka.Inspector2.Types.FindingTypeAggregation
import Amazonka.Inspector2.Types.FindingTypeAggregationResponse
import Amazonka.Inspector2.Types.FreeTrialAccountInfo
import Amazonka.Inspector2.Types.FreeTrialInfo
import Amazonka.Inspector2.Types.FreeTrialInfoError
import Amazonka.Inspector2.Types.ImageLayerAggregation
import Amazonka.Inspector2.Types.ImageLayerAggregationResponse
import Amazonka.Inspector2.Types.InspectorScoreDetails
import Amazonka.Inspector2.Types.MapFilter
import Amazonka.Inspector2.Types.Member
import Amazonka.Inspector2.Types.NetworkPath
import Amazonka.Inspector2.Types.NetworkReachabilityDetails
import Amazonka.Inspector2.Types.NumberFilter
import Amazonka.Inspector2.Types.PackageAggregation
import Amazonka.Inspector2.Types.PackageAggregationResponse
import Amazonka.Inspector2.Types.PackageFilter
import Amazonka.Inspector2.Types.PackageVulnerabilityDetails
import Amazonka.Inspector2.Types.Permission
import Amazonka.Inspector2.Types.PortRange
import Amazonka.Inspector2.Types.PortRangeFilter
import Amazonka.Inspector2.Types.Recommendation
import Amazonka.Inspector2.Types.Remediation
import Amazonka.Inspector2.Types.RepositoryAggregation
import Amazonka.Inspector2.Types.RepositoryAggregationResponse
import Amazonka.Inspector2.Types.Resource
import Amazonka.Inspector2.Types.ResourceDetails
import Amazonka.Inspector2.Types.ResourceScanMetadata
import Amazonka.Inspector2.Types.ResourceState
import Amazonka.Inspector2.Types.ResourceStatus
import Amazonka.Inspector2.Types.ScanStatus
import Amazonka.Inspector2.Types.SeverityCounts
import Amazonka.Inspector2.Types.SortCriteria
import Amazonka.Inspector2.Types.State
import Amazonka.Inspector2.Types.Step
import Amazonka.Inspector2.Types.StringFilter
import Amazonka.Inspector2.Types.TitleAggregation
import Amazonka.Inspector2.Types.TitleAggregationResponse
import Amazonka.Inspector2.Types.Usage
import Amazonka.Inspector2.Types.UsageTotal
import Amazonka.Inspector2.Types.VulnerablePackage
import Amazonka.Inspector2.UntagResource
import Amazonka.Inspector2.UpdateConfiguration
import Amazonka.Inspector2.UpdateFilter
import Amazonka.Inspector2.UpdateOrganizationConfiguration
