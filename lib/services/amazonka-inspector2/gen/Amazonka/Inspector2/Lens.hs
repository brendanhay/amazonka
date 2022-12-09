{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Inspector2.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createFilter_description,
    createFilter_reason,
    createFilter_tags,
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
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_maxAccountLimitReached,
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
    getFindingsReportStatusResponse_errorCode,
    getFindingsReportStatusResponse_errorMessage,
    getFindingsReportStatusResponse_filterCriteria,
    getFindingsReportStatusResponse_reportId,
    getFindingsReportStatusResponse_status,
    getFindingsReportStatusResponse_httpStatus,

    -- ** GetMember
    getMember_accountId,
    getMemberResponse_member,
    getMemberResponse_httpStatus,

    -- ** ListAccountPermissions
    listAccountPermissions_maxResults,
    listAccountPermissions_nextToken,
    listAccountPermissions_service,
    listAccountPermissionsResponse_nextToken,
    listAccountPermissionsResponse_httpStatus,
    listAccountPermissionsResponse_permissions,

    -- ** ListCoverage
    listCoverage_filterCriteria,
    listCoverage_maxResults,
    listCoverage_nextToken,
    listCoverageResponse_coveredResources,
    listCoverageResponse_nextToken,
    listCoverageResponse_httpStatus,

    -- ** ListCoverageStatistics
    listCoverageStatistics_filterCriteria,
    listCoverageStatistics_groupBy,
    listCoverageStatistics_nextToken,
    listCoverageStatisticsResponse_countsByGroup,
    listCoverageStatisticsResponse_nextToken,
    listCoverageStatisticsResponse_httpStatus,
    listCoverageStatisticsResponse_totalCounts,

    -- ** ListDelegatedAdminAccounts
    listDelegatedAdminAccounts_maxResults,
    listDelegatedAdminAccounts_nextToken,
    listDelegatedAdminAccountsResponse_delegatedAdminAccounts,
    listDelegatedAdminAccountsResponse_nextToken,
    listDelegatedAdminAccountsResponse_httpStatus,

    -- ** ListFilters
    listFilters_action,
    listFilters_arns,
    listFilters_maxResults,
    listFilters_nextToken,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filters,

    -- ** ListFindingAggregations
    listFindingAggregations_accountIds,
    listFindingAggregations_aggregationRequest,
    listFindingAggregations_maxResults,
    listFindingAggregations_nextToken,
    listFindingAggregations_aggregationType,
    listFindingAggregationsResponse_nextToken,
    listFindingAggregationsResponse_responses,
    listFindingAggregationsResponse_httpStatus,
    listFindingAggregationsResponse_aggregationType,

    -- ** ListFindings
    listFindings_filterCriteria,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindings_sortCriteria,
    listFindingsResponse_findings,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListUsageTotals
    listUsageTotals_accountIds,
    listUsageTotals_maxResults,
    listUsageTotals_nextToken,
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
    updateFilter_action,
    updateFilter_description,
    updateFilter_filterCriteria,
    updateFilter_name,
    updateFilter_reason,
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
    accountAggregation_findingType,
    accountAggregation_resourceType,
    accountAggregation_sortBy,
    accountAggregation_sortOrder,

    -- ** AccountAggregationResponse
    accountAggregationResponse_accountId,
    accountAggregationResponse_severityCounts,

    -- ** AccountState
    accountState_accountId,
    accountState_resourceState,
    accountState_state,

    -- ** AggregationRequest
    aggregationRequest_accountAggregation,
    aggregationRequest_amiAggregation,
    aggregationRequest_awsEcrContainerAggregation,
    aggregationRequest_ec2InstanceAggregation,
    aggregationRequest_findingTypeAggregation,
    aggregationRequest_imageLayerAggregation,
    aggregationRequest_lambdaFunctionAggregation,
    aggregationRequest_lambdaLayerAggregation,
    aggregationRequest_packageAggregation,
    aggregationRequest_repositoryAggregation,
    aggregationRequest_titleAggregation,

    -- ** AggregationResponse
    aggregationResponse_accountAggregation,
    aggregationResponse_amiAggregation,
    aggregationResponse_awsEcrContainerAggregation,
    aggregationResponse_ec2InstanceAggregation,
    aggregationResponse_findingTypeAggregation,
    aggregationResponse_imageLayerAggregation,
    aggregationResponse_lambdaFunctionAggregation,
    aggregationResponse_lambdaLayerAggregation,
    aggregationResponse_packageAggregation,
    aggregationResponse_repositoryAggregation,
    aggregationResponse_titleAggregation,

    -- ** AmiAggregation
    amiAggregation_amis,
    amiAggregation_sortBy,
    amiAggregation_sortOrder,

    -- ** AmiAggregationResponse
    amiAggregationResponse_accountId,
    amiAggregationResponse_affectedInstances,
    amiAggregationResponse_severityCounts,
    amiAggregationResponse_ami,

    -- ** AutoEnable
    autoEnable_lambda,
    autoEnable_ec2,
    autoEnable_ecr,

    -- ** AwsEc2InstanceDetails
    awsEc2InstanceDetails_iamInstanceProfileArn,
    awsEc2InstanceDetails_imageId,
    awsEc2InstanceDetails_ipV4Addresses,
    awsEc2InstanceDetails_ipV6Addresses,
    awsEc2InstanceDetails_keyName,
    awsEc2InstanceDetails_launchedAt,
    awsEc2InstanceDetails_platform,
    awsEc2InstanceDetails_subnetId,
    awsEc2InstanceDetails_type,
    awsEc2InstanceDetails_vpcId,

    -- ** AwsEcrContainerAggregation
    awsEcrContainerAggregation_architectures,
    awsEcrContainerAggregation_imageShas,
    awsEcrContainerAggregation_imageTags,
    awsEcrContainerAggregation_repositories,
    awsEcrContainerAggregation_resourceIds,
    awsEcrContainerAggregation_sortBy,
    awsEcrContainerAggregation_sortOrder,

    -- ** AwsEcrContainerAggregationResponse
    awsEcrContainerAggregationResponse_accountId,
    awsEcrContainerAggregationResponse_architecture,
    awsEcrContainerAggregationResponse_imageSha,
    awsEcrContainerAggregationResponse_imageTags,
    awsEcrContainerAggregationResponse_repository,
    awsEcrContainerAggregationResponse_severityCounts,
    awsEcrContainerAggregationResponse_resourceId,

    -- ** AwsEcrContainerImageDetails
    awsEcrContainerImageDetails_architecture,
    awsEcrContainerImageDetails_author,
    awsEcrContainerImageDetails_imageTags,
    awsEcrContainerImageDetails_platform,
    awsEcrContainerImageDetails_pushedAt,
    awsEcrContainerImageDetails_imageHash,
    awsEcrContainerImageDetails_registry,
    awsEcrContainerImageDetails_repositoryName,

    -- ** AwsLambdaFunctionDetails
    awsLambdaFunctionDetails_architectures,
    awsLambdaFunctionDetails_lastModifiedAt,
    awsLambdaFunctionDetails_layers,
    awsLambdaFunctionDetails_packageType,
    awsLambdaFunctionDetails_vpcConfig,
    awsLambdaFunctionDetails_codeSha256,
    awsLambdaFunctionDetails_executionRoleArn,
    awsLambdaFunctionDetails_functionName,
    awsLambdaFunctionDetails_runtime,
    awsLambdaFunctionDetails_version,

    -- ** Counts
    counts_count,
    counts_groupKey,

    -- ** CoverageFilterCriteria
    coverageFilterCriteria_accountId,
    coverageFilterCriteria_ec2InstanceTags,
    coverageFilterCriteria_ecrImageTags,
    coverageFilterCriteria_ecrRepositoryName,
    coverageFilterCriteria_lambdaFunctionName,
    coverageFilterCriteria_lambdaFunctionRuntime,
    coverageFilterCriteria_lambdaFunctionTags,
    coverageFilterCriteria_resourceId,
    coverageFilterCriteria_resourceType,
    coverageFilterCriteria_scanStatusCode,
    coverageFilterCriteria_scanStatusReason,
    coverageFilterCriteria_scanType,

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
    dateFilter_endInclusive,
    dateFilter_startInclusive,

    -- ** DelegatedAdmin
    delegatedAdmin_accountId,
    delegatedAdmin_relationshipStatus,

    -- ** DelegatedAdminAccount
    delegatedAdminAccount_accountId,
    delegatedAdminAccount_status,

    -- ** Destination
    destination_keyPrefix,
    destination_bucketName,
    destination_kmsKeyArn,

    -- ** Ec2InstanceAggregation
    ec2InstanceAggregation_amis,
    ec2InstanceAggregation_instanceIds,
    ec2InstanceAggregation_instanceTags,
    ec2InstanceAggregation_operatingSystems,
    ec2InstanceAggregation_sortBy,
    ec2InstanceAggregation_sortOrder,

    -- ** Ec2InstanceAggregationResponse
    ec2InstanceAggregationResponse_accountId,
    ec2InstanceAggregationResponse_ami,
    ec2InstanceAggregationResponse_instanceTags,
    ec2InstanceAggregationResponse_networkFindings,
    ec2InstanceAggregationResponse_operatingSystem,
    ec2InstanceAggregationResponse_severityCounts,
    ec2InstanceAggregationResponse_instanceId,

    -- ** Ec2Metadata
    ec2Metadata_amiId,
    ec2Metadata_platform,
    ec2Metadata_tags,

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
    ecrRescanDurationState_rescanDuration,
    ecrRescanDurationState_status,
    ecrRescanDurationState_updatedAt,

    -- ** ExploitabilityDetails
    exploitabilityDetails_lastKnownExploitAt,

    -- ** FailedAccount
    failedAccount_resourceStatus,
    failedAccount_status,
    failedAccount_accountId,
    failedAccount_errorCode,
    failedAccount_errorMessage,

    -- ** Filter
    filter_description,
    filter_reason,
    filter_tags,
    filter_action,
    filter_arn,
    filter_createdAt,
    filter_criteria,
    filter_name,
    filter_ownerId,
    filter_updatedAt,

    -- ** FilterCriteria
    filterCriteria_awsAccountId,
    filterCriteria_componentId,
    filterCriteria_componentType,
    filterCriteria_ec2InstanceImageId,
    filterCriteria_ec2InstanceSubnetId,
    filterCriteria_ec2InstanceVpcId,
    filterCriteria_ecrImageArchitecture,
    filterCriteria_ecrImageHash,
    filterCriteria_ecrImagePushedAt,
    filterCriteria_ecrImageRegistry,
    filterCriteria_ecrImageRepositoryName,
    filterCriteria_ecrImageTags,
    filterCriteria_exploitAvailable,
    filterCriteria_findingArn,
    filterCriteria_findingStatus,
    filterCriteria_findingType,
    filterCriteria_firstObservedAt,
    filterCriteria_fixAvailable,
    filterCriteria_inspectorScore,
    filterCriteria_lambdaFunctionExecutionRoleArn,
    filterCriteria_lambdaFunctionLastModifiedAt,
    filterCriteria_lambdaFunctionLayers,
    filterCriteria_lambdaFunctionName,
    filterCriteria_lambdaFunctionRuntime,
    filterCriteria_lastObservedAt,
    filterCriteria_networkProtocol,
    filterCriteria_portRange,
    filterCriteria_relatedVulnerabilities,
    filterCriteria_resourceId,
    filterCriteria_resourceTags,
    filterCriteria_resourceType,
    filterCriteria_severity,
    filterCriteria_title,
    filterCriteria_updatedAt,
    filterCriteria_vendorSeverity,
    filterCriteria_vulnerabilityId,
    filterCriteria_vulnerabilitySource,
    filterCriteria_vulnerablePackages,

    -- ** Finding
    finding_exploitAvailable,
    finding_exploitabilityDetails,
    finding_fixAvailable,
    finding_inspectorScore,
    finding_inspectorScoreDetails,
    finding_networkReachabilityDetails,
    finding_packageVulnerabilityDetails,
    finding_title,
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
    findingTypeAggregation_findingType,
    findingTypeAggregation_resourceType,
    findingTypeAggregation_sortBy,
    findingTypeAggregation_sortOrder,

    -- ** FindingTypeAggregationResponse
    findingTypeAggregationResponse_accountId,
    findingTypeAggregationResponse_severityCounts,

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
    imageLayerAggregation_layerHashes,
    imageLayerAggregation_repositories,
    imageLayerAggregation_resourceIds,
    imageLayerAggregation_sortBy,
    imageLayerAggregation_sortOrder,

    -- ** ImageLayerAggregationResponse
    imageLayerAggregationResponse_severityCounts,
    imageLayerAggregationResponse_accountId,
    imageLayerAggregationResponse_layerHash,
    imageLayerAggregationResponse_repository,
    imageLayerAggregationResponse_resourceId,

    -- ** InspectorScoreDetails
    inspectorScoreDetails_adjustedCvss,

    -- ** LambdaFunctionAggregation
    lambdaFunctionAggregation_functionNames,
    lambdaFunctionAggregation_functionTags,
    lambdaFunctionAggregation_resourceIds,
    lambdaFunctionAggregation_runtimes,
    lambdaFunctionAggregation_sortBy,
    lambdaFunctionAggregation_sortOrder,

    -- ** LambdaFunctionAggregationResponse
    lambdaFunctionAggregationResponse_accountId,
    lambdaFunctionAggregationResponse_functionName,
    lambdaFunctionAggregationResponse_lambdaTags,
    lambdaFunctionAggregationResponse_lastModifiedAt,
    lambdaFunctionAggregationResponse_runtime,
    lambdaFunctionAggregationResponse_severityCounts,
    lambdaFunctionAggregationResponse_resourceId,

    -- ** LambdaFunctionMetadata
    lambdaFunctionMetadata_functionName,
    lambdaFunctionMetadata_functionTags,
    lambdaFunctionMetadata_layers,
    lambdaFunctionMetadata_runtime,

    -- ** LambdaLayerAggregation
    lambdaLayerAggregation_functionNames,
    lambdaLayerAggregation_layerArns,
    lambdaLayerAggregation_resourceIds,
    lambdaLayerAggregation_sortBy,
    lambdaLayerAggregation_sortOrder,

    -- ** LambdaLayerAggregationResponse
    lambdaLayerAggregationResponse_severityCounts,
    lambdaLayerAggregationResponse_accountId,
    lambdaLayerAggregationResponse_functionName,
    lambdaLayerAggregationResponse_layerArn,
    lambdaLayerAggregationResponse_resourceId,

    -- ** LambdaVpcConfig
    lambdaVpcConfig_securityGroupIds,
    lambdaVpcConfig_subnetIds,
    lambdaVpcConfig_vpcId,

    -- ** MapFilter
    mapFilter_value,
    mapFilter_comparison,
    mapFilter_key,

    -- ** Member
    member_accountId,
    member_delegatedAdminAccountId,
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
    packageAggregation_packageNames,
    packageAggregation_sortBy,
    packageAggregation_sortOrder,

    -- ** PackageAggregationResponse
    packageAggregationResponse_accountId,
    packageAggregationResponse_severityCounts,
    packageAggregationResponse_packageName,

    -- ** PackageFilter
    packageFilter_architecture,
    packageFilter_epoch,
    packageFilter_name,
    packageFilter_release,
    packageFilter_sourceLambdaLayerArn,
    packageFilter_sourceLayerHash,
    packageFilter_version,

    -- ** PackageVulnerabilityDetails
    packageVulnerabilityDetails_cvss,
    packageVulnerabilityDetails_referenceUrls,
    packageVulnerabilityDetails_relatedVulnerabilities,
    packageVulnerabilityDetails_sourceUrl,
    packageVulnerabilityDetails_vendorCreatedAt,
    packageVulnerabilityDetails_vendorSeverity,
    packageVulnerabilityDetails_vendorUpdatedAt,
    packageVulnerabilityDetails_vulnerablePackages,
    packageVulnerabilityDetails_source,
    packageVulnerabilityDetails_vulnerabilityId,

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
    repositoryAggregation_repositories,
    repositoryAggregation_sortBy,
    repositoryAggregation_sortOrder,

    -- ** RepositoryAggregationResponse
    repositoryAggregationResponse_accountId,
    repositoryAggregationResponse_affectedImages,
    repositoryAggregationResponse_severityCounts,
    repositoryAggregationResponse_repository,

    -- ** Resource
    resource_details,
    resource_partition,
    resource_region,
    resource_tags,
    resource_id,
    resource_type,

    -- ** ResourceDetails
    resourceDetails_awsEc2Instance,
    resourceDetails_awsEcrContainerImage,
    resourceDetails_awsLambdaFunction,

    -- ** ResourceScanMetadata
    resourceScanMetadata_ec2,
    resourceScanMetadata_ecrImage,
    resourceScanMetadata_ecrRepository,
    resourceScanMetadata_lambdaFunction,

    -- ** ResourceState
    resourceState_lambda,
    resourceState_ec2,
    resourceState_ecr,

    -- ** ResourceStatus
    resourceStatus_lambda,
    resourceStatus_ec2,
    resourceStatus_ecr,

    -- ** ScanStatus
    scanStatus_reason,
    scanStatus_statusCode,

    -- ** SeverityCounts
    severityCounts_all,
    severityCounts_critical,
    severityCounts_high,
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
    titleAggregation_sortBy,
    titleAggregation_sortOrder,
    titleAggregation_titles,
    titleAggregation_vulnerabilityIds,

    -- ** TitleAggregationResponse
    titleAggregationResponse_accountId,
    titleAggregationResponse_severityCounts,
    titleAggregationResponse_vulnerabilityId,
    titleAggregationResponse_title,

    -- ** Usage
    usage_currency,
    usage_estimatedMonthlyCost,
    usage_total,
    usage_type,

    -- ** UsageTotal
    usageTotal_accountId,
    usageTotal_usage,

    -- ** VulnerablePackage
    vulnerablePackage_arch,
    vulnerablePackage_epoch,
    vulnerablePackage_filePath,
    vulnerablePackage_fixedInVersion,
    vulnerablePackage_packageManager,
    vulnerablePackage_release,
    vulnerablePackage_remediation,
    vulnerablePackage_sourceLambdaLayerArn,
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
import Amazonka.Inspector2.Types.AwsLambdaFunctionDetails
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
import Amazonka.Inspector2.Types.ExploitabilityDetails
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
import Amazonka.Inspector2.Types.LambdaFunctionAggregation
import Amazonka.Inspector2.Types.LambdaFunctionAggregationResponse
import Amazonka.Inspector2.Types.LambdaFunctionMetadata
import Amazonka.Inspector2.Types.LambdaLayerAggregation
import Amazonka.Inspector2.Types.LambdaLayerAggregationResponse
import Amazonka.Inspector2.Types.LambdaVpcConfig
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
