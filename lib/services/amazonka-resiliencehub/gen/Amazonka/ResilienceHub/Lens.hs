{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResilienceHub.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Lens
  ( -- * Operations

    -- ** AddDraftAppVersionResourceMappings
    addDraftAppVersionResourceMappings_appArn,
    addDraftAppVersionResourceMappings_resourceMappings,
    addDraftAppVersionResourceMappingsResponse_httpStatus,
    addDraftAppVersionResourceMappingsResponse_appArn,
    addDraftAppVersionResourceMappingsResponse_appVersion,
    addDraftAppVersionResourceMappingsResponse_resourceMappings,

    -- ** CreateApp
    createApp_assessmentSchedule,
    createApp_clientToken,
    createApp_description,
    createApp_policyArn,
    createApp_tags,
    createApp_name,
    createAppResponse_httpStatus,
    createAppResponse_app,

    -- ** CreateAppVersionAppComponent
    createAppVersionAppComponent_additionalInfo,
    createAppVersionAppComponent_clientToken,
    createAppVersionAppComponent_id,
    createAppVersionAppComponent_appArn,
    createAppVersionAppComponent_name,
    createAppVersionAppComponent_type,
    createAppVersionAppComponentResponse_appComponent,
    createAppVersionAppComponentResponse_httpStatus,
    createAppVersionAppComponentResponse_appArn,
    createAppVersionAppComponentResponse_appVersion,

    -- ** CreateAppVersionResource
    createAppVersionResource_additionalInfo,
    createAppVersionResource_awsAccountId,
    createAppVersionResource_awsRegion,
    createAppVersionResource_clientToken,
    createAppVersionResource_resourceName,
    createAppVersionResource_appArn,
    createAppVersionResource_appComponents,
    createAppVersionResource_logicalResourceId,
    createAppVersionResource_physicalResourceId,
    createAppVersionResource_resourceType,
    createAppVersionResourceResponse_physicalResource,
    createAppVersionResourceResponse_httpStatus,
    createAppVersionResourceResponse_appArn,
    createAppVersionResourceResponse_appVersion,

    -- ** CreateRecommendationTemplate
    createRecommendationTemplate_bucketName,
    createRecommendationTemplate_clientToken,
    createRecommendationTemplate_format,
    createRecommendationTemplate_recommendationIds,
    createRecommendationTemplate_recommendationTypes,
    createRecommendationTemplate_tags,
    createRecommendationTemplate_assessmentArn,
    createRecommendationTemplate_name,
    createRecommendationTemplateResponse_recommendationTemplate,
    createRecommendationTemplateResponse_httpStatus,

    -- ** CreateResiliencyPolicy
    createResiliencyPolicy_clientToken,
    createResiliencyPolicy_dataLocationConstraint,
    createResiliencyPolicy_policyDescription,
    createResiliencyPolicy_tags,
    createResiliencyPolicy_policy,
    createResiliencyPolicy_policyName,
    createResiliencyPolicy_tier,
    createResiliencyPolicyResponse_httpStatus,
    createResiliencyPolicyResponse_policy,

    -- ** DeleteApp
    deleteApp_clientToken,
    deleteApp_forceDelete,
    deleteApp_appArn,
    deleteAppResponse_httpStatus,
    deleteAppResponse_appArn,

    -- ** DeleteAppAssessment
    deleteAppAssessment_clientToken,
    deleteAppAssessment_assessmentArn,
    deleteAppAssessmentResponse_httpStatus,
    deleteAppAssessmentResponse_assessmentArn,
    deleteAppAssessmentResponse_assessmentStatus,

    -- ** DeleteAppInputSource
    deleteAppInputSource_clientToken,
    deleteAppInputSource_eksSourceClusterNamespace,
    deleteAppInputSource_sourceArn,
    deleteAppInputSource_terraformSource,
    deleteAppInputSource_appArn,
    deleteAppInputSourceResponse_appArn,
    deleteAppInputSourceResponse_appInputSource,
    deleteAppInputSourceResponse_httpStatus,

    -- ** DeleteAppVersionAppComponent
    deleteAppVersionAppComponent_clientToken,
    deleteAppVersionAppComponent_appArn,
    deleteAppVersionAppComponent_id,
    deleteAppVersionAppComponentResponse_appComponent,
    deleteAppVersionAppComponentResponse_httpStatus,
    deleteAppVersionAppComponentResponse_appArn,
    deleteAppVersionAppComponentResponse_appVersion,

    -- ** DeleteAppVersionResource
    deleteAppVersionResource_awsAccountId,
    deleteAppVersionResource_awsRegion,
    deleteAppVersionResource_clientToken,
    deleteAppVersionResource_logicalResourceId,
    deleteAppVersionResource_physicalResourceId,
    deleteAppVersionResource_resourceName,
    deleteAppVersionResource_appArn,
    deleteAppVersionResourceResponse_physicalResource,
    deleteAppVersionResourceResponse_httpStatus,
    deleteAppVersionResourceResponse_appArn,
    deleteAppVersionResourceResponse_appVersion,

    -- ** DeleteRecommendationTemplate
    deleteRecommendationTemplate_clientToken,
    deleteRecommendationTemplate_recommendationTemplateArn,
    deleteRecommendationTemplateResponse_httpStatus,
    deleteRecommendationTemplateResponse_recommendationTemplateArn,
    deleteRecommendationTemplateResponse_status,

    -- ** DeleteResiliencyPolicy
    deleteResiliencyPolicy_clientToken,
    deleteResiliencyPolicy_policyArn,
    deleteResiliencyPolicyResponse_httpStatus,
    deleteResiliencyPolicyResponse_policyArn,

    -- ** DescribeApp
    describeApp_appArn,
    describeAppResponse_httpStatus,
    describeAppResponse_app,

    -- ** DescribeAppAssessment
    describeAppAssessment_assessmentArn,
    describeAppAssessmentResponse_httpStatus,
    describeAppAssessmentResponse_assessment,

    -- ** DescribeAppVersion
    describeAppVersion_appArn,
    describeAppVersion_appVersion,
    describeAppVersionResponse_additionalInfo,
    describeAppVersionResponse_httpStatus,
    describeAppVersionResponse_appArn,
    describeAppVersionResponse_appVersion,

    -- ** DescribeAppVersionAppComponent
    describeAppVersionAppComponent_appArn,
    describeAppVersionAppComponent_appVersion,
    describeAppVersionAppComponent_id,
    describeAppVersionAppComponentResponse_appComponent,
    describeAppVersionAppComponentResponse_httpStatus,
    describeAppVersionAppComponentResponse_appArn,
    describeAppVersionAppComponentResponse_appVersion,

    -- ** DescribeAppVersionResource
    describeAppVersionResource_awsAccountId,
    describeAppVersionResource_awsRegion,
    describeAppVersionResource_logicalResourceId,
    describeAppVersionResource_physicalResourceId,
    describeAppVersionResource_resourceName,
    describeAppVersionResource_appArn,
    describeAppVersionResource_appVersion,
    describeAppVersionResourceResponse_physicalResource,
    describeAppVersionResourceResponse_httpStatus,
    describeAppVersionResourceResponse_appArn,
    describeAppVersionResourceResponse_appVersion,

    -- ** DescribeAppVersionResourcesResolutionStatus
    describeAppVersionResourcesResolutionStatus_resolutionId,
    describeAppVersionResourcesResolutionStatus_appArn,
    describeAppVersionResourcesResolutionStatus_appVersion,
    describeAppVersionResourcesResolutionStatusResponse_errorMessage,
    describeAppVersionResourcesResolutionStatusResponse_httpStatus,
    describeAppVersionResourcesResolutionStatusResponse_appArn,
    describeAppVersionResourcesResolutionStatusResponse_appVersion,
    describeAppVersionResourcesResolutionStatusResponse_resolutionId,
    describeAppVersionResourcesResolutionStatusResponse_status,

    -- ** DescribeAppVersionTemplate
    describeAppVersionTemplate_appArn,
    describeAppVersionTemplate_appVersion,
    describeAppVersionTemplateResponse_httpStatus,
    describeAppVersionTemplateResponse_appArn,
    describeAppVersionTemplateResponse_appTemplateBody,
    describeAppVersionTemplateResponse_appVersion,

    -- ** DescribeDraftAppVersionResourcesImportStatus
    describeDraftAppVersionResourcesImportStatus_appArn,
    describeDraftAppVersionResourcesImportStatusResponse_errorMessage,
    describeDraftAppVersionResourcesImportStatusResponse_httpStatus,
    describeDraftAppVersionResourcesImportStatusResponse_appArn,
    describeDraftAppVersionResourcesImportStatusResponse_appVersion,
    describeDraftAppVersionResourcesImportStatusResponse_status,
    describeDraftAppVersionResourcesImportStatusResponse_statusChangeTime,

    -- ** DescribeResiliencyPolicy
    describeResiliencyPolicy_policyArn,
    describeResiliencyPolicyResponse_httpStatus,
    describeResiliencyPolicyResponse_policy,

    -- ** ImportResourcesToDraftAppVersion
    importResourcesToDraftAppVersion_eksSources,
    importResourcesToDraftAppVersion_importStrategy,
    importResourcesToDraftAppVersion_sourceArns,
    importResourcesToDraftAppVersion_terraformSources,
    importResourcesToDraftAppVersion_appArn,
    importResourcesToDraftAppVersionResponse_eksSources,
    importResourcesToDraftAppVersionResponse_sourceArns,
    importResourcesToDraftAppVersionResponse_terraformSources,
    importResourcesToDraftAppVersionResponse_httpStatus,
    importResourcesToDraftAppVersionResponse_appArn,
    importResourcesToDraftAppVersionResponse_appVersion,
    importResourcesToDraftAppVersionResponse_status,

    -- ** ListAlarmRecommendations
    listAlarmRecommendations_maxResults,
    listAlarmRecommendations_nextToken,
    listAlarmRecommendations_assessmentArn,
    listAlarmRecommendationsResponse_nextToken,
    listAlarmRecommendationsResponse_httpStatus,
    listAlarmRecommendationsResponse_alarmRecommendations,

    -- ** ListAppAssessments
    listAppAssessments_appArn,
    listAppAssessments_assessmentName,
    listAppAssessments_assessmentStatus,
    listAppAssessments_complianceStatus,
    listAppAssessments_invoker,
    listAppAssessments_maxResults,
    listAppAssessments_nextToken,
    listAppAssessments_reverseOrder,
    listAppAssessmentsResponse_nextToken,
    listAppAssessmentsResponse_httpStatus,
    listAppAssessmentsResponse_assessmentSummaries,

    -- ** ListAppComponentCompliances
    listAppComponentCompliances_maxResults,
    listAppComponentCompliances_nextToken,
    listAppComponentCompliances_assessmentArn,
    listAppComponentCompliancesResponse_nextToken,
    listAppComponentCompliancesResponse_httpStatus,
    listAppComponentCompliancesResponse_componentCompliances,

    -- ** ListAppComponentRecommendations
    listAppComponentRecommendations_maxResults,
    listAppComponentRecommendations_nextToken,
    listAppComponentRecommendations_assessmentArn,
    listAppComponentRecommendationsResponse_nextToken,
    listAppComponentRecommendationsResponse_httpStatus,
    listAppComponentRecommendationsResponse_componentRecommendations,

    -- ** ListAppInputSources
    listAppInputSources_maxResults,
    listAppInputSources_nextToken,
    listAppInputSources_appArn,
    listAppInputSources_appVersion,
    listAppInputSourcesResponse_nextToken,
    listAppInputSourcesResponse_httpStatus,
    listAppInputSourcesResponse_appInputSources,

    -- ** ListAppVersionAppComponents
    listAppVersionAppComponents_maxResults,
    listAppVersionAppComponents_nextToken,
    listAppVersionAppComponents_appArn,
    listAppVersionAppComponents_appVersion,
    listAppVersionAppComponentsResponse_appComponents,
    listAppVersionAppComponentsResponse_nextToken,
    listAppVersionAppComponentsResponse_httpStatus,
    listAppVersionAppComponentsResponse_appArn,
    listAppVersionAppComponentsResponse_appVersion,

    -- ** ListAppVersionResourceMappings
    listAppVersionResourceMappings_maxResults,
    listAppVersionResourceMappings_nextToken,
    listAppVersionResourceMappings_appArn,
    listAppVersionResourceMappings_appVersion,
    listAppVersionResourceMappingsResponse_nextToken,
    listAppVersionResourceMappingsResponse_httpStatus,
    listAppVersionResourceMappingsResponse_resourceMappings,

    -- ** ListAppVersionResources
    listAppVersionResources_maxResults,
    listAppVersionResources_nextToken,
    listAppVersionResources_resolutionId,
    listAppVersionResources_appArn,
    listAppVersionResources_appVersion,
    listAppVersionResourcesResponse_nextToken,
    listAppVersionResourcesResponse_httpStatus,
    listAppVersionResourcesResponse_physicalResources,
    listAppVersionResourcesResponse_resolutionId,

    -- ** ListAppVersions
    listAppVersions_maxResults,
    listAppVersions_nextToken,
    listAppVersions_appArn,
    listAppVersionsResponse_nextToken,
    listAppVersionsResponse_httpStatus,
    listAppVersionsResponse_appVersions,

    -- ** ListApps
    listApps_appArn,
    listApps_maxResults,
    listApps_name,
    listApps_nextToken,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,
    listAppsResponse_appSummaries,

    -- ** ListRecommendationTemplates
    listRecommendationTemplates_maxResults,
    listRecommendationTemplates_name,
    listRecommendationTemplates_nextToken,
    listRecommendationTemplates_recommendationTemplateArn,
    listRecommendationTemplates_reverseOrder,
    listRecommendationTemplates_status,
    listRecommendationTemplates_assessmentArn,
    listRecommendationTemplatesResponse_nextToken,
    listRecommendationTemplatesResponse_recommendationTemplates,
    listRecommendationTemplatesResponse_httpStatus,

    -- ** ListResiliencyPolicies
    listResiliencyPolicies_maxResults,
    listResiliencyPolicies_nextToken,
    listResiliencyPolicies_policyName,
    listResiliencyPoliciesResponse_nextToken,
    listResiliencyPoliciesResponse_httpStatus,
    listResiliencyPoliciesResponse_resiliencyPolicies,

    -- ** ListSopRecommendations
    listSopRecommendations_maxResults,
    listSopRecommendations_nextToken,
    listSopRecommendations_assessmentArn,
    listSopRecommendationsResponse_nextToken,
    listSopRecommendationsResponse_httpStatus,
    listSopRecommendationsResponse_sopRecommendations,

    -- ** ListSuggestedResiliencyPolicies
    listSuggestedResiliencyPolicies_maxResults,
    listSuggestedResiliencyPolicies_nextToken,
    listSuggestedResiliencyPoliciesResponse_nextToken,
    listSuggestedResiliencyPoliciesResponse_httpStatus,
    listSuggestedResiliencyPoliciesResponse_resiliencyPolicies,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTestRecommendations
    listTestRecommendations_maxResults,
    listTestRecommendations_nextToken,
    listTestRecommendations_assessmentArn,
    listTestRecommendationsResponse_nextToken,
    listTestRecommendationsResponse_httpStatus,
    listTestRecommendationsResponse_testRecommendations,

    -- ** ListUnsupportedAppVersionResources
    listUnsupportedAppVersionResources_maxResults,
    listUnsupportedAppVersionResources_nextToken,
    listUnsupportedAppVersionResources_resolutionId,
    listUnsupportedAppVersionResources_appArn,
    listUnsupportedAppVersionResources_appVersion,
    listUnsupportedAppVersionResourcesResponse_nextToken,
    listUnsupportedAppVersionResourcesResponse_httpStatus,
    listUnsupportedAppVersionResourcesResponse_resolutionId,
    listUnsupportedAppVersionResourcesResponse_unsupportedResources,

    -- ** PublishAppVersion
    publishAppVersion_appArn,
    publishAppVersionResponse_appVersion,
    publishAppVersionResponse_httpStatus,
    publishAppVersionResponse_appArn,

    -- ** PutDraftAppVersionTemplate
    putDraftAppVersionTemplate_appArn,
    putDraftAppVersionTemplate_appTemplateBody,
    putDraftAppVersionTemplateResponse_appArn,
    putDraftAppVersionTemplateResponse_appVersion,
    putDraftAppVersionTemplateResponse_httpStatus,

    -- ** RemoveDraftAppVersionResourceMappings
    removeDraftAppVersionResourceMappings_appRegistryAppNames,
    removeDraftAppVersionResourceMappings_eksSourceNames,
    removeDraftAppVersionResourceMappings_logicalStackNames,
    removeDraftAppVersionResourceMappings_resourceGroupNames,
    removeDraftAppVersionResourceMappings_resourceNames,
    removeDraftAppVersionResourceMappings_terraformSourceNames,
    removeDraftAppVersionResourceMappings_appArn,
    removeDraftAppVersionResourceMappingsResponse_appArn,
    removeDraftAppVersionResourceMappingsResponse_appVersion,
    removeDraftAppVersionResourceMappingsResponse_httpStatus,

    -- ** ResolveAppVersionResources
    resolveAppVersionResources_appArn,
    resolveAppVersionResources_appVersion,
    resolveAppVersionResourcesResponse_httpStatus,
    resolveAppVersionResourcesResponse_appArn,
    resolveAppVersionResourcesResponse_appVersion,
    resolveAppVersionResourcesResponse_resolutionId,
    resolveAppVersionResourcesResponse_status,

    -- ** StartAppAssessment
    startAppAssessment_clientToken,
    startAppAssessment_tags,
    startAppAssessment_appArn,
    startAppAssessment_appVersion,
    startAppAssessment_assessmentName,
    startAppAssessmentResponse_httpStatus,
    startAppAssessmentResponse_assessment,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApp
    updateApp_assessmentSchedule,
    updateApp_clearResiliencyPolicyArn,
    updateApp_description,
    updateApp_policyArn,
    updateApp_appArn,
    updateAppResponse_httpStatus,
    updateAppResponse_app,

    -- ** UpdateAppVersion
    updateAppVersion_additionalInfo,
    updateAppVersion_appArn,
    updateAppVersionResponse_additionalInfo,
    updateAppVersionResponse_httpStatus,
    updateAppVersionResponse_appArn,
    updateAppVersionResponse_appVersion,

    -- ** UpdateAppVersionAppComponent
    updateAppVersionAppComponent_additionalInfo,
    updateAppVersionAppComponent_name,
    updateAppVersionAppComponent_type,
    updateAppVersionAppComponent_appArn,
    updateAppVersionAppComponent_id,
    updateAppVersionAppComponentResponse_appComponent,
    updateAppVersionAppComponentResponse_httpStatus,
    updateAppVersionAppComponentResponse_appArn,
    updateAppVersionAppComponentResponse_appVersion,

    -- ** UpdateAppVersionResource
    updateAppVersionResource_additionalInfo,
    updateAppVersionResource_appComponents,
    updateAppVersionResource_awsAccountId,
    updateAppVersionResource_awsRegion,
    updateAppVersionResource_excluded,
    updateAppVersionResource_logicalResourceId,
    updateAppVersionResource_physicalResourceId,
    updateAppVersionResource_resourceName,
    updateAppVersionResource_resourceType,
    updateAppVersionResource_appArn,
    updateAppVersionResourceResponse_physicalResource,
    updateAppVersionResourceResponse_httpStatus,
    updateAppVersionResourceResponse_appArn,
    updateAppVersionResourceResponse_appVersion,

    -- ** UpdateResiliencyPolicy
    updateResiliencyPolicy_dataLocationConstraint,
    updateResiliencyPolicy_policy,
    updateResiliencyPolicy_policyDescription,
    updateResiliencyPolicy_policyName,
    updateResiliencyPolicy_tier,
    updateResiliencyPolicy_policyArn,
    updateResiliencyPolicyResponse_httpStatus,
    updateResiliencyPolicyResponse_policy,

    -- * Types

    -- ** AlarmRecommendation
    alarmRecommendation_appComponentName,
    alarmRecommendation_description,
    alarmRecommendation_items,
    alarmRecommendation_prerequisite,
    alarmRecommendation_name,
    alarmRecommendation_recommendationId,
    alarmRecommendation_referenceId,
    alarmRecommendation_type,

    -- ** App
    app_assessmentSchedule,
    app_complianceStatus,
    app_description,
    app_lastAppComplianceEvaluationTime,
    app_lastResiliencyScoreEvaluationTime,
    app_policyArn,
    app_resiliencyScore,
    app_status,
    app_tags,
    app_appArn,
    app_creationTime,
    app_name,

    -- ** AppAssessment
    appAssessment_appArn,
    appAssessment_appVersion,
    appAssessment_assessmentName,
    appAssessment_compliance,
    appAssessment_complianceStatus,
    appAssessment_cost,
    appAssessment_endTime,
    appAssessment_message,
    appAssessment_policy,
    appAssessment_resiliencyScore,
    appAssessment_resourceErrorsDetails,
    appAssessment_startTime,
    appAssessment_tags,
    appAssessment_assessmentArn,
    appAssessment_assessmentStatus,
    appAssessment_invoker,

    -- ** AppAssessmentSummary
    appAssessmentSummary_appArn,
    appAssessmentSummary_appVersion,
    appAssessmentSummary_assessmentName,
    appAssessmentSummary_complianceStatus,
    appAssessmentSummary_cost,
    appAssessmentSummary_endTime,
    appAssessmentSummary_invoker,
    appAssessmentSummary_message,
    appAssessmentSummary_resiliencyScore,
    appAssessmentSummary_startTime,
    appAssessmentSummary_assessmentArn,
    appAssessmentSummary_assessmentStatus,

    -- ** AppComponent
    appComponent_additionalInfo,
    appComponent_id,
    appComponent_name,
    appComponent_type,

    -- ** AppComponentCompliance
    appComponentCompliance_appComponentName,
    appComponentCompliance_compliance,
    appComponentCompliance_cost,
    appComponentCompliance_message,
    appComponentCompliance_resiliencyScore,
    appComponentCompliance_status,

    -- ** AppInputSource
    appInputSource_eksSourceClusterNamespace,
    appInputSource_resourceCount,
    appInputSource_sourceArn,
    appInputSource_sourceName,
    appInputSource_terraformSource,
    appInputSource_importType,

    -- ** AppSummary
    appSummary_assessmentSchedule,
    appSummary_complianceStatus,
    appSummary_description,
    appSummary_resiliencyScore,
    appSummary_status,
    appSummary_appArn,
    appSummary_creationTime,
    appSummary_name,

    -- ** AppVersionSummary
    appVersionSummary_appVersion,

    -- ** ComponentRecommendation
    componentRecommendation_appComponentName,
    componentRecommendation_configRecommendations,
    componentRecommendation_recommendationStatus,

    -- ** ConfigRecommendation
    configRecommendation_appComponentName,
    configRecommendation_compliance,
    configRecommendation_cost,
    configRecommendation_description,
    configRecommendation_haArchitecture,
    configRecommendation_recommendationCompliance,
    configRecommendation_suggestedChanges,
    configRecommendation_name,
    configRecommendation_optimizationType,
    configRecommendation_referenceId,

    -- ** Cost
    cost_amount,
    cost_currency,
    cost_frequency,

    -- ** DisruptionCompliance
    disruptionCompliance_achievableRpoInSecs,
    disruptionCompliance_achievableRtoInSecs,
    disruptionCompliance_currentRpoInSecs,
    disruptionCompliance_currentRtoInSecs,
    disruptionCompliance_message,
    disruptionCompliance_rpoDescription,
    disruptionCompliance_rpoReferenceId,
    disruptionCompliance_rtoDescription,
    disruptionCompliance_rtoReferenceId,
    disruptionCompliance_complianceStatus,

    -- ** EksSource
    eksSource_eksClusterArn,
    eksSource_namespaces,

    -- ** EksSourceClusterNamespace
    eksSourceClusterNamespace_eksClusterArn,
    eksSourceClusterNamespace_namespace,

    -- ** FailurePolicy
    failurePolicy_rpoInSecs,
    failurePolicy_rtoInSecs,

    -- ** LogicalResourceId
    logicalResourceId_eksSourceName,
    logicalResourceId_logicalStackName,
    logicalResourceId_resourceGroupName,
    logicalResourceId_terraformSourceName,
    logicalResourceId_identifier,

    -- ** PhysicalResource
    physicalResource_additionalInfo,
    physicalResource_appComponents,
    physicalResource_excluded,
    physicalResource_parentResourceName,
    physicalResource_resourceName,
    physicalResource_sourceType,
    physicalResource_logicalResourceId,
    physicalResource_physicalResourceId,
    physicalResource_resourceType,

    -- ** PhysicalResourceId
    physicalResourceId_awsAccountId,
    physicalResourceId_awsRegion,
    physicalResourceId_identifier,
    physicalResourceId_type,

    -- ** RecommendationDisruptionCompliance
    recommendationDisruptionCompliance_expectedRpoDescription,
    recommendationDisruptionCompliance_expectedRpoInSecs,
    recommendationDisruptionCompliance_expectedRtoDescription,
    recommendationDisruptionCompliance_expectedRtoInSecs,
    recommendationDisruptionCompliance_expectedComplianceStatus,

    -- ** RecommendationItem
    recommendationItem_alreadyImplemented,
    recommendationItem_resourceId,
    recommendationItem_targetAccountId,
    recommendationItem_targetRegion,

    -- ** RecommendationTemplate
    recommendationTemplate_appArn,
    recommendationTemplate_endTime,
    recommendationTemplate_message,
    recommendationTemplate_needsReplacements,
    recommendationTemplate_recommendationIds,
    recommendationTemplate_startTime,
    recommendationTemplate_tags,
    recommendationTemplate_templatesLocation,
    recommendationTemplate_assessmentArn,
    recommendationTemplate_format,
    recommendationTemplate_name,
    recommendationTemplate_recommendationTemplateArn,
    recommendationTemplate_recommendationTypes,
    recommendationTemplate_status,

    -- ** ResiliencyPolicy
    resiliencyPolicy_creationTime,
    resiliencyPolicy_dataLocationConstraint,
    resiliencyPolicy_estimatedCostTier,
    resiliencyPolicy_policy,
    resiliencyPolicy_policyArn,
    resiliencyPolicy_policyDescription,
    resiliencyPolicy_policyName,
    resiliencyPolicy_tags,
    resiliencyPolicy_tier,

    -- ** ResiliencyScore
    resiliencyScore_disruptionScore,
    resiliencyScore_score,

    -- ** ResourceError
    resourceError_logicalResourceId,
    resourceError_physicalResourceId,
    resourceError_reason,

    -- ** ResourceErrorsDetails
    resourceErrorsDetails_hasMoreErrors,
    resourceErrorsDetails_resourceErrors,

    -- ** ResourceMapping
    resourceMapping_appRegistryAppName,
    resourceMapping_eksSourceName,
    resourceMapping_logicalStackName,
    resourceMapping_resourceGroupName,
    resourceMapping_resourceName,
    resourceMapping_terraformSourceName,
    resourceMapping_mappingType,
    resourceMapping_physicalResourceId,

    -- ** S3Location
    s3Location_bucket,
    s3Location_prefix,

    -- ** SopRecommendation
    sopRecommendation_appComponentName,
    sopRecommendation_description,
    sopRecommendation_items,
    sopRecommendation_name,
    sopRecommendation_prerequisite,
    sopRecommendation_recommendationId,
    sopRecommendation_referenceId,
    sopRecommendation_serviceType,

    -- ** TerraformSource
    terraformSource_s3StateFileUrl,

    -- ** TestRecommendation
    testRecommendation_appComponentName,
    testRecommendation_dependsOnAlarms,
    testRecommendation_description,
    testRecommendation_intent,
    testRecommendation_items,
    testRecommendation_name,
    testRecommendation_prerequisite,
    testRecommendation_recommendationId,
    testRecommendation_risk,
    testRecommendation_type,
    testRecommendation_referenceId,

    -- ** UnsupportedResource
    unsupportedResource_unsupportedResourceStatus,
    unsupportedResource_logicalResourceId,
    unsupportedResource_physicalResourceId,
    unsupportedResource_resourceType,
  )
where

import Amazonka.ResilienceHub.AddDraftAppVersionResourceMappings
import Amazonka.ResilienceHub.CreateApp
import Amazonka.ResilienceHub.CreateAppVersionAppComponent
import Amazonka.ResilienceHub.CreateAppVersionResource
import Amazonka.ResilienceHub.CreateRecommendationTemplate
import Amazonka.ResilienceHub.CreateResiliencyPolicy
import Amazonka.ResilienceHub.DeleteApp
import Amazonka.ResilienceHub.DeleteAppAssessment
import Amazonka.ResilienceHub.DeleteAppInputSource
import Amazonka.ResilienceHub.DeleteAppVersionAppComponent
import Amazonka.ResilienceHub.DeleteAppVersionResource
import Amazonka.ResilienceHub.DeleteRecommendationTemplate
import Amazonka.ResilienceHub.DeleteResiliencyPolicy
import Amazonka.ResilienceHub.DescribeApp
import Amazonka.ResilienceHub.DescribeAppAssessment
import Amazonka.ResilienceHub.DescribeAppVersion
import Amazonka.ResilienceHub.DescribeAppVersionAppComponent
import Amazonka.ResilienceHub.DescribeAppVersionResource
import Amazonka.ResilienceHub.DescribeAppVersionResourcesResolutionStatus
import Amazonka.ResilienceHub.DescribeAppVersionTemplate
import Amazonka.ResilienceHub.DescribeDraftAppVersionResourcesImportStatus
import Amazonka.ResilienceHub.DescribeResiliencyPolicy
import Amazonka.ResilienceHub.ImportResourcesToDraftAppVersion
import Amazonka.ResilienceHub.ListAlarmRecommendations
import Amazonka.ResilienceHub.ListAppAssessments
import Amazonka.ResilienceHub.ListAppComponentCompliances
import Amazonka.ResilienceHub.ListAppComponentRecommendations
import Amazonka.ResilienceHub.ListAppInputSources
import Amazonka.ResilienceHub.ListAppVersionAppComponents
import Amazonka.ResilienceHub.ListAppVersionResourceMappings
import Amazonka.ResilienceHub.ListAppVersionResources
import Amazonka.ResilienceHub.ListAppVersions
import Amazonka.ResilienceHub.ListApps
import Amazonka.ResilienceHub.ListRecommendationTemplates
import Amazonka.ResilienceHub.ListResiliencyPolicies
import Amazonka.ResilienceHub.ListSopRecommendations
import Amazonka.ResilienceHub.ListSuggestedResiliencyPolicies
import Amazonka.ResilienceHub.ListTagsForResource
import Amazonka.ResilienceHub.ListTestRecommendations
import Amazonka.ResilienceHub.ListUnsupportedAppVersionResources
import Amazonka.ResilienceHub.PublishAppVersion
import Amazonka.ResilienceHub.PutDraftAppVersionTemplate
import Amazonka.ResilienceHub.RemoveDraftAppVersionResourceMappings
import Amazonka.ResilienceHub.ResolveAppVersionResources
import Amazonka.ResilienceHub.StartAppAssessment
import Amazonka.ResilienceHub.TagResource
import Amazonka.ResilienceHub.Types.AlarmRecommendation
import Amazonka.ResilienceHub.Types.App
import Amazonka.ResilienceHub.Types.AppAssessment
import Amazonka.ResilienceHub.Types.AppAssessmentSummary
import Amazonka.ResilienceHub.Types.AppComponent
import Amazonka.ResilienceHub.Types.AppComponentCompliance
import Amazonka.ResilienceHub.Types.AppInputSource
import Amazonka.ResilienceHub.Types.AppSummary
import Amazonka.ResilienceHub.Types.AppVersionSummary
import Amazonka.ResilienceHub.Types.ComponentRecommendation
import Amazonka.ResilienceHub.Types.ConfigRecommendation
import Amazonka.ResilienceHub.Types.Cost
import Amazonka.ResilienceHub.Types.DisruptionCompliance
import Amazonka.ResilienceHub.Types.EksSource
import Amazonka.ResilienceHub.Types.EksSourceClusterNamespace
import Amazonka.ResilienceHub.Types.FailurePolicy
import Amazonka.ResilienceHub.Types.LogicalResourceId
import Amazonka.ResilienceHub.Types.PhysicalResource
import Amazonka.ResilienceHub.Types.PhysicalResourceId
import Amazonka.ResilienceHub.Types.RecommendationDisruptionCompliance
import Amazonka.ResilienceHub.Types.RecommendationItem
import Amazonka.ResilienceHub.Types.RecommendationTemplate
import Amazonka.ResilienceHub.Types.ResiliencyPolicy
import Amazonka.ResilienceHub.Types.ResiliencyScore
import Amazonka.ResilienceHub.Types.ResourceError
import Amazonka.ResilienceHub.Types.ResourceErrorsDetails
import Amazonka.ResilienceHub.Types.ResourceMapping
import Amazonka.ResilienceHub.Types.S3Location
import Amazonka.ResilienceHub.Types.SopRecommendation
import Amazonka.ResilienceHub.Types.TerraformSource
import Amazonka.ResilienceHub.Types.TestRecommendation
import Amazonka.ResilienceHub.Types.UnsupportedResource
import Amazonka.ResilienceHub.UntagResource
import Amazonka.ResilienceHub.UpdateApp
import Amazonka.ResilienceHub.UpdateAppVersion
import Amazonka.ResilienceHub.UpdateAppVersionAppComponent
import Amazonka.ResilienceHub.UpdateAppVersionResource
import Amazonka.ResilienceHub.UpdateResiliencyPolicy
