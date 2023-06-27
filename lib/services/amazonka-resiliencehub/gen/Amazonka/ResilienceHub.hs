{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ResilienceHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-04-30@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Resilience Hub helps you proactively prepare and protect your Amazon Web
-- Services applications from disruptions. It offers continual resiliency
-- assessment and validation that integrates into your software development
-- lifecycle. This enables you to uncover resiliency weaknesses, ensure
-- recovery time objective (RTO) and recovery point objective (RPO) targets
-- for your applications are met, and resolve issues before they are
-- released into production.
module Amazonka.ResilienceHub
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

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

    -- ** AddDraftAppVersionResourceMappings
    AddDraftAppVersionResourceMappings (AddDraftAppVersionResourceMappings'),
    newAddDraftAppVersionResourceMappings,
    AddDraftAppVersionResourceMappingsResponse (AddDraftAppVersionResourceMappingsResponse'),
    newAddDraftAppVersionResourceMappingsResponse,

    -- ** CreateApp
    CreateApp (CreateApp'),
    newCreateApp,
    CreateAppResponse (CreateAppResponse'),
    newCreateAppResponse,

    -- ** CreateAppVersionAppComponent
    CreateAppVersionAppComponent (CreateAppVersionAppComponent'),
    newCreateAppVersionAppComponent,
    CreateAppVersionAppComponentResponse (CreateAppVersionAppComponentResponse'),
    newCreateAppVersionAppComponentResponse,

    -- ** CreateAppVersionResource
    CreateAppVersionResource (CreateAppVersionResource'),
    newCreateAppVersionResource,
    CreateAppVersionResourceResponse (CreateAppVersionResourceResponse'),
    newCreateAppVersionResourceResponse,

    -- ** CreateRecommendationTemplate
    CreateRecommendationTemplate (CreateRecommendationTemplate'),
    newCreateRecommendationTemplate,
    CreateRecommendationTemplateResponse (CreateRecommendationTemplateResponse'),
    newCreateRecommendationTemplateResponse,

    -- ** CreateResiliencyPolicy
    CreateResiliencyPolicy (CreateResiliencyPolicy'),
    newCreateResiliencyPolicy,
    CreateResiliencyPolicyResponse (CreateResiliencyPolicyResponse'),
    newCreateResiliencyPolicyResponse,

    -- ** DeleteApp
    DeleteApp (DeleteApp'),
    newDeleteApp,
    DeleteAppResponse (DeleteAppResponse'),
    newDeleteAppResponse,

    -- ** DeleteAppAssessment
    DeleteAppAssessment (DeleteAppAssessment'),
    newDeleteAppAssessment,
    DeleteAppAssessmentResponse (DeleteAppAssessmentResponse'),
    newDeleteAppAssessmentResponse,

    -- ** DeleteAppInputSource
    DeleteAppInputSource (DeleteAppInputSource'),
    newDeleteAppInputSource,
    DeleteAppInputSourceResponse (DeleteAppInputSourceResponse'),
    newDeleteAppInputSourceResponse,

    -- ** DeleteAppVersionAppComponent
    DeleteAppVersionAppComponent (DeleteAppVersionAppComponent'),
    newDeleteAppVersionAppComponent,
    DeleteAppVersionAppComponentResponse (DeleteAppVersionAppComponentResponse'),
    newDeleteAppVersionAppComponentResponse,

    -- ** DeleteAppVersionResource
    DeleteAppVersionResource (DeleteAppVersionResource'),
    newDeleteAppVersionResource,
    DeleteAppVersionResourceResponse (DeleteAppVersionResourceResponse'),
    newDeleteAppVersionResourceResponse,

    -- ** DeleteRecommendationTemplate
    DeleteRecommendationTemplate (DeleteRecommendationTemplate'),
    newDeleteRecommendationTemplate,
    DeleteRecommendationTemplateResponse (DeleteRecommendationTemplateResponse'),
    newDeleteRecommendationTemplateResponse,

    -- ** DeleteResiliencyPolicy
    DeleteResiliencyPolicy (DeleteResiliencyPolicy'),
    newDeleteResiliencyPolicy,
    DeleteResiliencyPolicyResponse (DeleteResiliencyPolicyResponse'),
    newDeleteResiliencyPolicyResponse,

    -- ** DescribeApp
    DescribeApp (DescribeApp'),
    newDescribeApp,
    DescribeAppResponse (DescribeAppResponse'),
    newDescribeAppResponse,

    -- ** DescribeAppAssessment
    DescribeAppAssessment (DescribeAppAssessment'),
    newDescribeAppAssessment,
    DescribeAppAssessmentResponse (DescribeAppAssessmentResponse'),
    newDescribeAppAssessmentResponse,

    -- ** DescribeAppVersion
    DescribeAppVersion (DescribeAppVersion'),
    newDescribeAppVersion,
    DescribeAppVersionResponse (DescribeAppVersionResponse'),
    newDescribeAppVersionResponse,

    -- ** DescribeAppVersionAppComponent
    DescribeAppVersionAppComponent (DescribeAppVersionAppComponent'),
    newDescribeAppVersionAppComponent,
    DescribeAppVersionAppComponentResponse (DescribeAppVersionAppComponentResponse'),
    newDescribeAppVersionAppComponentResponse,

    -- ** DescribeAppVersionResource
    DescribeAppVersionResource (DescribeAppVersionResource'),
    newDescribeAppVersionResource,
    DescribeAppVersionResourceResponse (DescribeAppVersionResourceResponse'),
    newDescribeAppVersionResourceResponse,

    -- ** DescribeAppVersionResourcesResolutionStatus
    DescribeAppVersionResourcesResolutionStatus (DescribeAppVersionResourcesResolutionStatus'),
    newDescribeAppVersionResourcesResolutionStatus,
    DescribeAppVersionResourcesResolutionStatusResponse (DescribeAppVersionResourcesResolutionStatusResponse'),
    newDescribeAppVersionResourcesResolutionStatusResponse,

    -- ** DescribeAppVersionTemplate
    DescribeAppVersionTemplate (DescribeAppVersionTemplate'),
    newDescribeAppVersionTemplate,
    DescribeAppVersionTemplateResponse (DescribeAppVersionTemplateResponse'),
    newDescribeAppVersionTemplateResponse,

    -- ** DescribeDraftAppVersionResourcesImportStatus
    DescribeDraftAppVersionResourcesImportStatus (DescribeDraftAppVersionResourcesImportStatus'),
    newDescribeDraftAppVersionResourcesImportStatus,
    DescribeDraftAppVersionResourcesImportStatusResponse (DescribeDraftAppVersionResourcesImportStatusResponse'),
    newDescribeDraftAppVersionResourcesImportStatusResponse,

    -- ** DescribeResiliencyPolicy
    DescribeResiliencyPolicy (DescribeResiliencyPolicy'),
    newDescribeResiliencyPolicy,
    DescribeResiliencyPolicyResponse (DescribeResiliencyPolicyResponse'),
    newDescribeResiliencyPolicyResponse,

    -- ** ImportResourcesToDraftAppVersion
    ImportResourcesToDraftAppVersion (ImportResourcesToDraftAppVersion'),
    newImportResourcesToDraftAppVersion,
    ImportResourcesToDraftAppVersionResponse (ImportResourcesToDraftAppVersionResponse'),
    newImportResourcesToDraftAppVersionResponse,

    -- ** ListAlarmRecommendations
    ListAlarmRecommendations (ListAlarmRecommendations'),
    newListAlarmRecommendations,
    ListAlarmRecommendationsResponse (ListAlarmRecommendationsResponse'),
    newListAlarmRecommendationsResponse,

    -- ** ListAppAssessments
    ListAppAssessments (ListAppAssessments'),
    newListAppAssessments,
    ListAppAssessmentsResponse (ListAppAssessmentsResponse'),
    newListAppAssessmentsResponse,

    -- ** ListAppComponentCompliances
    ListAppComponentCompliances (ListAppComponentCompliances'),
    newListAppComponentCompliances,
    ListAppComponentCompliancesResponse (ListAppComponentCompliancesResponse'),
    newListAppComponentCompliancesResponse,

    -- ** ListAppComponentRecommendations
    ListAppComponentRecommendations (ListAppComponentRecommendations'),
    newListAppComponentRecommendations,
    ListAppComponentRecommendationsResponse (ListAppComponentRecommendationsResponse'),
    newListAppComponentRecommendationsResponse,

    -- ** ListAppInputSources
    ListAppInputSources (ListAppInputSources'),
    newListAppInputSources,
    ListAppInputSourcesResponse (ListAppInputSourcesResponse'),
    newListAppInputSourcesResponse,

    -- ** ListAppVersionAppComponents
    ListAppVersionAppComponents (ListAppVersionAppComponents'),
    newListAppVersionAppComponents,
    ListAppVersionAppComponentsResponse (ListAppVersionAppComponentsResponse'),
    newListAppVersionAppComponentsResponse,

    -- ** ListAppVersionResourceMappings
    ListAppVersionResourceMappings (ListAppVersionResourceMappings'),
    newListAppVersionResourceMappings,
    ListAppVersionResourceMappingsResponse (ListAppVersionResourceMappingsResponse'),
    newListAppVersionResourceMappingsResponse,

    -- ** ListAppVersionResources
    ListAppVersionResources (ListAppVersionResources'),
    newListAppVersionResources,
    ListAppVersionResourcesResponse (ListAppVersionResourcesResponse'),
    newListAppVersionResourcesResponse,

    -- ** ListAppVersions
    ListAppVersions (ListAppVersions'),
    newListAppVersions,
    ListAppVersionsResponse (ListAppVersionsResponse'),
    newListAppVersionsResponse,

    -- ** ListApps
    ListApps (ListApps'),
    newListApps,
    ListAppsResponse (ListAppsResponse'),
    newListAppsResponse,

    -- ** ListRecommendationTemplates
    ListRecommendationTemplates (ListRecommendationTemplates'),
    newListRecommendationTemplates,
    ListRecommendationTemplatesResponse (ListRecommendationTemplatesResponse'),
    newListRecommendationTemplatesResponse,

    -- ** ListResiliencyPolicies
    ListResiliencyPolicies (ListResiliencyPolicies'),
    newListResiliencyPolicies,
    ListResiliencyPoliciesResponse (ListResiliencyPoliciesResponse'),
    newListResiliencyPoliciesResponse,

    -- ** ListSopRecommendations
    ListSopRecommendations (ListSopRecommendations'),
    newListSopRecommendations,
    ListSopRecommendationsResponse (ListSopRecommendationsResponse'),
    newListSopRecommendationsResponse,

    -- ** ListSuggestedResiliencyPolicies
    ListSuggestedResiliencyPolicies (ListSuggestedResiliencyPolicies'),
    newListSuggestedResiliencyPolicies,
    ListSuggestedResiliencyPoliciesResponse (ListSuggestedResiliencyPoliciesResponse'),
    newListSuggestedResiliencyPoliciesResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListTestRecommendations
    ListTestRecommendations (ListTestRecommendations'),
    newListTestRecommendations,
    ListTestRecommendationsResponse (ListTestRecommendationsResponse'),
    newListTestRecommendationsResponse,

    -- ** ListUnsupportedAppVersionResources
    ListUnsupportedAppVersionResources (ListUnsupportedAppVersionResources'),
    newListUnsupportedAppVersionResources,
    ListUnsupportedAppVersionResourcesResponse (ListUnsupportedAppVersionResourcesResponse'),
    newListUnsupportedAppVersionResourcesResponse,

    -- ** PublishAppVersion
    PublishAppVersion (PublishAppVersion'),
    newPublishAppVersion,
    PublishAppVersionResponse (PublishAppVersionResponse'),
    newPublishAppVersionResponse,

    -- ** PutDraftAppVersionTemplate
    PutDraftAppVersionTemplate (PutDraftAppVersionTemplate'),
    newPutDraftAppVersionTemplate,
    PutDraftAppVersionTemplateResponse (PutDraftAppVersionTemplateResponse'),
    newPutDraftAppVersionTemplateResponse,

    -- ** RemoveDraftAppVersionResourceMappings
    RemoveDraftAppVersionResourceMappings (RemoveDraftAppVersionResourceMappings'),
    newRemoveDraftAppVersionResourceMappings,
    RemoveDraftAppVersionResourceMappingsResponse (RemoveDraftAppVersionResourceMappingsResponse'),
    newRemoveDraftAppVersionResourceMappingsResponse,

    -- ** ResolveAppVersionResources
    ResolveAppVersionResources (ResolveAppVersionResources'),
    newResolveAppVersionResources,
    ResolveAppVersionResourcesResponse (ResolveAppVersionResourcesResponse'),
    newResolveAppVersionResourcesResponse,

    -- ** StartAppAssessment
    StartAppAssessment (StartAppAssessment'),
    newStartAppAssessment,
    StartAppAssessmentResponse (StartAppAssessmentResponse'),
    newStartAppAssessmentResponse,

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

    -- ** UpdateApp
    UpdateApp (UpdateApp'),
    newUpdateApp,
    UpdateAppResponse (UpdateAppResponse'),
    newUpdateAppResponse,

    -- ** UpdateAppVersion
    UpdateAppVersion (UpdateAppVersion'),
    newUpdateAppVersion,
    UpdateAppVersionResponse (UpdateAppVersionResponse'),
    newUpdateAppVersionResponse,

    -- ** UpdateAppVersionAppComponent
    UpdateAppVersionAppComponent (UpdateAppVersionAppComponent'),
    newUpdateAppVersionAppComponent,
    UpdateAppVersionAppComponentResponse (UpdateAppVersionAppComponentResponse'),
    newUpdateAppVersionAppComponentResponse,

    -- ** UpdateAppVersionResource
    UpdateAppVersionResource (UpdateAppVersionResource'),
    newUpdateAppVersionResource,
    UpdateAppVersionResourceResponse (UpdateAppVersionResourceResponse'),
    newUpdateAppVersionResourceResponse,

    -- ** UpdateResiliencyPolicy
    UpdateResiliencyPolicy (UpdateResiliencyPolicy'),
    newUpdateResiliencyPolicy,
    UpdateResiliencyPolicyResponse (UpdateResiliencyPolicyResponse'),
    newUpdateResiliencyPolicyResponse,

    -- * Types

    -- ** AlarmType
    AlarmType (..),

    -- ** AppAssessmentScheduleType
    AppAssessmentScheduleType (..),

    -- ** AppComplianceStatusType
    AppComplianceStatusType (..),

    -- ** AppStatusType
    AppStatusType (..),

    -- ** AssessmentInvoker
    AssessmentInvoker (..),

    -- ** AssessmentStatus
    AssessmentStatus (..),

    -- ** ComplianceStatus
    ComplianceStatus (..),

    -- ** ConfigRecommendationOptimizationType
    ConfigRecommendationOptimizationType (..),

    -- ** CostFrequency
    CostFrequency (..),

    -- ** DataLocationConstraint
    DataLocationConstraint (..),

    -- ** DisruptionType
    DisruptionType (..),

    -- ** EstimatedCostTier
    EstimatedCostTier (..),

    -- ** HaArchitecture
    HaArchitecture (..),

    -- ** PhysicalIdentifierType
    PhysicalIdentifierType (..),

    -- ** RecommendationComplianceStatus
    RecommendationComplianceStatus (..),

    -- ** RecommendationTemplateStatus
    RecommendationTemplateStatus (..),

    -- ** RenderRecommendationType
    RenderRecommendationType (..),

    -- ** ResiliencyPolicyTier
    ResiliencyPolicyTier (..),

    -- ** ResourceImportStatusType
    ResourceImportStatusType (..),

    -- ** ResourceImportStrategyType
    ResourceImportStrategyType (..),

    -- ** ResourceMappingType
    ResourceMappingType (..),

    -- ** ResourceResolutionStatusType
    ResourceResolutionStatusType (..),

    -- ** ResourceSourceType
    ResourceSourceType (..),

    -- ** SopServiceType
    SopServiceType (..),

    -- ** TemplateFormat
    TemplateFormat (..),

    -- ** TestRisk
    TestRisk (..),

    -- ** TestType
    TestType (..),

    -- ** AlarmRecommendation
    AlarmRecommendation (AlarmRecommendation'),
    newAlarmRecommendation,

    -- ** App
    App (App'),
    newApp,

    -- ** AppAssessment
    AppAssessment (AppAssessment'),
    newAppAssessment,

    -- ** AppAssessmentSummary
    AppAssessmentSummary (AppAssessmentSummary'),
    newAppAssessmentSummary,

    -- ** AppComponent
    AppComponent (AppComponent'),
    newAppComponent,

    -- ** AppComponentCompliance
    AppComponentCompliance (AppComponentCompliance'),
    newAppComponentCompliance,

    -- ** AppInputSource
    AppInputSource (AppInputSource'),
    newAppInputSource,

    -- ** AppSummary
    AppSummary (AppSummary'),
    newAppSummary,

    -- ** AppVersionSummary
    AppVersionSummary (AppVersionSummary'),
    newAppVersionSummary,

    -- ** ComponentRecommendation
    ComponentRecommendation (ComponentRecommendation'),
    newComponentRecommendation,

    -- ** ConfigRecommendation
    ConfigRecommendation (ConfigRecommendation'),
    newConfigRecommendation,

    -- ** Cost
    Cost (Cost'),
    newCost,

    -- ** DisruptionCompliance
    DisruptionCompliance (DisruptionCompliance'),
    newDisruptionCompliance,

    -- ** EksSource
    EksSource (EksSource'),
    newEksSource,

    -- ** EksSourceClusterNamespace
    EksSourceClusterNamespace (EksSourceClusterNamespace'),
    newEksSourceClusterNamespace,

    -- ** FailurePolicy
    FailurePolicy (FailurePolicy'),
    newFailurePolicy,

    -- ** LogicalResourceId
    LogicalResourceId (LogicalResourceId'),
    newLogicalResourceId,

    -- ** PhysicalResource
    PhysicalResource (PhysicalResource'),
    newPhysicalResource,

    -- ** PhysicalResourceId
    PhysicalResourceId (PhysicalResourceId'),
    newPhysicalResourceId,

    -- ** RecommendationDisruptionCompliance
    RecommendationDisruptionCompliance (RecommendationDisruptionCompliance'),
    newRecommendationDisruptionCompliance,

    -- ** RecommendationItem
    RecommendationItem (RecommendationItem'),
    newRecommendationItem,

    -- ** RecommendationTemplate
    RecommendationTemplate (RecommendationTemplate'),
    newRecommendationTemplate,

    -- ** ResiliencyPolicy
    ResiliencyPolicy (ResiliencyPolicy'),
    newResiliencyPolicy,

    -- ** ResiliencyScore
    ResiliencyScore (ResiliencyScore'),
    newResiliencyScore,

    -- ** ResourceError
    ResourceError (ResourceError'),
    newResourceError,

    -- ** ResourceErrorsDetails
    ResourceErrorsDetails (ResourceErrorsDetails'),
    newResourceErrorsDetails,

    -- ** ResourceMapping
    ResourceMapping (ResourceMapping'),
    newResourceMapping,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** SopRecommendation
    SopRecommendation (SopRecommendation'),
    newSopRecommendation,

    -- ** TerraformSource
    TerraformSource (TerraformSource'),
    newTerraformSource,

    -- ** TestRecommendation
    TestRecommendation (TestRecommendation'),
    newTestRecommendation,

    -- ** UnsupportedResource
    UnsupportedResource (UnsupportedResource'),
    newUnsupportedResource,
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
import Amazonka.ResilienceHub.Lens
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
import Amazonka.ResilienceHub.Types
import Amazonka.ResilienceHub.UntagResource
import Amazonka.ResilienceHub.UpdateApp
import Amazonka.ResilienceHub.UpdateAppVersion
import Amazonka.ResilienceHub.UpdateAppVersionAppComponent
import Amazonka.ResilienceHub.UpdateAppVersionResource
import Amazonka.ResilienceHub.UpdateResiliencyPolicy
import Amazonka.ResilienceHub.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ResilienceHub'.

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
