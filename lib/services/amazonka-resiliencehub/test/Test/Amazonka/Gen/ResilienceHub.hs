{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ResilienceHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ResilienceHub where

import Amazonka.ResilienceHub
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ResilienceHub.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddDraftAppVersionResourceMappings $
--             newAddDraftAppVersionResourceMappings
--
--         , requestCreateApp $
--             newCreateApp
--
--         , requestCreateAppVersionAppComponent $
--             newCreateAppVersionAppComponent
--
--         , requestCreateAppVersionResource $
--             newCreateAppVersionResource
--
--         , requestCreateRecommendationTemplate $
--             newCreateRecommendationTemplate
--
--         , requestCreateResiliencyPolicy $
--             newCreateResiliencyPolicy
--
--         , requestDeleteApp $
--             newDeleteApp
--
--         , requestDeleteAppAssessment $
--             newDeleteAppAssessment
--
--         , requestDeleteAppInputSource $
--             newDeleteAppInputSource
--
--         , requestDeleteAppVersionAppComponent $
--             newDeleteAppVersionAppComponent
--
--         , requestDeleteAppVersionResource $
--             newDeleteAppVersionResource
--
--         , requestDeleteRecommendationTemplate $
--             newDeleteRecommendationTemplate
--
--         , requestDeleteResiliencyPolicy $
--             newDeleteResiliencyPolicy
--
--         , requestDescribeApp $
--             newDescribeApp
--
--         , requestDescribeAppAssessment $
--             newDescribeAppAssessment
--
--         , requestDescribeAppVersion $
--             newDescribeAppVersion
--
--         , requestDescribeAppVersionAppComponent $
--             newDescribeAppVersionAppComponent
--
--         , requestDescribeAppVersionResource $
--             newDescribeAppVersionResource
--
--         , requestDescribeAppVersionResourcesResolutionStatus $
--             newDescribeAppVersionResourcesResolutionStatus
--
--         , requestDescribeAppVersionTemplate $
--             newDescribeAppVersionTemplate
--
--         , requestDescribeDraftAppVersionResourcesImportStatus $
--             newDescribeDraftAppVersionResourcesImportStatus
--
--         , requestDescribeResiliencyPolicy $
--             newDescribeResiliencyPolicy
--
--         , requestImportResourcesToDraftAppVersion $
--             newImportResourcesToDraftAppVersion
--
--         , requestListAlarmRecommendations $
--             newListAlarmRecommendations
--
--         , requestListAppAssessments $
--             newListAppAssessments
--
--         , requestListAppComponentCompliances $
--             newListAppComponentCompliances
--
--         , requestListAppComponentRecommendations $
--             newListAppComponentRecommendations
--
--         , requestListAppInputSources $
--             newListAppInputSources
--
--         , requestListAppVersionAppComponents $
--             newListAppVersionAppComponents
--
--         , requestListAppVersionResourceMappings $
--             newListAppVersionResourceMappings
--
--         , requestListAppVersionResources $
--             newListAppVersionResources
--
--         , requestListAppVersions $
--             newListAppVersions
--
--         , requestListApps $
--             newListApps
--
--         , requestListRecommendationTemplates $
--             newListRecommendationTemplates
--
--         , requestListResiliencyPolicies $
--             newListResiliencyPolicies
--
--         , requestListSopRecommendations $
--             newListSopRecommendations
--
--         , requestListSuggestedResiliencyPolicies $
--             newListSuggestedResiliencyPolicies
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTestRecommendations $
--             newListTestRecommendations
--
--         , requestListUnsupportedAppVersionResources $
--             newListUnsupportedAppVersionResources
--
--         , requestPublishAppVersion $
--             newPublishAppVersion
--
--         , requestPutDraftAppVersionTemplate $
--             newPutDraftAppVersionTemplate
--
--         , requestRemoveDraftAppVersionResourceMappings $
--             newRemoveDraftAppVersionResourceMappings
--
--         , requestResolveAppVersionResources $
--             newResolveAppVersionResources
--
--         , requestStartAppAssessment $
--             newStartAppAssessment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApp $
--             newUpdateApp
--
--         , requestUpdateAppVersion $
--             newUpdateAppVersion
--
--         , requestUpdateAppVersionAppComponent $
--             newUpdateAppVersionAppComponent
--
--         , requestUpdateAppVersionResource $
--             newUpdateAppVersionResource
--
--         , requestUpdateResiliencyPolicy $
--             newUpdateResiliencyPolicy
--
--           ]

--     , testGroup "response"
--         [ responseAddDraftAppVersionResourceMappings $
--             newAddDraftAppVersionResourceMappingsResponse
--
--         , responseCreateApp $
--             newCreateAppResponse
--
--         , responseCreateAppVersionAppComponent $
--             newCreateAppVersionAppComponentResponse
--
--         , responseCreateAppVersionResource $
--             newCreateAppVersionResourceResponse
--
--         , responseCreateRecommendationTemplate $
--             newCreateRecommendationTemplateResponse
--
--         , responseCreateResiliencyPolicy $
--             newCreateResiliencyPolicyResponse
--
--         , responseDeleteApp $
--             newDeleteAppResponse
--
--         , responseDeleteAppAssessment $
--             newDeleteAppAssessmentResponse
--
--         , responseDeleteAppInputSource $
--             newDeleteAppInputSourceResponse
--
--         , responseDeleteAppVersionAppComponent $
--             newDeleteAppVersionAppComponentResponse
--
--         , responseDeleteAppVersionResource $
--             newDeleteAppVersionResourceResponse
--
--         , responseDeleteRecommendationTemplate $
--             newDeleteRecommendationTemplateResponse
--
--         , responseDeleteResiliencyPolicy $
--             newDeleteResiliencyPolicyResponse
--
--         , responseDescribeApp $
--             newDescribeAppResponse
--
--         , responseDescribeAppAssessment $
--             newDescribeAppAssessmentResponse
--
--         , responseDescribeAppVersion $
--             newDescribeAppVersionResponse
--
--         , responseDescribeAppVersionAppComponent $
--             newDescribeAppVersionAppComponentResponse
--
--         , responseDescribeAppVersionResource $
--             newDescribeAppVersionResourceResponse
--
--         , responseDescribeAppVersionResourcesResolutionStatus $
--             newDescribeAppVersionResourcesResolutionStatusResponse
--
--         , responseDescribeAppVersionTemplate $
--             newDescribeAppVersionTemplateResponse
--
--         , responseDescribeDraftAppVersionResourcesImportStatus $
--             newDescribeDraftAppVersionResourcesImportStatusResponse
--
--         , responseDescribeResiliencyPolicy $
--             newDescribeResiliencyPolicyResponse
--
--         , responseImportResourcesToDraftAppVersion $
--             newImportResourcesToDraftAppVersionResponse
--
--         , responseListAlarmRecommendations $
--             newListAlarmRecommendationsResponse
--
--         , responseListAppAssessments $
--             newListAppAssessmentsResponse
--
--         , responseListAppComponentCompliances $
--             newListAppComponentCompliancesResponse
--
--         , responseListAppComponentRecommendations $
--             newListAppComponentRecommendationsResponse
--
--         , responseListAppInputSources $
--             newListAppInputSourcesResponse
--
--         , responseListAppVersionAppComponents $
--             newListAppVersionAppComponentsResponse
--
--         , responseListAppVersionResourceMappings $
--             newListAppVersionResourceMappingsResponse
--
--         , responseListAppVersionResources $
--             newListAppVersionResourcesResponse
--
--         , responseListAppVersions $
--             newListAppVersionsResponse
--
--         , responseListApps $
--             newListAppsResponse
--
--         , responseListRecommendationTemplates $
--             newListRecommendationTemplatesResponse
--
--         , responseListResiliencyPolicies $
--             newListResiliencyPoliciesResponse
--
--         , responseListSopRecommendations $
--             newListSopRecommendationsResponse
--
--         , responseListSuggestedResiliencyPolicies $
--             newListSuggestedResiliencyPoliciesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTestRecommendations $
--             newListTestRecommendationsResponse
--
--         , responseListUnsupportedAppVersionResources $
--             newListUnsupportedAppVersionResourcesResponse
--
--         , responsePublishAppVersion $
--             newPublishAppVersionResponse
--
--         , responsePutDraftAppVersionTemplate $
--             newPutDraftAppVersionTemplateResponse
--
--         , responseRemoveDraftAppVersionResourceMappings $
--             newRemoveDraftAppVersionResourceMappingsResponse
--
--         , responseResolveAppVersionResources $
--             newResolveAppVersionResourcesResponse
--
--         , responseStartAppAssessment $
--             newStartAppAssessmentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApp $
--             newUpdateAppResponse
--
--         , responseUpdateAppVersion $
--             newUpdateAppVersionResponse
--
--         , responseUpdateAppVersionAppComponent $
--             newUpdateAppVersionAppComponentResponse
--
--         , responseUpdateAppVersionResource $
--             newUpdateAppVersionResourceResponse
--
--         , responseUpdateResiliencyPolicy $
--             newUpdateResiliencyPolicyResponse
--
--           ]
--     ]

-- Requests

requestAddDraftAppVersionResourceMappings :: AddDraftAppVersionResourceMappings -> TestTree
requestAddDraftAppVersionResourceMappings =
  req
    "AddDraftAppVersionResourceMappings"
    "fixture/AddDraftAppVersionResourceMappings.yaml"

requestCreateApp :: CreateApp -> TestTree
requestCreateApp =
  req
    "CreateApp"
    "fixture/CreateApp.yaml"

requestCreateAppVersionAppComponent :: CreateAppVersionAppComponent -> TestTree
requestCreateAppVersionAppComponent =
  req
    "CreateAppVersionAppComponent"
    "fixture/CreateAppVersionAppComponent.yaml"

requestCreateAppVersionResource :: CreateAppVersionResource -> TestTree
requestCreateAppVersionResource =
  req
    "CreateAppVersionResource"
    "fixture/CreateAppVersionResource.yaml"

requestCreateRecommendationTemplate :: CreateRecommendationTemplate -> TestTree
requestCreateRecommendationTemplate =
  req
    "CreateRecommendationTemplate"
    "fixture/CreateRecommendationTemplate.yaml"

requestCreateResiliencyPolicy :: CreateResiliencyPolicy -> TestTree
requestCreateResiliencyPolicy =
  req
    "CreateResiliencyPolicy"
    "fixture/CreateResiliencyPolicy.yaml"

requestDeleteApp :: DeleteApp -> TestTree
requestDeleteApp =
  req
    "DeleteApp"
    "fixture/DeleteApp.yaml"

requestDeleteAppAssessment :: DeleteAppAssessment -> TestTree
requestDeleteAppAssessment =
  req
    "DeleteAppAssessment"
    "fixture/DeleteAppAssessment.yaml"

requestDeleteAppInputSource :: DeleteAppInputSource -> TestTree
requestDeleteAppInputSource =
  req
    "DeleteAppInputSource"
    "fixture/DeleteAppInputSource.yaml"

requestDeleteAppVersionAppComponent :: DeleteAppVersionAppComponent -> TestTree
requestDeleteAppVersionAppComponent =
  req
    "DeleteAppVersionAppComponent"
    "fixture/DeleteAppVersionAppComponent.yaml"

requestDeleteAppVersionResource :: DeleteAppVersionResource -> TestTree
requestDeleteAppVersionResource =
  req
    "DeleteAppVersionResource"
    "fixture/DeleteAppVersionResource.yaml"

requestDeleteRecommendationTemplate :: DeleteRecommendationTemplate -> TestTree
requestDeleteRecommendationTemplate =
  req
    "DeleteRecommendationTemplate"
    "fixture/DeleteRecommendationTemplate.yaml"

requestDeleteResiliencyPolicy :: DeleteResiliencyPolicy -> TestTree
requestDeleteResiliencyPolicy =
  req
    "DeleteResiliencyPolicy"
    "fixture/DeleteResiliencyPolicy.yaml"

requestDescribeApp :: DescribeApp -> TestTree
requestDescribeApp =
  req
    "DescribeApp"
    "fixture/DescribeApp.yaml"

requestDescribeAppAssessment :: DescribeAppAssessment -> TestTree
requestDescribeAppAssessment =
  req
    "DescribeAppAssessment"
    "fixture/DescribeAppAssessment.yaml"

requestDescribeAppVersion :: DescribeAppVersion -> TestTree
requestDescribeAppVersion =
  req
    "DescribeAppVersion"
    "fixture/DescribeAppVersion.yaml"

requestDescribeAppVersionAppComponent :: DescribeAppVersionAppComponent -> TestTree
requestDescribeAppVersionAppComponent =
  req
    "DescribeAppVersionAppComponent"
    "fixture/DescribeAppVersionAppComponent.yaml"

requestDescribeAppVersionResource :: DescribeAppVersionResource -> TestTree
requestDescribeAppVersionResource =
  req
    "DescribeAppVersionResource"
    "fixture/DescribeAppVersionResource.yaml"

requestDescribeAppVersionResourcesResolutionStatus :: DescribeAppVersionResourcesResolutionStatus -> TestTree
requestDescribeAppVersionResourcesResolutionStatus =
  req
    "DescribeAppVersionResourcesResolutionStatus"
    "fixture/DescribeAppVersionResourcesResolutionStatus.yaml"

requestDescribeAppVersionTemplate :: DescribeAppVersionTemplate -> TestTree
requestDescribeAppVersionTemplate =
  req
    "DescribeAppVersionTemplate"
    "fixture/DescribeAppVersionTemplate.yaml"

requestDescribeDraftAppVersionResourcesImportStatus :: DescribeDraftAppVersionResourcesImportStatus -> TestTree
requestDescribeDraftAppVersionResourcesImportStatus =
  req
    "DescribeDraftAppVersionResourcesImportStatus"
    "fixture/DescribeDraftAppVersionResourcesImportStatus.yaml"

requestDescribeResiliencyPolicy :: DescribeResiliencyPolicy -> TestTree
requestDescribeResiliencyPolicy =
  req
    "DescribeResiliencyPolicy"
    "fixture/DescribeResiliencyPolicy.yaml"

requestImportResourcesToDraftAppVersion :: ImportResourcesToDraftAppVersion -> TestTree
requestImportResourcesToDraftAppVersion =
  req
    "ImportResourcesToDraftAppVersion"
    "fixture/ImportResourcesToDraftAppVersion.yaml"

requestListAlarmRecommendations :: ListAlarmRecommendations -> TestTree
requestListAlarmRecommendations =
  req
    "ListAlarmRecommendations"
    "fixture/ListAlarmRecommendations.yaml"

requestListAppAssessments :: ListAppAssessments -> TestTree
requestListAppAssessments =
  req
    "ListAppAssessments"
    "fixture/ListAppAssessments.yaml"

requestListAppComponentCompliances :: ListAppComponentCompliances -> TestTree
requestListAppComponentCompliances =
  req
    "ListAppComponentCompliances"
    "fixture/ListAppComponentCompliances.yaml"

requestListAppComponentRecommendations :: ListAppComponentRecommendations -> TestTree
requestListAppComponentRecommendations =
  req
    "ListAppComponentRecommendations"
    "fixture/ListAppComponentRecommendations.yaml"

requestListAppInputSources :: ListAppInputSources -> TestTree
requestListAppInputSources =
  req
    "ListAppInputSources"
    "fixture/ListAppInputSources.yaml"

requestListAppVersionAppComponents :: ListAppVersionAppComponents -> TestTree
requestListAppVersionAppComponents =
  req
    "ListAppVersionAppComponents"
    "fixture/ListAppVersionAppComponents.yaml"

requestListAppVersionResourceMappings :: ListAppVersionResourceMappings -> TestTree
requestListAppVersionResourceMappings =
  req
    "ListAppVersionResourceMappings"
    "fixture/ListAppVersionResourceMappings.yaml"

requestListAppVersionResources :: ListAppVersionResources -> TestTree
requestListAppVersionResources =
  req
    "ListAppVersionResources"
    "fixture/ListAppVersionResources.yaml"

requestListAppVersions :: ListAppVersions -> TestTree
requestListAppVersions =
  req
    "ListAppVersions"
    "fixture/ListAppVersions.yaml"

requestListApps :: ListApps -> TestTree
requestListApps =
  req
    "ListApps"
    "fixture/ListApps.yaml"

requestListRecommendationTemplates :: ListRecommendationTemplates -> TestTree
requestListRecommendationTemplates =
  req
    "ListRecommendationTemplates"
    "fixture/ListRecommendationTemplates.yaml"

requestListResiliencyPolicies :: ListResiliencyPolicies -> TestTree
requestListResiliencyPolicies =
  req
    "ListResiliencyPolicies"
    "fixture/ListResiliencyPolicies.yaml"

requestListSopRecommendations :: ListSopRecommendations -> TestTree
requestListSopRecommendations =
  req
    "ListSopRecommendations"
    "fixture/ListSopRecommendations.yaml"

requestListSuggestedResiliencyPolicies :: ListSuggestedResiliencyPolicies -> TestTree
requestListSuggestedResiliencyPolicies =
  req
    "ListSuggestedResiliencyPolicies"
    "fixture/ListSuggestedResiliencyPolicies.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTestRecommendations :: ListTestRecommendations -> TestTree
requestListTestRecommendations =
  req
    "ListTestRecommendations"
    "fixture/ListTestRecommendations.yaml"

requestListUnsupportedAppVersionResources :: ListUnsupportedAppVersionResources -> TestTree
requestListUnsupportedAppVersionResources =
  req
    "ListUnsupportedAppVersionResources"
    "fixture/ListUnsupportedAppVersionResources.yaml"

requestPublishAppVersion :: PublishAppVersion -> TestTree
requestPublishAppVersion =
  req
    "PublishAppVersion"
    "fixture/PublishAppVersion.yaml"

requestPutDraftAppVersionTemplate :: PutDraftAppVersionTemplate -> TestTree
requestPutDraftAppVersionTemplate =
  req
    "PutDraftAppVersionTemplate"
    "fixture/PutDraftAppVersionTemplate.yaml"

requestRemoveDraftAppVersionResourceMappings :: RemoveDraftAppVersionResourceMappings -> TestTree
requestRemoveDraftAppVersionResourceMappings =
  req
    "RemoveDraftAppVersionResourceMappings"
    "fixture/RemoveDraftAppVersionResourceMappings.yaml"

requestResolveAppVersionResources :: ResolveAppVersionResources -> TestTree
requestResolveAppVersionResources =
  req
    "ResolveAppVersionResources"
    "fixture/ResolveAppVersionResources.yaml"

requestStartAppAssessment :: StartAppAssessment -> TestTree
requestStartAppAssessment =
  req
    "StartAppAssessment"
    "fixture/StartAppAssessment.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateApp :: UpdateApp -> TestTree
requestUpdateApp =
  req
    "UpdateApp"
    "fixture/UpdateApp.yaml"

requestUpdateAppVersion :: UpdateAppVersion -> TestTree
requestUpdateAppVersion =
  req
    "UpdateAppVersion"
    "fixture/UpdateAppVersion.yaml"

requestUpdateAppVersionAppComponent :: UpdateAppVersionAppComponent -> TestTree
requestUpdateAppVersionAppComponent =
  req
    "UpdateAppVersionAppComponent"
    "fixture/UpdateAppVersionAppComponent.yaml"

requestUpdateAppVersionResource :: UpdateAppVersionResource -> TestTree
requestUpdateAppVersionResource =
  req
    "UpdateAppVersionResource"
    "fixture/UpdateAppVersionResource.yaml"

requestUpdateResiliencyPolicy :: UpdateResiliencyPolicy -> TestTree
requestUpdateResiliencyPolicy =
  req
    "UpdateResiliencyPolicy"
    "fixture/UpdateResiliencyPolicy.yaml"

-- Responses

responseAddDraftAppVersionResourceMappings :: AddDraftAppVersionResourceMappingsResponse -> TestTree
responseAddDraftAppVersionResourceMappings =
  res
    "AddDraftAppVersionResourceMappingsResponse"
    "fixture/AddDraftAppVersionResourceMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddDraftAppVersionResourceMappings)

responseCreateApp :: CreateAppResponse -> TestTree
responseCreateApp =
  res
    "CreateAppResponse"
    "fixture/CreateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApp)

responseCreateAppVersionAppComponent :: CreateAppVersionAppComponentResponse -> TestTree
responseCreateAppVersionAppComponent =
  res
    "CreateAppVersionAppComponentResponse"
    "fixture/CreateAppVersionAppComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppVersionAppComponent)

responseCreateAppVersionResource :: CreateAppVersionResourceResponse -> TestTree
responseCreateAppVersionResource =
  res
    "CreateAppVersionResourceResponse"
    "fixture/CreateAppVersionResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAppVersionResource)

responseCreateRecommendationTemplate :: CreateRecommendationTemplateResponse -> TestTree
responseCreateRecommendationTemplate =
  res
    "CreateRecommendationTemplateResponse"
    "fixture/CreateRecommendationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecommendationTemplate)

responseCreateResiliencyPolicy :: CreateResiliencyPolicyResponse -> TestTree
responseCreateResiliencyPolicy =
  res
    "CreateResiliencyPolicyResponse"
    "fixture/CreateResiliencyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResiliencyPolicy)

responseDeleteApp :: DeleteAppResponse -> TestTree
responseDeleteApp =
  res
    "DeleteAppResponse"
    "fixture/DeleteAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApp)

responseDeleteAppAssessment :: DeleteAppAssessmentResponse -> TestTree
responseDeleteAppAssessment =
  res
    "DeleteAppAssessmentResponse"
    "fixture/DeleteAppAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppAssessment)

responseDeleteAppInputSource :: DeleteAppInputSourceResponse -> TestTree
responseDeleteAppInputSource =
  res
    "DeleteAppInputSourceResponse"
    "fixture/DeleteAppInputSourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppInputSource)

responseDeleteAppVersionAppComponent :: DeleteAppVersionAppComponentResponse -> TestTree
responseDeleteAppVersionAppComponent =
  res
    "DeleteAppVersionAppComponentResponse"
    "fixture/DeleteAppVersionAppComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppVersionAppComponent)

responseDeleteAppVersionResource :: DeleteAppVersionResourceResponse -> TestTree
responseDeleteAppVersionResource =
  res
    "DeleteAppVersionResourceResponse"
    "fixture/DeleteAppVersionResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAppVersionResource)

responseDeleteRecommendationTemplate :: DeleteRecommendationTemplateResponse -> TestTree
responseDeleteRecommendationTemplate =
  res
    "DeleteRecommendationTemplateResponse"
    "fixture/DeleteRecommendationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecommendationTemplate)

responseDeleteResiliencyPolicy :: DeleteResiliencyPolicyResponse -> TestTree
responseDeleteResiliencyPolicy =
  res
    "DeleteResiliencyPolicyResponse"
    "fixture/DeleteResiliencyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResiliencyPolicy)

responseDescribeApp :: DescribeAppResponse -> TestTree
responseDescribeApp =
  res
    "DescribeAppResponse"
    "fixture/DescribeAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApp)

responseDescribeAppAssessment :: DescribeAppAssessmentResponse -> TestTree
responseDescribeAppAssessment =
  res
    "DescribeAppAssessmentResponse"
    "fixture/DescribeAppAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppAssessment)

responseDescribeAppVersion :: DescribeAppVersionResponse -> TestTree
responseDescribeAppVersion =
  res
    "DescribeAppVersionResponse"
    "fixture/DescribeAppVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppVersion)

responseDescribeAppVersionAppComponent :: DescribeAppVersionAppComponentResponse -> TestTree
responseDescribeAppVersionAppComponent =
  res
    "DescribeAppVersionAppComponentResponse"
    "fixture/DescribeAppVersionAppComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppVersionAppComponent)

responseDescribeAppVersionResource :: DescribeAppVersionResourceResponse -> TestTree
responseDescribeAppVersionResource =
  res
    "DescribeAppVersionResourceResponse"
    "fixture/DescribeAppVersionResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppVersionResource)

responseDescribeAppVersionResourcesResolutionStatus :: DescribeAppVersionResourcesResolutionStatusResponse -> TestTree
responseDescribeAppVersionResourcesResolutionStatus =
  res
    "DescribeAppVersionResourcesResolutionStatusResponse"
    "fixture/DescribeAppVersionResourcesResolutionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppVersionResourcesResolutionStatus)

responseDescribeAppVersionTemplate :: DescribeAppVersionTemplateResponse -> TestTree
responseDescribeAppVersionTemplate =
  res
    "DescribeAppVersionTemplateResponse"
    "fixture/DescribeAppVersionTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAppVersionTemplate)

responseDescribeDraftAppVersionResourcesImportStatus :: DescribeDraftAppVersionResourcesImportStatusResponse -> TestTree
responseDescribeDraftAppVersionResourcesImportStatus =
  res
    "DescribeDraftAppVersionResourcesImportStatusResponse"
    "fixture/DescribeDraftAppVersionResourcesImportStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDraftAppVersionResourcesImportStatus)

responseDescribeResiliencyPolicy :: DescribeResiliencyPolicyResponse -> TestTree
responseDescribeResiliencyPolicy =
  res
    "DescribeResiliencyPolicyResponse"
    "fixture/DescribeResiliencyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResiliencyPolicy)

responseImportResourcesToDraftAppVersion :: ImportResourcesToDraftAppVersionResponse -> TestTree
responseImportResourcesToDraftAppVersion =
  res
    "ImportResourcesToDraftAppVersionResponse"
    "fixture/ImportResourcesToDraftAppVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportResourcesToDraftAppVersion)

responseListAlarmRecommendations :: ListAlarmRecommendationsResponse -> TestTree
responseListAlarmRecommendations =
  res
    "ListAlarmRecommendationsResponse"
    "fixture/ListAlarmRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAlarmRecommendations)

responseListAppAssessments :: ListAppAssessmentsResponse -> TestTree
responseListAppAssessments =
  res
    "ListAppAssessmentsResponse"
    "fixture/ListAppAssessmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppAssessments)

responseListAppComponentCompliances :: ListAppComponentCompliancesResponse -> TestTree
responseListAppComponentCompliances =
  res
    "ListAppComponentCompliancesResponse"
    "fixture/ListAppComponentCompliancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppComponentCompliances)

responseListAppComponentRecommendations :: ListAppComponentRecommendationsResponse -> TestTree
responseListAppComponentRecommendations =
  res
    "ListAppComponentRecommendationsResponse"
    "fixture/ListAppComponentRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppComponentRecommendations)

responseListAppInputSources :: ListAppInputSourcesResponse -> TestTree
responseListAppInputSources =
  res
    "ListAppInputSourcesResponse"
    "fixture/ListAppInputSourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppInputSources)

responseListAppVersionAppComponents :: ListAppVersionAppComponentsResponse -> TestTree
responseListAppVersionAppComponents =
  res
    "ListAppVersionAppComponentsResponse"
    "fixture/ListAppVersionAppComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppVersionAppComponents)

responseListAppVersionResourceMappings :: ListAppVersionResourceMappingsResponse -> TestTree
responseListAppVersionResourceMappings =
  res
    "ListAppVersionResourceMappingsResponse"
    "fixture/ListAppVersionResourceMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppVersionResourceMappings)

responseListAppVersionResources :: ListAppVersionResourcesResponse -> TestTree
responseListAppVersionResources =
  res
    "ListAppVersionResourcesResponse"
    "fixture/ListAppVersionResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppVersionResources)

responseListAppVersions :: ListAppVersionsResponse -> TestTree
responseListAppVersions =
  res
    "ListAppVersionsResponse"
    "fixture/ListAppVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAppVersions)

responseListApps :: ListAppsResponse -> TestTree
responseListApps =
  res
    "ListAppsResponse"
    "fixture/ListAppsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApps)

responseListRecommendationTemplates :: ListRecommendationTemplatesResponse -> TestTree
responseListRecommendationTemplates =
  res
    "ListRecommendationTemplatesResponse"
    "fixture/ListRecommendationTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendationTemplates)

responseListResiliencyPolicies :: ListResiliencyPoliciesResponse -> TestTree
responseListResiliencyPolicies =
  res
    "ListResiliencyPoliciesResponse"
    "fixture/ListResiliencyPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResiliencyPolicies)

responseListSopRecommendations :: ListSopRecommendationsResponse -> TestTree
responseListSopRecommendations =
  res
    "ListSopRecommendationsResponse"
    "fixture/ListSopRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSopRecommendations)

responseListSuggestedResiliencyPolicies :: ListSuggestedResiliencyPoliciesResponse -> TestTree
responseListSuggestedResiliencyPolicies =
  res
    "ListSuggestedResiliencyPoliciesResponse"
    "fixture/ListSuggestedResiliencyPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSuggestedResiliencyPolicies)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTestRecommendations :: ListTestRecommendationsResponse -> TestTree
responseListTestRecommendations =
  res
    "ListTestRecommendationsResponse"
    "fixture/ListTestRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTestRecommendations)

responseListUnsupportedAppVersionResources :: ListUnsupportedAppVersionResourcesResponse -> TestTree
responseListUnsupportedAppVersionResources =
  res
    "ListUnsupportedAppVersionResourcesResponse"
    "fixture/ListUnsupportedAppVersionResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUnsupportedAppVersionResources)

responsePublishAppVersion :: PublishAppVersionResponse -> TestTree
responsePublishAppVersion =
  res
    "PublishAppVersionResponse"
    "fixture/PublishAppVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishAppVersion)

responsePutDraftAppVersionTemplate :: PutDraftAppVersionTemplateResponse -> TestTree
responsePutDraftAppVersionTemplate =
  res
    "PutDraftAppVersionTemplateResponse"
    "fixture/PutDraftAppVersionTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDraftAppVersionTemplate)

responseRemoveDraftAppVersionResourceMappings :: RemoveDraftAppVersionResourceMappingsResponse -> TestTree
responseRemoveDraftAppVersionResourceMappings =
  res
    "RemoveDraftAppVersionResourceMappingsResponse"
    "fixture/RemoveDraftAppVersionResourceMappingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveDraftAppVersionResourceMappings)

responseResolveAppVersionResources :: ResolveAppVersionResourcesResponse -> TestTree
responseResolveAppVersionResources =
  res
    "ResolveAppVersionResourcesResponse"
    "fixture/ResolveAppVersionResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResolveAppVersionResources)

responseStartAppAssessment :: StartAppAssessmentResponse -> TestTree
responseStartAppAssessment =
  res
    "StartAppAssessmentResponse"
    "fixture/StartAppAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAppAssessment)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateApp :: UpdateAppResponse -> TestTree
responseUpdateApp =
  res
    "UpdateAppResponse"
    "fixture/UpdateAppResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApp)

responseUpdateAppVersion :: UpdateAppVersionResponse -> TestTree
responseUpdateAppVersion =
  res
    "UpdateAppVersionResponse"
    "fixture/UpdateAppVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppVersion)

responseUpdateAppVersionAppComponent :: UpdateAppVersionAppComponentResponse -> TestTree
responseUpdateAppVersionAppComponent =
  res
    "UpdateAppVersionAppComponentResponse"
    "fixture/UpdateAppVersionAppComponentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppVersionAppComponent)

responseUpdateAppVersionResource :: UpdateAppVersionResourceResponse -> TestTree
responseUpdateAppVersionResource =
  res
    "UpdateAppVersionResourceResponse"
    "fixture/UpdateAppVersionResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAppVersionResource)

responseUpdateResiliencyPolicy :: UpdateResiliencyPolicyResponse -> TestTree
responseUpdateResiliencyPolicy =
  res
    "UpdateResiliencyPolicyResponse"
    "fixture/UpdateResiliencyPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResiliencyPolicy)
