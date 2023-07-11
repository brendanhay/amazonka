{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AccessAnalyzer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AccessAnalyzer where

import Amazonka.AccessAnalyzer
import qualified Data.Proxy as Proxy
import Test.Amazonka.AccessAnalyzer.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestApplyArchiveRule $
--             newApplyArchiveRule
--
--         , requestCancelPolicyGeneration $
--             newCancelPolicyGeneration
--
--         , requestCreateAccessPreview $
--             newCreateAccessPreview
--
--         , requestCreateAnalyzer $
--             newCreateAnalyzer
--
--         , requestCreateArchiveRule $
--             newCreateArchiveRule
--
--         , requestDeleteAnalyzer $
--             newDeleteAnalyzer
--
--         , requestDeleteArchiveRule $
--             newDeleteArchiveRule
--
--         , requestGetAccessPreview $
--             newGetAccessPreview
--
--         , requestGetAnalyzedResource $
--             newGetAnalyzedResource
--
--         , requestGetAnalyzer $
--             newGetAnalyzer
--
--         , requestGetArchiveRule $
--             newGetArchiveRule
--
--         , requestGetFinding $
--             newGetFinding
--
--         , requestGetGeneratedPolicy $
--             newGetGeneratedPolicy
--
--         , requestListAccessPreviewFindings $
--             newListAccessPreviewFindings
--
--         , requestListAccessPreviews $
--             newListAccessPreviews
--
--         , requestListAnalyzedResources $
--             newListAnalyzedResources
--
--         , requestListAnalyzers $
--             newListAnalyzers
--
--         , requestListArchiveRules $
--             newListArchiveRules
--
--         , requestListFindings $
--             newListFindings
--
--         , requestListPolicyGenerations $
--             newListPolicyGenerations
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartPolicyGeneration $
--             newStartPolicyGeneration
--
--         , requestStartResourceScan $
--             newStartResourceScan
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateArchiveRule $
--             newUpdateArchiveRule
--
--         , requestUpdateFindings $
--             newUpdateFindings
--
--         , requestValidatePolicy $
--             newValidatePolicy
--
--           ]

--     , testGroup "response"
--         [ responseApplyArchiveRule $
--             newApplyArchiveRuleResponse
--
--         , responseCancelPolicyGeneration $
--             newCancelPolicyGenerationResponse
--
--         , responseCreateAccessPreview $
--             newCreateAccessPreviewResponse
--
--         , responseCreateAnalyzer $
--             newCreateAnalyzerResponse
--
--         , responseCreateArchiveRule $
--             newCreateArchiveRuleResponse
--
--         , responseDeleteAnalyzer $
--             newDeleteAnalyzerResponse
--
--         , responseDeleteArchiveRule $
--             newDeleteArchiveRuleResponse
--
--         , responseGetAccessPreview $
--             newGetAccessPreviewResponse
--
--         , responseGetAnalyzedResource $
--             newGetAnalyzedResourceResponse
--
--         , responseGetAnalyzer $
--             newGetAnalyzerResponse
--
--         , responseGetArchiveRule $
--             newGetArchiveRuleResponse
--
--         , responseGetFinding $
--             newGetFindingResponse
--
--         , responseGetGeneratedPolicy $
--             newGetGeneratedPolicyResponse
--
--         , responseListAccessPreviewFindings $
--             newListAccessPreviewFindingsResponse
--
--         , responseListAccessPreviews $
--             newListAccessPreviewsResponse
--
--         , responseListAnalyzedResources $
--             newListAnalyzedResourcesResponse
--
--         , responseListAnalyzers $
--             newListAnalyzersResponse
--
--         , responseListArchiveRules $
--             newListArchiveRulesResponse
--
--         , responseListFindings $
--             newListFindingsResponse
--
--         , responseListPolicyGenerations $
--             newListPolicyGenerationsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartPolicyGeneration $
--             newStartPolicyGenerationResponse
--
--         , responseStartResourceScan $
--             newStartResourceScanResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateArchiveRule $
--             newUpdateArchiveRuleResponse
--
--         , responseUpdateFindings $
--             newUpdateFindingsResponse
--
--         , responseValidatePolicy $
--             newValidatePolicyResponse
--
--           ]
--     ]

-- Requests

requestApplyArchiveRule :: ApplyArchiveRule -> TestTree
requestApplyArchiveRule =
  req
    "ApplyArchiveRule"
    "fixture/ApplyArchiveRule.yaml"

requestCancelPolicyGeneration :: CancelPolicyGeneration -> TestTree
requestCancelPolicyGeneration =
  req
    "CancelPolicyGeneration"
    "fixture/CancelPolicyGeneration.yaml"

requestCreateAccessPreview :: CreateAccessPreview -> TestTree
requestCreateAccessPreview =
  req
    "CreateAccessPreview"
    "fixture/CreateAccessPreview.yaml"

requestCreateAnalyzer :: CreateAnalyzer -> TestTree
requestCreateAnalyzer =
  req
    "CreateAnalyzer"
    "fixture/CreateAnalyzer.yaml"

requestCreateArchiveRule :: CreateArchiveRule -> TestTree
requestCreateArchiveRule =
  req
    "CreateArchiveRule"
    "fixture/CreateArchiveRule.yaml"

requestDeleteAnalyzer :: DeleteAnalyzer -> TestTree
requestDeleteAnalyzer =
  req
    "DeleteAnalyzer"
    "fixture/DeleteAnalyzer.yaml"

requestDeleteArchiveRule :: DeleteArchiveRule -> TestTree
requestDeleteArchiveRule =
  req
    "DeleteArchiveRule"
    "fixture/DeleteArchiveRule.yaml"

requestGetAccessPreview :: GetAccessPreview -> TestTree
requestGetAccessPreview =
  req
    "GetAccessPreview"
    "fixture/GetAccessPreview.yaml"

requestGetAnalyzedResource :: GetAnalyzedResource -> TestTree
requestGetAnalyzedResource =
  req
    "GetAnalyzedResource"
    "fixture/GetAnalyzedResource.yaml"

requestGetAnalyzer :: GetAnalyzer -> TestTree
requestGetAnalyzer =
  req
    "GetAnalyzer"
    "fixture/GetAnalyzer.yaml"

requestGetArchiveRule :: GetArchiveRule -> TestTree
requestGetArchiveRule =
  req
    "GetArchiveRule"
    "fixture/GetArchiveRule.yaml"

requestGetFinding :: GetFinding -> TestTree
requestGetFinding =
  req
    "GetFinding"
    "fixture/GetFinding.yaml"

requestGetGeneratedPolicy :: GetGeneratedPolicy -> TestTree
requestGetGeneratedPolicy =
  req
    "GetGeneratedPolicy"
    "fixture/GetGeneratedPolicy.yaml"

requestListAccessPreviewFindings :: ListAccessPreviewFindings -> TestTree
requestListAccessPreviewFindings =
  req
    "ListAccessPreviewFindings"
    "fixture/ListAccessPreviewFindings.yaml"

requestListAccessPreviews :: ListAccessPreviews -> TestTree
requestListAccessPreviews =
  req
    "ListAccessPreviews"
    "fixture/ListAccessPreviews.yaml"

requestListAnalyzedResources :: ListAnalyzedResources -> TestTree
requestListAnalyzedResources =
  req
    "ListAnalyzedResources"
    "fixture/ListAnalyzedResources.yaml"

requestListAnalyzers :: ListAnalyzers -> TestTree
requestListAnalyzers =
  req
    "ListAnalyzers"
    "fixture/ListAnalyzers.yaml"

requestListArchiveRules :: ListArchiveRules -> TestTree
requestListArchiveRules =
  req
    "ListArchiveRules"
    "fixture/ListArchiveRules.yaml"

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestListPolicyGenerations :: ListPolicyGenerations -> TestTree
requestListPolicyGenerations =
  req
    "ListPolicyGenerations"
    "fixture/ListPolicyGenerations.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartPolicyGeneration :: StartPolicyGeneration -> TestTree
requestStartPolicyGeneration =
  req
    "StartPolicyGeneration"
    "fixture/StartPolicyGeneration.yaml"

requestStartResourceScan :: StartResourceScan -> TestTree
requestStartResourceScan =
  req
    "StartResourceScan"
    "fixture/StartResourceScan.yaml"

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

requestUpdateArchiveRule :: UpdateArchiveRule -> TestTree
requestUpdateArchiveRule =
  req
    "UpdateArchiveRule"
    "fixture/UpdateArchiveRule.yaml"

requestUpdateFindings :: UpdateFindings -> TestTree
requestUpdateFindings =
  req
    "UpdateFindings"
    "fixture/UpdateFindings.yaml"

requestValidatePolicy :: ValidatePolicy -> TestTree
requestValidatePolicy =
  req
    "ValidatePolicy"
    "fixture/ValidatePolicy.yaml"

-- Responses

responseApplyArchiveRule :: ApplyArchiveRuleResponse -> TestTree
responseApplyArchiveRule =
  res
    "ApplyArchiveRuleResponse"
    "fixture/ApplyArchiveRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplyArchiveRule)

responseCancelPolicyGeneration :: CancelPolicyGenerationResponse -> TestTree
responseCancelPolicyGeneration =
  res
    "CancelPolicyGenerationResponse"
    "fixture/CancelPolicyGenerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelPolicyGeneration)

responseCreateAccessPreview :: CreateAccessPreviewResponse -> TestTree
responseCreateAccessPreview =
  res
    "CreateAccessPreviewResponse"
    "fixture/CreateAccessPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccessPreview)

responseCreateAnalyzer :: CreateAnalyzerResponse -> TestTree
responseCreateAnalyzer =
  res
    "CreateAnalyzerResponse"
    "fixture/CreateAnalyzerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAnalyzer)

responseCreateArchiveRule :: CreateArchiveRuleResponse -> TestTree
responseCreateArchiveRule =
  res
    "CreateArchiveRuleResponse"
    "fixture/CreateArchiveRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateArchiveRule)

responseDeleteAnalyzer :: DeleteAnalyzerResponse -> TestTree
responseDeleteAnalyzer =
  res
    "DeleteAnalyzerResponse"
    "fixture/DeleteAnalyzerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAnalyzer)

responseDeleteArchiveRule :: DeleteArchiveRuleResponse -> TestTree
responseDeleteArchiveRule =
  res
    "DeleteArchiveRuleResponse"
    "fixture/DeleteArchiveRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteArchiveRule)

responseGetAccessPreview :: GetAccessPreviewResponse -> TestTree
responseGetAccessPreview =
  res
    "GetAccessPreviewResponse"
    "fixture/GetAccessPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessPreview)

responseGetAnalyzedResource :: GetAnalyzedResourceResponse -> TestTree
responseGetAnalyzedResource =
  res
    "GetAnalyzedResourceResponse"
    "fixture/GetAnalyzedResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnalyzedResource)

responseGetAnalyzer :: GetAnalyzerResponse -> TestTree
responseGetAnalyzer =
  res
    "GetAnalyzerResponse"
    "fixture/GetAnalyzerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAnalyzer)

responseGetArchiveRule :: GetArchiveRuleResponse -> TestTree
responseGetArchiveRule =
  res
    "GetArchiveRuleResponse"
    "fixture/GetArchiveRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetArchiveRule)

responseGetFinding :: GetFindingResponse -> TestTree
responseGetFinding =
  res
    "GetFindingResponse"
    "fixture/GetFindingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFinding)

responseGetGeneratedPolicy :: GetGeneratedPolicyResponse -> TestTree
responseGetGeneratedPolicy =
  res
    "GetGeneratedPolicyResponse"
    "fixture/GetGeneratedPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeneratedPolicy)

responseListAccessPreviewFindings :: ListAccessPreviewFindingsResponse -> TestTree
responseListAccessPreviewFindings =
  res
    "ListAccessPreviewFindingsResponse"
    "fixture/ListAccessPreviewFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessPreviewFindings)

responseListAccessPreviews :: ListAccessPreviewsResponse -> TestTree
responseListAccessPreviews =
  res
    "ListAccessPreviewsResponse"
    "fixture/ListAccessPreviewsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessPreviews)

responseListAnalyzedResources :: ListAnalyzedResourcesResponse -> TestTree
responseListAnalyzedResources =
  res
    "ListAnalyzedResourcesResponse"
    "fixture/ListAnalyzedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnalyzedResources)

responseListAnalyzers :: ListAnalyzersResponse -> TestTree
responseListAnalyzers =
  res
    "ListAnalyzersResponse"
    "fixture/ListAnalyzersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAnalyzers)

responseListArchiveRules :: ListArchiveRulesResponse -> TestTree
responseListArchiveRules =
  res
    "ListArchiveRulesResponse"
    "fixture/ListArchiveRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListArchiveRules)

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFindings)

responseListPolicyGenerations :: ListPolicyGenerationsResponse -> TestTree
responseListPolicyGenerations =
  res
    "ListPolicyGenerationsResponse"
    "fixture/ListPolicyGenerationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicyGenerations)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartPolicyGeneration :: StartPolicyGenerationResponse -> TestTree
responseStartPolicyGeneration =
  res
    "StartPolicyGenerationResponse"
    "fixture/StartPolicyGenerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPolicyGeneration)

responseStartResourceScan :: StartResourceScanResponse -> TestTree
responseStartResourceScan =
  res
    "StartResourceScanResponse"
    "fixture/StartResourceScanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartResourceScan)

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

responseUpdateArchiveRule :: UpdateArchiveRuleResponse -> TestTree
responseUpdateArchiveRule =
  res
    "UpdateArchiveRuleResponse"
    "fixture/UpdateArchiveRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateArchiveRule)

responseUpdateFindings :: UpdateFindingsResponse -> TestTree
responseUpdateFindings =
  res
    "UpdateFindingsResponse"
    "fixture/UpdateFindingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFindings)

responseValidatePolicy :: ValidatePolicyResponse -> TestTree
responseValidatePolicy =
  res
    "ValidatePolicyResponse"
    "fixture/ValidatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidatePolicy)
