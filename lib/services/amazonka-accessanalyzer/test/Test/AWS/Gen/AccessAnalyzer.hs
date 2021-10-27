{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AccessAnalyzer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.AccessAnalyzer where

import Data.Proxy
import Network.AWS.AccessAnalyzer
import Test.AWS.AccessAnalyzer.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListFindings $
--             newListFindings
--
--         , requestGetAnalyzedResource $
--             newGetAnalyzedResource
--
--         , requestListPolicyGenerations $
--             newListPolicyGenerations
--
--         , requestListAccessPreviews $
--             newListAccessPreviews
--
--         , requestCreateAccessPreview $
--             newCreateAccessPreview
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartResourceScan $
--             newStartResourceScan
--
--         , requestDeleteArchiveRule $
--             newDeleteArchiveRule
--
--         , requestUpdateArchiveRule $
--             newUpdateArchiveRule
--
--         , requestGetAccessPreview $
--             newGetAccessPreview
--
--         , requestListAnalyzedResources $
--             newListAnalyzedResources
--
--         , requestStartPolicyGeneration $
--             newStartPolicyGeneration
--
--         , requestValidatePolicy $
--             newValidatePolicy
--
--         , requestDeleteAnalyzer $
--             newDeleteAnalyzer
--
--         , requestUpdateFindings $
--             newUpdateFindings
--
--         , requestListAnalyzers $
--             newListAnalyzers
--
--         , requestListAccessPreviewFindings $
--             newListAccessPreviewFindings
--
--         , requestGetArchiveRule $
--             newGetArchiveRule
--
--         , requestCreateAnalyzer $
--             newCreateAnalyzer
--
--         , requestListArchiveRules $
--             newListArchiveRules
--
--         , requestCreateArchiveRule $
--             newCreateArchiveRule
--
--         , requestCancelPolicyGeneration $
--             newCancelPolicyGeneration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestApplyArchiveRule $
--             newApplyArchiveRule
--
--         , requestGetAnalyzer $
--             newGetAnalyzer
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetFinding $
--             newGetFinding
--
--         , requestGetGeneratedPolicy $
--             newGetGeneratedPolicy
--
--           ]

--     , testGroup "response"
--         [ responseListFindings $
--             newListFindingsResponse
--
--         , responseGetAnalyzedResource $
--             newGetAnalyzedResourceResponse
--
--         , responseListPolicyGenerations $
--             newListPolicyGenerationsResponse
--
--         , responseListAccessPreviews $
--             newListAccessPreviewsResponse
--
--         , responseCreateAccessPreview $
--             newCreateAccessPreviewResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartResourceScan $
--             newStartResourceScanResponse
--
--         , responseDeleteArchiveRule $
--             newDeleteArchiveRuleResponse
--
--         , responseUpdateArchiveRule $
--             newUpdateArchiveRuleResponse
--
--         , responseGetAccessPreview $
--             newGetAccessPreviewResponse
--
--         , responseListAnalyzedResources $
--             newListAnalyzedResourcesResponse
--
--         , responseStartPolicyGeneration $
--             newStartPolicyGenerationResponse
--
--         , responseValidatePolicy $
--             newValidatePolicyResponse
--
--         , responseDeleteAnalyzer $
--             newDeleteAnalyzerResponse
--
--         , responseUpdateFindings $
--             newUpdateFindingsResponse
--
--         , responseListAnalyzers $
--             newListAnalyzersResponse
--
--         , responseListAccessPreviewFindings $
--             newListAccessPreviewFindingsResponse
--
--         , responseGetArchiveRule $
--             newGetArchiveRuleResponse
--
--         , responseCreateAnalyzer $
--             newCreateAnalyzerResponse
--
--         , responseListArchiveRules $
--             newListArchiveRulesResponse
--
--         , responseCreateArchiveRule $
--             newCreateArchiveRuleResponse
--
--         , responseCancelPolicyGeneration $
--             newCancelPolicyGenerationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseApplyArchiveRule $
--             newApplyArchiveRuleResponse
--
--         , responseGetAnalyzer $
--             newGetAnalyzerResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetFinding $
--             newGetFindingResponse
--
--         , responseGetGeneratedPolicy $
--             newGetGeneratedPolicyResponse
--
--           ]
--     ]

-- Requests

requestListFindings :: ListFindings -> TestTree
requestListFindings =
  req
    "ListFindings"
    "fixture/ListFindings.yaml"

requestGetAnalyzedResource :: GetAnalyzedResource -> TestTree
requestGetAnalyzedResource =
  req
    "GetAnalyzedResource"
    "fixture/GetAnalyzedResource.yaml"

requestListPolicyGenerations :: ListPolicyGenerations -> TestTree
requestListPolicyGenerations =
  req
    "ListPolicyGenerations"
    "fixture/ListPolicyGenerations.yaml"

requestListAccessPreviews :: ListAccessPreviews -> TestTree
requestListAccessPreviews =
  req
    "ListAccessPreviews"
    "fixture/ListAccessPreviews.yaml"

requestCreateAccessPreview :: CreateAccessPreview -> TestTree
requestCreateAccessPreview =
  req
    "CreateAccessPreview"
    "fixture/CreateAccessPreview.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartResourceScan :: StartResourceScan -> TestTree
requestStartResourceScan =
  req
    "StartResourceScan"
    "fixture/StartResourceScan.yaml"

requestDeleteArchiveRule :: DeleteArchiveRule -> TestTree
requestDeleteArchiveRule =
  req
    "DeleteArchiveRule"
    "fixture/DeleteArchiveRule.yaml"

requestUpdateArchiveRule :: UpdateArchiveRule -> TestTree
requestUpdateArchiveRule =
  req
    "UpdateArchiveRule"
    "fixture/UpdateArchiveRule.yaml"

requestGetAccessPreview :: GetAccessPreview -> TestTree
requestGetAccessPreview =
  req
    "GetAccessPreview"
    "fixture/GetAccessPreview.yaml"

requestListAnalyzedResources :: ListAnalyzedResources -> TestTree
requestListAnalyzedResources =
  req
    "ListAnalyzedResources"
    "fixture/ListAnalyzedResources.yaml"

requestStartPolicyGeneration :: StartPolicyGeneration -> TestTree
requestStartPolicyGeneration =
  req
    "StartPolicyGeneration"
    "fixture/StartPolicyGeneration.yaml"

requestValidatePolicy :: ValidatePolicy -> TestTree
requestValidatePolicy =
  req
    "ValidatePolicy"
    "fixture/ValidatePolicy.yaml"

requestDeleteAnalyzer :: DeleteAnalyzer -> TestTree
requestDeleteAnalyzer =
  req
    "DeleteAnalyzer"
    "fixture/DeleteAnalyzer.yaml"

requestUpdateFindings :: UpdateFindings -> TestTree
requestUpdateFindings =
  req
    "UpdateFindings"
    "fixture/UpdateFindings.yaml"

requestListAnalyzers :: ListAnalyzers -> TestTree
requestListAnalyzers =
  req
    "ListAnalyzers"
    "fixture/ListAnalyzers.yaml"

requestListAccessPreviewFindings :: ListAccessPreviewFindings -> TestTree
requestListAccessPreviewFindings =
  req
    "ListAccessPreviewFindings"
    "fixture/ListAccessPreviewFindings.yaml"

requestGetArchiveRule :: GetArchiveRule -> TestTree
requestGetArchiveRule =
  req
    "GetArchiveRule"
    "fixture/GetArchiveRule.yaml"

requestCreateAnalyzer :: CreateAnalyzer -> TestTree
requestCreateAnalyzer =
  req
    "CreateAnalyzer"
    "fixture/CreateAnalyzer.yaml"

requestListArchiveRules :: ListArchiveRules -> TestTree
requestListArchiveRules =
  req
    "ListArchiveRules"
    "fixture/ListArchiveRules.yaml"

requestCreateArchiveRule :: CreateArchiveRule -> TestTree
requestCreateArchiveRule =
  req
    "CreateArchiveRule"
    "fixture/CreateArchiveRule.yaml"

requestCancelPolicyGeneration :: CancelPolicyGeneration -> TestTree
requestCancelPolicyGeneration =
  req
    "CancelPolicyGeneration"
    "fixture/CancelPolicyGeneration.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestApplyArchiveRule :: ApplyArchiveRule -> TestTree
requestApplyArchiveRule =
  req
    "ApplyArchiveRule"
    "fixture/ApplyArchiveRule.yaml"

requestGetAnalyzer :: GetAnalyzer -> TestTree
requestGetAnalyzer =
  req
    "GetAnalyzer"
    "fixture/GetAnalyzer.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

-- Responses

responseListFindings :: ListFindingsResponse -> TestTree
responseListFindings =
  res
    "ListFindingsResponse"
    "fixture/ListFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFindings)

responseGetAnalyzedResource :: GetAnalyzedResourceResponse -> TestTree
responseGetAnalyzedResource =
  res
    "GetAnalyzedResourceResponse"
    "fixture/GetAnalyzedResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnalyzedResource)

responseListPolicyGenerations :: ListPolicyGenerationsResponse -> TestTree
responseListPolicyGenerations =
  res
    "ListPolicyGenerationsResponse"
    "fixture/ListPolicyGenerationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicyGenerations)

responseListAccessPreviews :: ListAccessPreviewsResponse -> TestTree
responseListAccessPreviews =
  res
    "ListAccessPreviewsResponse"
    "fixture/ListAccessPreviewsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessPreviews)

responseCreateAccessPreview :: CreateAccessPreviewResponse -> TestTree
responseCreateAccessPreview =
  res
    "CreateAccessPreviewResponse"
    "fixture/CreateAccessPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccessPreview)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseStartResourceScan :: StartResourceScanResponse -> TestTree
responseStartResourceScan =
  res
    "StartResourceScanResponse"
    "fixture/StartResourceScanResponse.proto"
    defaultService
    (Proxy :: Proxy StartResourceScan)

responseDeleteArchiveRule :: DeleteArchiveRuleResponse -> TestTree
responseDeleteArchiveRule =
  res
    "DeleteArchiveRuleResponse"
    "fixture/DeleteArchiveRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteArchiveRule)

responseUpdateArchiveRule :: UpdateArchiveRuleResponse -> TestTree
responseUpdateArchiveRule =
  res
    "UpdateArchiveRuleResponse"
    "fixture/UpdateArchiveRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateArchiveRule)

responseGetAccessPreview :: GetAccessPreviewResponse -> TestTree
responseGetAccessPreview =
  res
    "GetAccessPreviewResponse"
    "fixture/GetAccessPreviewResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessPreview)

responseListAnalyzedResources :: ListAnalyzedResourcesResponse -> TestTree
responseListAnalyzedResources =
  res
    "ListAnalyzedResourcesResponse"
    "fixture/ListAnalyzedResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAnalyzedResources)

responseStartPolicyGeneration :: StartPolicyGenerationResponse -> TestTree
responseStartPolicyGeneration =
  res
    "StartPolicyGenerationResponse"
    "fixture/StartPolicyGenerationResponse.proto"
    defaultService
    (Proxy :: Proxy StartPolicyGeneration)

responseValidatePolicy :: ValidatePolicyResponse -> TestTree
responseValidatePolicy =
  res
    "ValidatePolicyResponse"
    "fixture/ValidatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ValidatePolicy)

responseDeleteAnalyzer :: DeleteAnalyzerResponse -> TestTree
responseDeleteAnalyzer =
  res
    "DeleteAnalyzerResponse"
    "fixture/DeleteAnalyzerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAnalyzer)

responseUpdateFindings :: UpdateFindingsResponse -> TestTree
responseUpdateFindings =
  res
    "UpdateFindingsResponse"
    "fixture/UpdateFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFindings)

responseListAnalyzers :: ListAnalyzersResponse -> TestTree
responseListAnalyzers =
  res
    "ListAnalyzersResponse"
    "fixture/ListAnalyzersResponse.proto"
    defaultService
    (Proxy :: Proxy ListAnalyzers)

responseListAccessPreviewFindings :: ListAccessPreviewFindingsResponse -> TestTree
responseListAccessPreviewFindings =
  res
    "ListAccessPreviewFindingsResponse"
    "fixture/ListAccessPreviewFindingsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessPreviewFindings)

responseGetArchiveRule :: GetArchiveRuleResponse -> TestTree
responseGetArchiveRule =
  res
    "GetArchiveRuleResponse"
    "fixture/GetArchiveRuleResponse.proto"
    defaultService
    (Proxy :: Proxy GetArchiveRule)

responseCreateAnalyzer :: CreateAnalyzerResponse -> TestTree
responseCreateAnalyzer =
  res
    "CreateAnalyzerResponse"
    "fixture/CreateAnalyzerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAnalyzer)

responseListArchiveRules :: ListArchiveRulesResponse -> TestTree
responseListArchiveRules =
  res
    "ListArchiveRulesResponse"
    "fixture/ListArchiveRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListArchiveRules)

responseCreateArchiveRule :: CreateArchiveRuleResponse -> TestTree
responseCreateArchiveRule =
  res
    "CreateArchiveRuleResponse"
    "fixture/CreateArchiveRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateArchiveRule)

responseCancelPolicyGeneration :: CancelPolicyGenerationResponse -> TestTree
responseCancelPolicyGeneration =
  res
    "CancelPolicyGenerationResponse"
    "fixture/CancelPolicyGenerationResponse.proto"
    defaultService
    (Proxy :: Proxy CancelPolicyGeneration)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseApplyArchiveRule :: ApplyArchiveRuleResponse -> TestTree
responseApplyArchiveRule =
  res
    "ApplyArchiveRuleResponse"
    "fixture/ApplyArchiveRuleResponse.proto"
    defaultService
    (Proxy :: Proxy ApplyArchiveRule)

responseGetAnalyzer :: GetAnalyzerResponse -> TestTree
responseGetAnalyzer =
  res
    "GetAnalyzerResponse"
    "fixture/GetAnalyzerResponse.proto"
    defaultService
    (Proxy :: Proxy GetAnalyzer)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetFinding :: GetFindingResponse -> TestTree
responseGetFinding =
  res
    "GetFindingResponse"
    "fixture/GetFindingResponse.proto"
    defaultService
    (Proxy :: Proxy GetFinding)

responseGetGeneratedPolicy :: GetGeneratedPolicyResponse -> TestTree
responseGetGeneratedPolicy =
  res
    "GetGeneratedPolicyResponse"
    "fixture/GetGeneratedPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetGeneratedPolicy)
