{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ResourceGroupsTagging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ResourceGroupsTagging where

import Amazonka.ResourceGroupsTagging
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ResourceGroupsTagging.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeReportCreation $
--             newDescribeReportCreation
--
--         , requestGetComplianceSummary $
--             newGetComplianceSummary
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetTagKeys $
--             newGetTagKeys
--
--         , requestGetTagValues $
--             newGetTagValues
--
--         , requestStartReportCreation $
--             newStartReportCreation
--
--         , requestTagResources $
--             newTagResources
--
--         , requestUntagResources $
--             newUntagResources
--
--           ]

--     , testGroup "response"
--         [ responseDescribeReportCreation $
--             newDescribeReportCreationResponse
--
--         , responseGetComplianceSummary $
--             newGetComplianceSummaryResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetTagKeys $
--             newGetTagKeysResponse
--
--         , responseGetTagValues $
--             newGetTagValuesResponse
--
--         , responseStartReportCreation $
--             newStartReportCreationResponse
--
--         , responseTagResources $
--             newTagResourcesResponse
--
--         , responseUntagResources $
--             newUntagResourcesResponse
--
--           ]
--     ]

-- Requests

requestDescribeReportCreation :: DescribeReportCreation -> TestTree
requestDescribeReportCreation =
  req
    "DescribeReportCreation"
    "fixture/DescribeReportCreation.yaml"

requestGetComplianceSummary :: GetComplianceSummary -> TestTree
requestGetComplianceSummary =
  req
    "GetComplianceSummary"
    "fixture/GetComplianceSummary.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetTagKeys :: GetTagKeys -> TestTree
requestGetTagKeys =
  req
    "GetTagKeys"
    "fixture/GetTagKeys.yaml"

requestGetTagValues :: GetTagValues -> TestTree
requestGetTagValues =
  req
    "GetTagValues"
    "fixture/GetTagValues.yaml"

requestStartReportCreation :: StartReportCreation -> TestTree
requestStartReportCreation =
  req
    "StartReportCreation"
    "fixture/StartReportCreation.yaml"

requestTagResources :: TagResources -> TestTree
requestTagResources =
  req
    "TagResources"
    "fixture/TagResources.yaml"

requestUntagResources :: UntagResources -> TestTree
requestUntagResources =
  req
    "UntagResources"
    "fixture/UntagResources.yaml"

-- Responses

responseDescribeReportCreation :: DescribeReportCreationResponse -> TestTree
responseDescribeReportCreation =
  res
    "DescribeReportCreationResponse"
    "fixture/DescribeReportCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeReportCreation)

responseGetComplianceSummary :: GetComplianceSummaryResponse -> TestTree
responseGetComplianceSummary =
  res
    "GetComplianceSummaryResponse"
    "fixture/GetComplianceSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetComplianceSummary)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResources)

responseGetTagKeys :: GetTagKeysResponse -> TestTree
responseGetTagKeys =
  res
    "GetTagKeysResponse"
    "fixture/GetTagKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTagKeys)

responseGetTagValues :: GetTagValuesResponse -> TestTree
responseGetTagValues =
  res
    "GetTagValuesResponse"
    "fixture/GetTagValuesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTagValues)

responseStartReportCreation :: StartReportCreationResponse -> TestTree
responseStartReportCreation =
  res
    "StartReportCreationResponse"
    "fixture/StartReportCreationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartReportCreation)

responseTagResources :: TagResourcesResponse -> TestTree
responseTagResources =
  res
    "TagResourcesResponse"
    "fixture/TagResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResources)

responseUntagResources :: UntagResourcesResponse -> TestTree
responseUntagResources =
  res
    "UntagResourcesResponse"
    "fixture/UntagResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResources)
