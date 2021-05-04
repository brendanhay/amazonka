{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ResourceGroupsTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ResourceGroupsTagging where

import Data.Proxy
import Network.AWS.ResourceGroupsTagging
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.ResourceGroupsTagging.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetComplianceSummary $
--             newGetComplianceSummary
--
--         , requestStartReportCreation $
--             newStartReportCreation
--
--         , requestTagResources $
--             newTagResources
--
--         , requestGetTagKeys $
--             newGetTagKeys
--
--         , requestDescribeReportCreation $
--             newDescribeReportCreation
--
--         , requestGetResources $
--             newGetResources
--
--         , requestGetTagValues $
--             newGetTagValues
--
--         , requestUntagResources $
--             newUntagResources
--
--           ]

--     , testGroup "response"
--         [ responseGetComplianceSummary $
--             newGetComplianceSummaryResponse
--
--         , responseStartReportCreation $
--             newStartReportCreationResponse
--
--         , responseTagResources $
--             newTagResourcesResponse
--
--         , responseGetTagKeys $
--             newGetTagKeysResponse
--
--         , responseDescribeReportCreation $
--             newDescribeReportCreationResponse
--
--         , responseGetResources $
--             newGetResourcesResponse
--
--         , responseGetTagValues $
--             newGetTagValuesResponse
--
--         , responseUntagResources $
--             newUntagResourcesResponse
--
--           ]
--     ]

-- Requests

requestGetComplianceSummary :: GetComplianceSummary -> TestTree
requestGetComplianceSummary =
  req
    "GetComplianceSummary"
    "fixture/GetComplianceSummary.yaml"

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

requestGetTagKeys :: GetTagKeys -> TestTree
requestGetTagKeys =
  req
    "GetTagKeys"
    "fixture/GetTagKeys.yaml"

requestDescribeReportCreation :: DescribeReportCreation -> TestTree
requestDescribeReportCreation =
  req
    "DescribeReportCreation"
    "fixture/DescribeReportCreation.yaml"

requestGetResources :: GetResources -> TestTree
requestGetResources =
  req
    "GetResources"
    "fixture/GetResources.yaml"

requestGetTagValues :: GetTagValues -> TestTree
requestGetTagValues =
  req
    "GetTagValues"
    "fixture/GetTagValues.yaml"

requestUntagResources :: UntagResources -> TestTree
requestUntagResources =
  req
    "UntagResources"
    "fixture/UntagResources.yaml"

-- Responses

responseGetComplianceSummary :: GetComplianceSummaryResponse -> TestTree
responseGetComplianceSummary =
  res
    "GetComplianceSummaryResponse"
    "fixture/GetComplianceSummaryResponse.proto"
    defaultService
    (Proxy :: Proxy GetComplianceSummary)

responseStartReportCreation :: StartReportCreationResponse -> TestTree
responseStartReportCreation =
  res
    "StartReportCreationResponse"
    "fixture/StartReportCreationResponse.proto"
    defaultService
    (Proxy :: Proxy StartReportCreation)

responseTagResources :: TagResourcesResponse -> TestTree
responseTagResources =
  res
    "TagResourcesResponse"
    "fixture/TagResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy TagResources)

responseGetTagKeys :: GetTagKeysResponse -> TestTree
responseGetTagKeys =
  res
    "GetTagKeysResponse"
    "fixture/GetTagKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetTagKeys)

responseDescribeReportCreation :: DescribeReportCreationResponse -> TestTree
responseDescribeReportCreation =
  res
    "DescribeReportCreationResponse"
    "fixture/DescribeReportCreationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeReportCreation)

responseGetResources :: GetResourcesResponse -> TestTree
responseGetResources =
  res
    "GetResourcesResponse"
    "fixture/GetResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy GetResources)

responseGetTagValues :: GetTagValuesResponse -> TestTree
responseGetTagValues =
  res
    "GetTagValuesResponse"
    "fixture/GetTagValuesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTagValues)

responseUntagResources :: UntagResourcesResponse -> TestTree
responseUntagResources =
  res
    "UntagResourcesResponse"
    "fixture/UntagResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResources)
