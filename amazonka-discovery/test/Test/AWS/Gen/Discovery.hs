{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Discovery
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Discovery where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Discovery
import Test.AWS.Discovery.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeTags $
--             describeTags
--
--         , testExportConfigurations $
--             exportConfigurations
--
--         , testStopDataCollectionByAgentIds $
--             stopDataCollectionByAgentIds
--
--         , testCreateTags $
--             createTags
--
--         , testDeleteTags $
--             deleteTags
--
--         , testDescribeConfigurations $
--             describeConfigurations
--
--         , testListConfigurations $
--             listConfigurations
--
--         , testDescribeAgents $
--             describeAgents
--
--         , testDescribeExportConfigurations $
--             describeExportConfigurations
--
--         , testStartDataCollectionByAgentIds $
--             startDataCollectionByAgentIds
--
--           ]

--     , testGroup "response"
--         [ testDescribeTagsResponse $
--             describeTagsResponse
--
--         , testExportConfigurationsResponse $
--             exportConfigurationsResponse
--
--         , testStopDataCollectionByAgentIdsResponse $
--             stopDataCollectionByAgentIdsResponse
--
--         , testCreateTagsResponse $
--             createTagsResponse
--
--         , testDeleteTagsResponse $
--             deleteTagsResponse
--
--         , testDescribeConfigurationsResponse $
--             describeConfigurationsResponse
--
--         , testListConfigurationsResponse $
--             listConfigurationsResponse
--
--         , testDescribeAgentsResponse $
--             describeAgentsResponse
--
--         , testDescribeExportConfigurationsResponse $
--             describeExportConfigurationsResponse
--
--         , testStartDataCollectionByAgentIdsResponse $
--             startDataCollectionByAgentIdsResponse
--
--           ]
--     ]

-- Requests

testDescribeTags :: DescribeTags -> TestTree
testDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

testExportConfigurations :: ExportConfigurations -> TestTree
testExportConfigurations = req
    "ExportConfigurations"
    "fixture/ExportConfigurations.yaml"

testStopDataCollectionByAgentIds :: StopDataCollectionByAgentIds -> TestTree
testStopDataCollectionByAgentIds = req
    "StopDataCollectionByAgentIds"
    "fixture/StopDataCollectionByAgentIds.yaml"

testCreateTags :: CreateTags -> TestTree
testCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

testDeleteTags :: DeleteTags -> TestTree
testDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

testDescribeConfigurations :: DescribeConfigurations -> TestTree
testDescribeConfigurations = req
    "DescribeConfigurations"
    "fixture/DescribeConfigurations.yaml"

testListConfigurations :: ListConfigurations -> TestTree
testListConfigurations = req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

testDescribeAgents :: DescribeAgents -> TestTree
testDescribeAgents = req
    "DescribeAgents"
    "fixture/DescribeAgents.yaml"

testDescribeExportConfigurations :: DescribeExportConfigurations -> TestTree
testDescribeExportConfigurations = req
    "DescribeExportConfigurations"
    "fixture/DescribeExportConfigurations.yaml"

testStartDataCollectionByAgentIds :: StartDataCollectionByAgentIds -> TestTree
testStartDataCollectionByAgentIds = req
    "StartDataCollectionByAgentIds"
    "fixture/StartDataCollectionByAgentIds.yaml"

-- Responses

testDescribeTagsResponse :: DescribeTagsResponse -> TestTree
testDescribeTagsResponse = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeTags)

testExportConfigurationsResponse :: ExportConfigurationsResponse -> TestTree
testExportConfigurationsResponse = res
    "ExportConfigurationsResponse"
    "fixture/ExportConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy ExportConfigurations)

testStopDataCollectionByAgentIdsResponse :: StopDataCollectionByAgentIdsResponse -> TestTree
testStopDataCollectionByAgentIdsResponse = res
    "StopDataCollectionByAgentIdsResponse"
    "fixture/StopDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StopDataCollectionByAgentIds)

testCreateTagsResponse :: CreateTagsResponse -> TestTree
testCreateTagsResponse = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    discovery
    (Proxy :: Proxy CreateTags)

testDeleteTagsResponse :: DeleteTagsResponse -> TestTree
testDeleteTagsResponse = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    discovery
    (Proxy :: Proxy DeleteTags)

testDescribeConfigurationsResponse :: DescribeConfigurationsResponse -> TestTree
testDescribeConfigurationsResponse = res
    "DescribeConfigurationsResponse"
    "fixture/DescribeConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeConfigurations)

testListConfigurationsResponse :: ListConfigurationsResponse -> TestTree
testListConfigurationsResponse = res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy ListConfigurations)

testDescribeAgentsResponse :: DescribeAgentsResponse -> TestTree
testDescribeAgentsResponse = res
    "DescribeAgentsResponse"
    "fixture/DescribeAgentsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeAgents)

testDescribeExportConfigurationsResponse :: DescribeExportConfigurationsResponse -> TestTree
testDescribeExportConfigurationsResponse = res
    "DescribeExportConfigurationsResponse"
    "fixture/DescribeExportConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeExportConfigurations)

testStartDataCollectionByAgentIdsResponse :: StartDataCollectionByAgentIdsResponse -> TestTree
testStartDataCollectionByAgentIdsResponse = res
    "StartDataCollectionByAgentIdsResponse"
    "fixture/StartDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StartDataCollectionByAgentIds)
