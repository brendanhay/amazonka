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
--         [ requestDescribeTags $
--             describeTags
--
--         , requestExportConfigurations $
--             exportConfigurations
--
--         , requestStopDataCollectionByAgentIds $
--             stopDataCollectionByAgentIds
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestDescribeConfigurations $
--             describeConfigurations
--
--         , requestListConfigurations $
--             listConfigurations
--
--         , requestDescribeAgents $
--             describeAgents
--
--         , requestDescribeExportConfigurations $
--             describeExportConfigurations
--
--         , requestStartDataCollectionByAgentIds $
--             startDataCollectionByAgentIds
--
--           ]

--     , testGroup "response"
--         [ responseDescribeTags $
--             describeTagsResponse
--
--         , responseExportConfigurations $
--             exportConfigurationsResponse
--
--         , responseStopDataCollectionByAgentIds $
--             stopDataCollectionByAgentIdsResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseDescribeConfigurations $
--             describeConfigurationsResponse
--
--         , responseListConfigurations $
--             listConfigurationsResponse
--
--         , responseDescribeAgents $
--             describeAgentsResponse
--
--         , responseDescribeExportConfigurations $
--             describeExportConfigurationsResponse
--
--         , responseStartDataCollectionByAgentIds $
--             startDataCollectionByAgentIdsResponse
--
--           ]
--     ]

-- Requests

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

requestExportConfigurations :: ExportConfigurations -> TestTree
requestExportConfigurations = req
    "ExportConfigurations"
    "fixture/ExportConfigurations.yaml"

requestStopDataCollectionByAgentIds :: StopDataCollectionByAgentIds -> TestTree
requestStopDataCollectionByAgentIds = req
    "StopDataCollectionByAgentIds"
    "fixture/StopDataCollectionByAgentIds.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestDescribeConfigurations :: DescribeConfigurations -> TestTree
requestDescribeConfigurations = req
    "DescribeConfigurations"
    "fixture/DescribeConfigurations.yaml"

requestListConfigurations :: ListConfigurations -> TestTree
requestListConfigurations = req
    "ListConfigurations"
    "fixture/ListConfigurations.yaml"

requestDescribeAgents :: DescribeAgents -> TestTree
requestDescribeAgents = req
    "DescribeAgents"
    "fixture/DescribeAgents.yaml"

requestDescribeExportConfigurations :: DescribeExportConfigurations -> TestTree
requestDescribeExportConfigurations = req
    "DescribeExportConfigurations"
    "fixture/DescribeExportConfigurations.yaml"

requestStartDataCollectionByAgentIds :: StartDataCollectionByAgentIds -> TestTree
requestStartDataCollectionByAgentIds = req
    "StartDataCollectionByAgentIds"
    "fixture/StartDataCollectionByAgentIds.yaml"

-- Responses

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeTags)

responseExportConfigurations :: ExportConfigurationsResponse -> TestTree
responseExportConfigurations = res
    "ExportConfigurationsResponse"
    "fixture/ExportConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy ExportConfigurations)

responseStopDataCollectionByAgentIds :: StopDataCollectionByAgentIdsResponse -> TestTree
responseStopDataCollectionByAgentIds = res
    "StopDataCollectionByAgentIdsResponse"
    "fixture/StopDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StopDataCollectionByAgentIds)

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    discovery
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    discovery
    (Proxy :: Proxy DeleteTags)

responseDescribeConfigurations :: DescribeConfigurationsResponse -> TestTree
responseDescribeConfigurations = res
    "DescribeConfigurationsResponse"
    "fixture/DescribeConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeConfigurations)

responseListConfigurations :: ListConfigurationsResponse -> TestTree
responseListConfigurations = res
    "ListConfigurationsResponse"
    "fixture/ListConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy ListConfigurations)

responseDescribeAgents :: DescribeAgentsResponse -> TestTree
responseDescribeAgents = res
    "DescribeAgentsResponse"
    "fixture/DescribeAgentsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeAgents)

responseDescribeExportConfigurations :: DescribeExportConfigurationsResponse -> TestTree
responseDescribeExportConfigurations = res
    "DescribeExportConfigurationsResponse"
    "fixture/DescribeExportConfigurationsResponse.proto"
    discovery
    (Proxy :: Proxy DescribeExportConfigurations)

responseStartDataCollectionByAgentIds :: StartDataCollectionByAgentIdsResponse -> TestTree
responseStartDataCollectionByAgentIds = res
    "StartDataCollectionByAgentIdsResponse"
    "fixture/StartDataCollectionByAgentIdsResponse.proto"
    discovery
    (Proxy :: Proxy StartDataCollectionByAgentIds)
