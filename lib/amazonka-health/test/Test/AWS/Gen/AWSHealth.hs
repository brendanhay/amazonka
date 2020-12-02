{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AWSHealth
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AWSHealth where

import Data.Proxy
import Network.AWS.AWSHealth
import Test.AWS.AWSHealth.Internal
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
--         [ requestDescribeEntityAggregates $
--             describeEntityAggregates
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestDescribeEventDetails $
--             describeEventDetails
--
--         , requestDescribeEventAggregates $
--             describeEventAggregates
--
--         , requestDescribeAffectedEntities $
--             describeAffectedEntities
--
--         , requestDescribeEventTypes $
--             describeEventTypes
--
--           ]

--     , testGroup "response"
--         [ responseDescribeEntityAggregates $
--             describeEntityAggregatesResponse
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseDescribeEventDetails $
--             describeEventDetailsResponse
--
--         , responseDescribeEventAggregates $
--             describeEventAggregatesResponse
--
--         , responseDescribeAffectedEntities $
--             describeAffectedEntitiesResponse
--
--         , responseDescribeEventTypes $
--             describeEventTypesResponse
--
--           ]
--     ]

-- Requests

requestDescribeEntityAggregates :: DescribeEntityAggregates -> TestTree
requestDescribeEntityAggregates = req
    "DescribeEntityAggregates"
    "fixture/DescribeEntityAggregates.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEventDetails :: DescribeEventDetails -> TestTree
requestDescribeEventDetails = req
    "DescribeEventDetails"
    "fixture/DescribeEventDetails.yaml"

requestDescribeEventAggregates :: DescribeEventAggregates -> TestTree
requestDescribeEventAggregates = req
    "DescribeEventAggregates"
    "fixture/DescribeEventAggregates.yaml"

requestDescribeAffectedEntities :: DescribeAffectedEntities -> TestTree
requestDescribeAffectedEntities = req
    "DescribeAffectedEntities"
    "fixture/DescribeAffectedEntities.yaml"

requestDescribeEventTypes :: DescribeEventTypes -> TestTree
requestDescribeEventTypes = req
    "DescribeEventTypes"
    "fixture/DescribeEventTypes.yaml"

-- Responses

responseDescribeEntityAggregates :: DescribeEntityAggregatesResponse -> TestTree
responseDescribeEntityAggregates = res
    "DescribeEntityAggregatesResponse"
    "fixture/DescribeEntityAggregatesResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeEntityAggregates)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeEvents)

responseDescribeEventDetails :: DescribeEventDetailsResponse -> TestTree
responseDescribeEventDetails = res
    "DescribeEventDetailsResponse"
    "fixture/DescribeEventDetailsResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeEventDetails)

responseDescribeEventAggregates :: DescribeEventAggregatesResponse -> TestTree
responseDescribeEventAggregates = res
    "DescribeEventAggregatesResponse"
    "fixture/DescribeEventAggregatesResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeEventAggregates)

responseDescribeAffectedEntities :: DescribeAffectedEntitiesResponse -> TestTree
responseDescribeAffectedEntities = res
    "DescribeAffectedEntitiesResponse"
    "fixture/DescribeAffectedEntitiesResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeAffectedEntities)

responseDescribeEventTypes :: DescribeEventTypesResponse -> TestTree
responseDescribeEventTypes = res
    "DescribeEventTypesResponse"
    "fixture/DescribeEventTypesResponse.proto"
    awsHealth
    (Proxy :: Proxy DescribeEventTypes)
