{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AWSHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             newDescribeEntityAggregates
--
--         , requestEnableHealthServiceAccessForOrganization $
--             newEnableHealthServiceAccessForOrganization
--
--         , requestDisableHealthServiceAccessForOrganization $
--             newDisableHealthServiceAccessForOrganization
--
--         , requestDescribeEventDetailsForOrganization $
--             newDescribeEventDetailsForOrganization
--
--         , requestDescribeEventDetails $
--             newDescribeEventDetails
--
--         , requestDescribeAffectedAccountsForOrganization $
--             newDescribeAffectedAccountsForOrganization
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeAffectedEntitiesForOrganization $
--             newDescribeAffectedEntitiesForOrganization
--
--         , requestDescribeEventAggregates $
--             newDescribeEventAggregates
--
--         , requestDescribeHealthServiceStatusForOrganization $
--             newDescribeHealthServiceStatusForOrganization
--
--         , requestDescribeAffectedEntities $
--             newDescribeAffectedEntities
--
--         , requestDescribeEventTypes $
--             newDescribeEventTypes
--
--         , requestDescribeEventsForOrganization $
--             newDescribeEventsForOrganization
--
--           ]

--     , testGroup "response"
--         [ responseDescribeEntityAggregates $
--             newDescribeEntityAggregatesResponse
--
--         , responseEnableHealthServiceAccessForOrganization $
--             newEnableHealthServiceAccessForOrganizationResponse
--
--         , responseDisableHealthServiceAccessForOrganization $
--             newDisableHealthServiceAccessForOrganizationResponse
--
--         , responseDescribeEventDetailsForOrganization $
--             newDescribeEventDetailsForOrganizationResponse
--
--         , responseDescribeEventDetails $
--             newDescribeEventDetailsResponse
--
--         , responseDescribeAffectedAccountsForOrganization $
--             newDescribeAffectedAccountsForOrganizationResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeAffectedEntitiesForOrganization $
--             newDescribeAffectedEntitiesForOrganizationResponse
--
--         , responseDescribeEventAggregates $
--             newDescribeEventAggregatesResponse
--
--         , responseDescribeHealthServiceStatusForOrganization $
--             newDescribeHealthServiceStatusForOrganizationResponse
--
--         , responseDescribeAffectedEntities $
--             newDescribeAffectedEntitiesResponse
--
--         , responseDescribeEventTypes $
--             newDescribeEventTypesResponse
--
--         , responseDescribeEventsForOrganization $
--             newDescribeEventsForOrganizationResponse
--
--           ]
--     ]

-- Requests

requestDescribeEntityAggregates :: DescribeEntityAggregates -> TestTree
requestDescribeEntityAggregates =
  req
    "DescribeEntityAggregates"
    "fixture/DescribeEntityAggregates.yaml"

requestEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganization -> TestTree
requestEnableHealthServiceAccessForOrganization =
  req
    "EnableHealthServiceAccessForOrganization"
    "fixture/EnableHealthServiceAccessForOrganization.yaml"

requestDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganization -> TestTree
requestDisableHealthServiceAccessForOrganization =
  req
    "DisableHealthServiceAccessForOrganization"
    "fixture/DisableHealthServiceAccessForOrganization.yaml"

requestDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganization -> TestTree
requestDescribeEventDetailsForOrganization =
  req
    "DescribeEventDetailsForOrganization"
    "fixture/DescribeEventDetailsForOrganization.yaml"

requestDescribeEventDetails :: DescribeEventDetails -> TestTree
requestDescribeEventDetails =
  req
    "DescribeEventDetails"
    "fixture/DescribeEventDetails.yaml"

requestDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganization -> TestTree
requestDescribeAffectedAccountsForOrganization =
  req
    "DescribeAffectedAccountsForOrganization"
    "fixture/DescribeAffectedAccountsForOrganization.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganization -> TestTree
requestDescribeAffectedEntitiesForOrganization =
  req
    "DescribeAffectedEntitiesForOrganization"
    "fixture/DescribeAffectedEntitiesForOrganization.yaml"

requestDescribeEventAggregates :: DescribeEventAggregates -> TestTree
requestDescribeEventAggregates =
  req
    "DescribeEventAggregates"
    "fixture/DescribeEventAggregates.yaml"

requestDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganization -> TestTree
requestDescribeHealthServiceStatusForOrganization =
  req
    "DescribeHealthServiceStatusForOrganization"
    "fixture/DescribeHealthServiceStatusForOrganization.yaml"

requestDescribeAffectedEntities :: DescribeAffectedEntities -> TestTree
requestDescribeAffectedEntities =
  req
    "DescribeAffectedEntities"
    "fixture/DescribeAffectedEntities.yaml"

requestDescribeEventTypes :: DescribeEventTypes -> TestTree
requestDescribeEventTypes =
  req
    "DescribeEventTypes"
    "fixture/DescribeEventTypes.yaml"

requestDescribeEventsForOrganization :: DescribeEventsForOrganization -> TestTree
requestDescribeEventsForOrganization =
  req
    "DescribeEventsForOrganization"
    "fixture/DescribeEventsForOrganization.yaml"

-- Responses

responseDescribeEntityAggregates :: DescribeEntityAggregatesResponse -> TestTree
responseDescribeEntityAggregates =
  res
    "DescribeEntityAggregatesResponse"
    "fixture/DescribeEntityAggregatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEntityAggregates)

responseEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganizationResponse -> TestTree
responseEnableHealthServiceAccessForOrganization =
  res
    "EnableHealthServiceAccessForOrganizationResponse"
    "fixture/EnableHealthServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy EnableHealthServiceAccessForOrganization)

responseDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganizationResponse -> TestTree
responseDisableHealthServiceAccessForOrganization =
  res
    "DisableHealthServiceAccessForOrganizationResponse"
    "fixture/DisableHealthServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DisableHealthServiceAccessForOrganization)

responseDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganizationResponse -> TestTree
responseDescribeEventDetailsForOrganization =
  res
    "DescribeEventDetailsForOrganizationResponse"
    "fixture/DescribeEventDetailsForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventDetailsForOrganization)

responseDescribeEventDetails :: DescribeEventDetailsResponse -> TestTree
responseDescribeEventDetails =
  res
    "DescribeEventDetailsResponse"
    "fixture/DescribeEventDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventDetails)

responseDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganizationResponse -> TestTree
responseDescribeAffectedAccountsForOrganization =
  res
    "DescribeAffectedAccountsForOrganizationResponse"
    "fixture/DescribeAffectedAccountsForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAffectedAccountsForOrganization)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganizationResponse -> TestTree
responseDescribeAffectedEntitiesForOrganization =
  res
    "DescribeAffectedEntitiesForOrganizationResponse"
    "fixture/DescribeAffectedEntitiesForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAffectedEntitiesForOrganization)

responseDescribeEventAggregates :: DescribeEventAggregatesResponse -> TestTree
responseDescribeEventAggregates =
  res
    "DescribeEventAggregatesResponse"
    "fixture/DescribeEventAggregatesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventAggregates)

responseDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganizationResponse -> TestTree
responseDescribeHealthServiceStatusForOrganization =
  res
    "DescribeHealthServiceStatusForOrganizationResponse"
    "fixture/DescribeHealthServiceStatusForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHealthServiceStatusForOrganization)

responseDescribeAffectedEntities :: DescribeAffectedEntitiesResponse -> TestTree
responseDescribeAffectedEntities =
  res
    "DescribeAffectedEntitiesResponse"
    "fixture/DescribeAffectedEntitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAffectedEntities)

responseDescribeEventTypes :: DescribeEventTypesResponse -> TestTree
responseDescribeEventTypes =
  res
    "DescribeEventTypesResponse"
    "fixture/DescribeEventTypesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventTypes)

responseDescribeEventsForOrganization :: DescribeEventsForOrganizationResponse -> TestTree
responseDescribeEventsForOrganization =
  res
    "DescribeEventsForOrganizationResponse"
    "fixture/DescribeEventsForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEventsForOrganization)
