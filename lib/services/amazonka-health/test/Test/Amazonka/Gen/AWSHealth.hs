{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AWSHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AWSHealth where

import Amazonka.AWSHealth
import qualified Data.Proxy as Proxy
import Test.Amazonka.AWSHealth.Internal
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
--         [ requestDescribeAffectedAccountsForOrganization $
--             newDescribeAffectedAccountsForOrganization
--
--         , requestDescribeAffectedEntities $
--             newDescribeAffectedEntities
--
--         , requestDescribeAffectedEntitiesForOrganization $
--             newDescribeAffectedEntitiesForOrganization
--
--         , requestDescribeEntityAggregates $
--             newDescribeEntityAggregates
--
--         , requestDescribeEventAggregates $
--             newDescribeEventAggregates
--
--         , requestDescribeEventDetails $
--             newDescribeEventDetails
--
--         , requestDescribeEventDetailsForOrganization $
--             newDescribeEventDetailsForOrganization
--
--         , requestDescribeEventTypes $
--             newDescribeEventTypes
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeEventsForOrganization $
--             newDescribeEventsForOrganization
--
--         , requestDescribeHealthServiceStatusForOrganization $
--             newDescribeHealthServiceStatusForOrganization
--
--         , requestDisableHealthServiceAccessForOrganization $
--             newDisableHealthServiceAccessForOrganization
--
--         , requestEnableHealthServiceAccessForOrganization $
--             newEnableHealthServiceAccessForOrganization
--
--           ]

--     , testGroup "response"
--         [ responseDescribeAffectedAccountsForOrganization $
--             newDescribeAffectedAccountsForOrganizationResponse
--
--         , responseDescribeAffectedEntities $
--             newDescribeAffectedEntitiesResponse
--
--         , responseDescribeAffectedEntitiesForOrganization $
--             newDescribeAffectedEntitiesForOrganizationResponse
--
--         , responseDescribeEntityAggregates $
--             newDescribeEntityAggregatesResponse
--
--         , responseDescribeEventAggregates $
--             newDescribeEventAggregatesResponse
--
--         , responseDescribeEventDetails $
--             newDescribeEventDetailsResponse
--
--         , responseDescribeEventDetailsForOrganization $
--             newDescribeEventDetailsForOrganizationResponse
--
--         , responseDescribeEventTypes $
--             newDescribeEventTypesResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeEventsForOrganization $
--             newDescribeEventsForOrganizationResponse
--
--         , responseDescribeHealthServiceStatusForOrganization $
--             newDescribeHealthServiceStatusForOrganizationResponse
--
--         , responseDisableHealthServiceAccessForOrganization $
--             newDisableHealthServiceAccessForOrganizationResponse
--
--         , responseEnableHealthServiceAccessForOrganization $
--             newEnableHealthServiceAccessForOrganizationResponse
--
--           ]
--     ]

-- Requests

requestDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganization -> TestTree
requestDescribeAffectedAccountsForOrganization =
  req
    "DescribeAffectedAccountsForOrganization"
    "fixture/DescribeAffectedAccountsForOrganization.yaml"

requestDescribeAffectedEntities :: DescribeAffectedEntities -> TestTree
requestDescribeAffectedEntities =
  req
    "DescribeAffectedEntities"
    "fixture/DescribeAffectedEntities.yaml"

requestDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganization -> TestTree
requestDescribeAffectedEntitiesForOrganization =
  req
    "DescribeAffectedEntitiesForOrganization"
    "fixture/DescribeAffectedEntitiesForOrganization.yaml"

requestDescribeEntityAggregates :: DescribeEntityAggregates -> TestTree
requestDescribeEntityAggregates =
  req
    "DescribeEntityAggregates"
    "fixture/DescribeEntityAggregates.yaml"

requestDescribeEventAggregates :: DescribeEventAggregates -> TestTree
requestDescribeEventAggregates =
  req
    "DescribeEventAggregates"
    "fixture/DescribeEventAggregates.yaml"

requestDescribeEventDetails :: DescribeEventDetails -> TestTree
requestDescribeEventDetails =
  req
    "DescribeEventDetails"
    "fixture/DescribeEventDetails.yaml"

requestDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganization -> TestTree
requestDescribeEventDetailsForOrganization =
  req
    "DescribeEventDetailsForOrganization"
    "fixture/DescribeEventDetailsForOrganization.yaml"

requestDescribeEventTypes :: DescribeEventTypes -> TestTree
requestDescribeEventTypes =
  req
    "DescribeEventTypes"
    "fixture/DescribeEventTypes.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeEventsForOrganization :: DescribeEventsForOrganization -> TestTree
requestDescribeEventsForOrganization =
  req
    "DescribeEventsForOrganization"
    "fixture/DescribeEventsForOrganization.yaml"

requestDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganization -> TestTree
requestDescribeHealthServiceStatusForOrganization =
  req
    "DescribeHealthServiceStatusForOrganization"
    "fixture/DescribeHealthServiceStatusForOrganization.yaml"

requestDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganization -> TestTree
requestDisableHealthServiceAccessForOrganization =
  req
    "DisableHealthServiceAccessForOrganization"
    "fixture/DisableHealthServiceAccessForOrganization.yaml"

requestEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganization -> TestTree
requestEnableHealthServiceAccessForOrganization =
  req
    "EnableHealthServiceAccessForOrganization"
    "fixture/EnableHealthServiceAccessForOrganization.yaml"

-- Responses

responseDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganizationResponse -> TestTree
responseDescribeAffectedAccountsForOrganization =
  res
    "DescribeAffectedAccountsForOrganizationResponse"
    "fixture/DescribeAffectedAccountsForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAffectedAccountsForOrganization)

responseDescribeAffectedEntities :: DescribeAffectedEntitiesResponse -> TestTree
responseDescribeAffectedEntities =
  res
    "DescribeAffectedEntitiesResponse"
    "fixture/DescribeAffectedEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAffectedEntities)

responseDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganizationResponse -> TestTree
responseDescribeAffectedEntitiesForOrganization =
  res
    "DescribeAffectedEntitiesForOrganizationResponse"
    "fixture/DescribeAffectedEntitiesForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAffectedEntitiesForOrganization)

responseDescribeEntityAggregates :: DescribeEntityAggregatesResponse -> TestTree
responseDescribeEntityAggregates =
  res
    "DescribeEntityAggregatesResponse"
    "fixture/DescribeEntityAggregatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEntityAggregates)

responseDescribeEventAggregates :: DescribeEventAggregatesResponse -> TestTree
responseDescribeEventAggregates =
  res
    "DescribeEventAggregatesResponse"
    "fixture/DescribeEventAggregatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventAggregates)

responseDescribeEventDetails :: DescribeEventDetailsResponse -> TestTree
responseDescribeEventDetails =
  res
    "DescribeEventDetailsResponse"
    "fixture/DescribeEventDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventDetails)

responseDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganizationResponse -> TestTree
responseDescribeEventDetailsForOrganization =
  res
    "DescribeEventDetailsForOrganizationResponse"
    "fixture/DescribeEventDetailsForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventDetailsForOrganization)

responseDescribeEventTypes :: DescribeEventTypesResponse -> TestTree
responseDescribeEventTypes =
  res
    "DescribeEventTypesResponse"
    "fixture/DescribeEventTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventTypes)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeEventsForOrganization :: DescribeEventsForOrganizationResponse -> TestTree
responseDescribeEventsForOrganization =
  res
    "DescribeEventsForOrganizationResponse"
    "fixture/DescribeEventsForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEventsForOrganization)

responseDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganizationResponse -> TestTree
responseDescribeHealthServiceStatusForOrganization =
  res
    "DescribeHealthServiceStatusForOrganizationResponse"
    "fixture/DescribeHealthServiceStatusForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHealthServiceStatusForOrganization)

responseDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganizationResponse -> TestTree
responseDisableHealthServiceAccessForOrganization =
  res
    "DisableHealthServiceAccessForOrganizationResponse"
    "fixture/DisableHealthServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableHealthServiceAccessForOrganization)

responseEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganizationResponse -> TestTree
responseEnableHealthServiceAccessForOrganization =
  res
    "EnableHealthServiceAccessForOrganizationResponse"
    "fixture/EnableHealthServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableHealthServiceAccessForOrganization)
