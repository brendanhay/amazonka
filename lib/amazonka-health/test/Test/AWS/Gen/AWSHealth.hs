{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AWSHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestEnableHealthServiceAccessForOrganization $
--             mkEnableHealthServiceAccessForOrganization
--
--         , requestDescribeEntityAggregates $
--             mkDescribeEntityAggregates
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestDescribeEventsForOrganization $
--             mkDescribeEventsForOrganization
--
--         , requestDescribeAffectedAccountsForOrganization $
--             mkDescribeAffectedAccountsForOrganization
--
--         , requestDescribeEventDetails $
--             mkDescribeEventDetails
--
--         , requestDescribeEventAggregates $
--             mkDescribeEventAggregates
--
--         , requestDescribeAffectedEntities $
--             mkDescribeAffectedEntities
--
--         , requestDescribeEventTypes $
--             mkDescribeEventTypes
--
--         , requestDescribeAffectedEntitiesForOrganization $
--             mkDescribeAffectedEntitiesForOrganization
--
--         , requestDescribeHealthServiceStatusForOrganization $
--             mkDescribeHealthServiceStatusForOrganization
--
--         , requestDescribeEventDetailsForOrganization $
--             mkDescribeEventDetailsForOrganization
--
--         , requestDisableHealthServiceAccessForOrganization $
--             mkDisableHealthServiceAccessForOrganization
--
--           ]

--     , testGroup "response"
--         [ responseEnableHealthServiceAccessForOrganization $
--             mkEnableHealthServiceAccessForOrganizationResponse
--
--         , responseDescribeEntityAggregates $
--             mkDescribeEntityAggregatesResponse
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseDescribeEventsForOrganization $
--             mkDescribeEventsForOrganizationResponse
--
--         , responseDescribeAffectedAccountsForOrganization $
--             mkDescribeAffectedAccountsForOrganizationResponse
--
--         , responseDescribeEventDetails $
--             mkDescribeEventDetailsResponse
--
--         , responseDescribeEventAggregates $
--             mkDescribeEventAggregatesResponse
--
--         , responseDescribeAffectedEntities $
--             mkDescribeAffectedEntitiesResponse
--
--         , responseDescribeEventTypes $
--             mkDescribeEventTypesResponse
--
--         , responseDescribeAffectedEntitiesForOrganization $
--             mkDescribeAffectedEntitiesForOrganizationResponse
--
--         , responseDescribeHealthServiceStatusForOrganization $
--             mkDescribeHealthServiceStatusForOrganizationResponse
--
--         , responseDescribeEventDetailsForOrganization $
--             mkDescribeEventDetailsForOrganizationResponse
--
--         , responseDisableHealthServiceAccessForOrganization $
--             mkDisableHealthServiceAccessForOrganizationResponse
--
--           ]
--     ]

-- Requests

requestEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganization -> TestTree
requestEnableHealthServiceAccessForOrganization =
  req
    "EnableHealthServiceAccessForOrganization"
    "fixture/EnableHealthServiceAccessForOrganization.yaml"

requestDescribeEntityAggregates :: DescribeEntityAggregates -> TestTree
requestDescribeEntityAggregates =
  req
    "DescribeEntityAggregates"
    "fixture/DescribeEntityAggregates.yaml"

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

requestDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganization -> TestTree
requestDescribeAffectedAccountsForOrganization =
  req
    "DescribeAffectedAccountsForOrganization"
    "fixture/DescribeAffectedAccountsForOrganization.yaml"

requestDescribeEventDetails :: DescribeEventDetails -> TestTree
requestDescribeEventDetails =
  req
    "DescribeEventDetails"
    "fixture/DescribeEventDetails.yaml"

requestDescribeEventAggregates :: DescribeEventAggregates -> TestTree
requestDescribeEventAggregates =
  req
    "DescribeEventAggregates"
    "fixture/DescribeEventAggregates.yaml"

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

requestDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganization -> TestTree
requestDescribeAffectedEntitiesForOrganization =
  req
    "DescribeAffectedEntitiesForOrganization"
    "fixture/DescribeAffectedEntitiesForOrganization.yaml"

requestDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganization -> TestTree
requestDescribeHealthServiceStatusForOrganization =
  req
    "DescribeHealthServiceStatusForOrganization"
    "fixture/DescribeHealthServiceStatusForOrganization.yaml"

requestDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganization -> TestTree
requestDescribeEventDetailsForOrganization =
  req
    "DescribeEventDetailsForOrganization"
    "fixture/DescribeEventDetailsForOrganization.yaml"

requestDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganization -> TestTree
requestDisableHealthServiceAccessForOrganization =
  req
    "DisableHealthServiceAccessForOrganization"
    "fixture/DisableHealthServiceAccessForOrganization.yaml"

-- Responses

responseEnableHealthServiceAccessForOrganization :: EnableHealthServiceAccessForOrganizationResponse -> TestTree
responseEnableHealthServiceAccessForOrganization =
  res
    "EnableHealthServiceAccessForOrganizationResponse"
    "fixture/EnableHealthServiceAccessForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy EnableHealthServiceAccessForOrganization)

responseDescribeEntityAggregates :: DescribeEntityAggregatesResponse -> TestTree
responseDescribeEntityAggregates =
  res
    "DescribeEntityAggregatesResponse"
    "fixture/DescribeEntityAggregatesResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEntityAggregates)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEvents)

responseDescribeEventsForOrganization :: DescribeEventsForOrganizationResponse -> TestTree
responseDescribeEventsForOrganization =
  res
    "DescribeEventsForOrganizationResponse"
    "fixture/DescribeEventsForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEventsForOrganization)

responseDescribeAffectedAccountsForOrganization :: DescribeAffectedAccountsForOrganizationResponse -> TestTree
responseDescribeAffectedAccountsForOrganization =
  res
    "DescribeAffectedAccountsForOrganizationResponse"
    "fixture/DescribeAffectedAccountsForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeAffectedAccountsForOrganization)

responseDescribeEventDetails :: DescribeEventDetailsResponse -> TestTree
responseDescribeEventDetails =
  res
    "DescribeEventDetailsResponse"
    "fixture/DescribeEventDetailsResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEventDetails)

responseDescribeEventAggregates :: DescribeEventAggregatesResponse -> TestTree
responseDescribeEventAggregates =
  res
    "DescribeEventAggregatesResponse"
    "fixture/DescribeEventAggregatesResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEventAggregates)

responseDescribeAffectedEntities :: DescribeAffectedEntitiesResponse -> TestTree
responseDescribeAffectedEntities =
  res
    "DescribeAffectedEntitiesResponse"
    "fixture/DescribeAffectedEntitiesResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeAffectedEntities)

responseDescribeEventTypes :: DescribeEventTypesResponse -> TestTree
responseDescribeEventTypes =
  res
    "DescribeEventTypesResponse"
    "fixture/DescribeEventTypesResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEventTypes)

responseDescribeAffectedEntitiesForOrganization :: DescribeAffectedEntitiesForOrganizationResponse -> TestTree
responseDescribeAffectedEntitiesForOrganization =
  res
    "DescribeAffectedEntitiesForOrganizationResponse"
    "fixture/DescribeAffectedEntitiesForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeAffectedEntitiesForOrganization)

responseDescribeHealthServiceStatusForOrganization :: DescribeHealthServiceStatusForOrganizationResponse -> TestTree
responseDescribeHealthServiceStatusForOrganization =
  res
    "DescribeHealthServiceStatusForOrganizationResponse"
    "fixture/DescribeHealthServiceStatusForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeHealthServiceStatusForOrganization)

responseDescribeEventDetailsForOrganization :: DescribeEventDetailsForOrganizationResponse -> TestTree
responseDescribeEventDetailsForOrganization =
  res
    "DescribeEventDetailsForOrganizationResponse"
    "fixture/DescribeEventDetailsForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DescribeEventDetailsForOrganization)

responseDisableHealthServiceAccessForOrganization :: DisableHealthServiceAccessForOrganizationResponse -> TestTree
responseDisableHealthServiceAccessForOrganization =
  res
    "DisableHealthServiceAccessForOrganizationResponse"
    "fixture/DisableHealthServiceAccessForOrganizationResponse.proto"
    awsHealthService
    (Proxy :: Proxy DisableHealthServiceAccessForOrganization)
