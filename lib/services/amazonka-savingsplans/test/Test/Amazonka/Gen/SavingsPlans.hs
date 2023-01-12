{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SavingsPlans
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SavingsPlans where

import Amazonka.SavingsPlans
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SavingsPlans.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateSavingsPlan $
--             newCreateSavingsPlan
--
--         , requestDeleteQueuedSavingsPlan $
--             newDeleteQueuedSavingsPlan
--
--         , requestDescribeSavingsPlanRates $
--             newDescribeSavingsPlanRates
--
--         , requestDescribeSavingsPlans $
--             newDescribeSavingsPlans
--
--         , requestDescribeSavingsPlansOfferingRates $
--             newDescribeSavingsPlansOfferingRates
--
--         , requestDescribeSavingsPlansOfferings $
--             newDescribeSavingsPlansOfferings
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateSavingsPlan $
--             newCreateSavingsPlanResponse
--
--         , responseDeleteQueuedSavingsPlan $
--             newDeleteQueuedSavingsPlanResponse
--
--         , responseDescribeSavingsPlanRates $
--             newDescribeSavingsPlanRatesResponse
--
--         , responseDescribeSavingsPlans $
--             newDescribeSavingsPlansResponse
--
--         , responseDescribeSavingsPlansOfferingRates $
--             newDescribeSavingsPlansOfferingRatesResponse
--
--         , responseDescribeSavingsPlansOfferings $
--             newDescribeSavingsPlansOfferingsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCreateSavingsPlan :: CreateSavingsPlan -> TestTree
requestCreateSavingsPlan =
  req
    "CreateSavingsPlan"
    "fixture/CreateSavingsPlan.yaml"

requestDeleteQueuedSavingsPlan :: DeleteQueuedSavingsPlan -> TestTree
requestDeleteQueuedSavingsPlan =
  req
    "DeleteQueuedSavingsPlan"
    "fixture/DeleteQueuedSavingsPlan.yaml"

requestDescribeSavingsPlanRates :: DescribeSavingsPlanRates -> TestTree
requestDescribeSavingsPlanRates =
  req
    "DescribeSavingsPlanRates"
    "fixture/DescribeSavingsPlanRates.yaml"

requestDescribeSavingsPlans :: DescribeSavingsPlans -> TestTree
requestDescribeSavingsPlans =
  req
    "DescribeSavingsPlans"
    "fixture/DescribeSavingsPlans.yaml"

requestDescribeSavingsPlansOfferingRates :: DescribeSavingsPlansOfferingRates -> TestTree
requestDescribeSavingsPlansOfferingRates =
  req
    "DescribeSavingsPlansOfferingRates"
    "fixture/DescribeSavingsPlansOfferingRates.yaml"

requestDescribeSavingsPlansOfferings :: DescribeSavingsPlansOfferings -> TestTree
requestDescribeSavingsPlansOfferings =
  req
    "DescribeSavingsPlansOfferings"
    "fixture/DescribeSavingsPlansOfferings.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

-- Responses

responseCreateSavingsPlan :: CreateSavingsPlanResponse -> TestTree
responseCreateSavingsPlan =
  res
    "CreateSavingsPlanResponse"
    "fixture/CreateSavingsPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSavingsPlan)

responseDeleteQueuedSavingsPlan :: DeleteQueuedSavingsPlanResponse -> TestTree
responseDeleteQueuedSavingsPlan =
  res
    "DeleteQueuedSavingsPlanResponse"
    "fixture/DeleteQueuedSavingsPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueuedSavingsPlan)

responseDescribeSavingsPlanRates :: DescribeSavingsPlanRatesResponse -> TestTree
responseDescribeSavingsPlanRates =
  res
    "DescribeSavingsPlanRatesResponse"
    "fixture/DescribeSavingsPlanRatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlanRates)

responseDescribeSavingsPlans :: DescribeSavingsPlansResponse -> TestTree
responseDescribeSavingsPlans =
  res
    "DescribeSavingsPlansResponse"
    "fixture/DescribeSavingsPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlans)

responseDescribeSavingsPlansOfferingRates :: DescribeSavingsPlansOfferingRatesResponse -> TestTree
responseDescribeSavingsPlansOfferingRates =
  res
    "DescribeSavingsPlansOfferingRatesResponse"
    "fixture/DescribeSavingsPlansOfferingRatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlansOfferingRates)

responseDescribeSavingsPlansOfferings :: DescribeSavingsPlansOfferingsResponse -> TestTree
responseDescribeSavingsPlansOfferings =
  res
    "DescribeSavingsPlansOfferingsResponse"
    "fixture/DescribeSavingsPlansOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlansOfferings)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

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
