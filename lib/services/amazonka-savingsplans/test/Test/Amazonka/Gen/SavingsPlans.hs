{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SavingsPlans
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeSavingsPlanRates $
--             newDescribeSavingsPlanRates
--
--         , requestDeleteQueuedSavingsPlan $
--             newDeleteQueuedSavingsPlan
--
--         , requestCreateSavingsPlan $
--             newCreateSavingsPlan
--
--         , requestDescribeSavingsPlansOfferings $
--             newDescribeSavingsPlansOfferings
--
--         , requestDescribeSavingsPlans $
--             newDescribeSavingsPlans
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDescribeSavingsPlansOfferingRates $
--             newDescribeSavingsPlansOfferingRates
--
--           ]

--     , testGroup "response"
--         [ responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeSavingsPlanRates $
--             newDescribeSavingsPlanRatesResponse
--
--         , responseDeleteQueuedSavingsPlan $
--             newDeleteQueuedSavingsPlanResponse
--
--         , responseCreateSavingsPlan $
--             newCreateSavingsPlanResponse
--
--         , responseDescribeSavingsPlansOfferings $
--             newDescribeSavingsPlansOfferingsResponse
--
--         , responseDescribeSavingsPlans $
--             newDescribeSavingsPlansResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDescribeSavingsPlansOfferingRates $
--             newDescribeSavingsPlansOfferingRatesResponse
--
--           ]
--     ]

-- Requests

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeSavingsPlanRates :: DescribeSavingsPlanRates -> TestTree
requestDescribeSavingsPlanRates =
  req
    "DescribeSavingsPlanRates"
    "fixture/DescribeSavingsPlanRates.yaml"

requestDeleteQueuedSavingsPlan :: DeleteQueuedSavingsPlan -> TestTree
requestDeleteQueuedSavingsPlan =
  req
    "DeleteQueuedSavingsPlan"
    "fixture/DeleteQueuedSavingsPlan.yaml"

requestCreateSavingsPlan :: CreateSavingsPlan -> TestTree
requestCreateSavingsPlan =
  req
    "CreateSavingsPlan"
    "fixture/CreateSavingsPlan.yaml"

requestDescribeSavingsPlansOfferings :: DescribeSavingsPlansOfferings -> TestTree
requestDescribeSavingsPlansOfferings =
  req
    "DescribeSavingsPlansOfferings"
    "fixture/DescribeSavingsPlansOfferings.yaml"

requestDescribeSavingsPlans :: DescribeSavingsPlans -> TestTree
requestDescribeSavingsPlans =
  req
    "DescribeSavingsPlans"
    "fixture/DescribeSavingsPlans.yaml"

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

requestDescribeSavingsPlansOfferingRates :: DescribeSavingsPlansOfferingRates -> TestTree
requestDescribeSavingsPlansOfferingRates =
  req
    "DescribeSavingsPlansOfferingRates"
    "fixture/DescribeSavingsPlansOfferingRates.yaml"

-- Responses

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribeSavingsPlanRates :: DescribeSavingsPlanRatesResponse -> TestTree
responseDescribeSavingsPlanRates =
  res
    "DescribeSavingsPlanRatesResponse"
    "fixture/DescribeSavingsPlanRatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlanRates)

responseDeleteQueuedSavingsPlan :: DeleteQueuedSavingsPlanResponse -> TestTree
responseDeleteQueuedSavingsPlan =
  res
    "DeleteQueuedSavingsPlanResponse"
    "fixture/DeleteQueuedSavingsPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueuedSavingsPlan)

responseCreateSavingsPlan :: CreateSavingsPlanResponse -> TestTree
responseCreateSavingsPlan =
  res
    "CreateSavingsPlanResponse"
    "fixture/CreateSavingsPlanResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSavingsPlan)

responseDescribeSavingsPlansOfferings :: DescribeSavingsPlansOfferingsResponse -> TestTree
responseDescribeSavingsPlansOfferings =
  res
    "DescribeSavingsPlansOfferingsResponse"
    "fixture/DescribeSavingsPlansOfferingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlansOfferings)

responseDescribeSavingsPlans :: DescribeSavingsPlansResponse -> TestTree
responseDescribeSavingsPlans =
  res
    "DescribeSavingsPlansResponse"
    "fixture/DescribeSavingsPlansResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlans)

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

responseDescribeSavingsPlansOfferingRates :: DescribeSavingsPlansOfferingRatesResponse -> TestTree
responseDescribeSavingsPlansOfferingRates =
  res
    "DescribeSavingsPlansOfferingRatesResponse"
    "fixture/DescribeSavingsPlansOfferingRatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSavingsPlansOfferingRates)
