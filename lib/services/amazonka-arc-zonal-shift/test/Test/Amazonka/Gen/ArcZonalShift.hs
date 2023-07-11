{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ArcZonalShift
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ArcZonalShift where

import Amazonka.ArcZonalShift
import qualified Data.Proxy as Proxy
import Test.Amazonka.ArcZonalShift.Internal
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
--         [ requestCancelZonalShift $
--             newCancelZonalShift
--
--         , requestGetManagedResource $
--             newGetManagedResource
--
--         , requestListManagedResources $
--             newListManagedResources
--
--         , requestListZonalShifts $
--             newListZonalShifts
--
--         , requestStartZonalShift $
--             newStartZonalShift
--
--         , requestUpdateZonalShift $
--             newUpdateZonalShift
--
--           ]

--     , testGroup "response"
--         [ responseCancelZonalShift $
--             newZonalShift
--
--         , responseGetManagedResource $
--             newGetManagedResourceResponse
--
--         , responseListManagedResources $
--             newListManagedResourcesResponse
--
--         , responseListZonalShifts $
--             newListZonalShiftsResponse
--
--         , responseStartZonalShift $
--             newZonalShift
--
--         , responseUpdateZonalShift $
--             newZonalShift
--
--           ]
--     ]

-- Requests

requestCancelZonalShift :: CancelZonalShift -> TestTree
requestCancelZonalShift =
  req
    "CancelZonalShift"
    "fixture/CancelZonalShift.yaml"

requestGetManagedResource :: GetManagedResource -> TestTree
requestGetManagedResource =
  req
    "GetManagedResource"
    "fixture/GetManagedResource.yaml"

requestListManagedResources :: ListManagedResources -> TestTree
requestListManagedResources =
  req
    "ListManagedResources"
    "fixture/ListManagedResources.yaml"

requestListZonalShifts :: ListZonalShifts -> TestTree
requestListZonalShifts =
  req
    "ListZonalShifts"
    "fixture/ListZonalShifts.yaml"

requestStartZonalShift :: StartZonalShift -> TestTree
requestStartZonalShift =
  req
    "StartZonalShift"
    "fixture/StartZonalShift.yaml"

requestUpdateZonalShift :: UpdateZonalShift -> TestTree
requestUpdateZonalShift =
  req
    "UpdateZonalShift"
    "fixture/UpdateZonalShift.yaml"

-- Responses

responseCancelZonalShift :: ZonalShift -> TestTree
responseCancelZonalShift =
  res
    "CancelZonalShiftResponse"
    "fixture/CancelZonalShiftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelZonalShift)

responseGetManagedResource :: GetManagedResourceResponse -> TestTree
responseGetManagedResource =
  res
    "GetManagedResourceResponse"
    "fixture/GetManagedResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetManagedResource)

responseListManagedResources :: ListManagedResourcesResponse -> TestTree
responseListManagedResources =
  res
    "ListManagedResourcesResponse"
    "fixture/ListManagedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListManagedResources)

responseListZonalShifts :: ListZonalShiftsResponse -> TestTree
responseListZonalShifts =
  res
    "ListZonalShiftsResponse"
    "fixture/ListZonalShiftsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListZonalShifts)

responseStartZonalShift :: ZonalShift -> TestTree
responseStartZonalShift =
  res
    "StartZonalShiftResponse"
    "fixture/StartZonalShiftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartZonalShift)

responseUpdateZonalShift :: ZonalShift -> TestTree
responseUpdateZonalShift =
  res
    "UpdateZonalShiftResponse"
    "fixture/UpdateZonalShiftResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateZonalShift)
