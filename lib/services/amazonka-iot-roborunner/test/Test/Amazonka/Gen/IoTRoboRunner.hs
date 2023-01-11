{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTRoboRunner
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTRoboRunner where

import Amazonka.IoTRoboRunner
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTRoboRunner.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateDestination $
--             newCreateDestination
--
--         , requestCreateSite $
--             newCreateSite
--
--         , requestCreateWorker $
--             newCreateWorker
--
--         , requestCreateWorkerFleet $
--             newCreateWorkerFleet
--
--         , requestDeleteDestination $
--             newDeleteDestination
--
--         , requestDeleteSite $
--             newDeleteSite
--
--         , requestDeleteWorker $
--             newDeleteWorker
--
--         , requestDeleteWorkerFleet $
--             newDeleteWorkerFleet
--
--         , requestGetDestination $
--             newGetDestination
--
--         , requestGetSite $
--             newGetSite
--
--         , requestGetWorker $
--             newGetWorker
--
--         , requestGetWorkerFleet $
--             newGetWorkerFleet
--
--         , requestListDestinations $
--             newListDestinations
--
--         , requestListSites $
--             newListSites
--
--         , requestListWorkerFleets $
--             newListWorkerFleets
--
--         , requestListWorkers $
--             newListWorkers
--
--         , requestUpdateDestination $
--             newUpdateDestination
--
--         , requestUpdateSite $
--             newUpdateSite
--
--         , requestUpdateWorker $
--             newUpdateWorker
--
--         , requestUpdateWorkerFleet $
--             newUpdateWorkerFleet
--
--           ]

--     , testGroup "response"
--         [ responseCreateDestination $
--             newCreateDestinationResponse
--
--         , responseCreateSite $
--             newCreateSiteResponse
--
--         , responseCreateWorker $
--             newCreateWorkerResponse
--
--         , responseCreateWorkerFleet $
--             newCreateWorkerFleetResponse
--
--         , responseDeleteDestination $
--             newDeleteDestinationResponse
--
--         , responseDeleteSite $
--             newDeleteSiteResponse
--
--         , responseDeleteWorker $
--             newDeleteWorkerResponse
--
--         , responseDeleteWorkerFleet $
--             newDeleteWorkerFleetResponse
--
--         , responseGetDestination $
--             newGetDestinationResponse
--
--         , responseGetSite $
--             newGetSiteResponse
--
--         , responseGetWorker $
--             newGetWorkerResponse
--
--         , responseGetWorkerFleet $
--             newGetWorkerFleetResponse
--
--         , responseListDestinations $
--             newListDestinationsResponse
--
--         , responseListSites $
--             newListSitesResponse
--
--         , responseListWorkerFleets $
--             newListWorkerFleetsResponse
--
--         , responseListWorkers $
--             newListWorkersResponse
--
--         , responseUpdateDestination $
--             newUpdateDestinationResponse
--
--         , responseUpdateSite $
--             newUpdateSiteResponse
--
--         , responseUpdateWorker $
--             newUpdateWorkerResponse
--
--         , responseUpdateWorkerFleet $
--             newUpdateWorkerFleetResponse
--
--           ]
--     ]

-- Requests

requestCreateDestination :: CreateDestination -> TestTree
requestCreateDestination =
  req
    "CreateDestination"
    "fixture/CreateDestination.yaml"

requestCreateSite :: CreateSite -> TestTree
requestCreateSite =
  req
    "CreateSite"
    "fixture/CreateSite.yaml"

requestCreateWorker :: CreateWorker -> TestTree
requestCreateWorker =
  req
    "CreateWorker"
    "fixture/CreateWorker.yaml"

requestCreateWorkerFleet :: CreateWorkerFleet -> TestTree
requestCreateWorkerFleet =
  req
    "CreateWorkerFleet"
    "fixture/CreateWorkerFleet.yaml"

requestDeleteDestination :: DeleteDestination -> TestTree
requestDeleteDestination =
  req
    "DeleteDestination"
    "fixture/DeleteDestination.yaml"

requestDeleteSite :: DeleteSite -> TestTree
requestDeleteSite =
  req
    "DeleteSite"
    "fixture/DeleteSite.yaml"

requestDeleteWorker :: DeleteWorker -> TestTree
requestDeleteWorker =
  req
    "DeleteWorker"
    "fixture/DeleteWorker.yaml"

requestDeleteWorkerFleet :: DeleteWorkerFleet -> TestTree
requestDeleteWorkerFleet =
  req
    "DeleteWorkerFleet"
    "fixture/DeleteWorkerFleet.yaml"

requestGetDestination :: GetDestination -> TestTree
requestGetDestination =
  req
    "GetDestination"
    "fixture/GetDestination.yaml"

requestGetSite :: GetSite -> TestTree
requestGetSite =
  req
    "GetSite"
    "fixture/GetSite.yaml"

requestGetWorker :: GetWorker -> TestTree
requestGetWorker =
  req
    "GetWorker"
    "fixture/GetWorker.yaml"

requestGetWorkerFleet :: GetWorkerFleet -> TestTree
requestGetWorkerFleet =
  req
    "GetWorkerFleet"
    "fixture/GetWorkerFleet.yaml"

requestListDestinations :: ListDestinations -> TestTree
requestListDestinations =
  req
    "ListDestinations"
    "fixture/ListDestinations.yaml"

requestListSites :: ListSites -> TestTree
requestListSites =
  req
    "ListSites"
    "fixture/ListSites.yaml"

requestListWorkerFleets :: ListWorkerFleets -> TestTree
requestListWorkerFleets =
  req
    "ListWorkerFleets"
    "fixture/ListWorkerFleets.yaml"

requestListWorkers :: ListWorkers -> TestTree
requestListWorkers =
  req
    "ListWorkers"
    "fixture/ListWorkers.yaml"

requestUpdateDestination :: UpdateDestination -> TestTree
requestUpdateDestination =
  req
    "UpdateDestination"
    "fixture/UpdateDestination.yaml"

requestUpdateSite :: UpdateSite -> TestTree
requestUpdateSite =
  req
    "UpdateSite"
    "fixture/UpdateSite.yaml"

requestUpdateWorker :: UpdateWorker -> TestTree
requestUpdateWorker =
  req
    "UpdateWorker"
    "fixture/UpdateWorker.yaml"

requestUpdateWorkerFleet :: UpdateWorkerFleet -> TestTree
requestUpdateWorkerFleet =
  req
    "UpdateWorkerFleet"
    "fixture/UpdateWorkerFleet.yaml"

-- Responses

responseCreateDestination :: CreateDestinationResponse -> TestTree
responseCreateDestination =
  res
    "CreateDestinationResponse"
    "fixture/CreateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDestination)

responseCreateSite :: CreateSiteResponse -> TestTree
responseCreateSite =
  res
    "CreateSiteResponse"
    "fixture/CreateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSite)

responseCreateWorker :: CreateWorkerResponse -> TestTree
responseCreateWorker =
  res
    "CreateWorkerResponse"
    "fixture/CreateWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorker)

responseCreateWorkerFleet :: CreateWorkerFleetResponse -> TestTree
responseCreateWorkerFleet =
  res
    "CreateWorkerFleetResponse"
    "fixture/CreateWorkerFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkerFleet)

responseDeleteDestination :: DeleteDestinationResponse -> TestTree
responseDeleteDestination =
  res
    "DeleteDestinationResponse"
    "fixture/DeleteDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDestination)

responseDeleteSite :: DeleteSiteResponse -> TestTree
responseDeleteSite =
  res
    "DeleteSiteResponse"
    "fixture/DeleteSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSite)

responseDeleteWorker :: DeleteWorkerResponse -> TestTree
responseDeleteWorker =
  res
    "DeleteWorkerResponse"
    "fixture/DeleteWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorker)

responseDeleteWorkerFleet :: DeleteWorkerFleetResponse -> TestTree
responseDeleteWorkerFleet =
  res
    "DeleteWorkerFleetResponse"
    "fixture/DeleteWorkerFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkerFleet)

responseGetDestination :: GetDestinationResponse -> TestTree
responseGetDestination =
  res
    "GetDestinationResponse"
    "fixture/GetDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDestination)

responseGetSite :: GetSiteResponse -> TestTree
responseGetSite =
  res
    "GetSiteResponse"
    "fixture/GetSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSite)

responseGetWorker :: GetWorkerResponse -> TestTree
responseGetWorker =
  res
    "GetWorkerResponse"
    "fixture/GetWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorker)

responseGetWorkerFleet :: GetWorkerFleetResponse -> TestTree
responseGetWorkerFleet =
  res
    "GetWorkerFleetResponse"
    "fixture/GetWorkerFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkerFleet)

responseListDestinations :: ListDestinationsResponse -> TestTree
responseListDestinations =
  res
    "ListDestinationsResponse"
    "fixture/ListDestinationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDestinations)

responseListSites :: ListSitesResponse -> TestTree
responseListSites =
  res
    "ListSitesResponse"
    "fixture/ListSitesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSites)

responseListWorkerFleets :: ListWorkerFleetsResponse -> TestTree
responseListWorkerFleets =
  res
    "ListWorkerFleetsResponse"
    "fixture/ListWorkerFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkerFleets)

responseListWorkers :: ListWorkersResponse -> TestTree
responseListWorkers =
  res
    "ListWorkersResponse"
    "fixture/ListWorkersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkers)

responseUpdateDestination :: UpdateDestinationResponse -> TestTree
responseUpdateDestination =
  res
    "UpdateDestinationResponse"
    "fixture/UpdateDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDestination)

responseUpdateSite :: UpdateSiteResponse -> TestTree
responseUpdateSite =
  res
    "UpdateSiteResponse"
    "fixture/UpdateSiteResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSite)

responseUpdateWorker :: UpdateWorkerResponse -> TestTree
responseUpdateWorker =
  res
    "UpdateWorkerResponse"
    "fixture/UpdateWorkerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorker)

responseUpdateWorkerFleet :: UpdateWorkerFleetResponse -> TestTree
responseUpdateWorkerFleet =
  res
    "UpdateWorkerFleetResponse"
    "fixture/UpdateWorkerFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkerFleet)
