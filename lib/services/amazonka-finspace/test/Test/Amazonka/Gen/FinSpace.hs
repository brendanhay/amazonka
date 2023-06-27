{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.FinSpace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.FinSpace where

import Amazonka.FinSpace
import qualified Data.Proxy as Proxy
import Test.Amazonka.FinSpace.Internal
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
--         [ requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateKxChangeset $
--             newCreateKxChangeset
--
--         , requestCreateKxCluster $
--             newCreateKxCluster
--
--         , requestCreateKxDatabase $
--             newCreateKxDatabase
--
--         , requestCreateKxEnvironment $
--             newCreateKxEnvironment
--
--         , requestCreateKxUser $
--             newCreateKxUser
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDeleteKxCluster $
--             newDeleteKxCluster
--
--         , requestDeleteKxDatabase $
--             newDeleteKxDatabase
--
--         , requestDeleteKxEnvironment $
--             newDeleteKxEnvironment
--
--         , requestDeleteKxUser $
--             newDeleteKxUser
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestGetKxChangeset $
--             newGetKxChangeset
--
--         , requestGetKxCluster $
--             newGetKxCluster
--
--         , requestGetKxConnectionString $
--             newGetKxConnectionString
--
--         , requestGetKxDatabase $
--             newGetKxDatabase
--
--         , requestGetKxEnvironment $
--             newGetKxEnvironment
--
--         , requestGetKxUser $
--             newGetKxUser
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListKxChangesets $
--             newListKxChangesets
--
--         , requestListKxClusterNodes $
--             newListKxClusterNodes
--
--         , requestListKxClusters $
--             newListKxClusters
--
--         , requestListKxDatabases $
--             newListKxDatabases
--
--         , requestListKxEnvironments $
--             newListKxEnvironments
--
--         , requestListKxUsers $
--             newListKxUsers
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
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestUpdateKxClusterDatabases $
--             newUpdateKxClusterDatabases
--
--         , requestUpdateKxDatabase $
--             newUpdateKxDatabase
--
--         , requestUpdateKxEnvironment $
--             newUpdateKxEnvironment
--
--         , requestUpdateKxEnvironmentNetwork $
--             newUpdateKxEnvironmentNetwork
--
--         , requestUpdateKxUser $
--             newUpdateKxUser
--
--           ]

--     , testGroup "response"
--         [ responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseCreateKxChangeset $
--             newCreateKxChangesetResponse
--
--         , responseCreateKxCluster $
--             newCreateKxClusterResponse
--
--         , responseCreateKxDatabase $
--             newCreateKxDatabaseResponse
--
--         , responseCreateKxEnvironment $
--             newCreateKxEnvironmentResponse
--
--         , responseCreateKxUser $
--             newCreateKxUserResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDeleteKxCluster $
--             newDeleteKxClusterResponse
--
--         , responseDeleteKxDatabase $
--             newDeleteKxDatabaseResponse
--
--         , responseDeleteKxEnvironment $
--             newDeleteKxEnvironmentResponse
--
--         , responseDeleteKxUser $
--             newDeleteKxUserResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseGetKxChangeset $
--             newGetKxChangesetResponse
--
--         , responseGetKxCluster $
--             newGetKxClusterResponse
--
--         , responseGetKxConnectionString $
--             newGetKxConnectionStringResponse
--
--         , responseGetKxDatabase $
--             newGetKxDatabaseResponse
--
--         , responseGetKxEnvironment $
--             newGetKxEnvironmentResponse
--
--         , responseGetKxUser $
--             newGetKxUserResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListKxChangesets $
--             newListKxChangesetsResponse
--
--         , responseListKxClusterNodes $
--             newListKxClusterNodesResponse
--
--         , responseListKxClusters $
--             newListKxClustersResponse
--
--         , responseListKxDatabases $
--             newListKxDatabasesResponse
--
--         , responseListKxEnvironments $
--             newListKxEnvironmentsResponse
--
--         , responseListKxUsers $
--             newListKxUsersResponse
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
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--         , responseUpdateKxClusterDatabases $
--             newUpdateKxClusterDatabasesResponse
--
--         , responseUpdateKxDatabase $
--             newUpdateKxDatabaseResponse
--
--         , responseUpdateKxEnvironment $
--             newUpdateKxEnvironmentResponse
--
--         , responseUpdateKxEnvironmentNetwork $
--             newUpdateKxEnvironmentNetworkResponse
--
--         , responseUpdateKxUser $
--             newUpdateKxUserResponse
--
--           ]
--     ]

-- Requests

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateKxChangeset :: CreateKxChangeset -> TestTree
requestCreateKxChangeset =
  req
    "CreateKxChangeset"
    "fixture/CreateKxChangeset.yaml"

requestCreateKxCluster :: CreateKxCluster -> TestTree
requestCreateKxCluster =
  req
    "CreateKxCluster"
    "fixture/CreateKxCluster.yaml"

requestCreateKxDatabase :: CreateKxDatabase -> TestTree
requestCreateKxDatabase =
  req
    "CreateKxDatabase"
    "fixture/CreateKxDatabase.yaml"

requestCreateKxEnvironment :: CreateKxEnvironment -> TestTree
requestCreateKxEnvironment =
  req
    "CreateKxEnvironment"
    "fixture/CreateKxEnvironment.yaml"

requestCreateKxUser :: CreateKxUser -> TestTree
requestCreateKxUser =
  req
    "CreateKxUser"
    "fixture/CreateKxUser.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestDeleteKxCluster :: DeleteKxCluster -> TestTree
requestDeleteKxCluster =
  req
    "DeleteKxCluster"
    "fixture/DeleteKxCluster.yaml"

requestDeleteKxDatabase :: DeleteKxDatabase -> TestTree
requestDeleteKxDatabase =
  req
    "DeleteKxDatabase"
    "fixture/DeleteKxDatabase.yaml"

requestDeleteKxEnvironment :: DeleteKxEnvironment -> TestTree
requestDeleteKxEnvironment =
  req
    "DeleteKxEnvironment"
    "fixture/DeleteKxEnvironment.yaml"

requestDeleteKxUser :: DeleteKxUser -> TestTree
requestDeleteKxUser =
  req
    "DeleteKxUser"
    "fixture/DeleteKxUser.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestGetKxChangeset :: GetKxChangeset -> TestTree
requestGetKxChangeset =
  req
    "GetKxChangeset"
    "fixture/GetKxChangeset.yaml"

requestGetKxCluster :: GetKxCluster -> TestTree
requestGetKxCluster =
  req
    "GetKxCluster"
    "fixture/GetKxCluster.yaml"

requestGetKxConnectionString :: GetKxConnectionString -> TestTree
requestGetKxConnectionString =
  req
    "GetKxConnectionString"
    "fixture/GetKxConnectionString.yaml"

requestGetKxDatabase :: GetKxDatabase -> TestTree
requestGetKxDatabase =
  req
    "GetKxDatabase"
    "fixture/GetKxDatabase.yaml"

requestGetKxEnvironment :: GetKxEnvironment -> TestTree
requestGetKxEnvironment =
  req
    "GetKxEnvironment"
    "fixture/GetKxEnvironment.yaml"

requestGetKxUser :: GetKxUser -> TestTree
requestGetKxUser =
  req
    "GetKxUser"
    "fixture/GetKxUser.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListKxChangesets :: ListKxChangesets -> TestTree
requestListKxChangesets =
  req
    "ListKxChangesets"
    "fixture/ListKxChangesets.yaml"

requestListKxClusterNodes :: ListKxClusterNodes -> TestTree
requestListKxClusterNodes =
  req
    "ListKxClusterNodes"
    "fixture/ListKxClusterNodes.yaml"

requestListKxClusters :: ListKxClusters -> TestTree
requestListKxClusters =
  req
    "ListKxClusters"
    "fixture/ListKxClusters.yaml"

requestListKxDatabases :: ListKxDatabases -> TestTree
requestListKxDatabases =
  req
    "ListKxDatabases"
    "fixture/ListKxDatabases.yaml"

requestListKxEnvironments :: ListKxEnvironments -> TestTree
requestListKxEnvironments =
  req
    "ListKxEnvironments"
    "fixture/ListKxEnvironments.yaml"

requestListKxUsers :: ListKxUsers -> TestTree
requestListKxUsers =
  req
    "ListKxUsers"
    "fixture/ListKxUsers.yaml"

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

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestUpdateKxClusterDatabases :: UpdateKxClusterDatabases -> TestTree
requestUpdateKxClusterDatabases =
  req
    "UpdateKxClusterDatabases"
    "fixture/UpdateKxClusterDatabases.yaml"

requestUpdateKxDatabase :: UpdateKxDatabase -> TestTree
requestUpdateKxDatabase =
  req
    "UpdateKxDatabase"
    "fixture/UpdateKxDatabase.yaml"

requestUpdateKxEnvironment :: UpdateKxEnvironment -> TestTree
requestUpdateKxEnvironment =
  req
    "UpdateKxEnvironment"
    "fixture/UpdateKxEnvironment.yaml"

requestUpdateKxEnvironmentNetwork :: UpdateKxEnvironmentNetwork -> TestTree
requestUpdateKxEnvironmentNetwork =
  req
    "UpdateKxEnvironmentNetwork"
    "fixture/UpdateKxEnvironmentNetwork.yaml"

requestUpdateKxUser :: UpdateKxUser -> TestTree
requestUpdateKxUser =
  req
    "UpdateKxUser"
    "fixture/UpdateKxUser.yaml"

-- Responses

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateKxChangeset :: CreateKxChangesetResponse -> TestTree
responseCreateKxChangeset =
  res
    "CreateKxChangesetResponse"
    "fixture/CreateKxChangesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKxChangeset)

responseCreateKxCluster :: CreateKxClusterResponse -> TestTree
responseCreateKxCluster =
  res
    "CreateKxClusterResponse"
    "fixture/CreateKxClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKxCluster)

responseCreateKxDatabase :: CreateKxDatabaseResponse -> TestTree
responseCreateKxDatabase =
  res
    "CreateKxDatabaseResponse"
    "fixture/CreateKxDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKxDatabase)

responseCreateKxEnvironment :: CreateKxEnvironmentResponse -> TestTree
responseCreateKxEnvironment =
  res
    "CreateKxEnvironmentResponse"
    "fixture/CreateKxEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKxEnvironment)

responseCreateKxUser :: CreateKxUserResponse -> TestTree
responseCreateKxUser =
  res
    "CreateKxUserResponse"
    "fixture/CreateKxUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKxUser)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseDeleteKxCluster :: DeleteKxClusterResponse -> TestTree
responseDeleteKxCluster =
  res
    "DeleteKxClusterResponse"
    "fixture/DeleteKxClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKxCluster)

responseDeleteKxDatabase :: DeleteKxDatabaseResponse -> TestTree
responseDeleteKxDatabase =
  res
    "DeleteKxDatabaseResponse"
    "fixture/DeleteKxDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKxDatabase)

responseDeleteKxEnvironment :: DeleteKxEnvironmentResponse -> TestTree
responseDeleteKxEnvironment =
  res
    "DeleteKxEnvironmentResponse"
    "fixture/DeleteKxEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKxEnvironment)

responseDeleteKxUser :: DeleteKxUserResponse -> TestTree
responseDeleteKxUser =
  res
    "DeleteKxUserResponse"
    "fixture/DeleteKxUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKxUser)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseGetKxChangeset :: GetKxChangesetResponse -> TestTree
responseGetKxChangeset =
  res
    "GetKxChangesetResponse"
    "fixture/GetKxChangesetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxChangeset)

responseGetKxCluster :: GetKxClusterResponse -> TestTree
responseGetKxCluster =
  res
    "GetKxClusterResponse"
    "fixture/GetKxClusterResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxCluster)

responseGetKxConnectionString :: GetKxConnectionStringResponse -> TestTree
responseGetKxConnectionString =
  res
    "GetKxConnectionStringResponse"
    "fixture/GetKxConnectionStringResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxConnectionString)

responseGetKxDatabase :: GetKxDatabaseResponse -> TestTree
responseGetKxDatabase =
  res
    "GetKxDatabaseResponse"
    "fixture/GetKxDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxDatabase)

responseGetKxEnvironment :: GetKxEnvironmentResponse -> TestTree
responseGetKxEnvironment =
  res
    "GetKxEnvironmentResponse"
    "fixture/GetKxEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxEnvironment)

responseGetKxUser :: GetKxUserResponse -> TestTree
responseGetKxUser =
  res
    "GetKxUserResponse"
    "fixture/GetKxUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKxUser)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListKxChangesets :: ListKxChangesetsResponse -> TestTree
responseListKxChangesets =
  res
    "ListKxChangesetsResponse"
    "fixture/ListKxChangesetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxChangesets)

responseListKxClusterNodes :: ListKxClusterNodesResponse -> TestTree
responseListKxClusterNodes =
  res
    "ListKxClusterNodesResponse"
    "fixture/ListKxClusterNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxClusterNodes)

responseListKxClusters :: ListKxClustersResponse -> TestTree
responseListKxClusters =
  res
    "ListKxClustersResponse"
    "fixture/ListKxClustersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxClusters)

responseListKxDatabases :: ListKxDatabasesResponse -> TestTree
responseListKxDatabases =
  res
    "ListKxDatabasesResponse"
    "fixture/ListKxDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxDatabases)

responseListKxEnvironments :: ListKxEnvironmentsResponse -> TestTree
responseListKxEnvironments =
  res
    "ListKxEnvironmentsResponse"
    "fixture/ListKxEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxEnvironments)

responseListKxUsers :: ListKxUsersResponse -> TestTree
responseListKxUsers =
  res
    "ListKxUsersResponse"
    "fixture/ListKxUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListKxUsers)

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

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseUpdateKxClusterDatabases :: UpdateKxClusterDatabasesResponse -> TestTree
responseUpdateKxClusterDatabases =
  res
    "UpdateKxClusterDatabasesResponse"
    "fixture/UpdateKxClusterDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKxClusterDatabases)

responseUpdateKxDatabase :: UpdateKxDatabaseResponse -> TestTree
responseUpdateKxDatabase =
  res
    "UpdateKxDatabaseResponse"
    "fixture/UpdateKxDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKxDatabase)

responseUpdateKxEnvironment :: UpdateKxEnvironmentResponse -> TestTree
responseUpdateKxEnvironment =
  res
    "UpdateKxEnvironmentResponse"
    "fixture/UpdateKxEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKxEnvironment)

responseUpdateKxEnvironmentNetwork :: UpdateKxEnvironmentNetworkResponse -> TestTree
responseUpdateKxEnvironmentNetwork =
  res
    "UpdateKxEnvironmentNetworkResponse"
    "fixture/UpdateKxEnvironmentNetworkResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKxEnvironmentNetwork)

responseUpdateKxUser :: UpdateKxUserResponse -> TestTree
responseUpdateKxUser =
  res
    "UpdateKxUserResponse"
    "fixture/UpdateKxUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateKxUser)
