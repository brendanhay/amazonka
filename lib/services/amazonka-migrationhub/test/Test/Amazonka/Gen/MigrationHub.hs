{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHub
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MigrationHub where

import Amazonka.MigrationHub
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MigrationHub.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateCreatedArtifact $
--             newAssociateCreatedArtifact
--
--         , requestAssociateDiscoveredResource $
--             newAssociateDiscoveredResource
--
--         , requestCreateProgressUpdateStream $
--             newCreateProgressUpdateStream
--
--         , requestDeleteProgressUpdateStream $
--             newDeleteProgressUpdateStream
--
--         , requestDescribeApplicationState $
--             newDescribeApplicationState
--
--         , requestDescribeMigrationTask $
--             newDescribeMigrationTask
--
--         , requestDisassociateCreatedArtifact $
--             newDisassociateCreatedArtifact
--
--         , requestDisassociateDiscoveredResource $
--             newDisassociateDiscoveredResource
--
--         , requestImportMigrationTask $
--             newImportMigrationTask
--
--         , requestListApplicationStates $
--             newListApplicationStates
--
--         , requestListCreatedArtifacts $
--             newListCreatedArtifacts
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestListMigrationTasks $
--             newListMigrationTasks
--
--         , requestListProgressUpdateStreams $
--             newListProgressUpdateStreams
--
--         , requestNotifyApplicationState $
--             newNotifyApplicationState
--
--         , requestNotifyMigrationTaskState $
--             newNotifyMigrationTaskState
--
--         , requestPutResourceAttributes $
--             newPutResourceAttributes
--
--           ]

--     , testGroup "response"
--         [ responseAssociateCreatedArtifact $
--             newAssociateCreatedArtifactResponse
--
--         , responseAssociateDiscoveredResource $
--             newAssociateDiscoveredResourceResponse
--
--         , responseCreateProgressUpdateStream $
--             newCreateProgressUpdateStreamResponse
--
--         , responseDeleteProgressUpdateStream $
--             newDeleteProgressUpdateStreamResponse
--
--         , responseDescribeApplicationState $
--             newDescribeApplicationStateResponse
--
--         , responseDescribeMigrationTask $
--             newDescribeMigrationTaskResponse
--
--         , responseDisassociateCreatedArtifact $
--             newDisassociateCreatedArtifactResponse
--
--         , responseDisassociateDiscoveredResource $
--             newDisassociateDiscoveredResourceResponse
--
--         , responseImportMigrationTask $
--             newImportMigrationTaskResponse
--
--         , responseListApplicationStates $
--             newListApplicationStatesResponse
--
--         , responseListCreatedArtifacts $
--             newListCreatedArtifactsResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseListMigrationTasks $
--             newListMigrationTasksResponse
--
--         , responseListProgressUpdateStreams $
--             newListProgressUpdateStreamsResponse
--
--         , responseNotifyApplicationState $
--             newNotifyApplicationStateResponse
--
--         , responseNotifyMigrationTaskState $
--             newNotifyMigrationTaskStateResponse
--
--         , responsePutResourceAttributes $
--             newPutResourceAttributesResponse
--
--           ]
--     ]

-- Requests

requestAssociateCreatedArtifact :: AssociateCreatedArtifact -> TestTree
requestAssociateCreatedArtifact =
  req
    "AssociateCreatedArtifact"
    "fixture/AssociateCreatedArtifact.yaml"

requestAssociateDiscoveredResource :: AssociateDiscoveredResource -> TestTree
requestAssociateDiscoveredResource =
  req
    "AssociateDiscoveredResource"
    "fixture/AssociateDiscoveredResource.yaml"

requestCreateProgressUpdateStream :: CreateProgressUpdateStream -> TestTree
requestCreateProgressUpdateStream =
  req
    "CreateProgressUpdateStream"
    "fixture/CreateProgressUpdateStream.yaml"

requestDeleteProgressUpdateStream :: DeleteProgressUpdateStream -> TestTree
requestDeleteProgressUpdateStream =
  req
    "DeleteProgressUpdateStream"
    "fixture/DeleteProgressUpdateStream.yaml"

requestDescribeApplicationState :: DescribeApplicationState -> TestTree
requestDescribeApplicationState =
  req
    "DescribeApplicationState"
    "fixture/DescribeApplicationState.yaml"

requestDescribeMigrationTask :: DescribeMigrationTask -> TestTree
requestDescribeMigrationTask =
  req
    "DescribeMigrationTask"
    "fixture/DescribeMigrationTask.yaml"

requestDisassociateCreatedArtifact :: DisassociateCreatedArtifact -> TestTree
requestDisassociateCreatedArtifact =
  req
    "DisassociateCreatedArtifact"
    "fixture/DisassociateCreatedArtifact.yaml"

requestDisassociateDiscoveredResource :: DisassociateDiscoveredResource -> TestTree
requestDisassociateDiscoveredResource =
  req
    "DisassociateDiscoveredResource"
    "fixture/DisassociateDiscoveredResource.yaml"

requestImportMigrationTask :: ImportMigrationTask -> TestTree
requestImportMigrationTask =
  req
    "ImportMigrationTask"
    "fixture/ImportMigrationTask.yaml"

requestListApplicationStates :: ListApplicationStates -> TestTree
requestListApplicationStates =
  req
    "ListApplicationStates"
    "fixture/ListApplicationStates.yaml"

requestListCreatedArtifacts :: ListCreatedArtifacts -> TestTree
requestListCreatedArtifacts =
  req
    "ListCreatedArtifacts"
    "fixture/ListCreatedArtifacts.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestListMigrationTasks :: ListMigrationTasks -> TestTree
requestListMigrationTasks =
  req
    "ListMigrationTasks"
    "fixture/ListMigrationTasks.yaml"

requestListProgressUpdateStreams :: ListProgressUpdateStreams -> TestTree
requestListProgressUpdateStreams =
  req
    "ListProgressUpdateStreams"
    "fixture/ListProgressUpdateStreams.yaml"

requestNotifyApplicationState :: NotifyApplicationState -> TestTree
requestNotifyApplicationState =
  req
    "NotifyApplicationState"
    "fixture/NotifyApplicationState.yaml"

requestNotifyMigrationTaskState :: NotifyMigrationTaskState -> TestTree
requestNotifyMigrationTaskState =
  req
    "NotifyMigrationTaskState"
    "fixture/NotifyMigrationTaskState.yaml"

requestPutResourceAttributes :: PutResourceAttributes -> TestTree
requestPutResourceAttributes =
  req
    "PutResourceAttributes"
    "fixture/PutResourceAttributes.yaml"

-- Responses

responseAssociateCreatedArtifact :: AssociateCreatedArtifactResponse -> TestTree
responseAssociateCreatedArtifact =
  res
    "AssociateCreatedArtifactResponse"
    "fixture/AssociateCreatedArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateCreatedArtifact)

responseAssociateDiscoveredResource :: AssociateDiscoveredResourceResponse -> TestTree
responseAssociateDiscoveredResource =
  res
    "AssociateDiscoveredResourceResponse"
    "fixture/AssociateDiscoveredResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDiscoveredResource)

responseCreateProgressUpdateStream :: CreateProgressUpdateStreamResponse -> TestTree
responseCreateProgressUpdateStream =
  res
    "CreateProgressUpdateStreamResponse"
    "fixture/CreateProgressUpdateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProgressUpdateStream)

responseDeleteProgressUpdateStream :: DeleteProgressUpdateStreamResponse -> TestTree
responseDeleteProgressUpdateStream =
  res
    "DeleteProgressUpdateStreamResponse"
    "fixture/DeleteProgressUpdateStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProgressUpdateStream)

responseDescribeApplicationState :: DescribeApplicationStateResponse -> TestTree
responseDescribeApplicationState =
  res
    "DescribeApplicationStateResponse"
    "fixture/DescribeApplicationStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationState)

responseDescribeMigrationTask :: DescribeMigrationTaskResponse -> TestTree
responseDescribeMigrationTask =
  res
    "DescribeMigrationTaskResponse"
    "fixture/DescribeMigrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMigrationTask)

responseDisassociateCreatedArtifact :: DisassociateCreatedArtifactResponse -> TestTree
responseDisassociateCreatedArtifact =
  res
    "DisassociateCreatedArtifactResponse"
    "fixture/DisassociateCreatedArtifactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateCreatedArtifact)

responseDisassociateDiscoveredResource :: DisassociateDiscoveredResourceResponse -> TestTree
responseDisassociateDiscoveredResource =
  res
    "DisassociateDiscoveredResourceResponse"
    "fixture/DisassociateDiscoveredResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDiscoveredResource)

responseImportMigrationTask :: ImportMigrationTaskResponse -> TestTree
responseImportMigrationTask =
  res
    "ImportMigrationTaskResponse"
    "fixture/ImportMigrationTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportMigrationTask)

responseListApplicationStates :: ListApplicationStatesResponse -> TestTree
responseListApplicationStates =
  res
    "ListApplicationStatesResponse"
    "fixture/ListApplicationStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationStates)

responseListCreatedArtifacts :: ListCreatedArtifactsResponse -> TestTree
responseListCreatedArtifacts =
  res
    "ListCreatedArtifactsResponse"
    "fixture/ListCreatedArtifactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCreatedArtifacts)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDiscoveredResources)

responseListMigrationTasks :: ListMigrationTasksResponse -> TestTree
responseListMigrationTasks =
  res
    "ListMigrationTasksResponse"
    "fixture/ListMigrationTasksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMigrationTasks)

responseListProgressUpdateStreams :: ListProgressUpdateStreamsResponse -> TestTree
responseListProgressUpdateStreams =
  res
    "ListProgressUpdateStreamsResponse"
    "fixture/ListProgressUpdateStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProgressUpdateStreams)

responseNotifyApplicationState :: NotifyApplicationStateResponse -> TestTree
responseNotifyApplicationState =
  res
    "NotifyApplicationStateResponse"
    "fixture/NotifyApplicationStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyApplicationState)

responseNotifyMigrationTaskState :: NotifyMigrationTaskStateResponse -> TestTree
responseNotifyMigrationTaskState =
  res
    "NotifyMigrationTaskStateResponse"
    "fixture/NotifyMigrationTaskStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy NotifyMigrationTaskState)

responsePutResourceAttributes :: PutResourceAttributesResponse -> TestTree
responsePutResourceAttributes =
  res
    "PutResourceAttributesResponse"
    "fixture/PutResourceAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourceAttributes)
