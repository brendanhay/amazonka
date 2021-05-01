{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MigrationHub
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MigrationHub where

import Data.Proxy
import Network.AWS.MigrationHub
import Test.AWS.Fixture
import Test.AWS.MigrationHub.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListCreatedArtifacts $
--             newListCreatedArtifacts
--
--         , requestDescribeMigrationTask $
--             newDescribeMigrationTask
--
--         , requestDisassociateCreatedArtifact $
--             newDisassociateCreatedArtifact
--
--         , requestImportMigrationTask $
--             newImportMigrationTask
--
--         , requestCreateProgressUpdateStream $
--             newCreateProgressUpdateStream
--
--         , requestPutResourceAttributes $
--             newPutResourceAttributes
--
--         , requestListDiscoveredResources $
--             newListDiscoveredResources
--
--         , requestDeleteProgressUpdateStream $
--             newDeleteProgressUpdateStream
--
--         , requestNotifyMigrationTaskState $
--             newNotifyMigrationTaskState
--
--         , requestDisassociateDiscoveredResource $
--             newDisassociateDiscoveredResource
--
--         , requestListApplicationStates $
--             newListApplicationStates
--
--         , requestAssociateDiscoveredResource $
--             newAssociateDiscoveredResource
--
--         , requestListProgressUpdateStreams $
--             newListProgressUpdateStreams
--
--         , requestNotifyApplicationState $
--             newNotifyApplicationState
--
--         , requestListMigrationTasks $
--             newListMigrationTasks
--
--         , requestDescribeApplicationState $
--             newDescribeApplicationState
--
--         , requestAssociateCreatedArtifact $
--             newAssociateCreatedArtifact
--
--           ]

--     , testGroup "response"
--         [ responseListCreatedArtifacts $
--             newListCreatedArtifactsResponse
--
--         , responseDescribeMigrationTask $
--             newDescribeMigrationTaskResponse
--
--         , responseDisassociateCreatedArtifact $
--             newDisassociateCreatedArtifactResponse
--
--         , responseImportMigrationTask $
--             newImportMigrationTaskResponse
--
--         , responseCreateProgressUpdateStream $
--             newCreateProgressUpdateStreamResponse
--
--         , responsePutResourceAttributes $
--             newPutResourceAttributesResponse
--
--         , responseListDiscoveredResources $
--             newListDiscoveredResourcesResponse
--
--         , responseDeleteProgressUpdateStream $
--             newDeleteProgressUpdateStreamResponse
--
--         , responseNotifyMigrationTaskState $
--             newNotifyMigrationTaskStateResponse
--
--         , responseDisassociateDiscoveredResource $
--             newDisassociateDiscoveredResourceResponse
--
--         , responseListApplicationStates $
--             newListApplicationStatesResponse
--
--         , responseAssociateDiscoveredResource $
--             newAssociateDiscoveredResourceResponse
--
--         , responseListProgressUpdateStreams $
--             newListProgressUpdateStreamsResponse
--
--         , responseNotifyApplicationState $
--             newNotifyApplicationStateResponse
--
--         , responseListMigrationTasks $
--             newListMigrationTasksResponse
--
--         , responseDescribeApplicationState $
--             newDescribeApplicationStateResponse
--
--         , responseAssociateCreatedArtifact $
--             newAssociateCreatedArtifactResponse
--
--           ]
--     ]

-- Requests

requestListCreatedArtifacts :: ListCreatedArtifacts -> TestTree
requestListCreatedArtifacts =
  req
    "ListCreatedArtifacts"
    "fixture/ListCreatedArtifacts.yaml"

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

requestImportMigrationTask :: ImportMigrationTask -> TestTree
requestImportMigrationTask =
  req
    "ImportMigrationTask"
    "fixture/ImportMigrationTask.yaml"

requestCreateProgressUpdateStream :: CreateProgressUpdateStream -> TestTree
requestCreateProgressUpdateStream =
  req
    "CreateProgressUpdateStream"
    "fixture/CreateProgressUpdateStream.yaml"

requestPutResourceAttributes :: PutResourceAttributes -> TestTree
requestPutResourceAttributes =
  req
    "PutResourceAttributes"
    "fixture/PutResourceAttributes.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

requestDeleteProgressUpdateStream :: DeleteProgressUpdateStream -> TestTree
requestDeleteProgressUpdateStream =
  req
    "DeleteProgressUpdateStream"
    "fixture/DeleteProgressUpdateStream.yaml"

requestNotifyMigrationTaskState :: NotifyMigrationTaskState -> TestTree
requestNotifyMigrationTaskState =
  req
    "NotifyMigrationTaskState"
    "fixture/NotifyMigrationTaskState.yaml"

requestDisassociateDiscoveredResource :: DisassociateDiscoveredResource -> TestTree
requestDisassociateDiscoveredResource =
  req
    "DisassociateDiscoveredResource"
    "fixture/DisassociateDiscoveredResource.yaml"

requestListApplicationStates :: ListApplicationStates -> TestTree
requestListApplicationStates =
  req
    "ListApplicationStates"
    "fixture/ListApplicationStates.yaml"

requestAssociateDiscoveredResource :: AssociateDiscoveredResource -> TestTree
requestAssociateDiscoveredResource =
  req
    "AssociateDiscoveredResource"
    "fixture/AssociateDiscoveredResource.yaml"

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

requestListMigrationTasks :: ListMigrationTasks -> TestTree
requestListMigrationTasks =
  req
    "ListMigrationTasks"
    "fixture/ListMigrationTasks.yaml"

requestDescribeApplicationState :: DescribeApplicationState -> TestTree
requestDescribeApplicationState =
  req
    "DescribeApplicationState"
    "fixture/DescribeApplicationState.yaml"

requestAssociateCreatedArtifact :: AssociateCreatedArtifact -> TestTree
requestAssociateCreatedArtifact =
  req
    "AssociateCreatedArtifact"
    "fixture/AssociateCreatedArtifact.yaml"

-- Responses

responseListCreatedArtifacts :: ListCreatedArtifactsResponse -> TestTree
responseListCreatedArtifacts =
  res
    "ListCreatedArtifactsResponse"
    "fixture/ListCreatedArtifactsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCreatedArtifacts)

responseDescribeMigrationTask :: DescribeMigrationTaskResponse -> TestTree
responseDescribeMigrationTask =
  res
    "DescribeMigrationTaskResponse"
    "fixture/DescribeMigrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMigrationTask)

responseDisassociateCreatedArtifact :: DisassociateCreatedArtifactResponse -> TestTree
responseDisassociateCreatedArtifact =
  res
    "DisassociateCreatedArtifactResponse"
    "fixture/DisassociateCreatedArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateCreatedArtifact)

responseImportMigrationTask :: ImportMigrationTaskResponse -> TestTree
responseImportMigrationTask =
  res
    "ImportMigrationTaskResponse"
    "fixture/ImportMigrationTaskResponse.proto"
    defaultService
    (Proxy :: Proxy ImportMigrationTask)

responseCreateProgressUpdateStream :: CreateProgressUpdateStreamResponse -> TestTree
responseCreateProgressUpdateStream =
  res
    "CreateProgressUpdateStreamResponse"
    "fixture/CreateProgressUpdateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProgressUpdateStream)

responsePutResourceAttributes :: PutResourceAttributesResponse -> TestTree
responsePutResourceAttributes =
  res
    "PutResourceAttributesResponse"
    "fixture/PutResourceAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy PutResourceAttributes)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDiscoveredResources)

responseDeleteProgressUpdateStream :: DeleteProgressUpdateStreamResponse -> TestTree
responseDeleteProgressUpdateStream =
  res
    "DeleteProgressUpdateStreamResponse"
    "fixture/DeleteProgressUpdateStreamResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProgressUpdateStream)

responseNotifyMigrationTaskState :: NotifyMigrationTaskStateResponse -> TestTree
responseNotifyMigrationTaskState =
  res
    "NotifyMigrationTaskStateResponse"
    "fixture/NotifyMigrationTaskStateResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyMigrationTaskState)

responseDisassociateDiscoveredResource :: DisassociateDiscoveredResourceResponse -> TestTree
responseDisassociateDiscoveredResource =
  res
    "DisassociateDiscoveredResourceResponse"
    "fixture/DisassociateDiscoveredResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDiscoveredResource)

responseListApplicationStates :: ListApplicationStatesResponse -> TestTree
responseListApplicationStates =
  res
    "ListApplicationStatesResponse"
    "fixture/ListApplicationStatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationStates)

responseAssociateDiscoveredResource :: AssociateDiscoveredResourceResponse -> TestTree
responseAssociateDiscoveredResource =
  res
    "AssociateDiscoveredResourceResponse"
    "fixture/AssociateDiscoveredResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDiscoveredResource)

responseListProgressUpdateStreams :: ListProgressUpdateStreamsResponse -> TestTree
responseListProgressUpdateStreams =
  res
    "ListProgressUpdateStreamsResponse"
    "fixture/ListProgressUpdateStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProgressUpdateStreams)

responseNotifyApplicationState :: NotifyApplicationStateResponse -> TestTree
responseNotifyApplicationState =
  res
    "NotifyApplicationStateResponse"
    "fixture/NotifyApplicationStateResponse.proto"
    defaultService
    (Proxy :: Proxy NotifyApplicationState)

responseListMigrationTasks :: ListMigrationTasksResponse -> TestTree
responseListMigrationTasks =
  res
    "ListMigrationTasksResponse"
    "fixture/ListMigrationTasksResponse.proto"
    defaultService
    (Proxy :: Proxy ListMigrationTasks)

responseDescribeApplicationState :: DescribeApplicationStateResponse -> TestTree
responseDescribeApplicationState =
  res
    "DescribeApplicationStateResponse"
    "fixture/DescribeApplicationStateResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicationState)

responseAssociateCreatedArtifact :: AssociateCreatedArtifactResponse -> TestTree
responseAssociateCreatedArtifact =
  res
    "AssociateCreatedArtifactResponse"
    "fixture/AssociateCreatedArtifactResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateCreatedArtifact)
