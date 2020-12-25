{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MigrationHub
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestAssociateDiscoveredResource $
--             mkAssociateDiscoveredResource
--
--         , requestCreateProgressUpdateStream $
--             mkCreateProgressUpdateStream
--
--         , requestListApplicationStates $
--             mkListApplicationStates
--
--         , requestListCreatedArtifacts $
--             mkListCreatedArtifacts
--
--         , requestDisassociateDiscoveredResource $
--             mkDisassociateDiscoveredResource
--
--         , requestNotifyApplicationState $
--             mkNotifyApplicationState
--
--         , requestDeleteProgressUpdateStream $
--             mkDeleteProgressUpdateStream
--
--         , requestListProgressUpdateStreams $
--             mkListProgressUpdateStreams
--
--         , requestDisassociateCreatedArtifact $
--             mkDisassociateCreatedArtifact
--
--         , requestImportMigrationTask $
--             mkImportMigrationTask
--
--         , requestDescribeMigrationTask $
--             mkDescribeMigrationTask
--
--         , requestPutResourceAttributes $
--             mkPutResourceAttributes
--
--         , requestAssociateCreatedArtifact $
--             mkAssociateCreatedArtifact
--
--         , requestNotifyMigrationTaskState $
--             mkNotifyMigrationTaskState
--
--         , requestDescribeApplicationState $
--             mkDescribeApplicationState
--
--         , requestListMigrationTasks $
--             mkListMigrationTasks
--
--         , requestListDiscoveredResources $
--             mkListDiscoveredResources
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDiscoveredResource $
--             mkAssociateDiscoveredResourceResponse
--
--         , responseCreateProgressUpdateStream $
--             mkCreateProgressUpdateStreamResponse
--
--         , responseListApplicationStates $
--             mkListApplicationStatesResponse
--
--         , responseListCreatedArtifacts $
--             mkListCreatedArtifactsResponse
--
--         , responseDisassociateDiscoveredResource $
--             mkDisassociateDiscoveredResourceResponse
--
--         , responseNotifyApplicationState $
--             mkNotifyApplicationStateResponse
--
--         , responseDeleteProgressUpdateStream $
--             mkDeleteProgressUpdateStreamResponse
--
--         , responseListProgressUpdateStreams $
--             mkListProgressUpdateStreamsResponse
--
--         , responseDisassociateCreatedArtifact $
--             mkDisassociateCreatedArtifactResponse
--
--         , responseImportMigrationTask $
--             mkImportMigrationTaskResponse
--
--         , responseDescribeMigrationTask $
--             mkDescribeMigrationTaskResponse
--
--         , responsePutResourceAttributes $
--             mkPutResourceAttributesResponse
--
--         , responseAssociateCreatedArtifact $
--             mkAssociateCreatedArtifactResponse
--
--         , responseNotifyMigrationTaskState $
--             mkNotifyMigrationTaskStateResponse
--
--         , responseDescribeApplicationState $
--             mkDescribeApplicationStateResponse
--
--         , responseListMigrationTasks $
--             mkListMigrationTasksResponse
--
--         , responseListDiscoveredResources $
--             mkListDiscoveredResourcesResponse
--
--           ]
--     ]

-- Requests

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

requestDisassociateDiscoveredResource :: DisassociateDiscoveredResource -> TestTree
requestDisassociateDiscoveredResource =
  req
    "DisassociateDiscoveredResource"
    "fixture/DisassociateDiscoveredResource.yaml"

requestNotifyApplicationState :: NotifyApplicationState -> TestTree
requestNotifyApplicationState =
  req
    "NotifyApplicationState"
    "fixture/NotifyApplicationState.yaml"

requestDeleteProgressUpdateStream :: DeleteProgressUpdateStream -> TestTree
requestDeleteProgressUpdateStream =
  req
    "DeleteProgressUpdateStream"
    "fixture/DeleteProgressUpdateStream.yaml"

requestListProgressUpdateStreams :: ListProgressUpdateStreams -> TestTree
requestListProgressUpdateStreams =
  req
    "ListProgressUpdateStreams"
    "fixture/ListProgressUpdateStreams.yaml"

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

requestDescribeMigrationTask :: DescribeMigrationTask -> TestTree
requestDescribeMigrationTask =
  req
    "DescribeMigrationTask"
    "fixture/DescribeMigrationTask.yaml"

requestPutResourceAttributes :: PutResourceAttributes -> TestTree
requestPutResourceAttributes =
  req
    "PutResourceAttributes"
    "fixture/PutResourceAttributes.yaml"

requestAssociateCreatedArtifact :: AssociateCreatedArtifact -> TestTree
requestAssociateCreatedArtifact =
  req
    "AssociateCreatedArtifact"
    "fixture/AssociateCreatedArtifact.yaml"

requestNotifyMigrationTaskState :: NotifyMigrationTaskState -> TestTree
requestNotifyMigrationTaskState =
  req
    "NotifyMigrationTaskState"
    "fixture/NotifyMigrationTaskState.yaml"

requestDescribeApplicationState :: DescribeApplicationState -> TestTree
requestDescribeApplicationState =
  req
    "DescribeApplicationState"
    "fixture/DescribeApplicationState.yaml"

requestListMigrationTasks :: ListMigrationTasks -> TestTree
requestListMigrationTasks =
  req
    "ListMigrationTasks"
    "fixture/ListMigrationTasks.yaml"

requestListDiscoveredResources :: ListDiscoveredResources -> TestTree
requestListDiscoveredResources =
  req
    "ListDiscoveredResources"
    "fixture/ListDiscoveredResources.yaml"

-- Responses

responseAssociateDiscoveredResource :: AssociateDiscoveredResourceResponse -> TestTree
responseAssociateDiscoveredResource =
  res
    "AssociateDiscoveredResourceResponse"
    "fixture/AssociateDiscoveredResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateDiscoveredResource)

responseCreateProgressUpdateStream :: CreateProgressUpdateStreamResponse -> TestTree
responseCreateProgressUpdateStream =
  res
    "CreateProgressUpdateStreamResponse"
    "fixture/CreateProgressUpdateStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateProgressUpdateStream)

responseListApplicationStates :: ListApplicationStatesResponse -> TestTree
responseListApplicationStates =
  res
    "ListApplicationStatesResponse"
    "fixture/ListApplicationStatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListApplicationStates)

responseListCreatedArtifacts :: ListCreatedArtifactsResponse -> TestTree
responseListCreatedArtifacts =
  res
    "ListCreatedArtifactsResponse"
    "fixture/ListCreatedArtifactsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCreatedArtifacts)

responseDisassociateDiscoveredResource :: DisassociateDiscoveredResourceResponse -> TestTree
responseDisassociateDiscoveredResource =
  res
    "DisassociateDiscoveredResourceResponse"
    "fixture/DisassociateDiscoveredResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateDiscoveredResource)

responseNotifyApplicationState :: NotifyApplicationStateResponse -> TestTree
responseNotifyApplicationState =
  res
    "NotifyApplicationStateResponse"
    "fixture/NotifyApplicationStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy NotifyApplicationState)

responseDeleteProgressUpdateStream :: DeleteProgressUpdateStreamResponse -> TestTree
responseDeleteProgressUpdateStream =
  res
    "DeleteProgressUpdateStreamResponse"
    "fixture/DeleteProgressUpdateStreamResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteProgressUpdateStream)

responseListProgressUpdateStreams :: ListProgressUpdateStreamsResponse -> TestTree
responseListProgressUpdateStreams =
  res
    "ListProgressUpdateStreamsResponse"
    "fixture/ListProgressUpdateStreamsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListProgressUpdateStreams)

responseDisassociateCreatedArtifact :: DisassociateCreatedArtifactResponse -> TestTree
responseDisassociateCreatedArtifact =
  res
    "DisassociateCreatedArtifactResponse"
    "fixture/DisassociateCreatedArtifactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateCreatedArtifact)

responseImportMigrationTask :: ImportMigrationTaskResponse -> TestTree
responseImportMigrationTask =
  res
    "ImportMigrationTaskResponse"
    "fixture/ImportMigrationTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportMigrationTask)

responseDescribeMigrationTask :: DescribeMigrationTaskResponse -> TestTree
responseDescribeMigrationTask =
  res
    "DescribeMigrationTaskResponse"
    "fixture/DescribeMigrationTaskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeMigrationTask)

responsePutResourceAttributes :: PutResourceAttributesResponse -> TestTree
responsePutResourceAttributes =
  res
    "PutResourceAttributesResponse"
    "fixture/PutResourceAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutResourceAttributes)

responseAssociateCreatedArtifact :: AssociateCreatedArtifactResponse -> TestTree
responseAssociateCreatedArtifact =
  res
    "AssociateCreatedArtifactResponse"
    "fixture/AssociateCreatedArtifactResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateCreatedArtifact)

responseNotifyMigrationTaskState :: NotifyMigrationTaskStateResponse -> TestTree
responseNotifyMigrationTaskState =
  res
    "NotifyMigrationTaskStateResponse"
    "fixture/NotifyMigrationTaskStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy NotifyMigrationTaskState)

responseDescribeApplicationState :: DescribeApplicationStateResponse -> TestTree
responseDescribeApplicationState =
  res
    "DescribeApplicationStateResponse"
    "fixture/DescribeApplicationStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeApplicationState)

responseListMigrationTasks :: ListMigrationTasksResponse -> TestTree
responseListMigrationTasks =
  res
    "ListMigrationTasksResponse"
    "fixture/ListMigrationTasksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListMigrationTasks)

responseListDiscoveredResources :: ListDiscoveredResourcesResponse -> TestTree
responseListDiscoveredResources =
  res
    "ListDiscoveredResourcesResponse"
    "fixture/ListDiscoveredResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDiscoveredResources)
