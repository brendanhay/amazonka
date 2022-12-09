{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Pipes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Pipes where

import Amazonka.Pipes
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Pipes.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreatePipe $
--             newCreatePipe
--
--         , requestDeletePipe $
--             newDeletePipe
--
--         , requestDescribePipe $
--             newDescribePipe
--
--         , requestListPipes $
--             newListPipes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartPipe $
--             newStartPipe
--
--         , requestStopPipe $
--             newStopPipe
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePipe $
--             newUpdatePipe
--
--           ]

--     , testGroup "response"
--         [ responseCreatePipe $
--             newCreatePipeResponse
--
--         , responseDeletePipe $
--             newDeletePipeResponse
--
--         , responseDescribePipe $
--             newDescribePipeResponse
--
--         , responseListPipes $
--             newListPipesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartPipe $
--             newStartPipeResponse
--
--         , responseStopPipe $
--             newStopPipeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePipe $
--             newUpdatePipeResponse
--
--           ]
--     ]

-- Requests

requestCreatePipe :: CreatePipe -> TestTree
requestCreatePipe =
  req
    "CreatePipe"
    "fixture/CreatePipe.yaml"

requestDeletePipe :: DeletePipe -> TestTree
requestDeletePipe =
  req
    "DeletePipe"
    "fixture/DeletePipe.yaml"

requestDescribePipe :: DescribePipe -> TestTree
requestDescribePipe =
  req
    "DescribePipe"
    "fixture/DescribePipe.yaml"

requestListPipes :: ListPipes -> TestTree
requestListPipes =
  req
    "ListPipes"
    "fixture/ListPipes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartPipe :: StartPipe -> TestTree
requestStartPipe =
  req
    "StartPipe"
    "fixture/StartPipe.yaml"

requestStopPipe :: StopPipe -> TestTree
requestStopPipe =
  req
    "StopPipe"
    "fixture/StopPipe.yaml"

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

requestUpdatePipe :: UpdatePipe -> TestTree
requestUpdatePipe =
  req
    "UpdatePipe"
    "fixture/UpdatePipe.yaml"

-- Responses

responseCreatePipe :: CreatePipeResponse -> TestTree
responseCreatePipe =
  res
    "CreatePipeResponse"
    "fixture/CreatePipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePipe)

responseDeletePipe :: DeletePipeResponse -> TestTree
responseDeletePipe =
  res
    "DeletePipeResponse"
    "fixture/DeletePipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePipe)

responseDescribePipe :: DescribePipeResponse -> TestTree
responseDescribePipe =
  res
    "DescribePipeResponse"
    "fixture/DescribePipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePipe)

responseListPipes :: ListPipesResponse -> TestTree
responseListPipes =
  res
    "ListPipesResponse"
    "fixture/ListPipesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPipes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartPipe :: StartPipeResponse -> TestTree
responseStartPipe =
  res
    "StartPipeResponse"
    "fixture/StartPipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartPipe)

responseStopPipe :: StopPipeResponse -> TestTree
responseStopPipe =
  res
    "StopPipeResponse"
    "fixture/StopPipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopPipe)

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

responseUpdatePipe :: UpdatePipeResponse -> TestTree
responseUpdatePipe =
  res
    "UpdatePipeResponse"
    "fixture/UpdatePipeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePipe)
