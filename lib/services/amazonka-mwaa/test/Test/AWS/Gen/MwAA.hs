{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.MwAA
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.MwAA where

import Amazonka.MwAA
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.MwAA.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListEnvironments $
--             newListEnvironments
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestCreateWebLoginToken $
--             newCreateWebLoginToken
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestPublishMetrics $
--             newPublishMetrics
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateCliToken $
--             newCreateCliToken
--
--           ]

--     , testGroup "response"
--         [ responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseCreateWebLoginToken $
--             newCreateWebLoginTokenResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responsePublishMetrics $
--             newPublishMetricsResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseCreateCliToken $
--             newCreateCliTokenResponse
--
--           ]
--     ]

-- Requests

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestCreateWebLoginToken :: CreateWebLoginToken -> TestTree
requestCreateWebLoginToken =
  req
    "CreateWebLoginToken"
    "fixture/CreateWebLoginToken.yaml"

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

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPublishMetrics :: PublishMetrics -> TestTree
requestPublishMetrics =
  req
    "PublishMetrics"
    "fixture/PublishMetrics.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateCliToken :: CreateCliToken -> TestTree
requestCreateCliToken =
  req
    "CreateCliToken"
    "fixture/CreateCliToken.yaml"

-- Responses

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseCreateWebLoginToken :: CreateWebLoginTokenResponse -> TestTree
responseCreateWebLoginToken =
  res
    "CreateWebLoginTokenResponse"
    "fixture/CreateWebLoginTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebLoginToken)

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

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responsePublishMetrics :: PublishMetricsResponse -> TestTree
responsePublishMetrics =
  res
    "PublishMetricsResponse"
    "fixture/PublishMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishMetrics)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateCliToken :: CreateCliTokenResponse -> TestTree
responseCreateCliToken =
  res
    "CreateCliTokenResponse"
    "fixture/CreateCliTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCliToken)
