{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MwAA
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MwAA where

import Amazonka.MwAA
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MwAA.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateCliToken $
--             newCreateCliToken
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateWebLoginToken $
--             newCreateWebLoginToken
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestGetEnvironment $
--             newGetEnvironment
--
--         , requestListEnvironments $
--             newListEnvironments
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPublishMetrics $
--             newPublishMetrics
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
--           ]

--     , testGroup "response"
--         [ responseCreateCliToken $
--             newCreateCliTokenResponse
--
--         , responseCreateEnvironment $
--             newCreateEnvironmentResponse
--
--         , responseCreateWebLoginToken $
--             newCreateWebLoginTokenResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseGetEnvironment $
--             newGetEnvironmentResponse
--
--         , responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePublishMetrics $
--             newPublishMetricsResponse
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
--           ]
--     ]

-- Requests

requestCreateCliToken :: CreateCliToken -> TestTree
requestCreateCliToken =
  req
    "CreateCliToken"
    "fixture/CreateCliToken.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreateWebLoginToken :: CreateWebLoginToken -> TestTree
requestCreateWebLoginToken =
  req
    "CreateWebLoginToken"
    "fixture/CreateWebLoginToken.yaml"

requestDeleteEnvironment :: DeleteEnvironment -> TestTree
requestDeleteEnvironment =
  req
    "DeleteEnvironment"
    "fixture/DeleteEnvironment.yaml"

requestGetEnvironment :: GetEnvironment -> TestTree
requestGetEnvironment =
  req
    "GetEnvironment"
    "fixture/GetEnvironment.yaml"

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPublishMetrics :: PublishMetrics -> TestTree
requestPublishMetrics =
  req
    "PublishMetrics"
    "fixture/PublishMetrics.yaml"

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

-- Responses

responseCreateCliToken :: CreateCliTokenResponse -> TestTree
responseCreateCliToken =
  res
    "CreateCliTokenResponse"
    "fixture/CreateCliTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCliToken)

responseCreateEnvironment :: CreateEnvironmentResponse -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreateWebLoginToken :: CreateWebLoginTokenResponse -> TestTree
responseCreateWebLoginToken =
  res
    "CreateWebLoginTokenResponse"
    "fixture/CreateWebLoginTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWebLoginToken)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironment)

responseGetEnvironment :: GetEnvironmentResponse -> TestTree
responseGetEnvironment =
  res
    "GetEnvironmentResponse"
    "fixture/GetEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnvironment)

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEnvironments)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePublishMetrics :: PublishMetricsResponse -> TestTree
responsePublishMetrics =
  res
    "PublishMetricsResponse"
    "fixture/PublishMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishMetrics)

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
