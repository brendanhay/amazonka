{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Cloud9
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Cloud9 where

import Data.Proxy
import Network.AWS.Cloud9
import Test.AWS.Cloud9.Internal
import Test.AWS.Fixture
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
--         , requestCreateEnvironmentMembership $
--             newCreateEnvironmentMembership
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeEnvironmentStatus $
--             newDescribeEnvironmentStatus
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDescribeEnvironmentMemberships $
--             newDescribeEnvironmentMemberships
--
--         , requestUpdateEnvironmentMembership $
--             newUpdateEnvironmentMembership
--
--         , requestDeleteEnvironmentMembership $
--             newDeleteEnvironmentMembership
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestCreateEnvironmentEC $
--             newCreateEnvironmentEC
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseListEnvironments $
--             newListEnvironmentsResponse
--
--         , responseCreateEnvironmentMembership $
--             newCreateEnvironmentMembershipResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeEnvironmentStatus $
--             newDescribeEnvironmentStatusResponse
--
--         , responseUpdateEnvironment $
--             newUpdateEnvironmentResponse
--
--         , responseDeleteEnvironment $
--             newDeleteEnvironmentResponse
--
--         , responseDescribeEnvironmentMemberships $
--             newDescribeEnvironmentMembershipsResponse
--
--         , responseUpdateEnvironmentMembership $
--             newUpdateEnvironmentMembershipResponse
--
--         , responseDeleteEnvironmentMembership $
--             newDeleteEnvironmentMembershipResponse
--
--         , responseDescribeEnvironments $
--             newDescribeEnvironmentsResponse
--
--         , responseCreateEnvironmentEC $
--             newCreateEnvironmentECResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestListEnvironments :: ListEnvironments -> TestTree
requestListEnvironments =
  req
    "ListEnvironments"
    "fixture/ListEnvironments.yaml"

requestCreateEnvironmentMembership :: CreateEnvironmentMembership -> TestTree
requestCreateEnvironmentMembership =
  req
    "CreateEnvironmentMembership"
    "fixture/CreateEnvironmentMembership.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeEnvironmentStatus :: DescribeEnvironmentStatus -> TestTree
requestDescribeEnvironmentStatus =
  req
    "DescribeEnvironmentStatus"
    "fixture/DescribeEnvironmentStatus.yaml"

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

requestDescribeEnvironmentMemberships :: DescribeEnvironmentMemberships -> TestTree
requestDescribeEnvironmentMemberships =
  req
    "DescribeEnvironmentMemberships"
    "fixture/DescribeEnvironmentMemberships.yaml"

requestUpdateEnvironmentMembership :: UpdateEnvironmentMembership -> TestTree
requestUpdateEnvironmentMembership =
  req
    "UpdateEnvironmentMembership"
    "fixture/UpdateEnvironmentMembership.yaml"

requestDeleteEnvironmentMembership :: DeleteEnvironmentMembership -> TestTree
requestDeleteEnvironmentMembership =
  req
    "DeleteEnvironmentMembership"
    "fixture/DeleteEnvironmentMembership.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments =
  req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestCreateEnvironmentEC :: CreateEnvironmentEC -> TestTree
requestCreateEnvironmentEC =
  req
    "CreateEnvironmentEC"
    "fixture/CreateEnvironmentEC.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEnvironments)

responseCreateEnvironmentMembership :: CreateEnvironmentMembershipResponse -> TestTree
responseCreateEnvironmentMembership =
  res
    "CreateEnvironmentMembershipResponse"
    "fixture/CreateEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironmentMembership)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeEnvironmentStatus :: DescribeEnvironmentStatusResponse -> TestTree
responseDescribeEnvironmentStatus =
  res
    "DescribeEnvironmentStatusResponse"
    "fixture/DescribeEnvironmentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentStatus)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironment)

responseDescribeEnvironmentMemberships :: DescribeEnvironmentMembershipsResponse -> TestTree
responseDescribeEnvironmentMemberships =
  res
    "DescribeEnvironmentMembershipsResponse"
    "fixture/DescribeEnvironmentMembershipsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentMemberships)

responseUpdateEnvironmentMembership :: UpdateEnvironmentMembershipResponse -> TestTree
responseUpdateEnvironmentMembership =
  res
    "UpdateEnvironmentMembershipResponse"
    "fixture/UpdateEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironmentMembership)

responseDeleteEnvironmentMembership :: DeleteEnvironmentMembershipResponse -> TestTree
responseDeleteEnvironmentMembership =
  res
    "DeleteEnvironmentMembershipResponse"
    "fixture/DeleteEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironmentMembership)

responseDescribeEnvironments :: DescribeEnvironmentsResponse -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironments)

responseCreateEnvironmentEC :: CreateEnvironmentECResponse -> TestTree
responseCreateEnvironmentEC =
  res
    "CreateEnvironmentECResponse"
    "fixture/CreateEnvironmentECResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironmentEC)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
