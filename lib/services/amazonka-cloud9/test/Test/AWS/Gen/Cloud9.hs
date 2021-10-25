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
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeleteEnvironment $
--             newDeleteEnvironment
--
--         , requestDescribeEnvironmentStatus $
--             newDescribeEnvironmentStatus
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateEnvironmentEC2 $
--             newCreateEnvironmentEC2
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateEnvironmentMembership $
--             newCreateEnvironmentMembership
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteEnvironmentMembership $
--             newDeleteEnvironmentMembership
--
--         , requestUpdateEnvironmentMembership $
--             newUpdateEnvironmentMembership
--
--         , requestDescribeEnvironmentMemberships $
--             newDescribeEnvironmentMemberships
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
--         , responseDescribeEnvironmentStatus $
--             newDescribeEnvironmentStatusResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateEnvironmentEC2 $
--             newCreateEnvironmentEC2Response
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateEnvironmentMembership $
--             newCreateEnvironmentMembershipResponse
--
--         , responseDescribeEnvironments $
--             newDescribeEnvironmentsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteEnvironmentMembership $
--             newDeleteEnvironmentMembershipResponse
--
--         , responseUpdateEnvironmentMembership $
--             newUpdateEnvironmentMembershipResponse
--
--         , responseDescribeEnvironmentMemberships $
--             newDescribeEnvironmentMembershipsResponse
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

requestDescribeEnvironmentStatus :: DescribeEnvironmentStatus -> TestTree
requestDescribeEnvironmentStatus =
  req
    "DescribeEnvironmentStatus"
    "fixture/DescribeEnvironmentStatus.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateEnvironmentEC2 :: CreateEnvironmentEC2 -> TestTree
requestCreateEnvironmentEC2 =
  req
    "CreateEnvironmentEC2"
    "fixture/CreateEnvironmentEC2.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateEnvironmentMembership :: CreateEnvironmentMembership -> TestTree
requestCreateEnvironmentMembership =
  req
    "CreateEnvironmentMembership"
    "fixture/CreateEnvironmentMembership.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments =
  req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteEnvironmentMembership :: DeleteEnvironmentMembership -> TestTree
requestDeleteEnvironmentMembership =
  req
    "DeleteEnvironmentMembership"
    "fixture/DeleteEnvironmentMembership.yaml"

requestUpdateEnvironmentMembership :: UpdateEnvironmentMembership -> TestTree
requestUpdateEnvironmentMembership =
  req
    "UpdateEnvironmentMembership"
    "fixture/UpdateEnvironmentMembership.yaml"

requestDescribeEnvironmentMemberships :: DescribeEnvironmentMemberships -> TestTree
requestDescribeEnvironmentMemberships =
  req
    "DescribeEnvironmentMemberships"
    "fixture/DescribeEnvironmentMemberships.yaml"

-- Responses

responseListEnvironments :: ListEnvironmentsResponse -> TestTree
responseListEnvironments =
  res
    "ListEnvironmentsResponse"
    "fixture/ListEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListEnvironments)

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

responseDescribeEnvironmentStatus :: DescribeEnvironmentStatusResponse -> TestTree
responseDescribeEnvironmentStatus =
  res
    "DescribeEnvironmentStatusResponse"
    "fixture/DescribeEnvironmentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentStatus)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateEnvironmentEC2 :: CreateEnvironmentEC2Response -> TestTree
responseCreateEnvironmentEC2 =
  res
    "CreateEnvironmentEC2Response"
    "fixture/CreateEnvironmentEC2Response.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironmentEC2)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateEnvironmentMembership :: CreateEnvironmentMembershipResponse -> TestTree
responseCreateEnvironmentMembership =
  res
    "CreateEnvironmentMembershipResponse"
    "fixture/CreateEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironmentMembership)

responseDescribeEnvironments :: DescribeEnvironmentsResponse -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironments)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteEnvironmentMembership :: DeleteEnvironmentMembershipResponse -> TestTree
responseDeleteEnvironmentMembership =
  res
    "DeleteEnvironmentMembershipResponse"
    "fixture/DeleteEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironmentMembership)

responseUpdateEnvironmentMembership :: UpdateEnvironmentMembershipResponse -> TestTree
responseUpdateEnvironmentMembership =
  res
    "UpdateEnvironmentMembershipResponse"
    "fixture/UpdateEnvironmentMembershipResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironmentMembership)

responseDescribeEnvironmentMemberships :: DescribeEnvironmentMembershipsResponse -> TestTree
responseDescribeEnvironmentMemberships =
  res
    "DescribeEnvironmentMembershipsResponse"
    "fixture/DescribeEnvironmentMembershipsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentMemberships)
