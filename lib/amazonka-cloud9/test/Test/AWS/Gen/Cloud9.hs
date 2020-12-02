{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Cloud9
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             listEnvironments
--
--         , requestUpdateEnvironment $
--             updateEnvironment
--
--         , requestDeleteEnvironment $
--             deleteEnvironment
--
--         , requestDescribeEnvironmentStatus $
--             describeEnvironmentStatus
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestCreateEnvironmentEC $
--             createEnvironmentEC
--
--         , requestTagResource $
--             tagResource
--
--         , requestCreateEnvironmentMembership $
--             createEnvironmentMembership
--
--         , requestDescribeEnvironments $
--             describeEnvironments
--
--         , requestUntagResource $
--             untagResource
--
--         , requestDeleteEnvironmentMembership $
--             deleteEnvironmentMembership
--
--         , requestUpdateEnvironmentMembership $
--             updateEnvironmentMembership
--
--         , requestDescribeEnvironmentMemberships $
--             describeEnvironmentMemberships
--
--           ]

--     , testGroup "response"
--         [ responseListEnvironments $
--             listEnvironmentsResponse
--
--         , responseUpdateEnvironment $
--             updateEnvironmentResponse
--
--         , responseDeleteEnvironment $
--             deleteEnvironmentResponse
--
--         , responseDescribeEnvironmentStatus $
--             describeEnvironmentStatusResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseCreateEnvironmentEC $
--             createEnvironmentECResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseCreateEnvironmentMembership $
--             createEnvironmentMembershipResponse
--
--         , responseDescribeEnvironments $
--             describeEnvironmentsResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseDeleteEnvironmentMembership $
--             deleteEnvironmentMembershipResponse
--
--         , responseUpdateEnvironmentMembership $
--             updateEnvironmentMembershipResponse
--
--         , responseDescribeEnvironmentMemberships $
--             describeEnvironmentMembershipsResponse
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

requestCreateEnvironmentEC :: CreateEnvironmentEC -> TestTree
requestCreateEnvironmentEC =
  req
    "CreateEnvironmentEC"
    "fixture/CreateEnvironmentEC.yaml"

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
    cloud9
    (Proxy :: Proxy ListEnvironments)

responseUpdateEnvironment :: UpdateEnvironmentResponse -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    cloud9
    (Proxy :: Proxy UpdateEnvironment)

responseDeleteEnvironment :: DeleteEnvironmentResponse -> TestTree
responseDeleteEnvironment =
  res
    "DeleteEnvironmentResponse"
    "fixture/DeleteEnvironmentResponse.proto"
    cloud9
    (Proxy :: Proxy DeleteEnvironment)

responseDescribeEnvironmentStatus :: DescribeEnvironmentStatusResponse -> TestTree
responseDescribeEnvironmentStatus =
  res
    "DescribeEnvironmentStatusResponse"
    "fixture/DescribeEnvironmentStatusResponse.proto"
    cloud9
    (Proxy :: Proxy DescribeEnvironmentStatus)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cloud9
    (Proxy :: Proxy ListTagsForResource)

responseCreateEnvironmentEC :: CreateEnvironmentECResponse -> TestTree
responseCreateEnvironmentEC =
  res
    "CreateEnvironmentECResponse"
    "fixture/CreateEnvironmentECResponse.proto"
    cloud9
    (Proxy :: Proxy CreateEnvironmentEC)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cloud9
    (Proxy :: Proxy TagResource)

responseCreateEnvironmentMembership :: CreateEnvironmentMembershipResponse -> TestTree
responseCreateEnvironmentMembership =
  res
    "CreateEnvironmentMembershipResponse"
    "fixture/CreateEnvironmentMembershipResponse.proto"
    cloud9
    (Proxy :: Proxy CreateEnvironmentMembership)

responseDescribeEnvironments :: DescribeEnvironmentsResponse -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    cloud9
    (Proxy :: Proxy DescribeEnvironments)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cloud9
    (Proxy :: Proxy UntagResource)

responseDeleteEnvironmentMembership :: DeleteEnvironmentMembershipResponse -> TestTree
responseDeleteEnvironmentMembership =
  res
    "DeleteEnvironmentMembershipResponse"
    "fixture/DeleteEnvironmentMembershipResponse.proto"
    cloud9
    (Proxy :: Proxy DeleteEnvironmentMembership)

responseUpdateEnvironmentMembership :: UpdateEnvironmentMembershipResponse -> TestTree
responseUpdateEnvironmentMembership =
  res
    "UpdateEnvironmentMembershipResponse"
    "fixture/UpdateEnvironmentMembershipResponse.proto"
    cloud9
    (Proxy :: Proxy UpdateEnvironmentMembership)

responseDescribeEnvironmentMemberships :: DescribeEnvironmentMembershipsResponse -> TestTree
responseDescribeEnvironmentMemberships =
  res
    "DescribeEnvironmentMembershipsResponse"
    "fixture/DescribeEnvironmentMembershipsResponse.proto"
    cloud9
    (Proxy :: Proxy DescribeEnvironmentMemberships)
