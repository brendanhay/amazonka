{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CognitoIdentity where

import Data.Proxy
import Network.AWS.CognitoIdentity
import Test.AWS.CognitoIdentity.Internal
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
--         [ requestGetOpenIdToken $
--             mkGetOpenIdToken
--
--         , requestGetOpenIdTokenForDeveloperIdentity $
--             mkGetOpenIdTokenForDeveloperIdentity
--
--         , requestDescribeIdentityPool $
--             mkDescribeIdentityPool
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetId $
--             mkGetId
--
--         , requestDeleteIdentityPool $
--             mkDeleteIdentityPool
--
--         , requestUpdateIdentityPool $
--             mkUpdateIdentityPool
--
--         , requestUnlinkDeveloperIdentity $
--             mkUnlinkDeveloperIdentity
--
--         , requestGetIdentityPoolRoles $
--             mkGetIdentityPoolRoles
--
--         , requestListIdentityPools $
--             mkListIdentityPools
--
--         , requestGetCredentialsForIdentity $
--             mkGetCredentialsForIdentity
--
--         , requestDeleteIdentities $
--             mkDeleteIdentities
--
--         , requestSetIdentityPoolRoles $
--             mkSetIdentityPoolRoles
--
--         , requestListIdentities $
--             mkListIdentities
--
--         , requestLookupDeveloperIdentity $
--             mkLookupDeveloperIdentity
--
--         , requestUnlinkIdentity $
--             mkUnlinkIdentity
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestDescribeIdentity $
--             mkDescribeIdentity
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestCreateIdentityPool $
--             mkCreateIdentityPool
--
--         , requestMergeDeveloperIdentities $
--             mkMergeDeveloperIdentities
--
--           ]

--     , testGroup "response"
--         [ responseGetOpenIdToken $
--             mkGetOpenIdTokenResponse
--
--         , responseGetOpenIdTokenForDeveloperIdentity $
--             mkGetOpenIdTokenForDeveloperIdentityResponse
--
--         , responseDescribeIdentityPool $
--             mkIdentityPool
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetId $
--             mkGetIdResponse
--
--         , responseDeleteIdentityPool $
--             mkDeleteIdentityPoolResponse
--
--         , responseUpdateIdentityPool $
--             mkIdentityPool
--
--         , responseUnlinkDeveloperIdentity $
--             mkUnlinkDeveloperIdentityResponse
--
--         , responseGetIdentityPoolRoles $
--             mkGetIdentityPoolRolesResponse
--
--         , responseListIdentityPools $
--             mkListIdentityPoolsResponse
--
--         , responseGetCredentialsForIdentity $
--             mkGetCredentialsForIdentityResponse
--
--         , responseDeleteIdentities $
--             mkDeleteIdentitiesResponse
--
--         , responseSetIdentityPoolRoles $
--             mkSetIdentityPoolRolesResponse
--
--         , responseListIdentities $
--             mkListIdentitiesResponse
--
--         , responseLookupDeveloperIdentity $
--             mkLookupDeveloperIdentityResponse
--
--         , responseUnlinkIdentity $
--             mkUnlinkIdentityResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseDescribeIdentity $
--             mkIdentityDescription
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseCreateIdentityPool $
--             mkIdentityPool
--
--         , responseMergeDeveloperIdentities $
--             mkMergeDeveloperIdentitiesResponse
--
--           ]
--     ]

-- Requests

requestGetOpenIdToken :: GetOpenIdToken -> TestTree
requestGetOpenIdToken =
  req
    "GetOpenIdToken"
    "fixture/GetOpenIdToken.yaml"

requestGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
requestGetOpenIdTokenForDeveloperIdentity =
  req
    "GetOpenIdTokenForDeveloperIdentity"
    "fixture/GetOpenIdTokenForDeveloperIdentity.yaml"

requestDescribeIdentityPool :: DescribeIdentityPool -> TestTree
requestDescribeIdentityPool =
  req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetId :: GetId -> TestTree
requestGetId =
  req
    "GetId"
    "fixture/GetId.yaml"

requestDeleteIdentityPool :: DeleteIdentityPool -> TestTree
requestDeleteIdentityPool =
  req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool.yaml"

requestUpdateIdentityPool :: UpdateIdentityPool -> TestTree
requestUpdateIdentityPool =
  req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool.yaml"

requestUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
requestUnlinkDeveloperIdentity =
  req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity.yaml"

requestGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
requestGetIdentityPoolRoles =
  req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles.yaml"

requestListIdentityPools :: ListIdentityPools -> TestTree
requestListIdentityPools =
  req
    "ListIdentityPools"
    "fixture/ListIdentityPools.yaml"

requestGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
requestGetCredentialsForIdentity =
  req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity.yaml"

requestDeleteIdentities :: DeleteIdentities -> TestTree
requestDeleteIdentities =
  req
    "DeleteIdentities"
    "fixture/DeleteIdentities.yaml"

requestSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
requestSetIdentityPoolRoles =
  req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
requestLookupDeveloperIdentity =
  req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity.yaml"

requestUnlinkIdentity :: UnlinkIdentity -> TestTree
requestUnlinkIdentity =
  req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeIdentity :: DescribeIdentity -> TestTree
requestDescribeIdentity =
  req
    "DescribeIdentity"
    "fixture/DescribeIdentity.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateIdentityPool :: CreateIdentityPool -> TestTree
requestCreateIdentityPool =
  req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool.yaml"

requestMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
requestMergeDeveloperIdentities =
  req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities.yaml"

-- Responses

responseGetOpenIdToken :: GetOpenIdTokenResponse -> TestTree
responseGetOpenIdToken =
  res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy GetOpenIdToken)

responseGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
responseGetOpenIdTokenForDeveloperIdentity =
  res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

responseDescribeIdentityPool :: IdentityPool -> TestTree
responseDescribeIdentityPool =
  res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy DescribeIdentityPool)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy ListTagsForResource)

responseGetId :: GetIdResponse -> TestTree
responseGetId =
  res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy GetId)

responseDeleteIdentityPool :: DeleteIdentityPoolResponse -> TestTree
responseDeleteIdentityPool =
  res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy DeleteIdentityPool)

responseUpdateIdentityPool :: IdentityPool -> TestTree
responseUpdateIdentityPool =
  res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy UpdateIdentityPool)

responseUnlinkDeveloperIdentity :: UnlinkDeveloperIdentityResponse -> TestTree
responseUnlinkDeveloperIdentity =
  res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy UnlinkDeveloperIdentity)

responseGetIdentityPoolRoles :: GetIdentityPoolRolesResponse -> TestTree
responseGetIdentityPoolRoles =
  res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy GetIdentityPoolRoles)

responseListIdentityPools :: ListIdentityPoolsResponse -> TestTree
responseListIdentityPools =
  res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy ListIdentityPools)

responseGetCredentialsForIdentity :: GetCredentialsForIdentityResponse -> TestTree
responseGetCredentialsForIdentity =
  res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy GetCredentialsForIdentity)

responseDeleteIdentities :: DeleteIdentitiesResponse -> TestTree
responseDeleteIdentities =
  res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy DeleteIdentities)

responseSetIdentityPoolRoles :: SetIdentityPoolRolesResponse -> TestTree
responseSetIdentityPoolRoles =
  res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy SetIdentityPoolRoles)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy ListIdentities)

responseLookupDeveloperIdentity :: LookupDeveloperIdentityResponse -> TestTree
responseLookupDeveloperIdentity =
  res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy LookupDeveloperIdentity)

responseUnlinkIdentity :: UnlinkIdentityResponse -> TestTree
responseUnlinkIdentity =
  res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy UnlinkIdentity)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy TagResource)

responseDescribeIdentity :: IdentityDescription -> TestTree
responseDescribeIdentity =
  res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy DescribeIdentity)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy UntagResource)

responseCreateIdentityPool :: IdentityPool -> TestTree
responseCreateIdentityPool =
  res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy CreateIdentityPool)

responseMergeDeveloperIdentities :: MergeDeveloperIdentitiesResponse -> TestTree
responseMergeDeveloperIdentities =
  res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    cognitoIdentityService
    (Proxy :: Proxy MergeDeveloperIdentities)
