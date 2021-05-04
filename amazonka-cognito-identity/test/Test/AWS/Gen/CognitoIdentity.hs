{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDescribeIdentityPool $
--             newDescribeIdentityPool
--
--         , requestGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentity
--
--         , requestGetOpenIdToken $
--             newGetOpenIdToken
--
--         , requestDeleteIdentities $
--             newDeleteIdentities
--
--         , requestMergeDeveloperIdentities $
--             newMergeDeveloperIdentities
--
--         , requestCreateIdentityPool $
--             newCreateIdentityPool
--
--         , requestGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMap
--
--         , requestUpdateIdentityPool $
--             newUpdateIdentityPool
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteIdentityPool $
--             newDeleteIdentityPool
--
--         , requestGetIdentityPoolRoles $
--             newGetIdentityPoolRoles
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnlinkIdentity $
--             newUnlinkIdentity
--
--         , requestLookupDeveloperIdentity $
--             newLookupDeveloperIdentity
--
--         , requestSetIdentityPoolRoles $
--             newSetIdentityPoolRoles
--
--         , requestListIdentityPools $
--             newListIdentityPools
--
--         , requestDescribeIdentity $
--             newDescribeIdentity
--
--         , requestGetCredentialsForIdentity $
--             newGetCredentialsForIdentity
--
--         , requestUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentity
--
--         , requestGetId $
--             newGetId
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMap
--
--           ]

--     , testGroup "response"
--         [ responseDescribeIdentityPool $
--             newIdentityPool
--
--         , responseGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentityResponse
--
--         , responseGetOpenIdToken $
--             newGetOpenIdTokenResponse
--
--         , responseDeleteIdentities $
--             newDeleteIdentitiesResponse
--
--         , responseMergeDeveloperIdentities $
--             newMergeDeveloperIdentitiesResponse
--
--         , responseCreateIdentityPool $
--             newIdentityPool
--
--         , responseGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMapResponse
--
--         , responseUpdateIdentityPool $
--             newIdentityPool
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteIdentityPool $
--             newDeleteIdentityPoolResponse
--
--         , responseGetIdentityPoolRoles $
--             newGetIdentityPoolRolesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnlinkIdentity $
--             newUnlinkIdentityResponse
--
--         , responseLookupDeveloperIdentity $
--             newLookupDeveloperIdentityResponse
--
--         , responseSetIdentityPoolRoles $
--             newSetIdentityPoolRolesResponse
--
--         , responseListIdentityPools $
--             newListIdentityPoolsResponse
--
--         , responseDescribeIdentity $
--             newIdentityDescription
--
--         , responseGetCredentialsForIdentity $
--             newGetCredentialsForIdentityResponse
--
--         , responseUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentityResponse
--
--         , responseGetId $
--             newGetIdResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMapResponse
--
--           ]
--     ]

-- Requests

requestDescribeIdentityPool :: DescribeIdentityPool -> TestTree
requestDescribeIdentityPool =
  req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool.yaml"

requestGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
requestGetOpenIdTokenForDeveloperIdentity =
  req
    "GetOpenIdTokenForDeveloperIdentity"
    "fixture/GetOpenIdTokenForDeveloperIdentity.yaml"

requestGetOpenIdToken :: GetOpenIdToken -> TestTree
requestGetOpenIdToken =
  req
    "GetOpenIdToken"
    "fixture/GetOpenIdToken.yaml"

requestDeleteIdentities :: DeleteIdentities -> TestTree
requestDeleteIdentities =
  req
    "DeleteIdentities"
    "fixture/DeleteIdentities.yaml"

requestMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
requestMergeDeveloperIdentities =
  req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities.yaml"

requestCreateIdentityPool :: CreateIdentityPool -> TestTree
requestCreateIdentityPool =
  req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool.yaml"

requestGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMap -> TestTree
requestGetPrincipalTagAttributeMap =
  req
    "GetPrincipalTagAttributeMap"
    "fixture/GetPrincipalTagAttributeMap.yaml"

requestUpdateIdentityPool :: UpdateIdentityPool -> TestTree
requestUpdateIdentityPool =
  req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteIdentityPool :: DeleteIdentityPool -> TestTree
requestDeleteIdentityPool =
  req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool.yaml"

requestGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
requestGetIdentityPoolRoles =
  req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnlinkIdentity :: UnlinkIdentity -> TestTree
requestUnlinkIdentity =
  req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity.yaml"

requestLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
requestLookupDeveloperIdentity =
  req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity.yaml"

requestSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
requestSetIdentityPoolRoles =
  req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles.yaml"

requestListIdentityPools :: ListIdentityPools -> TestTree
requestListIdentityPools =
  req
    "ListIdentityPools"
    "fixture/ListIdentityPools.yaml"

requestDescribeIdentity :: DescribeIdentity -> TestTree
requestDescribeIdentity =
  req
    "DescribeIdentity"
    "fixture/DescribeIdentity.yaml"

requestGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
requestGetCredentialsForIdentity =
  req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity.yaml"

requestUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
requestUnlinkDeveloperIdentity =
  req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity.yaml"

requestGetId :: GetId -> TestTree
requestGetId =
  req
    "GetId"
    "fixture/GetId.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMap -> TestTree
requestSetPrincipalTagAttributeMap =
  req
    "SetPrincipalTagAttributeMap"
    "fixture/SetPrincipalTagAttributeMap.yaml"

-- Responses

responseDescribeIdentityPool :: IdentityPool -> TestTree
responseDescribeIdentityPool =
  res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityPool)

responseGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
responseGetOpenIdTokenForDeveloperIdentity =
  res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

responseGetOpenIdToken :: GetOpenIdTokenResponse -> TestTree
responseGetOpenIdToken =
  res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpenIdToken)

responseDeleteIdentities :: DeleteIdentitiesResponse -> TestTree
responseDeleteIdentities =
  res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentities)

responseMergeDeveloperIdentities :: MergeDeveloperIdentitiesResponse -> TestTree
responseMergeDeveloperIdentities =
  res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy MergeDeveloperIdentities)

responseCreateIdentityPool :: IdentityPool -> TestTree
responseCreateIdentityPool =
  res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIdentityPool)

responseGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMapResponse -> TestTree
responseGetPrincipalTagAttributeMap =
  res
    "GetPrincipalTagAttributeMapResponse"
    "fixture/GetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy :: Proxy GetPrincipalTagAttributeMap)

responseUpdateIdentityPool :: IdentityPool -> TestTree
responseUpdateIdentityPool =
  res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIdentityPool)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteIdentityPool :: DeleteIdentityPoolResponse -> TestTree
responseDeleteIdentityPool =
  res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityPool)

responseGetIdentityPoolRoles :: GetIdentityPoolRolesResponse -> TestTree
responseGetIdentityPoolRoles =
  res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityPoolRoles)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseUnlinkIdentity :: UnlinkIdentityResponse -> TestTree
responseUnlinkIdentity =
  res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UnlinkIdentity)

responseLookupDeveloperIdentity :: LookupDeveloperIdentityResponse -> TestTree
responseLookupDeveloperIdentity =
  res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy LookupDeveloperIdentity)

responseSetIdentityPoolRoles :: SetIdentityPoolRolesResponse -> TestTree
responseSetIdentityPoolRoles =
  res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityPoolRoles)

responseListIdentityPools :: ListIdentityPoolsResponse -> TestTree
responseListIdentityPools =
  res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityPools)

responseDescribeIdentity :: IdentityDescription -> TestTree
responseDescribeIdentity =
  res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentity)

responseGetCredentialsForIdentity :: GetCredentialsForIdentityResponse -> TestTree
responseGetCredentialsForIdentity =
  res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCredentialsForIdentity)

responseUnlinkDeveloperIdentity :: UnlinkDeveloperIdentityResponse -> TestTree
responseUnlinkDeveloperIdentity =
  res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UnlinkDeveloperIdentity)

responseGetId :: GetIdResponse -> TestTree
responseGetId =
  res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    defaultService
    (Proxy :: Proxy GetId)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentities)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMapResponse -> TestTree
responseSetPrincipalTagAttributeMap =
  res
    "SetPrincipalTagAttributeMapResponse"
    "fixture/SetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy :: Proxy SetPrincipalTagAttributeMap)
