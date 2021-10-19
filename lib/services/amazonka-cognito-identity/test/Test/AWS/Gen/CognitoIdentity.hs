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
--         [ requestGetOpenIdToken $
--             newGetOpenIdToken
--
--         , requestGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentity
--
--         , requestDescribeIdentityPool $
--             newDescribeIdentityPool
--
--         , requestSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMap
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetId $
--             newGetId
--
--         , requestDeleteIdentityPool $
--             newDeleteIdentityPool
--
--         , requestUpdateIdentityPool $
--             newUpdateIdentityPool
--
--         , requestUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentity
--
--         , requestGetIdentityPoolRoles $
--             newGetIdentityPoolRoles
--
--         , requestListIdentityPools $
--             newListIdentityPools
--
--         , requestGetCredentialsForIdentity $
--             newGetCredentialsForIdentity
--
--         , requestGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMap
--
--         , requestDeleteIdentities $
--             newDeleteIdentities
--
--         , requestSetIdentityPoolRoles $
--             newSetIdentityPoolRoles
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestLookupDeveloperIdentity $
--             newLookupDeveloperIdentity
--
--         , requestUnlinkIdentity $
--             newUnlinkIdentity
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeIdentity $
--             newDescribeIdentity
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateIdentityPool $
--             newCreateIdentityPool
--
--         , requestMergeDeveloperIdentities $
--             newMergeDeveloperIdentities
--
--           ]

--     , testGroup "response"
--         [ responseGetOpenIdToken $
--             newGetOpenIdTokenResponse
--
--         , responseGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentityResponse
--
--         , responseDescribeIdentityPool $
--             newIdentityPool
--
--         , responseSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMapResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetId $
--             newGetIdResponse
--
--         , responseDeleteIdentityPool $
--             newDeleteIdentityPoolResponse
--
--         , responseUpdateIdentityPool $
--             newIdentityPool
--
--         , responseUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentityResponse
--
--         , responseGetIdentityPoolRoles $
--             newGetIdentityPoolRolesResponse
--
--         , responseListIdentityPools $
--             newListIdentityPoolsResponse
--
--         , responseGetCredentialsForIdentity $
--             newGetCredentialsForIdentityResponse
--
--         , responseGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMapResponse
--
--         , responseDeleteIdentities $
--             newDeleteIdentitiesResponse
--
--         , responseSetIdentityPoolRoles $
--             newSetIdentityPoolRolesResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseLookupDeveloperIdentity $
--             newLookupDeveloperIdentityResponse
--
--         , responseUnlinkIdentity $
--             newUnlinkIdentityResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeIdentity $
--             newIdentityDescription
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateIdentityPool $
--             newIdentityPool
--
--         , responseMergeDeveloperIdentities $
--             newMergeDeveloperIdentitiesResponse
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

requestSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMap -> TestTree
requestSetPrincipalTagAttributeMap =
  req
    "SetPrincipalTagAttributeMap"
    "fixture/SetPrincipalTagAttributeMap.yaml"

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

requestGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMap -> TestTree
requestGetPrincipalTagAttributeMap =
  req
    "GetPrincipalTagAttributeMap"
    "fixture/GetPrincipalTagAttributeMap.yaml"

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
    defaultService
    (Proxy :: Proxy GetOpenIdToken)

responseGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
responseGetOpenIdTokenForDeveloperIdentity =
  res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

responseDescribeIdentityPool :: IdentityPool -> TestTree
responseDescribeIdentityPool =
  res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityPool)

responseSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMapResponse -> TestTree
responseSetPrincipalTagAttributeMap =
  res
    "SetPrincipalTagAttributeMapResponse"
    "fixture/SetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy :: Proxy SetPrincipalTagAttributeMap)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetId :: GetIdResponse -> TestTree
responseGetId =
  res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    defaultService
    (Proxy :: Proxy GetId)

responseDeleteIdentityPool :: DeleteIdentityPoolResponse -> TestTree
responseDeleteIdentityPool =
  res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentityPool)

responseUpdateIdentityPool :: IdentityPool -> TestTree
responseUpdateIdentityPool =
  res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIdentityPool)

responseUnlinkDeveloperIdentity :: UnlinkDeveloperIdentityResponse -> TestTree
responseUnlinkDeveloperIdentity =
  res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UnlinkDeveloperIdentity)

responseGetIdentityPoolRoles :: GetIdentityPoolRolesResponse -> TestTree
responseGetIdentityPoolRoles =
  res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityPoolRoles)

responseListIdentityPools :: ListIdentityPoolsResponse -> TestTree
responseListIdentityPools =
  res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityPools)

responseGetCredentialsForIdentity :: GetCredentialsForIdentityResponse -> TestTree
responseGetCredentialsForIdentity =
  res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCredentialsForIdentity)

responseGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMapResponse -> TestTree
responseGetPrincipalTagAttributeMap =
  res
    "GetPrincipalTagAttributeMapResponse"
    "fixture/GetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy :: Proxy GetPrincipalTagAttributeMap)

responseDeleteIdentities :: DeleteIdentitiesResponse -> TestTree
responseDeleteIdentities =
  res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIdentities)

responseSetIdentityPoolRoles :: SetIdentityPoolRolesResponse -> TestTree
responseSetIdentityPoolRoles =
  res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityPoolRoles)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentities)

responseLookupDeveloperIdentity :: LookupDeveloperIdentityResponse -> TestTree
responseLookupDeveloperIdentity =
  res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy LookupDeveloperIdentity)

responseUnlinkIdentity :: UnlinkIdentityResponse -> TestTree
responseUnlinkIdentity =
  res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy UnlinkIdentity)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeIdentity :: IdentityDescription -> TestTree
responseDescribeIdentity =
  res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentity)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateIdentityPool :: IdentityPool -> TestTree
responseCreateIdentityPool =
  res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIdentityPool)

responseMergeDeveloperIdentities :: MergeDeveloperIdentitiesResponse -> TestTree
responseMergeDeveloperIdentities =
  res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    defaultService
    (Proxy :: Proxy MergeDeveloperIdentities)
