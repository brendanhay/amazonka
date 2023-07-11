{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CognitoIdentity where

import Amazonka.CognitoIdentity
import qualified Data.Proxy as Proxy
import Test.Amazonka.CognitoIdentity.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateIdentityPool $
--             newCreateIdentityPool
--
--         , requestDeleteIdentities $
--             newDeleteIdentities
--
--         , requestDeleteIdentityPool $
--             newDeleteIdentityPool
--
--         , requestDescribeIdentity $
--             newDescribeIdentity
--
--         , requestDescribeIdentityPool $
--             newDescribeIdentityPool
--
--         , requestGetCredentialsForIdentity $
--             newGetCredentialsForIdentity
--
--         , requestGetId $
--             newGetId
--
--         , requestGetIdentityPoolRoles $
--             newGetIdentityPoolRoles
--
--         , requestGetOpenIdToken $
--             newGetOpenIdToken
--
--         , requestGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentity
--
--         , requestGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMap
--
--         , requestListIdentities $
--             newListIdentities
--
--         , requestListIdentityPools $
--             newListIdentityPools
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestLookupDeveloperIdentity $
--             newLookupDeveloperIdentity
--
--         , requestMergeDeveloperIdentities $
--             newMergeDeveloperIdentities
--
--         , requestSetIdentityPoolRoles $
--             newSetIdentityPoolRoles
--
--         , requestSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMap
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentity
--
--         , requestUnlinkIdentity $
--             newUnlinkIdentity
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateIdentityPool $
--             newUpdateIdentityPool
--
--           ]

--     , testGroup "response"
--         [ responseCreateIdentityPool $
--             newIdentityPool
--
--         , responseDeleteIdentities $
--             newDeleteIdentitiesResponse
--
--         , responseDeleteIdentityPool $
--             newDeleteIdentityPoolResponse
--
--         , responseDescribeIdentity $
--             newIdentityDescription
--
--         , responseDescribeIdentityPool $
--             newIdentityPool
--
--         , responseGetCredentialsForIdentity $
--             newGetCredentialsForIdentityResponse
--
--         , responseGetId $
--             newGetIdResponse
--
--         , responseGetIdentityPoolRoles $
--             newGetIdentityPoolRolesResponse
--
--         , responseGetOpenIdToken $
--             newGetOpenIdTokenResponse
--
--         , responseGetOpenIdTokenForDeveloperIdentity $
--             newGetOpenIdTokenForDeveloperIdentityResponse
--
--         , responseGetPrincipalTagAttributeMap $
--             newGetPrincipalTagAttributeMapResponse
--
--         , responseListIdentities $
--             newListIdentitiesResponse
--
--         , responseListIdentityPools $
--             newListIdentityPoolsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseLookupDeveloperIdentity $
--             newLookupDeveloperIdentityResponse
--
--         , responseMergeDeveloperIdentities $
--             newMergeDeveloperIdentitiesResponse
--
--         , responseSetIdentityPoolRoles $
--             newSetIdentityPoolRolesResponse
--
--         , responseSetPrincipalTagAttributeMap $
--             newSetPrincipalTagAttributeMapResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnlinkDeveloperIdentity $
--             newUnlinkDeveloperIdentityResponse
--
--         , responseUnlinkIdentity $
--             newUnlinkIdentityResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateIdentityPool $
--             newIdentityPool
--
--           ]
--     ]

-- Requests

requestCreateIdentityPool :: CreateIdentityPool -> TestTree
requestCreateIdentityPool =
  req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool.yaml"

requestDeleteIdentities :: DeleteIdentities -> TestTree
requestDeleteIdentities =
  req
    "DeleteIdentities"
    "fixture/DeleteIdentities.yaml"

requestDeleteIdentityPool :: DeleteIdentityPool -> TestTree
requestDeleteIdentityPool =
  req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool.yaml"

requestDescribeIdentity :: DescribeIdentity -> TestTree
requestDescribeIdentity =
  req
    "DescribeIdentity"
    "fixture/DescribeIdentity.yaml"

requestDescribeIdentityPool :: DescribeIdentityPool -> TestTree
requestDescribeIdentityPool =
  req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool.yaml"

requestGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
requestGetCredentialsForIdentity =
  req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity.yaml"

requestGetId :: GetId -> TestTree
requestGetId =
  req
    "GetId"
    "fixture/GetId.yaml"

requestGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
requestGetIdentityPoolRoles =
  req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles.yaml"

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

requestGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMap -> TestTree
requestGetPrincipalTagAttributeMap =
  req
    "GetPrincipalTagAttributeMap"
    "fixture/GetPrincipalTagAttributeMap.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities =
  req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestListIdentityPools :: ListIdentityPools -> TestTree
requestListIdentityPools =
  req
    "ListIdentityPools"
    "fixture/ListIdentityPools.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
requestLookupDeveloperIdentity =
  req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity.yaml"

requestMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
requestMergeDeveloperIdentities =
  req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities.yaml"

requestSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
requestSetIdentityPoolRoles =
  req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles.yaml"

requestSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMap -> TestTree
requestSetPrincipalTagAttributeMap =
  req
    "SetPrincipalTagAttributeMap"
    "fixture/SetPrincipalTagAttributeMap.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
requestUnlinkDeveloperIdentity =
  req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity.yaml"

requestUnlinkIdentity :: UnlinkIdentity -> TestTree
requestUnlinkIdentity =
  req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateIdentityPool :: UpdateIdentityPool -> TestTree
requestUpdateIdentityPool =
  req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool.yaml"

-- Responses

responseCreateIdentityPool :: IdentityPool -> TestTree
responseCreateIdentityPool =
  res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIdentityPool)

responseDeleteIdentities :: DeleteIdentitiesResponse -> TestTree
responseDeleteIdentities =
  res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentities)

responseDeleteIdentityPool :: DeleteIdentityPoolResponse -> TestTree
responseDeleteIdentityPool =
  res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIdentityPool)

responseDescribeIdentity :: IdentityDescription -> TestTree
responseDescribeIdentity =
  res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentity)

responseDescribeIdentityPool :: IdentityPool -> TestTree
responseDescribeIdentityPool =
  res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityPool)

responseGetCredentialsForIdentity :: GetCredentialsForIdentityResponse -> TestTree
responseGetCredentialsForIdentity =
  res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCredentialsForIdentity)

responseGetId :: GetIdResponse -> TestTree
responseGetId =
  res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetId)

responseGetIdentityPoolRoles :: GetIdentityPoolRolesResponse -> TestTree
responseGetIdentityPoolRoles =
  res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityPoolRoles)

responseGetOpenIdToken :: GetOpenIdTokenResponse -> TestTree
responseGetOpenIdToken =
  res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpenIdToken)

responseGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
responseGetOpenIdTokenForDeveloperIdentity =
  res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOpenIdTokenForDeveloperIdentity)

responseGetPrincipalTagAttributeMap :: GetPrincipalTagAttributeMapResponse -> TestTree
responseGetPrincipalTagAttributeMap =
  res
    "GetPrincipalTagAttributeMapResponse"
    "fixture/GetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPrincipalTagAttributeMap)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities =
  res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentities)

responseListIdentityPools :: ListIdentityPoolsResponse -> TestTree
responseListIdentityPools =
  res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityPools)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseLookupDeveloperIdentity :: LookupDeveloperIdentityResponse -> TestTree
responseLookupDeveloperIdentity =
  res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LookupDeveloperIdentity)

responseMergeDeveloperIdentities :: MergeDeveloperIdentitiesResponse -> TestTree
responseMergeDeveloperIdentities =
  res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeDeveloperIdentities)

responseSetIdentityPoolRoles :: SetIdentityPoolRolesResponse -> TestTree
responseSetIdentityPoolRoles =
  res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityPoolRoles)

responseSetPrincipalTagAttributeMap :: SetPrincipalTagAttributeMapResponse -> TestTree
responseSetPrincipalTagAttributeMap =
  res
    "SetPrincipalTagAttributeMapResponse"
    "fixture/SetPrincipalTagAttributeMapResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetPrincipalTagAttributeMap)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnlinkDeveloperIdentity :: UnlinkDeveloperIdentityResponse -> TestTree
responseUnlinkDeveloperIdentity =
  res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnlinkDeveloperIdentity)

responseUnlinkIdentity :: UnlinkIdentityResponse -> TestTree
responseUnlinkIdentity =
  res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnlinkIdentity)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateIdentityPool :: IdentityPool -> TestTree
responseUpdateIdentityPool =
  res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIdentityPool)
