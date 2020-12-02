{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             getOpenIdToken
--
--         , requestGetOpenIdTokenForDeveloperIdentity $
--             getOpenIdTokenForDeveloperIdentity
--
--         , requestDescribeIdentityPool $
--             describeIdentityPool
--
--         , requestGetId $
--             getId
--
--         , requestDeleteIdentityPool $
--             deleteIdentityPool
--
--         , requestUpdateIdentityPool $
--             updateIdentityPool
--
--         , requestUnlinkDeveloperIdentity $
--             unlinkDeveloperIdentity
--
--         , requestGetIdentityPoolRoles $
--             getIdentityPoolRoles
--
--         , requestListIdentityPools $
--             listIdentityPools
--
--         , requestGetCredentialsForIdentity $
--             getCredentialsForIdentity
--
--         , requestDeleteIdentities $
--             deleteIdentities
--
--         , requestSetIdentityPoolRoles $
--             setIdentityPoolRoles
--
--         , requestListIdentities $
--             listIdentities
--
--         , requestLookupDeveloperIdentity $
--             lookupDeveloperIdentity
--
--         , requestUnlinkIdentity $
--             unlinkIdentity
--
--         , requestDescribeIdentity $
--             describeIdentity
--
--         , requestCreateIdentityPool $
--             createIdentityPool
--
--         , requestMergeDeveloperIdentities $
--             mergeDeveloperIdentities
--
--           ]

--     , testGroup "response"
--         [ responseGetOpenIdToken $
--             getOpenIdTokenResponse
--
--         , responseGetOpenIdTokenForDeveloperIdentity $
--             getOpenIdTokenForDeveloperIdentityResponse
--
--         , responseDescribeIdentityPool $
--             identityPool
--
--         , responseGetId $
--             getIdResponse
--
--         , responseDeleteIdentityPool $
--             deleteIdentityPoolResponse
--
--         , responseUpdateIdentityPool $
--             identityPool
--
--         , responseUnlinkDeveloperIdentity $
--             unlinkDeveloperIdentityResponse
--
--         , responseGetIdentityPoolRoles $
--             getIdentityPoolRolesResponse
--
--         , responseListIdentityPools $
--             listIdentityPoolsResponse
--
--         , responseGetCredentialsForIdentity $
--             getCredentialsForIdentityResponse
--
--         , responseDeleteIdentities $
--             deleteIdentitiesResponse
--
--         , responseSetIdentityPoolRoles $
--             setIdentityPoolRolesResponse
--
--         , responseListIdentities $
--             listIdentitiesResponse
--
--         , responseLookupDeveloperIdentity $
--             lookupDeveloperIdentityResponse
--
--         , responseUnlinkIdentity $
--             unlinkIdentityResponse
--
--         , responseDescribeIdentity $
--             identityDescription
--
--         , responseCreateIdentityPool $
--             identityPool
--
--         , responseMergeDeveloperIdentities $
--             mergeDeveloperIdentitiesResponse
--
--           ]
--     ]

-- Requests

requestGetOpenIdToken :: GetOpenIdToken -> TestTree
requestGetOpenIdToken = req
    "GetOpenIdToken"
    "fixture/GetOpenIdToken.yaml"

requestGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
requestGetOpenIdTokenForDeveloperIdentity = req
    "GetOpenIdTokenForDeveloperIdentity"
    "fixture/GetOpenIdTokenForDeveloperIdentity.yaml"

requestDescribeIdentityPool :: DescribeIdentityPool -> TestTree
requestDescribeIdentityPool = req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool.yaml"

requestGetId :: GetId -> TestTree
requestGetId = req
    "GetId"
    "fixture/GetId.yaml"

requestDeleteIdentityPool :: DeleteIdentityPool -> TestTree
requestDeleteIdentityPool = req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool.yaml"

requestUpdateIdentityPool :: UpdateIdentityPool -> TestTree
requestUpdateIdentityPool = req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool.yaml"

requestUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
requestUnlinkDeveloperIdentity = req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity.yaml"

requestGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
requestGetIdentityPoolRoles = req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles.yaml"

requestListIdentityPools :: ListIdentityPools -> TestTree
requestListIdentityPools = req
    "ListIdentityPools"
    "fixture/ListIdentityPools.yaml"

requestGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
requestGetCredentialsForIdentity = req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity.yaml"

requestDeleteIdentities :: DeleteIdentities -> TestTree
requestDeleteIdentities = req
    "DeleteIdentities"
    "fixture/DeleteIdentities.yaml"

requestSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
requestSetIdentityPoolRoles = req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles.yaml"

requestListIdentities :: ListIdentities -> TestTree
requestListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

requestLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
requestLookupDeveloperIdentity = req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity.yaml"

requestUnlinkIdentity :: UnlinkIdentity -> TestTree
requestUnlinkIdentity = req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity.yaml"

requestDescribeIdentity :: DescribeIdentity -> TestTree
requestDescribeIdentity = req
    "DescribeIdentity"
    "fixture/DescribeIdentity.yaml"

requestCreateIdentityPool :: CreateIdentityPool -> TestTree
requestCreateIdentityPool = req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool.yaml"

requestMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
requestMergeDeveloperIdentities = req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities.yaml"

-- Responses

responseGetOpenIdToken :: GetOpenIdTokenResponse -> TestTree
responseGetOpenIdToken = res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetOpenIdToken)

responseGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
responseGetOpenIdTokenForDeveloperIdentity = res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

responseDescribeIdentityPool :: IdentityPool -> TestTree
responseDescribeIdentityPool = res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DescribeIdentityPool)

responseGetId :: GetIdResponse -> TestTree
responseGetId = res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetId)

responseDeleteIdentityPool :: DeleteIdentityPoolResponse -> TestTree
responseDeleteIdentityPool = res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DeleteIdentityPool)

responseUpdateIdentityPool :: IdentityPool -> TestTree
responseUpdateIdentityPool = res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UpdateIdentityPool)

responseUnlinkDeveloperIdentity :: UnlinkDeveloperIdentityResponse -> TestTree
responseUnlinkDeveloperIdentity = res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UnlinkDeveloperIdentity)

responseGetIdentityPoolRoles :: GetIdentityPoolRolesResponse -> TestTree
responseGetIdentityPoolRoles = res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetIdentityPoolRoles)

responseListIdentityPools :: ListIdentityPoolsResponse -> TestTree
responseListIdentityPools = res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy ListIdentityPools)

responseGetCredentialsForIdentity :: GetCredentialsForIdentityResponse -> TestTree
responseGetCredentialsForIdentity = res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetCredentialsForIdentity)

responseDeleteIdentities :: DeleteIdentitiesResponse -> TestTree
responseDeleteIdentities = res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DeleteIdentities)

responseSetIdentityPoolRoles :: SetIdentityPoolRolesResponse -> TestTree
responseSetIdentityPoolRoles = res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy SetIdentityPoolRoles)

responseListIdentities :: ListIdentitiesResponse -> TestTree
responseListIdentities = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy ListIdentities)

responseLookupDeveloperIdentity :: LookupDeveloperIdentityResponse -> TestTree
responseLookupDeveloperIdentity = res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy LookupDeveloperIdentity)

responseUnlinkIdentity :: UnlinkIdentityResponse -> TestTree
responseUnlinkIdentity = res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UnlinkIdentity)

responseDescribeIdentity :: IdentityDescription -> TestTree
responseDescribeIdentity = res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DescribeIdentity)

responseCreateIdentityPool :: IdentityPool -> TestTree
responseCreateIdentityPool = res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy CreateIdentityPool)

responseMergeDeveloperIdentities :: MergeDeveloperIdentitiesResponse -> TestTree
responseMergeDeveloperIdentities = res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy MergeDeveloperIdentities)
