{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CognitoIdentity where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CognitoIdentity
import Test.AWS.CognitoIdentity.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testGetOpenIdToken $
--             getOpenIdToken
--
--         , testGetOpenIdTokenForDeveloperIdentity $
--             getOpenIdTokenForDeveloperIdentity
--
--         , testDescribeIdentityPool $
--             describeIdentityPool
--
--         , testGetId $
--             getId
--
--         , testDeleteIdentityPool $
--             deleteIdentityPool
--
--         , testUpdateIdentityPool $
--             updateIdentityPool
--
--         , testUnlinkDeveloperIdentity $
--             unlinkDeveloperIdentity
--
--         , testGetIdentityPoolRoles $
--             getIdentityPoolRoles
--
--         , testListIdentityPools $
--             listIdentityPools
--
--         , testGetCredentialsForIdentity $
--             getCredentialsForIdentity
--
--         , testDeleteIdentities $
--             deleteIdentities
--
--         , testSetIdentityPoolRoles $
--             setIdentityPoolRoles
--
--         , testListIdentities $
--             listIdentities
--
--         , testLookupDeveloperIdentity $
--             lookupDeveloperIdentity
--
--         , testUnlinkIdentity $
--             unlinkIdentity
--
--         , testDescribeIdentity $
--             describeIdentity
--
--         , testCreateIdentityPool $
--             createIdentityPool
--
--         , testMergeDeveloperIdentities $
--             mergeDeveloperIdentities
--
--           ]

--     , testGroup "response"
--         [ testGetOpenIdTokenResponse $
--             getOpenIdTokenResponse
--
--         , testGetOpenIdTokenForDeveloperIdentityResponse $
--             getOpenIdTokenForDeveloperIdentityResponse
--
--         , testDescribeIdentityPoolResponse $
--             identityPool
--
--         , testGetIdResponse $
--             getIdResponse
--
--         , testDeleteIdentityPoolResponse $
--             deleteIdentityPoolResponse
--
--         , testUpdateIdentityPoolResponse $
--             identityPool
--
--         , testUnlinkDeveloperIdentityResponse $
--             unlinkDeveloperIdentityResponse
--
--         , testGetIdentityPoolRolesResponse $
--             getIdentityPoolRolesResponse
--
--         , testListIdentityPoolsResponse $
--             listIdentityPoolsResponse
--
--         , testGetCredentialsForIdentityResponse $
--             getCredentialsForIdentityResponse
--
--         , testDeleteIdentitiesResponse $
--             deleteIdentitiesResponse
--
--         , testSetIdentityPoolRolesResponse $
--             setIdentityPoolRolesResponse
--
--         , testListIdentitiesResponse $
--             listIdentitiesResponse
--
--         , testLookupDeveloperIdentityResponse $
--             lookupDeveloperIdentityResponse
--
--         , testUnlinkIdentityResponse $
--             unlinkIdentityResponse
--
--         , testDescribeIdentityResponse $
--             identityDescription
--
--         , testCreateIdentityPoolResponse $
--             identityPool
--
--         , testMergeDeveloperIdentitiesResponse $
--             mergeDeveloperIdentitiesResponse
--
--           ]
--     ]

-- Requests

testGetOpenIdToken :: GetOpenIdToken -> TestTree
testGetOpenIdToken = req
    "GetOpenIdToken"
    "fixture/GetOpenIdToken.yaml"

testGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
testGetOpenIdTokenForDeveloperIdentity = req
    "GetOpenIdTokenForDeveloperIdentity"
    "fixture/GetOpenIdTokenForDeveloperIdentity.yaml"

testDescribeIdentityPool :: DescribeIdentityPool -> TestTree
testDescribeIdentityPool = req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool.yaml"

testGetId :: GetId -> TestTree
testGetId = req
    "GetId"
    "fixture/GetId.yaml"

testDeleteIdentityPool :: DeleteIdentityPool -> TestTree
testDeleteIdentityPool = req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool.yaml"

testUpdateIdentityPool :: UpdateIdentityPool -> TestTree
testUpdateIdentityPool = req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool.yaml"

testUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
testUnlinkDeveloperIdentity = req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity.yaml"

testGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
testGetIdentityPoolRoles = req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles.yaml"

testListIdentityPools :: ListIdentityPools -> TestTree
testListIdentityPools = req
    "ListIdentityPools"
    "fixture/ListIdentityPools.yaml"

testGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
testGetCredentialsForIdentity = req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity.yaml"

testDeleteIdentities :: DeleteIdentities -> TestTree
testDeleteIdentities = req
    "DeleteIdentities"
    "fixture/DeleteIdentities.yaml"

testSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
testSetIdentityPoolRoles = req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles.yaml"

testListIdentities :: ListIdentities -> TestTree
testListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities.yaml"

testLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
testLookupDeveloperIdentity = req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity.yaml"

testUnlinkIdentity :: UnlinkIdentity -> TestTree
testUnlinkIdentity = req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity.yaml"

testDescribeIdentity :: DescribeIdentity -> TestTree
testDescribeIdentity = req
    "DescribeIdentity"
    "fixture/DescribeIdentity.yaml"

testCreateIdentityPool :: CreateIdentityPool -> TestTree
testCreateIdentityPool = req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool.yaml"

testMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
testMergeDeveloperIdentities = req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities.yaml"

-- Responses

testGetOpenIdTokenResponse :: GetOpenIdTokenResponse -> TestTree
testGetOpenIdTokenResponse = res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetOpenIdToken)

testGetOpenIdTokenForDeveloperIdentityResponse :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
testGetOpenIdTokenForDeveloperIdentityResponse = res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

testDescribeIdentityPoolResponse :: IdentityPool -> TestTree
testDescribeIdentityPoolResponse = res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DescribeIdentityPool)

testGetIdResponse :: GetIdResponse -> TestTree
testGetIdResponse = res
    "GetIdResponse"
    "fixture/GetIdResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetId)

testDeleteIdentityPoolResponse :: DeleteIdentityPoolResponse -> TestTree
testDeleteIdentityPoolResponse = res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DeleteIdentityPool)

testUpdateIdentityPoolResponse :: IdentityPool -> TestTree
testUpdateIdentityPoolResponse = res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UpdateIdentityPool)

testUnlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse -> TestTree
testUnlinkDeveloperIdentityResponse = res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UnlinkDeveloperIdentity)

testGetIdentityPoolRolesResponse :: GetIdentityPoolRolesResponse -> TestTree
testGetIdentityPoolRolesResponse = res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetIdentityPoolRoles)

testListIdentityPoolsResponse :: ListIdentityPoolsResponse -> TestTree
testListIdentityPoolsResponse = res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy ListIdentityPools)

testGetCredentialsForIdentityResponse :: GetCredentialsForIdentityResponse -> TestTree
testGetCredentialsForIdentityResponse = res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy GetCredentialsForIdentity)

testDeleteIdentitiesResponse :: DeleteIdentitiesResponse -> TestTree
testDeleteIdentitiesResponse = res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DeleteIdentities)

testSetIdentityPoolRolesResponse :: SetIdentityPoolRolesResponse -> TestTree
testSetIdentityPoolRolesResponse = res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy SetIdentityPoolRoles)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy ListIdentities)

testLookupDeveloperIdentityResponse :: LookupDeveloperIdentityResponse -> TestTree
testLookupDeveloperIdentityResponse = res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy LookupDeveloperIdentity)

testUnlinkIdentityResponse :: UnlinkIdentityResponse -> TestTree
testUnlinkIdentityResponse = res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy UnlinkIdentity)

testDescribeIdentityResponse :: IdentityDescription -> TestTree
testDescribeIdentityResponse = res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy DescribeIdentity)

testCreateIdentityPoolResponse :: IdentityPool -> TestTree
testCreateIdentityPoolResponse = res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy CreateIdentityPool)

testMergeDeveloperIdentitiesResponse :: MergeDeveloperIdentitiesResponse -> TestTree
testMergeDeveloperIdentitiesResponse = res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse.proto"
    cognitoIdentity
    (Proxy :: Proxy MergeDeveloperIdentities)
