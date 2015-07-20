{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
--         , testDescribeIdentityPool $
--             describeIdentityPool
--
--         , testGetOpenIdTokenForDeveloperIdentity $
--             getOpenIdTokenForDeveloperIdentity
--
--         , testUnlinkDeveloperIdentity $
--             unlinkDeveloperIdentity
--
--         , testGetCredentialsForIdentity $
--             getCredentialsForIdentity
--
--         , testListIdentityPools $
--             listIdentityPools
--
--         , testGetIdentityPoolRoles $
--             getIdentityPoolRoles
--
--         , testDeleteIdentityPool $
--             deleteIdentityPool
--
--         , testUpdateIdentityPool $
--             updateIdentityPool
--
--         , testGetId $
--             getId
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
--         , testDescribeIdentityPoolResponse $
--             identityPool
--
--         , testGetOpenIdTokenForDeveloperIdentityResponse $
--             getOpenIdTokenForDeveloperIdentityResponse
--
--         , testUnlinkDeveloperIdentityResponse $
--             unlinkDeveloperIdentityResponse
--
--         , testGetCredentialsForIdentityResponse $
--             getCredentialsForIdentityResponse
--
--         , testListIdentityPoolsResponse $
--             listIdentityPoolsResponse
--
--         , testGetIdentityPoolRolesResponse $
--             getIdentityPoolRolesResponse
--
--         , testDeleteIdentityPoolResponse $
--             deleteIdentityPoolResponse
--
--         , testUpdateIdentityPoolResponse $
--             identityPool
--
--         , testGetIdResponse $
--             getIdResponse
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
    "fixture/GetOpenIdToken"

testDescribeIdentityPool :: DescribeIdentityPool -> TestTree
testDescribeIdentityPool = req
    "DescribeIdentityPool"
    "fixture/DescribeIdentityPool"

testGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
testGetOpenIdTokenForDeveloperIdentity = req
    "GetOpenIdTokenForDeveloperIdentity"
    "fixture/GetOpenIdTokenForDeveloperIdentity"

testUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
testUnlinkDeveloperIdentity = req
    "UnlinkDeveloperIdentity"
    "fixture/UnlinkDeveloperIdentity"

testGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
testGetCredentialsForIdentity = req
    "GetCredentialsForIdentity"
    "fixture/GetCredentialsForIdentity"

testListIdentityPools :: ListIdentityPools -> TestTree
testListIdentityPools = req
    "ListIdentityPools"
    "fixture/ListIdentityPools"

testGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
testGetIdentityPoolRoles = req
    "GetIdentityPoolRoles"
    "fixture/GetIdentityPoolRoles"

testDeleteIdentityPool :: DeleteIdentityPool -> TestTree
testDeleteIdentityPool = req
    "DeleteIdentityPool"
    "fixture/DeleteIdentityPool"

testUpdateIdentityPool :: UpdateIdentityPool -> TestTree
testUpdateIdentityPool = req
    "UpdateIdentityPool"
    "fixture/UpdateIdentityPool"

testGetId :: GetId -> TestTree
testGetId = req
    "GetId"
    "fixture/GetId"

testDeleteIdentities :: DeleteIdentities -> TestTree
testDeleteIdentities = req
    "DeleteIdentities"
    "fixture/DeleteIdentities"

testSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
testSetIdentityPoolRoles = req
    "SetIdentityPoolRoles"
    "fixture/SetIdentityPoolRoles"

testListIdentities :: ListIdentities -> TestTree
testListIdentities = req
    "ListIdentities"
    "fixture/ListIdentities"

testLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
testLookupDeveloperIdentity = req
    "LookupDeveloperIdentity"
    "fixture/LookupDeveloperIdentity"

testUnlinkIdentity :: UnlinkIdentity -> TestTree
testUnlinkIdentity = req
    "UnlinkIdentity"
    "fixture/UnlinkIdentity"

testDescribeIdentity :: DescribeIdentity -> TestTree
testDescribeIdentity = req
    "DescribeIdentity"
    "fixture/DescribeIdentity"

testCreateIdentityPool :: CreateIdentityPool -> TestTree
testCreateIdentityPool = req
    "CreateIdentityPool"
    "fixture/CreateIdentityPool"

testMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
testMergeDeveloperIdentities = req
    "MergeDeveloperIdentities"
    "fixture/MergeDeveloperIdentities"

-- Responses

testGetOpenIdTokenResponse :: GetOpenIdTokenResponse -> TestTree
testGetOpenIdTokenResponse = res
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse"
    (Proxy :: Proxy GetOpenIdToken)

testDescribeIdentityPoolResponse :: IdentityPool -> TestTree
testDescribeIdentityPoolResponse = res
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse"
    (Proxy :: Proxy DescribeIdentityPool)

testGetOpenIdTokenForDeveloperIdentityResponse :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
testGetOpenIdTokenForDeveloperIdentityResponse = res
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse"
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

testUnlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse -> TestTree
testUnlinkDeveloperIdentityResponse = res
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse"
    (Proxy :: Proxy UnlinkDeveloperIdentity)

testGetCredentialsForIdentityResponse :: GetCredentialsForIdentityResponse -> TestTree
testGetCredentialsForIdentityResponse = res
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse"
    (Proxy :: Proxy GetCredentialsForIdentity)

testListIdentityPoolsResponse :: ListIdentityPoolsResponse -> TestTree
testListIdentityPoolsResponse = res
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse"
    (Proxy :: Proxy ListIdentityPools)

testGetIdentityPoolRolesResponse :: GetIdentityPoolRolesResponse -> TestTree
testGetIdentityPoolRolesResponse = res
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse"
    (Proxy :: Proxy GetIdentityPoolRoles)

testDeleteIdentityPoolResponse :: DeleteIdentityPoolResponse -> TestTree
testDeleteIdentityPoolResponse = res
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse"
    (Proxy :: Proxy DeleteIdentityPool)

testUpdateIdentityPoolResponse :: IdentityPool -> TestTree
testUpdateIdentityPoolResponse = res
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse"
    (Proxy :: Proxy UpdateIdentityPool)

testGetIdResponse :: GetIdResponse -> TestTree
testGetIdResponse = res
    "GetIdResponse"
    "fixture/GetIdResponse"
    (Proxy :: Proxy GetId)

testDeleteIdentitiesResponse :: DeleteIdentitiesResponse -> TestTree
testDeleteIdentitiesResponse = res
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse"
    (Proxy :: Proxy DeleteIdentities)

testSetIdentityPoolRolesResponse :: SetIdentityPoolRolesResponse -> TestTree
testSetIdentityPoolRolesResponse = res
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse"
    (Proxy :: Proxy SetIdentityPoolRoles)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = res
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

testLookupDeveloperIdentityResponse :: LookupDeveloperIdentityResponse -> TestTree
testLookupDeveloperIdentityResponse = res
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse"
    (Proxy :: Proxy LookupDeveloperIdentity)

testUnlinkIdentityResponse :: UnlinkIdentityResponse -> TestTree
testUnlinkIdentityResponse = res
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse"
    (Proxy :: Proxy UnlinkIdentity)

testDescribeIdentityResponse :: IdentityDescription -> TestTree
testDescribeIdentityResponse = res
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse"
    (Proxy :: Proxy DescribeIdentity)

testCreateIdentityPoolResponse :: IdentityPool -> TestTree
testCreateIdentityPoolResponse = res
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse"
    (Proxy :: Proxy CreateIdentityPool)

testMergeDeveloperIdentitiesResponse :: MergeDeveloperIdentitiesResponse -> TestTree
testMergeDeveloperIdentitiesResponse = res
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse"
    (Proxy :: Proxy MergeDeveloperIdentities)
