{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CognitoIdentity where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CognitoIdentity

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
testGetOpenIdToken = undefined

testDescribeIdentityPool :: DescribeIdentityPool -> TestTree
testDescribeIdentityPool = undefined

testGetOpenIdTokenForDeveloperIdentity :: GetOpenIdTokenForDeveloperIdentity -> TestTree
testGetOpenIdTokenForDeveloperIdentity = undefined

testUnlinkDeveloperIdentity :: UnlinkDeveloperIdentity -> TestTree
testUnlinkDeveloperIdentity = undefined

testGetCredentialsForIdentity :: GetCredentialsForIdentity -> TestTree
testGetCredentialsForIdentity = undefined

testListIdentityPools :: ListIdentityPools -> TestTree
testListIdentityPools = undefined

testGetIdentityPoolRoles :: GetIdentityPoolRoles -> TestTree
testGetIdentityPoolRoles = undefined

testDeleteIdentityPool :: DeleteIdentityPool -> TestTree
testDeleteIdentityPool = undefined

testUpdateIdentityPool :: UpdateIdentityPool -> TestTree
testUpdateIdentityPool = undefined

testGetId :: GetId -> TestTree
testGetId = undefined

testDeleteIdentities :: DeleteIdentities -> TestTree
testDeleteIdentities = undefined

testSetIdentityPoolRoles :: SetIdentityPoolRoles -> TestTree
testSetIdentityPoolRoles = undefined

testListIdentities :: ListIdentities -> TestTree
testListIdentities = undefined

testLookupDeveloperIdentity :: LookupDeveloperIdentity -> TestTree
testLookupDeveloperIdentity = undefined

testUnlinkIdentity :: UnlinkIdentity -> TestTree
testUnlinkIdentity = undefined

testDescribeIdentity :: DescribeIdentity -> TestTree
testDescribeIdentity = undefined

testCreateIdentityPool :: CreateIdentityPool -> TestTree
testCreateIdentityPool = undefined

testMergeDeveloperIdentities :: MergeDeveloperIdentities -> TestTree
testMergeDeveloperIdentities = undefined

-- Responses

testGetOpenIdTokenResponse :: GetOpenIdTokenResponse -> TestTree
testGetOpenIdTokenResponse = resp
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse"
    (Proxy :: Proxy GetOpenIdToken)

testDescribeIdentityPoolResponse :: IdentityPool -> TestTree
testDescribeIdentityPoolResponse = resp
    "DescribeIdentityPoolResponse"
    "fixture/DescribeIdentityPoolResponse"
    (Proxy :: Proxy DescribeIdentityPool)

testGetOpenIdTokenForDeveloperIdentityResponse :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
testGetOpenIdTokenForDeveloperIdentityResponse = resp
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse"
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

testUnlinkDeveloperIdentityResponse :: UnlinkDeveloperIdentityResponse -> TestTree
testUnlinkDeveloperIdentityResponse = resp
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse"
    (Proxy :: Proxy UnlinkDeveloperIdentity)

testGetCredentialsForIdentityResponse :: GetCredentialsForIdentityResponse -> TestTree
testGetCredentialsForIdentityResponse = resp
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse"
    (Proxy :: Proxy GetCredentialsForIdentity)

testListIdentityPoolsResponse :: ListIdentityPoolsResponse -> TestTree
testListIdentityPoolsResponse = resp
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse"
    (Proxy :: Proxy ListIdentityPools)

testGetIdentityPoolRolesResponse :: GetIdentityPoolRolesResponse -> TestTree
testGetIdentityPoolRolesResponse = resp
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse"
    (Proxy :: Proxy GetIdentityPoolRoles)

testDeleteIdentityPoolResponse :: DeleteIdentityPoolResponse -> TestTree
testDeleteIdentityPoolResponse = resp
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse"
    (Proxy :: Proxy DeleteIdentityPool)

testUpdateIdentityPoolResponse :: IdentityPool -> TestTree
testUpdateIdentityPoolResponse = resp
    "UpdateIdentityPoolResponse"
    "fixture/UpdateIdentityPoolResponse"
    (Proxy :: Proxy UpdateIdentityPool)

testGetIdResponse :: GetIdResponse -> TestTree
testGetIdResponse = resp
    "GetIdResponse"
    "fixture/GetIdResponse"
    (Proxy :: Proxy GetId)

testDeleteIdentitiesResponse :: DeleteIdentitiesResponse -> TestTree
testDeleteIdentitiesResponse = resp
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse"
    (Proxy :: Proxy DeleteIdentities)

testSetIdentityPoolRolesResponse :: SetIdentityPoolRolesResponse -> TestTree
testSetIdentityPoolRolesResponse = resp
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse"
    (Proxy :: Proxy SetIdentityPoolRoles)

testListIdentitiesResponse :: ListIdentitiesResponse -> TestTree
testListIdentitiesResponse = resp
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

testLookupDeveloperIdentityResponse :: LookupDeveloperIdentityResponse -> TestTree
testLookupDeveloperIdentityResponse = resp
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse"
    (Proxy :: Proxy LookupDeveloperIdentity)

testUnlinkIdentityResponse :: UnlinkIdentityResponse -> TestTree
testUnlinkIdentityResponse = resp
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse"
    (Proxy :: Proxy UnlinkIdentity)

testDescribeIdentityResponse :: IdentityDescription -> TestTree
testDescribeIdentityResponse = resp
    "DescribeIdentityResponse"
    "fixture/DescribeIdentityResponse"
    (Proxy :: Proxy DescribeIdentity)

testCreateIdentityPoolResponse :: IdentityPool -> TestTree
testCreateIdentityPoolResponse = resp
    "CreateIdentityPoolResponse"
    "fixture/CreateIdentityPoolResponse"
    (Proxy :: Proxy CreateIdentityPool)

testMergeDeveloperIdentitiesResponse :: MergeDeveloperIdentitiesResponse -> TestTree
testMergeDeveloperIdentitiesResponse = resp
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse"
    (Proxy :: Proxy MergeDeveloperIdentities)

instance Out CognitoErrorCode
instance Out CreateIdentityPool
instance Out Credentials
instance Out DeleteIdentities
instance Out DeleteIdentitiesResponse
instance Out DeleteIdentityPool
instance Out DeleteIdentityPoolResponse
instance Out DescribeIdentity
instance Out DescribeIdentityPool
instance Out GetCredentialsForIdentity
instance Out GetCredentialsForIdentityResponse
instance Out GetId
instance Out GetIdResponse
instance Out GetIdentityPoolRoles
instance Out GetIdentityPoolRolesResponse
instance Out GetOpenIdToken
instance Out GetOpenIdTokenForDeveloperIdentity
instance Out GetOpenIdTokenForDeveloperIdentityResponse
instance Out GetOpenIdTokenResponse
instance Out IdentityDescription
instance Out IdentityPool
instance Out IdentityPoolShortDescription
instance Out ListIdentities
instance Out ListIdentitiesResponse
instance Out ListIdentityPools
instance Out ListIdentityPoolsResponse
instance Out LookupDeveloperIdentity
instance Out LookupDeveloperIdentityResponse
instance Out MergeDeveloperIdentities
instance Out MergeDeveloperIdentitiesResponse
instance Out SetIdentityPoolRoles
instance Out SetIdentityPoolRolesResponse
instance Out UnlinkDeveloperIdentity
instance Out UnlinkDeveloperIdentityResponse
instance Out UnlinkIdentity
instance Out UnlinkIdentityResponse
instance Out UnprocessedIdentityId
instance Out UpdateIdentityPool
