-- Module      : Test.AWS.Gen.CognitoIdentity
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ getOpenIdTokenTest $
--             getOpenIdToken
--
--         , describeIdentityPoolTest $
--             describeIdentityPool
--
--         , getOpenIdTokenForDeveloperIdentityTest $
--             getOpenIdTokenForDeveloperIdentity
--
--         , unlinkDeveloperIdentityTest $
--             unlinkDeveloperIdentity
--
--         , getCredentialsForIdentityTest $
--             getCredentialsForIdentity
--
--         , listIdentityPoolsTest $
--             listIdentityPools
--
--         , getIdentityPoolRolesTest $
--             getIdentityPoolRoles
--
--         , deleteIdentityPoolTest $
--             deleteIdentityPool
--
--         , updateIdentityPoolTest $
--             updateIdentityPool
--
--         , getIdTest $
--             getId
--
--         , deleteIdentitiesTest $
--             deleteIdentities
--
--         , setIdentityPoolRolesTest $
--             setIdentityPoolRoles
--
--         , listIdentitiesTest $
--             listIdentities
--
--         , lookupDeveloperIdentityTest $
--             lookupDeveloperIdentity
--
--         , unlinkIdentityTest $
--             unlinkIdentity
--
--         , describeIdentityTest $
--             describeIdentity
--
--         , createIdentityPoolTest $
--             createIdentityPool
--
--         , mergeDeveloperIdentitiesTest $
--             mergeDeveloperIdentities
--
--           ]

--     , testGroup "response"
--         [ getOpenIdTokenResponseTest $
--             getOpenIdTokenResponse
--
--         , identityPoolTest $
--             identityPool
--
--         , getOpenIdTokenForDeveloperIdentityResponseTest $
--             getOpenIdTokenForDeveloperIdentityResponse
--
--         , unlinkDeveloperIdentityResponseTest $
--             unlinkDeveloperIdentityResponse
--
--         , getCredentialsForIdentityResponseTest $
--             getCredentialsForIdentityResponse
--
--         , listIdentityPoolsResponseTest $
--             listIdentityPoolsResponse
--
--         , getIdentityPoolRolesResponseTest $
--             getIdentityPoolRolesResponse
--
--         , deleteIdentityPoolResponseTest $
--             deleteIdentityPoolResponse
--
--         , identityPoolTest $
--             identityPool
--
--         , getIdResponseTest $
--             getIdResponse
--
--         , deleteIdentitiesResponseTest $
--             deleteIdentitiesResponse
--
--         , setIdentityPoolRolesResponseTest $
--             setIdentityPoolRolesResponse
--
--         , listIdentitiesResponseTest $
--             listIdentitiesResponse
--
--         , lookupDeveloperIdentityResponseTest $
--             lookupDeveloperIdentityResponse
--
--         , unlinkIdentityResponseTest $
--             unlinkIdentityResponse
--
--         , identityDescriptionTest $
--             identityDescription
--
--         , identityPoolTest $
--             identityPool
--
--         , mergeDeveloperIdentitiesResponseTest $
--             mergeDeveloperIdentitiesResponse
--
--           ]
--     ]

-- Requests

getOpenIdTokenTest :: GetOpenIdToken -> TestTree
getOpenIdTokenTest = undefined

describeIdentityPoolTest :: DescribeIdentityPool -> TestTree
describeIdentityPoolTest = undefined

getOpenIdTokenForDeveloperIdentityTest :: GetOpenIdTokenForDeveloperIdentity -> TestTree
getOpenIdTokenForDeveloperIdentityTest = undefined

unlinkDeveloperIdentityTest :: UnlinkDeveloperIdentity -> TestTree
unlinkDeveloperIdentityTest = undefined

getCredentialsForIdentityTest :: GetCredentialsForIdentity -> TestTree
getCredentialsForIdentityTest = undefined

listIdentityPoolsTest :: ListIdentityPools -> TestTree
listIdentityPoolsTest = undefined

getIdentityPoolRolesTest :: GetIdentityPoolRoles -> TestTree
getIdentityPoolRolesTest = undefined

deleteIdentityPoolTest :: DeleteIdentityPool -> TestTree
deleteIdentityPoolTest = undefined

updateIdentityPoolTest :: UpdateIdentityPool -> TestTree
updateIdentityPoolTest = undefined

getIdTest :: GetId -> TestTree
getIdTest = undefined

deleteIdentitiesTest :: DeleteIdentities -> TestTree
deleteIdentitiesTest = undefined

setIdentityPoolRolesTest :: SetIdentityPoolRoles -> TestTree
setIdentityPoolRolesTest = undefined

listIdentitiesTest :: ListIdentities -> TestTree
listIdentitiesTest = undefined

lookupDeveloperIdentityTest :: LookupDeveloperIdentity -> TestTree
lookupDeveloperIdentityTest = undefined

unlinkIdentityTest :: UnlinkIdentity -> TestTree
unlinkIdentityTest = undefined

describeIdentityTest :: DescribeIdentity -> TestTree
describeIdentityTest = undefined

createIdentityPoolTest :: CreateIdentityPool -> TestTree
createIdentityPoolTest = undefined

mergeDeveloperIdentitiesTest :: MergeDeveloperIdentities -> TestTree
mergeDeveloperIdentitiesTest = undefined

-- Responses

getOpenIdTokenResponseTest :: GetOpenIdTokenResponse -> TestTree
getOpenIdTokenResponseTest = resp
    "GetOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse"
    (Proxy :: Proxy GetOpenIdToken)

identityPoolTest :: IdentityPool -> TestTree
identityPoolTest = resp
    "IdentityPool"
    "fixture/IdentityPool"
    (Proxy :: Proxy DescribeIdentityPool)

getOpenIdTokenForDeveloperIdentityResponseTest :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
getOpenIdTokenForDeveloperIdentityResponseTest = resp
    "GetOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse"
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

unlinkDeveloperIdentityResponseTest :: UnlinkDeveloperIdentityResponse -> TestTree
unlinkDeveloperIdentityResponseTest = resp
    "UnlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse"
    (Proxy :: Proxy UnlinkDeveloperIdentity)

getCredentialsForIdentityResponseTest :: GetCredentialsForIdentityResponse -> TestTree
getCredentialsForIdentityResponseTest = resp
    "GetCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse"
    (Proxy :: Proxy GetCredentialsForIdentity)

listIdentityPoolsResponseTest :: ListIdentityPoolsResponse -> TestTree
listIdentityPoolsResponseTest = resp
    "ListIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse"
    (Proxy :: Proxy ListIdentityPools)

getIdentityPoolRolesResponseTest :: GetIdentityPoolRolesResponse -> TestTree
getIdentityPoolRolesResponseTest = resp
    "GetIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse"
    (Proxy :: Proxy GetIdentityPoolRoles)

deleteIdentityPoolResponseTest :: DeleteIdentityPoolResponse -> TestTree
deleteIdentityPoolResponseTest = resp
    "DeleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse"
    (Proxy :: Proxy DeleteIdentityPool)

identityPoolTest :: IdentityPool -> TestTree
identityPoolTest = resp
    "IdentityPool"
    "fixture/IdentityPool"
    (Proxy :: Proxy UpdateIdentityPool)

getIdResponseTest :: GetIdResponse -> TestTree
getIdResponseTest = resp
    "GetIdResponse"
    "fixture/GetIdResponse"
    (Proxy :: Proxy GetId)

deleteIdentitiesResponseTest :: DeleteIdentitiesResponse -> TestTree
deleteIdentitiesResponseTest = resp
    "DeleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse"
    (Proxy :: Proxy DeleteIdentities)

setIdentityPoolRolesResponseTest :: SetIdentityPoolRolesResponse -> TestTree
setIdentityPoolRolesResponseTest = resp
    "SetIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse"
    (Proxy :: Proxy SetIdentityPoolRoles)

listIdentitiesResponseTest :: ListIdentitiesResponse -> TestTree
listIdentitiesResponseTest = resp
    "ListIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

lookupDeveloperIdentityResponseTest :: LookupDeveloperIdentityResponse -> TestTree
lookupDeveloperIdentityResponseTest = resp
    "LookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse"
    (Proxy :: Proxy LookupDeveloperIdentity)

unlinkIdentityResponseTest :: UnlinkIdentityResponse -> TestTree
unlinkIdentityResponseTest = resp
    "UnlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse"
    (Proxy :: Proxy UnlinkIdentity)

identityDescriptionTest :: IdentityDescription -> TestTree
identityDescriptionTest = resp
    "IdentityDescription"
    "fixture/IdentityDescription"
    (Proxy :: Proxy DescribeIdentity)

identityPoolTest :: IdentityPool -> TestTree
identityPoolTest = resp
    "IdentityPool"
    "fixture/IdentityPool"
    (Proxy :: Proxy CreateIdentityPool)

mergeDeveloperIdentitiesResponseTest :: MergeDeveloperIdentitiesResponse -> TestTree
mergeDeveloperIdentitiesResponseTest = resp
    "MergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse"
    (Proxy :: Proxy MergeDeveloperIdentities)
