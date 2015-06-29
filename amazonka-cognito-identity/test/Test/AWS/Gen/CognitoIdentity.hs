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
--         [ createIdentityPoolTest $
--             createIdentityPool
--
--         , deleteIdentitiesTest $
--             deleteIdentities
--
--         , deleteIdentityPoolTest $
--             deleteIdentityPool
--
--         , describeIdentityTest $
--             describeIdentity
--
--         , describeIdentityPoolTest $
--             describeIdentityPool
--
--         , getCredentialsForIdentityTest $
--             getCredentialsForIdentity
--
--         , getIdTest $
--             getId
--
--         , getIdentityPoolRolesTest $
--             getIdentityPoolRoles
--
--         , getOpenIdTokenTest $
--             getOpenIdToken
--
--         , getOpenIdTokenForDeveloperIdentityTest $
--             getOpenIdTokenForDeveloperIdentity
--
--         , listIdentitiesTest $
--             listIdentities
--
--         , listIdentityPoolsTest $
--             listIdentityPools
--
--         , lookupDeveloperIdentityTest $
--             lookupDeveloperIdentity
--
--         , mergeDeveloperIdentitiesTest $
--             mergeDeveloperIdentities
--
--         , setIdentityPoolRolesTest $
--             setIdentityPoolRoles
--
--         , unlinkDeveloperIdentityTest $
--             unlinkDeveloperIdentity
--
--         , unlinkIdentityTest $
--             unlinkIdentity
--
--         , updateIdentityPoolTest $
--             updateIdentityPool
--
--           ]

--     , testGroup "response"
--         [ createIdentityPoolResponseTest $
--             identityPool
--
--         , deleteIdentitiesResponseTest $
--             deleteIdentitiesResponse
--
--         , deleteIdentityPoolResponseTest $
--             deleteIdentityPoolResponse
--
--         , describeIdentityResponseTest $
--             identityDescription
--
--         , describeIdentityPoolResponseTest $
--             identityPool
--
--         , getCredentialsForIdentityResponseTest $
--             getCredentialsForIdentityResponse
--
--         , getIdResponseTest $
--             getIdResponse
--
--         , getIdentityPoolRolesResponseTest $
--             getIdentityPoolRolesResponse
--
--         , getOpenIdTokenResponseTest $
--             getOpenIdTokenResponse
--
--         , getOpenIdTokenForDeveloperIdentityResponseTest $
--             getOpenIdTokenForDeveloperIdentityResponse
--
--         , listIdentitiesResponseTest $
--             listIdentitiesResponse
--
--         , listIdentityPoolsResponseTest $
--             listIdentityPoolsResponse
--
--         , lookupDeveloperIdentityResponseTest $
--             lookupDeveloperIdentityResponse
--
--         , mergeDeveloperIdentitiesResponseTest $
--             mergeDeveloperIdentitiesResponse
--
--         , setIdentityPoolRolesResponseTest $
--             setIdentityPoolRolesResponse
--
--         , unlinkDeveloperIdentityResponseTest $
--             unlinkDeveloperIdentityResponse
--
--         , unlinkIdentityResponseTest $
--             unlinkIdentityResponse
--
--         , updateIdentityPoolResponseTest $
--             identityPool
--
--           ]
--     ]

-- Requests

createIdentityPoolTest :: CreateIdentityPool -> TestTree
createIdentityPoolTest = undefined

deleteIdentitiesTest :: DeleteIdentities -> TestTree
deleteIdentitiesTest = undefined

deleteIdentityPoolTest :: DeleteIdentityPool -> TestTree
deleteIdentityPoolTest = undefined

describeIdentityTest :: DescribeIdentity -> TestTree
describeIdentityTest = undefined

describeIdentityPoolTest :: DescribeIdentityPool -> TestTree
describeIdentityPoolTest = undefined

getCredentialsForIdentityTest :: GetCredentialsForIdentity -> TestTree
getCredentialsForIdentityTest = undefined

getIdTest :: GetId -> TestTree
getIdTest = undefined

getIdentityPoolRolesTest :: GetIdentityPoolRoles -> TestTree
getIdentityPoolRolesTest = undefined

getOpenIdTokenTest :: GetOpenIdToken -> TestTree
getOpenIdTokenTest = undefined

getOpenIdTokenForDeveloperIdentityTest :: GetOpenIdTokenForDeveloperIdentity -> TestTree
getOpenIdTokenForDeveloperIdentityTest = undefined

listIdentitiesTest :: ListIdentities -> TestTree
listIdentitiesTest = undefined

listIdentityPoolsTest :: ListIdentityPools -> TestTree
listIdentityPoolsTest = undefined

lookupDeveloperIdentityTest :: LookupDeveloperIdentity -> TestTree
lookupDeveloperIdentityTest = undefined

mergeDeveloperIdentitiesTest :: MergeDeveloperIdentities -> TestTree
mergeDeveloperIdentitiesTest = undefined

setIdentityPoolRolesTest :: SetIdentityPoolRoles -> TestTree
setIdentityPoolRolesTest = undefined

unlinkDeveloperIdentityTest :: UnlinkDeveloperIdentity -> TestTree
unlinkDeveloperIdentityTest = undefined

unlinkIdentityTest :: UnlinkIdentity -> TestTree
unlinkIdentityTest = undefined

updateIdentityPoolTest :: UpdateIdentityPool -> TestTree
updateIdentityPoolTest = undefined

-- Responses

createIdentityPoolResponseTest :: IdentityPool -> TestTree
createIdentityPoolResponseTest = resp
    "createIdentityPoolResponse"
    "fixture/IdentityPool"
    (Proxy :: Proxy CreateIdentityPool)

deleteIdentitiesResponseTest :: DeleteIdentitiesResponse -> TestTree
deleteIdentitiesResponseTest = resp
    "deleteIdentitiesResponse"
    "fixture/DeleteIdentitiesResponse"
    (Proxy :: Proxy DeleteIdentities)

deleteIdentityPoolResponseTest :: DeleteIdentityPoolResponse -> TestTree
deleteIdentityPoolResponseTest = resp
    "deleteIdentityPoolResponse"
    "fixture/DeleteIdentityPoolResponse"
    (Proxy :: Proxy DeleteIdentityPool)

describeIdentityResponseTest :: IdentityDescription -> TestTree
describeIdentityResponseTest = resp
    "describeIdentityResponse"
    "fixture/IdentityDescription"
    (Proxy :: Proxy DescribeIdentity)

describeIdentityPoolResponseTest :: IdentityPool -> TestTree
describeIdentityPoolResponseTest = resp
    "describeIdentityPoolResponse"
    "fixture/IdentityPool"
    (Proxy :: Proxy DescribeIdentityPool)

getCredentialsForIdentityResponseTest :: GetCredentialsForIdentityResponse -> TestTree
getCredentialsForIdentityResponseTest = resp
    "getCredentialsForIdentityResponse"
    "fixture/GetCredentialsForIdentityResponse"
    (Proxy :: Proxy GetCredentialsForIdentity)

getIdResponseTest :: GetIdResponse -> TestTree
getIdResponseTest = resp
    "getIdResponse"
    "fixture/GetIdResponse"
    (Proxy :: Proxy GetId)

getIdentityPoolRolesResponseTest :: GetIdentityPoolRolesResponse -> TestTree
getIdentityPoolRolesResponseTest = resp
    "getIdentityPoolRolesResponse"
    "fixture/GetIdentityPoolRolesResponse"
    (Proxy :: Proxy GetIdentityPoolRoles)

getOpenIdTokenResponseTest :: GetOpenIdTokenResponse -> TestTree
getOpenIdTokenResponseTest = resp
    "getOpenIdTokenResponse"
    "fixture/GetOpenIdTokenResponse"
    (Proxy :: Proxy GetOpenIdToken)

getOpenIdTokenForDeveloperIdentityResponseTest :: GetOpenIdTokenForDeveloperIdentityResponse -> TestTree
getOpenIdTokenForDeveloperIdentityResponseTest = resp
    "getOpenIdTokenForDeveloperIdentityResponse"
    "fixture/GetOpenIdTokenForDeveloperIdentityResponse"
    (Proxy :: Proxy GetOpenIdTokenForDeveloperIdentity)

listIdentitiesResponseTest :: ListIdentitiesResponse -> TestTree
listIdentitiesResponseTest = resp
    "listIdentitiesResponse"
    "fixture/ListIdentitiesResponse"
    (Proxy :: Proxy ListIdentities)

listIdentityPoolsResponseTest :: ListIdentityPoolsResponse -> TestTree
listIdentityPoolsResponseTest = resp
    "listIdentityPoolsResponse"
    "fixture/ListIdentityPoolsResponse"
    (Proxy :: Proxy ListIdentityPools)

lookupDeveloperIdentityResponseTest :: LookupDeveloperIdentityResponse -> TestTree
lookupDeveloperIdentityResponseTest = resp
    "lookupDeveloperIdentityResponse"
    "fixture/LookupDeveloperIdentityResponse"
    (Proxy :: Proxy LookupDeveloperIdentity)

mergeDeveloperIdentitiesResponseTest :: MergeDeveloperIdentitiesResponse -> TestTree
mergeDeveloperIdentitiesResponseTest = resp
    "mergeDeveloperIdentitiesResponse"
    "fixture/MergeDeveloperIdentitiesResponse"
    (Proxy :: Proxy MergeDeveloperIdentities)

setIdentityPoolRolesResponseTest :: SetIdentityPoolRolesResponse -> TestTree
setIdentityPoolRolesResponseTest = resp
    "setIdentityPoolRolesResponse"
    "fixture/SetIdentityPoolRolesResponse"
    (Proxy :: Proxy SetIdentityPoolRoles)

unlinkDeveloperIdentityResponseTest :: UnlinkDeveloperIdentityResponse -> TestTree
unlinkDeveloperIdentityResponseTest = resp
    "unlinkDeveloperIdentityResponse"
    "fixture/UnlinkDeveloperIdentityResponse"
    (Proxy :: Proxy UnlinkDeveloperIdentity)

unlinkIdentityResponseTest :: UnlinkIdentityResponse -> TestTree
unlinkIdentityResponseTest = resp
    "unlinkIdentityResponse"
    "fixture/UnlinkIdentityResponse"
    (Proxy :: Proxy UnlinkIdentity)

updateIdentityPoolResponseTest :: IdentityPool -> TestTree
updateIdentityPoolResponseTest = resp
    "updateIdentityPoolResponse"
    "fixture/IdentityPool"
    (Proxy :: Proxy UpdateIdentityPool)
