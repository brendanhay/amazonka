-- Module      : Test.AWS.Gen.STS
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

module Test.AWS.Gen.STS where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.STS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ assumeRoleTest $
--             assumeRole
--
--         , decodeAuthorizationMessageTest $
--             decodeAuthorizationMessage
--
--         , assumeRoleWithWebIdentityTest $
--             assumeRoleWithWebIdentity
--
--         , getFederationTokenTest $
--             getFederationToken
--
--         , getSessionTokenTest $
--             getSessionToken
--
--         , assumeRoleWithSAMLTest $
--             assumeRoleWithSAML
--
--           ]

--     , testGroup "response"
--         [ assumeRoleResponseTest $
--             assumeRoleResponse
--
--         , decodeAuthorizationMessageResponseTest $
--             decodeAuthorizationMessageResponse
--
--         , assumeRoleWithWebIdentityResponseTest $
--             assumeRoleWithWebIdentityResponse
--
--         , getFederationTokenResponseTest $
--             getFederationTokenResponse
--
--         , getSessionTokenResponseTest $
--             getSessionTokenResponse
--
--         , assumeRoleWithSAMLResponseTest $
--             assumeRoleWithSAMLResponse
--
--           ]
--     ]

-- Requests

assumeRoleTest :: AssumeRole -> TestTree
assumeRoleTest = undefined

decodeAuthorizationMessageTest :: DecodeAuthorizationMessage -> TestTree
decodeAuthorizationMessageTest = undefined

assumeRoleWithWebIdentityTest :: AssumeRoleWithWebIdentity -> TestTree
assumeRoleWithWebIdentityTest = undefined

getFederationTokenTest :: GetFederationToken -> TestTree
getFederationTokenTest = undefined

getSessionTokenTest :: GetSessionToken -> TestTree
getSessionTokenTest = undefined

assumeRoleWithSAMLTest :: AssumeRoleWithSAML -> TestTree
assumeRoleWithSAMLTest = undefined

-- Responses

assumeRoleResponseTest :: AssumeRoleResponse -> TestTree
assumeRoleResponseTest = resp
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse"
    (Proxy :: Proxy AssumeRole)

decodeAuthorizationMessageResponseTest :: DecodeAuthorizationMessageResponse -> TestTree
decodeAuthorizationMessageResponseTest = resp
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse"
    (Proxy :: Proxy DecodeAuthorizationMessage)

assumeRoleWithWebIdentityResponseTest :: AssumeRoleWithWebIdentityResponse -> TestTree
assumeRoleWithWebIdentityResponseTest = resp
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse"
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

getFederationTokenResponseTest :: GetFederationTokenResponse -> TestTree
getFederationTokenResponseTest = resp
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse"
    (Proxy :: Proxy GetFederationToken)

getSessionTokenResponseTest :: GetSessionTokenResponse -> TestTree
getSessionTokenResponseTest = resp
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse"
    (Proxy :: Proxy GetSessionToken)

assumeRoleWithSAMLResponseTest :: AssumeRoleWithSAMLResponse -> TestTree
assumeRoleWithSAMLResponseTest = resp
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse"
    (Proxy :: Proxy AssumeRoleWithSAML)
