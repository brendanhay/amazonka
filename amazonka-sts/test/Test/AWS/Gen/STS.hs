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
--         , assumeRoleWithSAMLTest $
--             assumeRoleWithSAML
--
--         , assumeRoleWithWebIdentityTest $
--             assumeRoleWithWebIdentity
--
--         , decodeAuthorizationMessageTest $
--             decodeAuthorizationMessage
--
--         , getFederationTokenTest $
--             getFederationToken
--
--         , getSessionTokenTest $
--             getSessionToken
--
--           ]

--     , testGroup "response"
--         [ assumeRoleResponseTest $
--             assumeRoleResponse
--
--         , assumeRoleWithSAMLResponseTest $
--             assumeRoleWithSAMLResponse
--
--         , assumeRoleWithWebIdentityResponseTest $
--             assumeRoleWithWebIdentityResponse
--
--         , decodeAuthorizationMessageResponseTest $
--             decodeAuthorizationMessageResponse
--
--         , getFederationTokenResponseTest $
--             getFederationTokenResponse
--
--         , getSessionTokenResponseTest $
--             getSessionTokenResponse
--
--           ]
--     ]

-- Requests

assumeRoleTest :: AssumeRole -> TestTree
assumeRoleTest = undefined

assumeRoleWithSAMLTest :: AssumeRoleWithSAML -> TestTree
assumeRoleWithSAMLTest = undefined

assumeRoleWithWebIdentityTest :: AssumeRoleWithWebIdentity -> TestTree
assumeRoleWithWebIdentityTest = undefined

decodeAuthorizationMessageTest :: DecodeAuthorizationMessage -> TestTree
decodeAuthorizationMessageTest = undefined

getFederationTokenTest :: GetFederationToken -> TestTree
getFederationTokenTest = undefined

getSessionTokenTest :: GetSessionToken -> TestTree
getSessionTokenTest = undefined

-- Responses

assumeRoleResponseTest :: AssumeRoleResponse -> TestTree
assumeRoleResponseTest = resp
    "assumeRoleResponse"
    "fixture/AssumeRoleResponse"
    (Proxy :: Proxy AssumeRole)

assumeRoleWithSAMLResponseTest :: AssumeRoleWithSAMLResponse -> TestTree
assumeRoleWithSAMLResponseTest = resp
    "assumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse"
    (Proxy :: Proxy AssumeRoleWithSAML)

assumeRoleWithWebIdentityResponseTest :: AssumeRoleWithWebIdentityResponse -> TestTree
assumeRoleWithWebIdentityResponseTest = resp
    "assumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse"
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

decodeAuthorizationMessageResponseTest :: DecodeAuthorizationMessageResponse -> TestTree
decodeAuthorizationMessageResponseTest = resp
    "decodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse"
    (Proxy :: Proxy DecodeAuthorizationMessage)

getFederationTokenResponseTest :: GetFederationTokenResponse -> TestTree
getFederationTokenResponseTest = resp
    "getFederationTokenResponse"
    "fixture/GetFederationTokenResponse"
    (Proxy :: Proxy GetFederationToken)

getSessionTokenResponseTest :: GetSessionTokenResponse -> TestTree
getSessionTokenResponseTest = resp
    "getSessionTokenResponse"
    "fixture/GetSessionTokenResponse"
    (Proxy :: Proxy GetSessionToken)
