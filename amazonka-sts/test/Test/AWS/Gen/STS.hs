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
-- fixtures =
--     [ testGroup "request"
--         [ testAssumeRole $
--             assumeRole
--
--         , testDecodeAuthorizationMessage $
--             decodeAuthorizationMessage
--
--         , testAssumeRoleWithWebIdentity $
--             assumeRoleWithWebIdentity
--
--         , testGetFederationToken $
--             getFederationToken
--
--         , testGetSessionToken $
--             getSessionToken
--
--         , testAssumeRoleWithSAML $
--             assumeRoleWithSAML
--
--           ]

--     , testGroup "response"
--         [ testAssumeRoleResponse $
--             assumeRoleResponse
--
--         , testDecodeAuthorizationMessageResponse $
--             decodeAuthorizationMessageResponse
--
--         , testAssumeRoleWithWebIdentityResponse $
--             assumeRoleWithWebIdentityResponse
--
--         , testGetFederationTokenResponse $
--             getFederationTokenResponse
--
--         , testGetSessionTokenResponse $
--             getSessionTokenResponse
--
--         , testAssumeRoleWithSAMLResponse $
--             assumeRoleWithSAMLResponse
--
--           ]
--     ]

-- Requests

testAssumeRole :: AssumeRole -> TestTree
testAssumeRole = undefined

testDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
testDecodeAuthorizationMessage = undefined

testAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
testAssumeRoleWithWebIdentity = undefined

testGetFederationToken :: GetFederationToken -> TestTree
testGetFederationToken = undefined

testGetSessionToken :: GetSessionToken -> TestTree
testGetSessionToken = undefined

testAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
testAssumeRoleWithSAML = undefined

-- Responses

testAssumeRoleResponse :: AssumeRoleResponse -> TestTree
testAssumeRoleResponse = resp
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse"
    (Proxy :: Proxy AssumeRole)

testDecodeAuthorizationMessageResponse :: DecodeAuthorizationMessageResponse -> TestTree
testDecodeAuthorizationMessageResponse = resp
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse"
    (Proxy :: Proxy DecodeAuthorizationMessage)

testAssumeRoleWithWebIdentityResponse :: AssumeRoleWithWebIdentityResponse -> TestTree
testAssumeRoleWithWebIdentityResponse = resp
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse"
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

testGetFederationTokenResponse :: GetFederationTokenResponse -> TestTree
testGetFederationTokenResponse = resp
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse"
    (Proxy :: Proxy GetFederationToken)

testGetSessionTokenResponse :: GetSessionTokenResponse -> TestTree
testGetSessionTokenResponse = resp
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse"
    (Proxy :: Proxy GetSessionToken)

testAssumeRoleWithSAMLResponse :: AssumeRoleWithSAMLResponse -> TestTree
testAssumeRoleWithSAMLResponse = resp
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse"
    (Proxy :: Proxy AssumeRoleWithSAML)
