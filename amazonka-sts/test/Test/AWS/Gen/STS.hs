{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.STS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.STS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.STS
import Test.AWS.STS.Internal

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

instance Out AssumeRole
instance Out AssumeRoleResponse
instance Out AssumeRoleWithSAML
instance Out AssumeRoleWithSAMLResponse
instance Out AssumeRoleWithWebIdentity
instance Out AssumeRoleWithWebIdentityResponse
instance Out AssumedRoleUser
instance Out Credentials
instance Out DecodeAuthorizationMessage
instance Out DecodeAuthorizationMessageResponse
instance Out FederatedUser
instance Out GetFederationToken
instance Out GetFederationTokenResponse
instance Out GetSessionToken
instance Out GetSessionTokenResponse
