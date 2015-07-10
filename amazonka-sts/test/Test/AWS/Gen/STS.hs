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
testAssumeRole = req
    "AssumeRole"
    "fixture/AssumeRole"

testDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
testDecodeAuthorizationMessage = req
    "DecodeAuthorizationMessage"
    "fixture/DecodeAuthorizationMessage"

testAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
testAssumeRoleWithWebIdentity = req
    "AssumeRoleWithWebIdentity"
    "fixture/AssumeRoleWithWebIdentity"

testGetFederationToken :: GetFederationToken -> TestTree
testGetFederationToken = req
    "GetFederationToken"
    "fixture/GetFederationToken"

testGetSessionToken :: GetSessionToken -> TestTree
testGetSessionToken = req
    "GetSessionToken"
    "fixture/GetSessionToken"

testAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
testAssumeRoleWithSAML = req
    "AssumeRoleWithSAML"
    "fixture/AssumeRoleWithSAML"

-- Responses

testAssumeRoleResponse :: AssumeRoleResponse -> TestTree
testAssumeRoleResponse = res
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse"
    (Proxy :: Proxy AssumeRole)

testDecodeAuthorizationMessageResponse :: DecodeAuthorizationMessageResponse -> TestTree
testDecodeAuthorizationMessageResponse = res
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse"
    (Proxy :: Proxy DecodeAuthorizationMessage)

testAssumeRoleWithWebIdentityResponse :: AssumeRoleWithWebIdentityResponse -> TestTree
testAssumeRoleWithWebIdentityResponse = res
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse"
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

testGetFederationTokenResponse :: GetFederationTokenResponse -> TestTree
testGetFederationTokenResponse = res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse"
    (Proxy :: Proxy GetFederationToken)

testGetSessionTokenResponse :: GetSessionTokenResponse -> TestTree
testGetSessionTokenResponse = res
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse"
    (Proxy :: Proxy GetSessionToken)

testAssumeRoleWithSAMLResponse :: AssumeRoleWithSAMLResponse -> TestTree
testAssumeRoleWithSAMLResponse = res
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
