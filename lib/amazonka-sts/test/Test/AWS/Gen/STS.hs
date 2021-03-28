{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.STS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
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
--         [ requestGetCallerIdentity $
--             mkGetCallerIdentity
--
--         , requestAssumeRole $
--             mkAssumeRole
--
--         , requestGetAccessKeyInfo $
--             mkGetAccessKeyInfo
--
--         , requestDecodeAuthorizationMessage $
--             mkDecodeAuthorizationMessage
--
--         , requestAssumeRoleWithWebIdentity $
--             mkAssumeRoleWithWebIdentity
--
--         , requestGetFederationToken $
--             mkGetFederationToken
--
--         , requestGetSessionToken $
--             mkGetSessionToken
--
--         , requestAssumeRoleWithSAML $
--             mkAssumeRoleWithSAML
--
--           ]

--     , testGroup "response"
--         [ responseGetCallerIdentity $
--             mkGetCallerIdentityResponse
--
--         , responseAssumeRole $
--             mkAssumeRoleResponse
--
--         , responseGetAccessKeyInfo $
--             mkGetAccessKeyInfoResponse
--
--         , responseDecodeAuthorizationMessage $
--             mkDecodeAuthorizationMessageResponse
--
--         , responseAssumeRoleWithWebIdentity $
--             mkAssumeRoleWithWebIdentityResponse
--
--         , responseGetFederationToken $
--             mkGetFederationTokenResponse
--
--         , responseGetSessionToken $
--             mkGetSessionTokenResponse
--
--         , responseAssumeRoleWithSAML $
--             mkAssumeRoleWithSAMLResponse
--
--           ]
--     ]

-- Requests

requestGetCallerIdentity :: GetCallerIdentity -> TestTree
requestGetCallerIdentity = req
    "GetCallerIdentity"
    "fixture/GetCallerIdentity.yaml"

requestAssumeRole :: AssumeRole -> TestTree
requestAssumeRole = req
    "AssumeRole"
    "fixture/AssumeRole.yaml"

requestGetAccessKeyInfo :: GetAccessKeyInfo -> TestTree
requestGetAccessKeyInfo = req
    "GetAccessKeyInfo"
    "fixture/GetAccessKeyInfo.yaml"

requestDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
requestDecodeAuthorizationMessage = req
    "DecodeAuthorizationMessage"
    "fixture/DecodeAuthorizationMessage.yaml"

requestAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
requestAssumeRoleWithWebIdentity = req
    "AssumeRoleWithWebIdentity"
    "fixture/AssumeRoleWithWebIdentity.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken = req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestGetSessionToken :: GetSessionToken -> TestTree
requestGetSessionToken = req
    "GetSessionToken"
    "fixture/GetSessionToken.yaml"

requestAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
requestAssumeRoleWithSAML = req
    "AssumeRoleWithSAML"
    "fixture/AssumeRoleWithSAML.yaml"

-- Responses

responseGetCallerIdentity :: GetCallerIdentityResponse -> TestTree
responseGetCallerIdentity = res
    "GetCallerIdentityResponse"
    "fixture/GetCallerIdentityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCallerIdentity)

responseAssumeRole :: AssumeRoleResponse -> TestTree
responseAssumeRole = res
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssumeRole)

responseGetAccessKeyInfo :: GetAccessKeyInfoResponse -> TestTree
responseGetAccessKeyInfo = res
    "GetAccessKeyInfoResponse"
    "fixture/GetAccessKeyInfoResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccessKeyInfo)

responseDecodeAuthorizationMessage :: DecodeAuthorizationMessageResponse -> TestTree
responseDecodeAuthorizationMessage = res
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DecodeAuthorizationMessage)

responseAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentityResponse -> TestTree
responseAssumeRoleWithWebIdentity = res
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken = res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetFederationToken)

responseGetSessionToken :: GetSessionTokenResponse -> TestTree
responseGetSessionToken = res
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSessionToken)

responseAssumeRoleWithSAML :: AssumeRoleWithSAMLResponse -> TestTree
responseAssumeRoleWithSAML = res
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssumeRoleWithSAML)
