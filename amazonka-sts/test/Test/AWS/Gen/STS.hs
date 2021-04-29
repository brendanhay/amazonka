{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.STS
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.STS where

import Data.Proxy
import Network.AWS.STS
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.STS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssumeRoleWithWebIdentity $
--             newAssumeRoleWithWebIdentity
--
--         , requestGetAccessKeyInfo $
--             newGetAccessKeyInfo
--
--         , requestGetSessionToken $
--             newGetSessionToken
--
--         , requestAssumeRole $
--             newAssumeRole
--
--         , requestGetCallerIdentity $
--             newGetCallerIdentity
--
--         , requestDecodeAuthorizationMessage $
--             newDecodeAuthorizationMessage
--
--         , requestAssumeRoleWithSAML $
--             newAssumeRoleWithSAML
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--           ]

--     , testGroup "response"
--         [ responseAssumeRoleWithWebIdentity $
--             newAssumeRoleWithWebIdentityResponse
--
--         , responseGetAccessKeyInfo $
--             newGetAccessKeyInfoResponse
--
--         , responseGetSessionToken $
--             newGetSessionTokenResponse
--
--         , responseAssumeRole $
--             newAssumeRoleResponse
--
--         , responseGetCallerIdentity $
--             newGetCallerIdentityResponse
--
--         , responseDecodeAuthorizationMessage $
--             newDecodeAuthorizationMessageResponse
--
--         , responseAssumeRoleWithSAML $
--             newAssumeRoleWithSAMLResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--           ]
--     ]

-- Requests

requestAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
requestAssumeRoleWithWebIdentity =
  req
    "AssumeRoleWithWebIdentity"
    "fixture/AssumeRoleWithWebIdentity.yaml"

requestGetAccessKeyInfo :: GetAccessKeyInfo -> TestTree
requestGetAccessKeyInfo =
  req
    "GetAccessKeyInfo"
    "fixture/GetAccessKeyInfo.yaml"

requestGetSessionToken :: GetSessionToken -> TestTree
requestGetSessionToken =
  req
    "GetSessionToken"
    "fixture/GetSessionToken.yaml"

requestAssumeRole :: AssumeRole -> TestTree
requestAssumeRole =
  req
    "AssumeRole"
    "fixture/AssumeRole.yaml"

requestGetCallerIdentity :: GetCallerIdentity -> TestTree
requestGetCallerIdentity =
  req
    "GetCallerIdentity"
    "fixture/GetCallerIdentity.yaml"

requestDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
requestDecodeAuthorizationMessage =
  req
    "DecodeAuthorizationMessage"
    "fixture/DecodeAuthorizationMessage.yaml"

requestAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
requestAssumeRoleWithSAML =
  req
    "AssumeRoleWithSAML"
    "fixture/AssumeRoleWithSAML.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken =
  req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

-- Responses

responseAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentityResponse -> TestTree
responseAssumeRoleWithWebIdentity =
  res
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

responseGetAccessKeyInfo :: GetAccessKeyInfoResponse -> TestTree
responseGetAccessKeyInfo =
  res
    "GetAccessKeyInfoResponse"
    "fixture/GetAccessKeyInfoResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessKeyInfo)

responseGetSessionToken :: GetSessionTokenResponse -> TestTree
responseGetSessionToken =
  res
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetSessionToken)

responseAssumeRole :: AssumeRoleResponse -> TestTree
responseAssumeRole =
  res
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssumeRole)

responseGetCallerIdentity :: GetCallerIdentityResponse -> TestTree
responseGetCallerIdentity =
  res
    "GetCallerIdentityResponse"
    "fixture/GetCallerIdentityResponse.proto"
    defaultService
    (Proxy :: Proxy GetCallerIdentity)

responseDecodeAuthorizationMessage :: DecodeAuthorizationMessageResponse -> TestTree
responseDecodeAuthorizationMessage =
  res
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse.proto"
    defaultService
    (Proxy :: Proxy DecodeAuthorizationMessage)

responseAssumeRoleWithSAML :: AssumeRoleWithSAMLResponse -> TestTree
responseAssumeRoleWithSAML =
  res
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse.proto"
    defaultService
    (Proxy :: Proxy AssumeRoleWithSAML)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    defaultService
    (Proxy :: Proxy GetFederationToken)
