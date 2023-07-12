{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.STS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.STS where

import Amazonka.STS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.STS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssumeRole $
--             newAssumeRole
--
--         , requestAssumeRoleWithSAML $
--             newAssumeRoleWithSAML
--
--         , requestAssumeRoleWithWebIdentity $
--             newAssumeRoleWithWebIdentity
--
--         , requestDecodeAuthorizationMessage $
--             newDecodeAuthorizationMessage
--
--         , requestGetAccessKeyInfo $
--             newGetAccessKeyInfo
--
--         , requestGetCallerIdentity $
--             newGetCallerIdentity
--
--         , requestGetFederationToken $
--             newGetFederationToken
--
--         , requestGetSessionToken $
--             newGetSessionToken
--
--           ]

--     , testGroup "response"
--         [ responseAssumeRole $
--             newAssumeRoleResponse
--
--         , responseAssumeRoleWithSAML $
--             newAssumeRoleWithSAMLResponse
--
--         , responseAssumeRoleWithWebIdentity $
--             newAssumeRoleWithWebIdentityResponse
--
--         , responseDecodeAuthorizationMessage $
--             newDecodeAuthorizationMessageResponse
--
--         , responseGetAccessKeyInfo $
--             newGetAccessKeyInfoResponse
--
--         , responseGetCallerIdentity $
--             newGetCallerIdentityResponse
--
--         , responseGetFederationToken $
--             newGetFederationTokenResponse
--
--         , responseGetSessionToken $
--             newGetSessionTokenResponse
--
--           ]
--     ]

-- Requests

requestAssumeRole :: AssumeRole -> TestTree
requestAssumeRole =
  req
    "AssumeRole"
    "fixture/AssumeRole.yaml"

requestAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
requestAssumeRoleWithSAML =
  req
    "AssumeRoleWithSAML"
    "fixture/AssumeRoleWithSAML.yaml"

requestAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
requestAssumeRoleWithWebIdentity =
  req
    "AssumeRoleWithWebIdentity"
    "fixture/AssumeRoleWithWebIdentity.yaml"

requestDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
requestDecodeAuthorizationMessage =
  req
    "DecodeAuthorizationMessage"
    "fixture/DecodeAuthorizationMessage.yaml"

requestGetAccessKeyInfo :: GetAccessKeyInfo -> TestTree
requestGetAccessKeyInfo =
  req
    "GetAccessKeyInfo"
    "fixture/GetAccessKeyInfo.yaml"

requestGetCallerIdentity :: GetCallerIdentity -> TestTree
requestGetCallerIdentity =
  req
    "GetCallerIdentity"
    "fixture/GetCallerIdentity.yaml"

requestGetFederationToken :: GetFederationToken -> TestTree
requestGetFederationToken =
  req
    "GetFederationToken"
    "fixture/GetFederationToken.yaml"

requestGetSessionToken :: GetSessionToken -> TestTree
requestGetSessionToken =
  req
    "GetSessionToken"
    "fixture/GetSessionToken.yaml"

-- Responses

responseAssumeRole :: AssumeRoleResponse -> TestTree
responseAssumeRole =
  res
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssumeRole)

responseAssumeRoleWithSAML :: AssumeRoleWithSAMLResponse -> TestTree
responseAssumeRoleWithSAML =
  res
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssumeRoleWithSAML)

responseAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentityResponse -> TestTree
responseAssumeRoleWithWebIdentity =
  res
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssumeRoleWithWebIdentity)

responseDecodeAuthorizationMessage :: DecodeAuthorizationMessageResponse -> TestTree
responseDecodeAuthorizationMessage =
  res
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DecodeAuthorizationMessage)

responseGetAccessKeyInfo :: GetAccessKeyInfoResponse -> TestTree
responseGetAccessKeyInfo =
  res
    "GetAccessKeyInfoResponse"
    "fixture/GetAccessKeyInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessKeyInfo)

responseGetCallerIdentity :: GetCallerIdentityResponse -> TestTree
responseGetCallerIdentity =
  res
    "GetCallerIdentityResponse"
    "fixture/GetCallerIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCallerIdentity)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFederationToken)

responseGetSessionToken :: GetSessionTokenResponse -> TestTree
responseGetSessionToken =
  res
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSessionToken)
