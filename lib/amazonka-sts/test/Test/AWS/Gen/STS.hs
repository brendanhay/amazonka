{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.STS
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestGetCallerIdentity $
--             getCallerIdentity
--
--         , requestAssumeRole $
--             assumeRole
--
--         , requestGetAccessKeyInfo $
--             getAccessKeyInfo
--
--         , requestDecodeAuthorizationMessage $
--             decodeAuthorizationMessage
--
--         , requestAssumeRoleWithWebIdentity $
--             assumeRoleWithWebIdentity
--
--         , requestGetFederationToken $
--             getFederationToken
--
--         , requestGetSessionToken $
--             getSessionToken
--
--         , requestAssumeRoleWithSAML $
--             assumeRoleWithSAML
--
--           ]

--     , testGroup "response"
--         [ responseGetCallerIdentity $
--             getCallerIdentityResponse
--
--         , responseAssumeRole $
--             assumeRoleResponse
--
--         , responseGetAccessKeyInfo $
--             getAccessKeyInfoResponse
--
--         , responseDecodeAuthorizationMessage $
--             decodeAuthorizationMessageResponse
--
--         , responseAssumeRoleWithWebIdentity $
--             assumeRoleWithWebIdentityResponse
--
--         , responseGetFederationToken $
--             getFederationTokenResponse
--
--         , responseGetSessionToken $
--             getSessionTokenResponse
--
--         , responseAssumeRoleWithSAML $
--             assumeRoleWithSAMLResponse
--
--           ]
--     ]

-- Requests

requestGetCallerIdentity :: GetCallerIdentity -> TestTree
requestGetCallerIdentity =
  req
    "GetCallerIdentity"
    "fixture/GetCallerIdentity.yaml"

requestAssumeRole :: AssumeRole -> TestTree
requestAssumeRole =
  req
    "AssumeRole"
    "fixture/AssumeRole.yaml"

requestGetAccessKeyInfo :: GetAccessKeyInfo -> TestTree
requestGetAccessKeyInfo =
  req
    "GetAccessKeyInfo"
    "fixture/GetAccessKeyInfo.yaml"

requestDecodeAuthorizationMessage :: DecodeAuthorizationMessage -> TestTree
requestDecodeAuthorizationMessage =
  req
    "DecodeAuthorizationMessage"
    "fixture/DecodeAuthorizationMessage.yaml"

requestAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentity -> TestTree
requestAssumeRoleWithWebIdentity =
  req
    "AssumeRoleWithWebIdentity"
    "fixture/AssumeRoleWithWebIdentity.yaml"

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

requestAssumeRoleWithSAML :: AssumeRoleWithSAML -> TestTree
requestAssumeRoleWithSAML =
  req
    "AssumeRoleWithSAML"
    "fixture/AssumeRoleWithSAML.yaml"

-- Responses

responseGetCallerIdentity :: GetCallerIdentityResponse -> TestTree
responseGetCallerIdentity =
  res
    "GetCallerIdentityResponse"
    "fixture/GetCallerIdentityResponse.proto"
    sts
    (Proxy :: Proxy GetCallerIdentity)

responseAssumeRole :: AssumeRoleResponse -> TestTree
responseAssumeRole =
  res
    "AssumeRoleResponse"
    "fixture/AssumeRoleResponse.proto"
    sts
    (Proxy :: Proxy AssumeRole)

responseGetAccessKeyInfo :: GetAccessKeyInfoResponse -> TestTree
responseGetAccessKeyInfo =
  res
    "GetAccessKeyInfoResponse"
    "fixture/GetAccessKeyInfoResponse.proto"
    sts
    (Proxy :: Proxy GetAccessKeyInfo)

responseDecodeAuthorizationMessage :: DecodeAuthorizationMessageResponse -> TestTree
responseDecodeAuthorizationMessage =
  res
    "DecodeAuthorizationMessageResponse"
    "fixture/DecodeAuthorizationMessageResponse.proto"
    sts
    (Proxy :: Proxy DecodeAuthorizationMessage)

responseAssumeRoleWithWebIdentity :: AssumeRoleWithWebIdentityResponse -> TestTree
responseAssumeRoleWithWebIdentity =
  res
    "AssumeRoleWithWebIdentityResponse"
    "fixture/AssumeRoleWithWebIdentityResponse.proto"
    sts
    (Proxy :: Proxy AssumeRoleWithWebIdentity)

responseGetFederationToken :: GetFederationTokenResponse -> TestTree
responseGetFederationToken =
  res
    "GetFederationTokenResponse"
    "fixture/GetFederationTokenResponse.proto"
    sts
    (Proxy :: Proxy GetFederationToken)

responseGetSessionToken :: GetSessionTokenResponse -> TestTree
responseGetSessionToken =
  res
    "GetSessionTokenResponse"
    "fixture/GetSessionTokenResponse.proto"
    sts
    (Proxy :: Proxy GetSessionToken)

responseAssumeRoleWithSAML :: AssumeRoleWithSAMLResponse -> TestTree
responseAssumeRoleWithSAML =
  res
    "AssumeRoleWithSAMLResponse"
    "fixture/AssumeRoleWithSAMLResponse.proto"
    sts
    (Proxy :: Proxy AssumeRoleWithSAML)
