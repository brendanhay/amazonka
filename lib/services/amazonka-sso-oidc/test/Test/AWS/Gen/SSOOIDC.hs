{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSOOIDC
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SSOOIDC where

import Data.Proxy
import Network.AWS.SSOOIDC
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SSOOIDC.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartDeviceAuthorization $
--             newStartDeviceAuthorization
--
--         , requestCreateToken $
--             newCreateToken
--
--         , requestRegisterClient $
--             newRegisterClient
--
--           ]

--     , testGroup "response"
--         [ responseStartDeviceAuthorization $
--             newStartDeviceAuthorizationResponse
--
--         , responseCreateToken $
--             newCreateTokenResponse
--
--         , responseRegisterClient $
--             newRegisterClientResponse
--
--           ]
--     ]

-- Requests

requestStartDeviceAuthorization :: StartDeviceAuthorization -> TestTree
requestStartDeviceAuthorization =
  req
    "StartDeviceAuthorization"
    "fixture/StartDeviceAuthorization.yaml"

requestCreateToken :: CreateToken -> TestTree
requestCreateToken =
  req
    "CreateToken"
    "fixture/CreateToken.yaml"

requestRegisterClient :: RegisterClient -> TestTree
requestRegisterClient =
  req
    "RegisterClient"
    "fixture/RegisterClient.yaml"

-- Responses

responseStartDeviceAuthorization :: StartDeviceAuthorizationResponse -> TestTree
responseStartDeviceAuthorization =
  res
    "StartDeviceAuthorizationResponse"
    "fixture/StartDeviceAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy StartDeviceAuthorization)

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy :: Proxy CreateToken)

responseRegisterClient :: RegisterClientResponse -> TestTree
responseRegisterClient =
  res
    "RegisterClientResponse"
    "fixture/RegisterClientResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterClient)
