{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSOOIDC
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSOOIDC where

import Amazonka.SSOOIDC
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSOOIDC.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateToken $
--             newCreateToken
--
--         , requestRegisterClient $
--             newRegisterClient
--
--         , requestStartDeviceAuthorization $
--             newStartDeviceAuthorization
--
--           ]

--     , testGroup "response"
--         [ responseCreateToken $
--             newCreateTokenResponse
--
--         , responseRegisterClient $
--             newRegisterClientResponse
--
--         , responseStartDeviceAuthorization $
--             newStartDeviceAuthorizationResponse
--
--           ]
--     ]

-- Requests

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

requestStartDeviceAuthorization :: StartDeviceAuthorization -> TestTree
requestStartDeviceAuthorization =
  req
    "StartDeviceAuthorization"
    "fixture/StartDeviceAuthorization.yaml"

-- Responses

responseCreateToken :: CreateTokenResponse -> TestTree
responseCreateToken =
  res
    "CreateTokenResponse"
    "fixture/CreateTokenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateToken)

responseRegisterClient :: RegisterClientResponse -> TestTree
responseRegisterClient =
  res
    "RegisterClientResponse"
    "fixture/RegisterClientResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterClient)

responseStartDeviceAuthorization :: StartDeviceAuthorizationResponse -> TestTree
responseStartDeviceAuthorization =
  res
    "StartDeviceAuthorizationResponse"
    "fixture/StartDeviceAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartDeviceAuthorization)
