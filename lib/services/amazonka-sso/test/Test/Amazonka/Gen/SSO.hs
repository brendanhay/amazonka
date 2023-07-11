{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSO
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSO where

import Amazonka.SSO
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSO.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetRoleCredentials $
--             newGetRoleCredentials
--
--         , requestListAccountRoles $
--             newListAccountRoles
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestLogout $
--             newLogout
--
--           ]

--     , testGroup "response"
--         [ responseGetRoleCredentials $
--             newGetRoleCredentialsResponse
--
--         , responseListAccountRoles $
--             newListAccountRolesResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseLogout $
--             newLogoutResponse
--
--           ]
--     ]

-- Requests

requestGetRoleCredentials :: GetRoleCredentials -> TestTree
requestGetRoleCredentials =
  req
    "GetRoleCredentials"
    "fixture/GetRoleCredentials.yaml"

requestListAccountRoles :: ListAccountRoles -> TestTree
requestListAccountRoles =
  req
    "ListAccountRoles"
    "fixture/ListAccountRoles.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestLogout :: Logout -> TestTree
requestLogout =
  req
    "Logout"
    "fixture/Logout.yaml"

-- Responses

responseGetRoleCredentials :: GetRoleCredentialsResponse -> TestTree
responseGetRoleCredentials =
  res
    "GetRoleCredentialsResponse"
    "fixture/GetRoleCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoleCredentials)

responseListAccountRoles :: ListAccountRolesResponse -> TestTree
responseListAccountRoles =
  res
    "ListAccountRolesResponse"
    "fixture/ListAccountRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountRoles)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccounts)

responseLogout :: LogoutResponse -> TestTree
responseLogout =
  res
    "LogoutResponse"
    "fixture/LogoutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Logout)
