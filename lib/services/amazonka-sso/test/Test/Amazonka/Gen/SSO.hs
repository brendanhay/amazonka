{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSO
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestLogout $
--             newLogout
--
--         , requestGetRoleCredentials $
--             newGetRoleCredentials
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestListAccountRoles $
--             newListAccountRoles
--
--           ]

--     , testGroup "response"
--         [ responseLogout $
--             newLogoutResponse
--
--         , responseGetRoleCredentials $
--             newGetRoleCredentialsResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseListAccountRoles $
--             newListAccountRolesResponse
--
--           ]
--     ]

-- Requests

requestLogout :: Logout -> TestTree
requestLogout =
  req
    "Logout"
    "fixture/Logout.yaml"

requestGetRoleCredentials :: GetRoleCredentials -> TestTree
requestGetRoleCredentials =
  req
    "GetRoleCredentials"
    "fixture/GetRoleCredentials.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestListAccountRoles :: ListAccountRoles -> TestTree
requestListAccountRoles =
  req
    "ListAccountRoles"
    "fixture/ListAccountRoles.yaml"

-- Responses

responseLogout :: LogoutResponse -> TestTree
responseLogout =
  res
    "LogoutResponse"
    "fixture/LogoutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Logout)

responseGetRoleCredentials :: GetRoleCredentialsResponse -> TestTree
responseGetRoleCredentials =
  res
    "GetRoleCredentialsResponse"
    "fixture/GetRoleCredentialsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRoleCredentials)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccounts)

responseListAccountRoles :: ListAccountRolesResponse -> TestTree
responseListAccountRoles =
  res
    "ListAccountRolesResponse"
    "fixture/ListAccountRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountRoles)
