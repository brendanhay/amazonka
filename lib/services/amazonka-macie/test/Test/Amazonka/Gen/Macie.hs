{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Macie
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Macie where

import Amazonka.Macie
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Macie.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateMemberAccount $
--             newAssociateMemberAccount
--
--         , requestAssociateS3Resources $
--             newAssociateS3Resources
--
--         , requestDisassociateMemberAccount $
--             newDisassociateMemberAccount
--
--         , requestDisassociateS3Resources $
--             newDisassociateS3Resources
--
--         , requestListMemberAccounts $
--             newListMemberAccounts
--
--         , requestListS3Resources $
--             newListS3Resources
--
--         , requestUpdateS3Resources $
--             newUpdateS3Resources
--
--           ]

--     , testGroup "response"
--         [ responseAssociateMemberAccount $
--             newAssociateMemberAccountResponse
--
--         , responseAssociateS3Resources $
--             newAssociateS3ResourcesResponse
--
--         , responseDisassociateMemberAccount $
--             newDisassociateMemberAccountResponse
--
--         , responseDisassociateS3Resources $
--             newDisassociateS3ResourcesResponse
--
--         , responseListMemberAccounts $
--             newListMemberAccountsResponse
--
--         , responseListS3Resources $
--             newListS3ResourcesResponse
--
--         , responseUpdateS3Resources $
--             newUpdateS3ResourcesResponse
--
--           ]
--     ]

-- Requests

requestAssociateMemberAccount :: AssociateMemberAccount -> TestTree
requestAssociateMemberAccount =
  req
    "AssociateMemberAccount"
    "fixture/AssociateMemberAccount.yaml"

requestAssociateS3Resources :: AssociateS3Resources -> TestTree
requestAssociateS3Resources =
  req
    "AssociateS3Resources"
    "fixture/AssociateS3Resources.yaml"

requestDisassociateMemberAccount :: DisassociateMemberAccount -> TestTree
requestDisassociateMemberAccount =
  req
    "DisassociateMemberAccount"
    "fixture/DisassociateMemberAccount.yaml"

requestDisassociateS3Resources :: DisassociateS3Resources -> TestTree
requestDisassociateS3Resources =
  req
    "DisassociateS3Resources"
    "fixture/DisassociateS3Resources.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts =
  req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

requestListS3Resources :: ListS3Resources -> TestTree
requestListS3Resources =
  req
    "ListS3Resources"
    "fixture/ListS3Resources.yaml"

requestUpdateS3Resources :: UpdateS3Resources -> TestTree
requestUpdateS3Resources =
  req
    "UpdateS3Resources"
    "fixture/UpdateS3Resources.yaml"

-- Responses

responseAssociateMemberAccount :: AssociateMemberAccountResponse -> TestTree
responseAssociateMemberAccount =
  res
    "AssociateMemberAccountResponse"
    "fixture/AssociateMemberAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMemberAccount)

responseAssociateS3Resources :: AssociateS3ResourcesResponse -> TestTree
responseAssociateS3Resources =
  res
    "AssociateS3ResourcesResponse"
    "fixture/AssociateS3ResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateS3Resources)

responseDisassociateMemberAccount :: DisassociateMemberAccountResponse -> TestTree
responseDisassociateMemberAccount =
  res
    "DisassociateMemberAccountResponse"
    "fixture/DisassociateMemberAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMemberAccount)

responseDisassociateS3Resources :: DisassociateS3ResourcesResponse -> TestTree
responseDisassociateS3Resources =
  res
    "DisassociateS3ResourcesResponse"
    "fixture/DisassociateS3ResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateS3Resources)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts =
  res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMemberAccounts)

responseListS3Resources :: ListS3ResourcesResponse -> TestTree
responseListS3Resources =
  res
    "ListS3ResourcesResponse"
    "fixture/ListS3ResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListS3Resources)

responseUpdateS3Resources :: UpdateS3ResourcesResponse -> TestTree
responseUpdateS3Resources =
  res
    "UpdateS3ResourcesResponse"
    "fixture/UpdateS3ResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateS3Resources)
