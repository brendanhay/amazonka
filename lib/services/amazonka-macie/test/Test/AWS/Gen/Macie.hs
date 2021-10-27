{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Macie
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Macie where

import Data.Proxy
import Network.AWS.Macie
import Test.AWS.Fixture
import Test.AWS.Macie.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateS3Resources $
--             newAssociateS3Resources
--
--         , requestAssociateMemberAccount $
--             newAssociateMemberAccount
--
--         , requestUpdateS3Resources $
--             newUpdateS3Resources
--
--         , requestListMemberAccounts $
--             newListMemberAccounts
--
--         , requestDisassociateMemberAccount $
--             newDisassociateMemberAccount
--
--         , requestListS3Resources $
--             newListS3Resources
--
--         , requestDisassociateS3Resources $
--             newDisassociateS3Resources
--
--           ]

--     , testGroup "response"
--         [ responseAssociateS3Resources $
--             newAssociateS3ResourcesResponse
--
--         , responseAssociateMemberAccount $
--             newAssociateMemberAccountResponse
--
--         , responseUpdateS3Resources $
--             newUpdateS3ResourcesResponse
--
--         , responseListMemberAccounts $
--             newListMemberAccountsResponse
--
--         , responseDisassociateMemberAccount $
--             newDisassociateMemberAccountResponse
--
--         , responseListS3Resources $
--             newListS3ResourcesResponse
--
--         , responseDisassociateS3Resources $
--             newDisassociateS3ResourcesResponse
--
--           ]
--     ]

-- Requests

requestAssociateS3Resources :: AssociateS3Resources -> TestTree
requestAssociateS3Resources =
  req
    "AssociateS3Resources"
    "fixture/AssociateS3Resources.yaml"

requestAssociateMemberAccount :: AssociateMemberAccount -> TestTree
requestAssociateMemberAccount =
  req
    "AssociateMemberAccount"
    "fixture/AssociateMemberAccount.yaml"

requestUpdateS3Resources :: UpdateS3Resources -> TestTree
requestUpdateS3Resources =
  req
    "UpdateS3Resources"
    "fixture/UpdateS3Resources.yaml"

requestListMemberAccounts :: ListMemberAccounts -> TestTree
requestListMemberAccounts =
  req
    "ListMemberAccounts"
    "fixture/ListMemberAccounts.yaml"

requestDisassociateMemberAccount :: DisassociateMemberAccount -> TestTree
requestDisassociateMemberAccount =
  req
    "DisassociateMemberAccount"
    "fixture/DisassociateMemberAccount.yaml"

requestListS3Resources :: ListS3Resources -> TestTree
requestListS3Resources =
  req
    "ListS3Resources"
    "fixture/ListS3Resources.yaml"

requestDisassociateS3Resources :: DisassociateS3Resources -> TestTree
requestDisassociateS3Resources =
  req
    "DisassociateS3Resources"
    "fixture/DisassociateS3Resources.yaml"

-- Responses

responseAssociateS3Resources :: AssociateS3ResourcesResponse -> TestTree
responseAssociateS3Resources =
  res
    "AssociateS3ResourcesResponse"
    "fixture/AssociateS3ResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateS3Resources)

responseAssociateMemberAccount :: AssociateMemberAccountResponse -> TestTree
responseAssociateMemberAccount =
  res
    "AssociateMemberAccountResponse"
    "fixture/AssociateMemberAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateMemberAccount)

responseUpdateS3Resources :: UpdateS3ResourcesResponse -> TestTree
responseUpdateS3Resources =
  res
    "UpdateS3ResourcesResponse"
    "fixture/UpdateS3ResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateS3Resources)

responseListMemberAccounts :: ListMemberAccountsResponse -> TestTree
responseListMemberAccounts =
  res
    "ListMemberAccountsResponse"
    "fixture/ListMemberAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMemberAccounts)

responseDisassociateMemberAccount :: DisassociateMemberAccountResponse -> TestTree
responseDisassociateMemberAccount =
  res
    "DisassociateMemberAccountResponse"
    "fixture/DisassociateMemberAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateMemberAccount)

responseListS3Resources :: ListS3ResourcesResponse -> TestTree
responseListS3Resources =
  res
    "ListS3ResourcesResponse"
    "fixture/ListS3ResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListS3Resources)

responseDisassociateS3Resources :: DisassociateS3ResourcesResponse -> TestTree
responseDisassociateS3Resources =
  res
    "DisassociateS3ResourcesResponse"
    "fixture/DisassociateS3ResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateS3Resources)
