{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FMS
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.FMS where

import Data.Proxy
import Network.AWS.FMS
import Test.AWS.Fixture
import Test.AWS.FMS.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPolicies $
--             listPolicies
--
--         , requestGetComplianceDetail $
--             getComplianceDetail
--
--         , requestGetNotificationChannel $
--             getNotificationChannel
--
--         , requestGetAdminAccount $
--             getAdminAccount
--
--         , requestListComplianceStatus $
--             listComplianceStatus
--
--         , requestPutPolicy $
--             putPolicy
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestDisassociateAdminAccount $
--             disassociateAdminAccount
--
--         , requestPutNotificationChannel $
--             putNotificationChannel
--
--         , requestDeleteNotificationChannel $
--             deleteNotificationChannel
--
--         , requestAssociateAdminAccount $
--             associateAdminAccount
--
--         , requestGetPolicy $
--             getPolicy
--
--           ]

--     , testGroup "response"
--         [ responseListPolicies $
--             listPoliciesResponse
--
--         , responseGetComplianceDetail $
--             getComplianceDetailResponse
--
--         , responseGetNotificationChannel $
--             getNotificationChannelResponse
--
--         , responseGetAdminAccount $
--             getAdminAccountResponse
--
--         , responseListComplianceStatus $
--             listComplianceStatusResponse
--
--         , responsePutPolicy $
--             putPolicyResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseDisassociateAdminAccount $
--             disassociateAdminAccountResponse
--
--         , responsePutNotificationChannel $
--             putNotificationChannelResponse
--
--         , responseDeleteNotificationChannel $
--             deleteNotificationChannelResponse
--
--         , responseAssociateAdminAccount $
--             associateAdminAccountResponse
--
--         , responseGetPolicy $
--             getPolicyResponse
--
--           ]
--     ]

-- Requests

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestGetComplianceDetail :: GetComplianceDetail -> TestTree
requestGetComplianceDetail = req
    "GetComplianceDetail"
    "fixture/GetComplianceDetail.yaml"

requestGetNotificationChannel :: GetNotificationChannel -> TestTree
requestGetNotificationChannel = req
    "GetNotificationChannel"
    "fixture/GetNotificationChannel.yaml"

requestGetAdminAccount :: GetAdminAccount -> TestTree
requestGetAdminAccount = req
    "GetAdminAccount"
    "fixture/GetAdminAccount.yaml"

requestListComplianceStatus :: ListComplianceStatus -> TestTree
requestListComplianceStatus = req
    "ListComplianceStatus"
    "fixture/ListComplianceStatus.yaml"

requestPutPolicy :: PutPolicy -> TestTree
requestPutPolicy = req
    "PutPolicy"
    "fixture/PutPolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDisassociateAdminAccount :: DisassociateAdminAccount -> TestTree
requestDisassociateAdminAccount = req
    "DisassociateAdminAccount"
    "fixture/DisassociateAdminAccount.yaml"

requestPutNotificationChannel :: PutNotificationChannel -> TestTree
requestPutNotificationChannel = req
    "PutNotificationChannel"
    "fixture/PutNotificationChannel.yaml"

requestDeleteNotificationChannel :: DeleteNotificationChannel -> TestTree
requestDeleteNotificationChannel = req
    "DeleteNotificationChannel"
    "fixture/DeleteNotificationChannel.yaml"

requestAssociateAdminAccount :: AssociateAdminAccount -> TestTree
requestAssociateAdminAccount = req
    "AssociateAdminAccount"
    "fixture/AssociateAdminAccount.yaml"

requestGetPolicy :: GetPolicy -> TestTree
requestGetPolicy = req
    "GetPolicy"
    "fixture/GetPolicy.yaml"

-- Responses

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    fms
    (Proxy :: Proxy ListPolicies)

responseGetComplianceDetail :: GetComplianceDetailResponse -> TestTree
responseGetComplianceDetail = res
    "GetComplianceDetailResponse"
    "fixture/GetComplianceDetailResponse.proto"
    fms
    (Proxy :: Proxy GetComplianceDetail)

responseGetNotificationChannel :: GetNotificationChannelResponse -> TestTree
responseGetNotificationChannel = res
    "GetNotificationChannelResponse"
    "fixture/GetNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy GetNotificationChannel)

responseGetAdminAccount :: GetAdminAccountResponse -> TestTree
responseGetAdminAccount = res
    "GetAdminAccountResponse"
    "fixture/GetAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy GetAdminAccount)

responseListComplianceStatus :: ListComplianceStatusResponse -> TestTree
responseListComplianceStatus = res
    "ListComplianceStatusResponse"
    "fixture/ListComplianceStatusResponse.proto"
    fms
    (Proxy :: Proxy ListComplianceStatus)

responsePutPolicy :: PutPolicyResponse -> TestTree
responsePutPolicy = res
    "PutPolicyResponse"
    "fixture/PutPolicyResponse.proto"
    fms
    (Proxy :: Proxy PutPolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    fms
    (Proxy :: Proxy DeletePolicy)

responseDisassociateAdminAccount :: DisassociateAdminAccountResponse -> TestTree
responseDisassociateAdminAccount = res
    "DisassociateAdminAccountResponse"
    "fixture/DisassociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy DisassociateAdminAccount)

responsePutNotificationChannel :: PutNotificationChannelResponse -> TestTree
responsePutNotificationChannel = res
    "PutNotificationChannelResponse"
    "fixture/PutNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy PutNotificationChannel)

responseDeleteNotificationChannel :: DeleteNotificationChannelResponse -> TestTree
responseDeleteNotificationChannel = res
    "DeleteNotificationChannelResponse"
    "fixture/DeleteNotificationChannelResponse.proto"
    fms
    (Proxy :: Proxy DeleteNotificationChannel)

responseAssociateAdminAccount :: AssociateAdminAccountResponse -> TestTree
responseAssociateAdminAccount = res
    "AssociateAdminAccountResponse"
    "fixture/AssociateAdminAccountResponse.proto"
    fms
    (Proxy :: Proxy AssociateAdminAccount)

responseGetPolicy :: GetPolicyResponse -> TestTree
responseGetPolicy = res
    "GetPolicyResponse"
    "fixture/GetPolicyResponse.proto"
    fms
    (Proxy :: Proxy GetPolicy)
