{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Budgets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Budgets where

import Amazonka.Budgets
import qualified Data.Proxy as Proxy
import Test.Amazonka.Budgets.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateBudget $
--             newCreateBudget
--
--         , requestCreateBudgetAction $
--             newCreateBudgetAction
--
--         , requestCreateNotification $
--             newCreateNotification
--
--         , requestCreateSubscriber $
--             newCreateSubscriber
--
--         , requestDeleteBudget $
--             newDeleteBudget
--
--         , requestDeleteBudgetAction $
--             newDeleteBudgetAction
--
--         , requestDeleteNotification $
--             newDeleteNotification
--
--         , requestDeleteSubscriber $
--             newDeleteSubscriber
--
--         , requestDescribeBudget $
--             newDescribeBudget
--
--         , requestDescribeBudgetAction $
--             newDescribeBudgetAction
--
--         , requestDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistories
--
--         , requestDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccount
--
--         , requestDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudget
--
--         , requestDescribeBudgetNotificationsForAccount $
--             newDescribeBudgetNotificationsForAccount
--
--         , requestDescribeBudgetPerformanceHistory $
--             newDescribeBudgetPerformanceHistory
--
--         , requestDescribeBudgets $
--             newDescribeBudgets
--
--         , requestDescribeNotificationsForBudget $
--             newDescribeNotificationsForBudget
--
--         , requestDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotification
--
--         , requestExecuteBudgetAction $
--             newExecuteBudgetAction
--
--         , requestUpdateBudget $
--             newUpdateBudget
--
--         , requestUpdateBudgetAction $
--             newUpdateBudgetAction
--
--         , requestUpdateNotification $
--             newUpdateNotification
--
--         , requestUpdateSubscriber $
--             newUpdateSubscriber
--
--           ]

--     , testGroup "response"
--         [ responseCreateBudget $
--             newCreateBudgetResponse
--
--         , responseCreateBudgetAction $
--             newCreateBudgetActionResponse
--
--         , responseCreateNotification $
--             newCreateNotificationResponse
--
--         , responseCreateSubscriber $
--             newCreateSubscriberResponse
--
--         , responseDeleteBudget $
--             newDeleteBudgetResponse
--
--         , responseDeleteBudgetAction $
--             newDeleteBudgetActionResponse
--
--         , responseDeleteNotification $
--             newDeleteNotificationResponse
--
--         , responseDeleteSubscriber $
--             newDeleteSubscriberResponse
--
--         , responseDescribeBudget $
--             newDescribeBudgetResponse
--
--         , responseDescribeBudgetAction $
--             newDescribeBudgetActionResponse
--
--         , responseDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistoriesResponse
--
--         , responseDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccountResponse
--
--         , responseDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudgetResponse
--
--         , responseDescribeBudgetNotificationsForAccount $
--             newDescribeBudgetNotificationsForAccountResponse
--
--         , responseDescribeBudgetPerformanceHistory $
--             newDescribeBudgetPerformanceHistoryResponse
--
--         , responseDescribeBudgets $
--             newDescribeBudgetsResponse
--
--         , responseDescribeNotificationsForBudget $
--             newDescribeNotificationsForBudgetResponse
--
--         , responseDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotificationResponse
--
--         , responseExecuteBudgetAction $
--             newExecuteBudgetActionResponse
--
--         , responseUpdateBudget $
--             newUpdateBudgetResponse
--
--         , responseUpdateBudgetAction $
--             newUpdateBudgetActionResponse
--
--         , responseUpdateNotification $
--             newUpdateNotificationResponse
--
--         , responseUpdateSubscriber $
--             newUpdateSubscriberResponse
--
--           ]
--     ]

-- Requests

requestCreateBudget :: CreateBudget -> TestTree
requestCreateBudget =
  req
    "CreateBudget"
    "fixture/CreateBudget.yaml"

requestCreateBudgetAction :: CreateBudgetAction -> TestTree
requestCreateBudgetAction =
  req
    "CreateBudgetAction"
    "fixture/CreateBudgetAction.yaml"

requestCreateNotification :: CreateNotification -> TestTree
requestCreateNotification =
  req
    "CreateNotification"
    "fixture/CreateNotification.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestDeleteBudget :: DeleteBudget -> TestTree
requestDeleteBudget =
  req
    "DeleteBudget"
    "fixture/DeleteBudget.yaml"

requestDeleteBudgetAction :: DeleteBudgetAction -> TestTree
requestDeleteBudgetAction =
  req
    "DeleteBudgetAction"
    "fixture/DeleteBudgetAction.yaml"

requestDeleteNotification :: DeleteNotification -> TestTree
requestDeleteNotification =
  req
    "DeleteNotification"
    "fixture/DeleteNotification.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber =
  req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestDescribeBudget :: DescribeBudget -> TestTree
requestDescribeBudget =
  req
    "DescribeBudget"
    "fixture/DescribeBudget.yaml"

requestDescribeBudgetAction :: DescribeBudgetAction -> TestTree
requestDescribeBudgetAction =
  req
    "DescribeBudgetAction"
    "fixture/DescribeBudgetAction.yaml"

requestDescribeBudgetActionHistories :: DescribeBudgetActionHistories -> TestTree
requestDescribeBudgetActionHistories =
  req
    "DescribeBudgetActionHistories"
    "fixture/DescribeBudgetActionHistories.yaml"

requestDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccount -> TestTree
requestDescribeBudgetActionsForAccount =
  req
    "DescribeBudgetActionsForAccount"
    "fixture/DescribeBudgetActionsForAccount.yaml"

requestDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudget -> TestTree
requestDescribeBudgetActionsForBudget =
  req
    "DescribeBudgetActionsForBudget"
    "fixture/DescribeBudgetActionsForBudget.yaml"

requestDescribeBudgetNotificationsForAccount :: DescribeBudgetNotificationsForAccount -> TestTree
requestDescribeBudgetNotificationsForAccount =
  req
    "DescribeBudgetNotificationsForAccount"
    "fixture/DescribeBudgetNotificationsForAccount.yaml"

requestDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistory -> TestTree
requestDescribeBudgetPerformanceHistory =
  req
    "DescribeBudgetPerformanceHistory"
    "fixture/DescribeBudgetPerformanceHistory.yaml"

requestDescribeBudgets :: DescribeBudgets -> TestTree
requestDescribeBudgets =
  req
    "DescribeBudgets"
    "fixture/DescribeBudgets.yaml"

requestDescribeNotificationsForBudget :: DescribeNotificationsForBudget -> TestTree
requestDescribeNotificationsForBudget =
  req
    "DescribeNotificationsForBudget"
    "fixture/DescribeNotificationsForBudget.yaml"

requestDescribeSubscribersForNotification :: DescribeSubscribersForNotification -> TestTree
requestDescribeSubscribersForNotification =
  req
    "DescribeSubscribersForNotification"
    "fixture/DescribeSubscribersForNotification.yaml"

requestExecuteBudgetAction :: ExecuteBudgetAction -> TestTree
requestExecuteBudgetAction =
  req
    "ExecuteBudgetAction"
    "fixture/ExecuteBudgetAction.yaml"

requestUpdateBudget :: UpdateBudget -> TestTree
requestUpdateBudget =
  req
    "UpdateBudget"
    "fixture/UpdateBudget.yaml"

requestUpdateBudgetAction :: UpdateBudgetAction -> TestTree
requestUpdateBudgetAction =
  req
    "UpdateBudgetAction"
    "fixture/UpdateBudgetAction.yaml"

requestUpdateNotification :: UpdateNotification -> TestTree
requestUpdateNotification =
  req
    "UpdateNotification"
    "fixture/UpdateNotification.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber =
  req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

-- Responses

responseCreateBudget :: CreateBudgetResponse -> TestTree
responseCreateBudget =
  res
    "CreateBudgetResponse"
    "fixture/CreateBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBudget)

responseCreateBudgetAction :: CreateBudgetActionResponse -> TestTree
responseCreateBudgetAction =
  res
    "CreateBudgetActionResponse"
    "fixture/CreateBudgetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBudgetAction)

responseCreateNotification :: CreateNotificationResponse -> TestTree
responseCreateNotification =
  res
    "CreateNotificationResponse"
    "fixture/CreateNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNotification)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriber)

responseDeleteBudget :: DeleteBudgetResponse -> TestTree
responseDeleteBudget =
  res
    "DeleteBudgetResponse"
    "fixture/DeleteBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBudget)

responseDeleteBudgetAction :: DeleteBudgetActionResponse -> TestTree
responseDeleteBudgetAction =
  res
    "DeleteBudgetActionResponse"
    "fixture/DeleteBudgetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBudgetAction)

responseDeleteNotification :: DeleteNotificationResponse -> TestTree
responseDeleteNotification =
  res
    "DeleteNotificationResponse"
    "fixture/DeleteNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNotification)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriber)

responseDescribeBudget :: DescribeBudgetResponse -> TestTree
responseDescribeBudget =
  res
    "DescribeBudgetResponse"
    "fixture/DescribeBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudget)

responseDescribeBudgetAction :: DescribeBudgetActionResponse -> TestTree
responseDescribeBudgetAction =
  res
    "DescribeBudgetActionResponse"
    "fixture/DescribeBudgetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetAction)

responseDescribeBudgetActionHistories :: DescribeBudgetActionHistoriesResponse -> TestTree
responseDescribeBudgetActionHistories =
  res
    "DescribeBudgetActionHistoriesResponse"
    "fixture/DescribeBudgetActionHistoriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetActionHistories)

responseDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccountResponse -> TestTree
responseDescribeBudgetActionsForAccount =
  res
    "DescribeBudgetActionsForAccountResponse"
    "fixture/DescribeBudgetActionsForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetActionsForAccount)

responseDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudgetResponse -> TestTree
responseDescribeBudgetActionsForBudget =
  res
    "DescribeBudgetActionsForBudgetResponse"
    "fixture/DescribeBudgetActionsForBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetActionsForBudget)

responseDescribeBudgetNotificationsForAccount :: DescribeBudgetNotificationsForAccountResponse -> TestTree
responseDescribeBudgetNotificationsForAccount =
  res
    "DescribeBudgetNotificationsForAccountResponse"
    "fixture/DescribeBudgetNotificationsForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetNotificationsForAccount)

responseDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistoryResponse -> TestTree
responseDescribeBudgetPerformanceHistory =
  res
    "DescribeBudgetPerformanceHistoryResponse"
    "fixture/DescribeBudgetPerformanceHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgetPerformanceHistory)

responseDescribeBudgets :: DescribeBudgetsResponse -> TestTree
responseDescribeBudgets =
  res
    "DescribeBudgetsResponse"
    "fixture/DescribeBudgetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBudgets)

responseDescribeNotificationsForBudget :: DescribeNotificationsForBudgetResponse -> TestTree
responseDescribeNotificationsForBudget =
  res
    "DescribeNotificationsForBudgetResponse"
    "fixture/DescribeNotificationsForBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNotificationsForBudget)

responseDescribeSubscribersForNotification :: DescribeSubscribersForNotificationResponse -> TestTree
responseDescribeSubscribersForNotification =
  res
    "DescribeSubscribersForNotificationResponse"
    "fixture/DescribeSubscribersForNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSubscribersForNotification)

responseExecuteBudgetAction :: ExecuteBudgetActionResponse -> TestTree
responseExecuteBudgetAction =
  res
    "ExecuteBudgetActionResponse"
    "fixture/ExecuteBudgetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExecuteBudgetAction)

responseUpdateBudget :: UpdateBudgetResponse -> TestTree
responseUpdateBudget =
  res
    "UpdateBudgetResponse"
    "fixture/UpdateBudgetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBudget)

responseUpdateBudgetAction :: UpdateBudgetActionResponse -> TestTree
responseUpdateBudgetAction =
  res
    "UpdateBudgetActionResponse"
    "fixture/UpdateBudgetActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBudgetAction)

responseUpdateNotification :: UpdateNotificationResponse -> TestTree
responseUpdateNotification =
  res
    "UpdateNotificationResponse"
    "fixture/UpdateNotificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateNotification)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriber)
