{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Budgets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Budgets where

import Data.Proxy
import Network.AWS.Budgets
import Test.AWS.Budgets.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateBudgetAction $
--             newCreateBudgetAction
--
--         , requestDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccount
--
--         , requestExecuteBudgetAction $
--             newExecuteBudgetAction
--
--         , requestDescribeBudgetAction $
--             newDescribeBudgetAction
--
--         , requestDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistories
--
--         , requestDeleteBudgetAction $
--             newDeleteBudgetAction
--
--         , requestUpdateBudgetAction $
--             newUpdateBudgetAction
--
--         , requestDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotification
--
--         , requestUpdateBudget $
--             newUpdateBudget
--
--         , requestDeleteBudget $
--             newDeleteBudget
--
--         , requestUpdateNotification $
--             newUpdateNotification
--
--         , requestDeleteNotification $
--             newDeleteNotification
--
--         , requestCreateNotification $
--             newCreateNotification
--
--         , requestCreateBudget $
--             newCreateBudget
--
--         , requestCreateSubscriber $
--             newCreateSubscriber
--
--         , requestUpdateSubscriber $
--             newUpdateSubscriber
--
--         , requestDeleteSubscriber $
--             newDeleteSubscriber
--
--         , requestDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudget
--
--         , requestDescribeBudgets $
--             newDescribeBudgets
--
--         , requestDescribeBudget $
--             newDescribeBudget
--
--         , requestDescribeNotificationsForBudget $
--             newDescribeNotificationsForBudget
--
--         , requestDescribeBudgetPerformanceHistory $
--             newDescribeBudgetPerformanceHistory
--
--           ]

--     , testGroup "response"
--         [ responseCreateBudgetAction $
--             newCreateBudgetActionResponse
--
--         , responseDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccountResponse
--
--         , responseExecuteBudgetAction $
--             newExecuteBudgetActionResponse
--
--         , responseDescribeBudgetAction $
--             newDescribeBudgetActionResponse
--
--         , responseDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistoriesResponse
--
--         , responseDeleteBudgetAction $
--             newDeleteBudgetActionResponse
--
--         , responseUpdateBudgetAction $
--             newUpdateBudgetActionResponse
--
--         , responseDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotificationResponse
--
--         , responseUpdateBudget $
--             newUpdateBudgetResponse
--
--         , responseDeleteBudget $
--             newDeleteBudgetResponse
--
--         , responseUpdateNotification $
--             newUpdateNotificationResponse
--
--         , responseDeleteNotification $
--             newDeleteNotificationResponse
--
--         , responseCreateNotification $
--             newCreateNotificationResponse
--
--         , responseCreateBudget $
--             newCreateBudgetResponse
--
--         , responseCreateSubscriber $
--             newCreateSubscriberResponse
--
--         , responseUpdateSubscriber $
--             newUpdateSubscriberResponse
--
--         , responseDeleteSubscriber $
--             newDeleteSubscriberResponse
--
--         , responseDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudgetResponse
--
--         , responseDescribeBudgets $
--             newDescribeBudgetsResponse
--
--         , responseDescribeBudget $
--             newDescribeBudgetResponse
--
--         , responseDescribeNotificationsForBudget $
--             newDescribeNotificationsForBudgetResponse
--
--         , responseDescribeBudgetPerformanceHistory $
--             newDescribeBudgetPerformanceHistoryResponse
--
--           ]
--     ]

-- Requests

requestCreateBudgetAction :: CreateBudgetAction -> TestTree
requestCreateBudgetAction =
  req
    "CreateBudgetAction"
    "fixture/CreateBudgetAction.yaml"

requestDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccount -> TestTree
requestDescribeBudgetActionsForAccount =
  req
    "DescribeBudgetActionsForAccount"
    "fixture/DescribeBudgetActionsForAccount.yaml"

requestExecuteBudgetAction :: ExecuteBudgetAction -> TestTree
requestExecuteBudgetAction =
  req
    "ExecuteBudgetAction"
    "fixture/ExecuteBudgetAction.yaml"

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

requestDeleteBudgetAction :: DeleteBudgetAction -> TestTree
requestDeleteBudgetAction =
  req
    "DeleteBudgetAction"
    "fixture/DeleteBudgetAction.yaml"

requestUpdateBudgetAction :: UpdateBudgetAction -> TestTree
requestUpdateBudgetAction =
  req
    "UpdateBudgetAction"
    "fixture/UpdateBudgetAction.yaml"

requestDescribeSubscribersForNotification :: DescribeSubscribersForNotification -> TestTree
requestDescribeSubscribersForNotification =
  req
    "DescribeSubscribersForNotification"
    "fixture/DescribeSubscribersForNotification.yaml"

requestUpdateBudget :: UpdateBudget -> TestTree
requestUpdateBudget =
  req
    "UpdateBudget"
    "fixture/UpdateBudget.yaml"

requestDeleteBudget :: DeleteBudget -> TestTree
requestDeleteBudget =
  req
    "DeleteBudget"
    "fixture/DeleteBudget.yaml"

requestUpdateNotification :: UpdateNotification -> TestTree
requestUpdateNotification =
  req
    "UpdateNotification"
    "fixture/UpdateNotification.yaml"

requestDeleteNotification :: DeleteNotification -> TestTree
requestDeleteNotification =
  req
    "DeleteNotification"
    "fixture/DeleteNotification.yaml"

requestCreateNotification :: CreateNotification -> TestTree
requestCreateNotification =
  req
    "CreateNotification"
    "fixture/CreateNotification.yaml"

requestCreateBudget :: CreateBudget -> TestTree
requestCreateBudget =
  req
    "CreateBudget"
    "fixture/CreateBudget.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber =
  req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber =
  req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudget -> TestTree
requestDescribeBudgetActionsForBudget =
  req
    "DescribeBudgetActionsForBudget"
    "fixture/DescribeBudgetActionsForBudget.yaml"

requestDescribeBudgets :: DescribeBudgets -> TestTree
requestDescribeBudgets =
  req
    "DescribeBudgets"
    "fixture/DescribeBudgets.yaml"

requestDescribeBudget :: DescribeBudget -> TestTree
requestDescribeBudget =
  req
    "DescribeBudget"
    "fixture/DescribeBudget.yaml"

requestDescribeNotificationsForBudget :: DescribeNotificationsForBudget -> TestTree
requestDescribeNotificationsForBudget =
  req
    "DescribeNotificationsForBudget"
    "fixture/DescribeNotificationsForBudget.yaml"

requestDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistory -> TestTree
requestDescribeBudgetPerformanceHistory =
  req
    "DescribeBudgetPerformanceHistory"
    "fixture/DescribeBudgetPerformanceHistory.yaml"

-- Responses

responseCreateBudgetAction :: CreateBudgetActionResponse -> TestTree
responseCreateBudgetAction =
  res
    "CreateBudgetActionResponse"
    "fixture/CreateBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBudgetAction)

responseDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccountResponse -> TestTree
responseDescribeBudgetActionsForAccount =
  res
    "DescribeBudgetActionsForAccountResponse"
    "fixture/DescribeBudgetActionsForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionsForAccount)

responseExecuteBudgetAction :: ExecuteBudgetActionResponse -> TestTree
responseExecuteBudgetAction =
  res
    "ExecuteBudgetActionResponse"
    "fixture/ExecuteBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy ExecuteBudgetAction)

responseDescribeBudgetAction :: DescribeBudgetActionResponse -> TestTree
responseDescribeBudgetAction =
  res
    "DescribeBudgetActionResponse"
    "fixture/DescribeBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetAction)

responseDescribeBudgetActionHistories :: DescribeBudgetActionHistoriesResponse -> TestTree
responseDescribeBudgetActionHistories =
  res
    "DescribeBudgetActionHistoriesResponse"
    "fixture/DescribeBudgetActionHistoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionHistories)

responseDeleteBudgetAction :: DeleteBudgetActionResponse -> TestTree
responseDeleteBudgetAction =
  res
    "DeleteBudgetActionResponse"
    "fixture/DeleteBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBudgetAction)

responseUpdateBudgetAction :: UpdateBudgetActionResponse -> TestTree
responseUpdateBudgetAction =
  res
    "UpdateBudgetActionResponse"
    "fixture/UpdateBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBudgetAction)

responseDescribeSubscribersForNotification :: DescribeSubscribersForNotificationResponse -> TestTree
responseDescribeSubscribersForNotification =
  res
    "DescribeSubscribersForNotificationResponse"
    "fixture/DescribeSubscribersForNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSubscribersForNotification)

responseUpdateBudget :: UpdateBudgetResponse -> TestTree
responseUpdateBudget =
  res
    "UpdateBudgetResponse"
    "fixture/UpdateBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBudget)

responseDeleteBudget :: DeleteBudgetResponse -> TestTree
responseDeleteBudget =
  res
    "DeleteBudgetResponse"
    "fixture/DeleteBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBudget)

responseUpdateNotification :: UpdateNotificationResponse -> TestTree
responseUpdateNotification =
  res
    "UpdateNotificationResponse"
    "fixture/UpdateNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateNotification)

responseDeleteNotification :: DeleteNotificationResponse -> TestTree
responseDeleteNotification =
  res
    "DeleteNotificationResponse"
    "fixture/DeleteNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotification)

responseCreateNotification :: CreateNotificationResponse -> TestTree
responseCreateNotification =
  res
    "CreateNotificationResponse"
    "fixture/CreateNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotification)

responseCreateBudget :: CreateBudgetResponse -> TestTree
responseCreateBudget =
  res
    "CreateBudgetResponse"
    "fixture/CreateBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBudget)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscriber)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSubscriber)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubscriber)

responseDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudgetResponse -> TestTree
responseDescribeBudgetActionsForBudget =
  res
    "DescribeBudgetActionsForBudgetResponse"
    "fixture/DescribeBudgetActionsForBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionsForBudget)

responseDescribeBudgets :: DescribeBudgetsResponse -> TestTree
responseDescribeBudgets =
  res
    "DescribeBudgetsResponse"
    "fixture/DescribeBudgetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgets)

responseDescribeBudget :: DescribeBudgetResponse -> TestTree
responseDescribeBudget =
  res
    "DescribeBudgetResponse"
    "fixture/DescribeBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudget)

responseDescribeNotificationsForBudget :: DescribeNotificationsForBudgetResponse -> TestTree
responseDescribeNotificationsForBudget =
  res
    "DescribeNotificationsForBudgetResponse"
    "fixture/DescribeNotificationsForBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNotificationsForBudget)

responseDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistoryResponse -> TestTree
responseDescribeBudgetPerformanceHistory =
  res
    "DescribeBudgetPerformanceHistoryResponse"
    "fixture/DescribeBudgetPerformanceHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetPerformanceHistory)
