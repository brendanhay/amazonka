{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Budgets
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             createBudgetAction
--
--         , requestUpdateBudgetAction $
--             updateBudgetAction
--
--         , requestDeleteBudgetAction $
--             deleteBudgetAction
--
--         , requestDescribeSubscribersForNotification $
--             describeSubscribersForNotification
--
--         , requestDescribeNotificationsForBudget $
--             describeNotificationsForBudget
--
--         , requestDescribeBudgets $
--             describeBudgets
--
--         , requestCreateSubscriber $
--             createSubscriber
--
--         , requestExecuteBudgetAction $
--             executeBudgetAction
--
--         , requestUpdateBudget $
--             updateBudget
--
--         , requestDeleteBudget $
--             deleteBudget
--
--         , requestDeleteNotification $
--             deleteNotification
--
--         , requestUpdateNotification $
--             updateNotification
--
--         , requestDescribeBudgetActionsForAccount $
--             describeBudgetActionsForAccount
--
--         , requestDescribeBudgetPerformanceHistory $
--             describeBudgetPerformanceHistory
--
--         , requestDescribeBudgetActionHistories $
--             describeBudgetActionHistories
--
--         , requestDescribeBudget $
--             describeBudget
--
--         , requestUpdateSubscriber $
--             updateSubscriber
--
--         , requestDeleteSubscriber $
--             deleteSubscriber
--
--         , requestDescribeBudgetActionsForBudget $
--             describeBudgetActionsForBudget
--
--         , requestDescribeBudgetAction $
--             describeBudgetAction
--
--         , requestCreateBudget $
--             createBudget
--
--         , requestCreateNotification $
--             createNotification
--
--           ]

--     , testGroup "response"
--         [ responseCreateBudgetAction $
--             createBudgetActionResponse
--
--         , responseUpdateBudgetAction $
--             updateBudgetActionResponse
--
--         , responseDeleteBudgetAction $
--             deleteBudgetActionResponse
--
--         , responseDescribeSubscribersForNotification $
--             describeSubscribersForNotificationResponse
--
--         , responseDescribeNotificationsForBudget $
--             describeNotificationsForBudgetResponse
--
--         , responseDescribeBudgets $
--             describeBudgetsResponse
--
--         , responseCreateSubscriber $
--             createSubscriberResponse
--
--         , responseExecuteBudgetAction $
--             executeBudgetActionResponse
--
--         , responseUpdateBudget $
--             updateBudgetResponse
--
--         , responseDeleteBudget $
--             deleteBudgetResponse
--
--         , responseDeleteNotification $
--             deleteNotificationResponse
--
--         , responseUpdateNotification $
--             updateNotificationResponse
--
--         , responseDescribeBudgetActionsForAccount $
--             describeBudgetActionsForAccountResponse
--
--         , responseDescribeBudgetPerformanceHistory $
--             describeBudgetPerformanceHistoryResponse
--
--         , responseDescribeBudgetActionHistories $
--             describeBudgetActionHistoriesResponse
--
--         , responseDescribeBudget $
--             describeBudgetResponse
--
--         , responseUpdateSubscriber $
--             updateSubscriberResponse
--
--         , responseDeleteSubscriber $
--             deleteSubscriberResponse
--
--         , responseDescribeBudgetActionsForBudget $
--             describeBudgetActionsForBudgetResponse
--
--         , responseDescribeBudgetAction $
--             describeBudgetActionResponse
--
--         , responseCreateBudget $
--             createBudgetResponse
--
--         , responseCreateNotification $
--             createNotificationResponse
--
--           ]
--     ]

-- Requests

requestCreateBudgetAction :: CreateBudgetAction -> TestTree
requestCreateBudgetAction =
  req
    "CreateBudgetAction"
    "fixture/CreateBudgetAction.yaml"

requestUpdateBudgetAction :: UpdateBudgetAction -> TestTree
requestUpdateBudgetAction =
  req
    "UpdateBudgetAction"
    "fixture/UpdateBudgetAction.yaml"

requestDeleteBudgetAction :: DeleteBudgetAction -> TestTree
requestDeleteBudgetAction =
  req
    "DeleteBudgetAction"
    "fixture/DeleteBudgetAction.yaml"

requestDescribeSubscribersForNotification :: DescribeSubscribersForNotification -> TestTree
requestDescribeSubscribersForNotification =
  req
    "DescribeSubscribersForNotification"
    "fixture/DescribeSubscribersForNotification.yaml"

requestDescribeNotificationsForBudget :: DescribeNotificationsForBudget -> TestTree
requestDescribeNotificationsForBudget =
  req
    "DescribeNotificationsForBudget"
    "fixture/DescribeNotificationsForBudget.yaml"

requestDescribeBudgets :: DescribeBudgets -> TestTree
requestDescribeBudgets =
  req
    "DescribeBudgets"
    "fixture/DescribeBudgets.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

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

requestDeleteBudget :: DeleteBudget -> TestTree
requestDeleteBudget =
  req
    "DeleteBudget"
    "fixture/DeleteBudget.yaml"

requestDeleteNotification :: DeleteNotification -> TestTree
requestDeleteNotification =
  req
    "DeleteNotification"
    "fixture/DeleteNotification.yaml"

requestUpdateNotification :: UpdateNotification -> TestTree
requestUpdateNotification =
  req
    "UpdateNotification"
    "fixture/UpdateNotification.yaml"

requestDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccount -> TestTree
requestDescribeBudgetActionsForAccount =
  req
    "DescribeBudgetActionsForAccount"
    "fixture/DescribeBudgetActionsForAccount.yaml"

requestDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistory -> TestTree
requestDescribeBudgetPerformanceHistory =
  req
    "DescribeBudgetPerformanceHistory"
    "fixture/DescribeBudgetPerformanceHistory.yaml"

requestDescribeBudgetActionHistories :: DescribeBudgetActionHistories -> TestTree
requestDescribeBudgetActionHistories =
  req
    "DescribeBudgetActionHistories"
    "fixture/DescribeBudgetActionHistories.yaml"

requestDescribeBudget :: DescribeBudget -> TestTree
requestDescribeBudget =
  req
    "DescribeBudget"
    "fixture/DescribeBudget.yaml"

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

requestDescribeBudgetAction :: DescribeBudgetAction -> TestTree
requestDescribeBudgetAction =
  req
    "DescribeBudgetAction"
    "fixture/DescribeBudgetAction.yaml"

requestCreateBudget :: CreateBudget -> TestTree
requestCreateBudget =
  req
    "CreateBudget"
    "fixture/CreateBudget.yaml"

requestCreateNotification :: CreateNotification -> TestTree
requestCreateNotification =
  req
    "CreateNotification"
    "fixture/CreateNotification.yaml"

-- Responses

responseCreateBudgetAction :: CreateBudgetActionResponse -> TestTree
responseCreateBudgetAction =
  res
    "CreateBudgetActionResponse"
    "fixture/CreateBudgetActionResponse.proto"
    budgets
    (Proxy :: Proxy CreateBudgetAction)

responseUpdateBudgetAction :: UpdateBudgetActionResponse -> TestTree
responseUpdateBudgetAction =
  res
    "UpdateBudgetActionResponse"
    "fixture/UpdateBudgetActionResponse.proto"
    budgets
    (Proxy :: Proxy UpdateBudgetAction)

responseDeleteBudgetAction :: DeleteBudgetActionResponse -> TestTree
responseDeleteBudgetAction =
  res
    "DeleteBudgetActionResponse"
    "fixture/DeleteBudgetActionResponse.proto"
    budgets
    (Proxy :: Proxy DeleteBudgetAction)

responseDescribeSubscribersForNotification :: DescribeSubscribersForNotificationResponse -> TestTree
responseDescribeSubscribersForNotification =
  res
    "DescribeSubscribersForNotificationResponse"
    "fixture/DescribeSubscribersForNotificationResponse.proto"
    budgets
    (Proxy :: Proxy DescribeSubscribersForNotification)

responseDescribeNotificationsForBudget :: DescribeNotificationsForBudgetResponse -> TestTree
responseDescribeNotificationsForBudget =
  res
    "DescribeNotificationsForBudgetResponse"
    "fixture/DescribeNotificationsForBudgetResponse.proto"
    budgets
    (Proxy :: Proxy DescribeNotificationsForBudget)

responseDescribeBudgets :: DescribeBudgetsResponse -> TestTree
responseDescribeBudgets =
  res
    "DescribeBudgetsResponse"
    "fixture/DescribeBudgetsResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgets)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    budgets
    (Proxy :: Proxy CreateSubscriber)

responseExecuteBudgetAction :: ExecuteBudgetActionResponse -> TestTree
responseExecuteBudgetAction =
  res
    "ExecuteBudgetActionResponse"
    "fixture/ExecuteBudgetActionResponse.proto"
    budgets
    (Proxy :: Proxy ExecuteBudgetAction)

responseUpdateBudget :: UpdateBudgetResponse -> TestTree
responseUpdateBudget =
  res
    "UpdateBudgetResponse"
    "fixture/UpdateBudgetResponse.proto"
    budgets
    (Proxy :: Proxy UpdateBudget)

responseDeleteBudget :: DeleteBudgetResponse -> TestTree
responseDeleteBudget =
  res
    "DeleteBudgetResponse"
    "fixture/DeleteBudgetResponse.proto"
    budgets
    (Proxy :: Proxy DeleteBudget)

responseDeleteNotification :: DeleteNotificationResponse -> TestTree
responseDeleteNotification =
  res
    "DeleteNotificationResponse"
    "fixture/DeleteNotificationResponse.proto"
    budgets
    (Proxy :: Proxy DeleteNotification)

responseUpdateNotification :: UpdateNotificationResponse -> TestTree
responseUpdateNotification =
  res
    "UpdateNotificationResponse"
    "fixture/UpdateNotificationResponse.proto"
    budgets
    (Proxy :: Proxy UpdateNotification)

responseDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccountResponse -> TestTree
responseDescribeBudgetActionsForAccount =
  res
    "DescribeBudgetActionsForAccountResponse"
    "fixture/DescribeBudgetActionsForAccountResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgetActionsForAccount)

responseDescribeBudgetPerformanceHistory :: DescribeBudgetPerformanceHistoryResponse -> TestTree
responseDescribeBudgetPerformanceHistory =
  res
    "DescribeBudgetPerformanceHistoryResponse"
    "fixture/DescribeBudgetPerformanceHistoryResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgetPerformanceHistory)

responseDescribeBudgetActionHistories :: DescribeBudgetActionHistoriesResponse -> TestTree
responseDescribeBudgetActionHistories =
  res
    "DescribeBudgetActionHistoriesResponse"
    "fixture/DescribeBudgetActionHistoriesResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgetActionHistories)

responseDescribeBudget :: DescribeBudgetResponse -> TestTree
responseDescribeBudget =
  res
    "DescribeBudgetResponse"
    "fixture/DescribeBudgetResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudget)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    budgets
    (Proxy :: Proxy UpdateSubscriber)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    budgets
    (Proxy :: Proxy DeleteSubscriber)

responseDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudgetResponse -> TestTree
responseDescribeBudgetActionsForBudget =
  res
    "DescribeBudgetActionsForBudgetResponse"
    "fixture/DescribeBudgetActionsForBudgetResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgetActionsForBudget)

responseDescribeBudgetAction :: DescribeBudgetActionResponse -> TestTree
responseDescribeBudgetAction =
  res
    "DescribeBudgetActionResponse"
    "fixture/DescribeBudgetActionResponse.proto"
    budgets
    (Proxy :: Proxy DescribeBudgetAction)

responseCreateBudget :: CreateBudgetResponse -> TestTree
responseCreateBudget =
  res
    "CreateBudgetResponse"
    "fixture/CreateBudgetResponse.proto"
    budgets
    (Proxy :: Proxy CreateBudget)

responseCreateNotification :: CreateNotificationResponse -> TestTree
responseCreateNotification =
  res
    "CreateNotificationResponse"
    "fixture/CreateNotificationResponse.proto"
    budgets
    (Proxy :: Proxy CreateNotification)
