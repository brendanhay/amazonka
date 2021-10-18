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
--         [ requestDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccount
--
--         , requestCreateBudgetAction $
--             newCreateBudgetAction
--
--         , requestExecuteBudgetAction $
--             newExecuteBudgetAction
--
--         , requestDescribeBudgetAction $
--             newDescribeBudgetAction
--
--         , requestUpdateBudgetAction $
--             newUpdateBudgetAction
--
--         , requestDeleteBudgetAction $
--             newDeleteBudgetAction
--
--         , requestDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistories
--
--         , requestDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotification
--
--         , requestUpdateBudget $
--             newUpdateBudget
--
--         , requestDeleteNotification $
--             newDeleteNotification
--
--         , requestDeleteBudget $
--             newDeleteBudget
--
--         , requestUpdateNotification $
--             newUpdateNotification
--
--         , requestCreateBudget $
--             newCreateBudget
--
--         , requestCreateNotification $
--             newCreateNotification
--
--         , requestCreateSubscriber $
--             newCreateSubscriber
--
--         , requestDescribeBudgets $
--             newDescribeBudgets
--
--         , requestDeleteSubscriber $
--             newDeleteSubscriber
--
--         , requestUpdateSubscriber $
--             newUpdateSubscriber
--
--         , requestDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudget
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
--         [ responseDescribeBudgetActionsForAccount $
--             newDescribeBudgetActionsForAccountResponse
--
--         , responseCreateBudgetAction $
--             newCreateBudgetActionResponse
--
--         , responseExecuteBudgetAction $
--             newExecuteBudgetActionResponse
--
--         , responseDescribeBudgetAction $
--             newDescribeBudgetActionResponse
--
--         , responseUpdateBudgetAction $
--             newUpdateBudgetActionResponse
--
--         , responseDeleteBudgetAction $
--             newDeleteBudgetActionResponse
--
--         , responseDescribeBudgetActionHistories $
--             newDescribeBudgetActionHistoriesResponse
--
--         , responseDescribeSubscribersForNotification $
--             newDescribeSubscribersForNotificationResponse
--
--         , responseUpdateBudget $
--             newUpdateBudgetResponse
--
--         , responseDeleteNotification $
--             newDeleteNotificationResponse
--
--         , responseDeleteBudget $
--             newDeleteBudgetResponse
--
--         , responseUpdateNotification $
--             newUpdateNotificationResponse
--
--         , responseCreateBudget $
--             newCreateBudgetResponse
--
--         , responseCreateNotification $
--             newCreateNotificationResponse
--
--         , responseCreateSubscriber $
--             newCreateSubscriberResponse
--
--         , responseDescribeBudgets $
--             newDescribeBudgetsResponse
--
--         , responseDeleteSubscriber $
--             newDeleteSubscriberResponse
--
--         , responseUpdateSubscriber $
--             newUpdateSubscriberResponse
--
--         , responseDescribeBudgetActionsForBudget $
--             newDescribeBudgetActionsForBudgetResponse
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

requestDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccount -> TestTree
requestDescribeBudgetActionsForAccount =
  req
    "DescribeBudgetActionsForAccount"
    "fixture/DescribeBudgetActionsForAccount.yaml"

requestCreateBudgetAction :: CreateBudgetAction -> TestTree
requestCreateBudgetAction =
  req
    "CreateBudgetAction"
    "fixture/CreateBudgetAction.yaml"

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

requestDescribeBudgetActionHistories :: DescribeBudgetActionHistories -> TestTree
requestDescribeBudgetActionHistories =
  req
    "DescribeBudgetActionHistories"
    "fixture/DescribeBudgetActionHistories.yaml"

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

requestDeleteNotification :: DeleteNotification -> TestTree
requestDeleteNotification =
  req
    "DeleteNotification"
    "fixture/DeleteNotification.yaml"

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

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber =
  req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestDescribeBudgets :: DescribeBudgets -> TestTree
requestDescribeBudgets =
  req
    "DescribeBudgets"
    "fixture/DescribeBudgets.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber =
  req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber =
  req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

requestDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudget -> TestTree
requestDescribeBudgetActionsForBudget =
  req
    "DescribeBudgetActionsForBudget"
    "fixture/DescribeBudgetActionsForBudget.yaml"

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

responseDescribeBudgetActionsForAccount :: DescribeBudgetActionsForAccountResponse -> TestTree
responseDescribeBudgetActionsForAccount =
  res
    "DescribeBudgetActionsForAccountResponse"
    "fixture/DescribeBudgetActionsForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionsForAccount)

responseCreateBudgetAction :: CreateBudgetActionResponse -> TestTree
responseCreateBudgetAction =
  res
    "CreateBudgetActionResponse"
    "fixture/CreateBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBudgetAction)

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

responseUpdateBudgetAction :: UpdateBudgetActionResponse -> TestTree
responseUpdateBudgetAction =
  res
    "UpdateBudgetActionResponse"
    "fixture/UpdateBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBudgetAction)

responseDeleteBudgetAction :: DeleteBudgetActionResponse -> TestTree
responseDeleteBudgetAction =
  res
    "DeleteBudgetActionResponse"
    "fixture/DeleteBudgetActionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBudgetAction)

responseDescribeBudgetActionHistories :: DescribeBudgetActionHistoriesResponse -> TestTree
responseDescribeBudgetActionHistories =
  res
    "DescribeBudgetActionHistoriesResponse"
    "fixture/DescribeBudgetActionHistoriesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionHistories)

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

responseDeleteNotification :: DeleteNotificationResponse -> TestTree
responseDeleteNotification =
  res
    "DeleteNotificationResponse"
    "fixture/DeleteNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteNotification)

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

responseCreateBudget :: CreateBudgetResponse -> TestTree
responseCreateBudget =
  res
    "CreateBudgetResponse"
    "fixture/CreateBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBudget)

responseCreateNotification :: CreateNotificationResponse -> TestTree
responseCreateNotification =
  res
    "CreateNotificationResponse"
    "fixture/CreateNotificationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNotification)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber =
  res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscriber)

responseDescribeBudgets :: DescribeBudgetsResponse -> TestTree
responseDescribeBudgets =
  res
    "DescribeBudgetsResponse"
    "fixture/DescribeBudgetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgets)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber =
  res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubscriber)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber =
  res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSubscriber)

responseDescribeBudgetActionsForBudget :: DescribeBudgetActionsForBudgetResponse -> TestTree
responseDescribeBudgetActionsForBudget =
  res
    "DescribeBudgetActionsForBudgetResponse"
    "fixture/DescribeBudgetActionsForBudgetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBudgetActionsForBudget)

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
