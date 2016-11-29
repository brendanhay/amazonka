{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AWSBudgets
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AWSBudgets where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AWSBudgets
import Test.AWS.AWSBudgets.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeSubscribersForNotification $
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
--         , requestDescribeBudget $
--             describeBudget
--
--         , requestUpdateSubscriber $
--             updateSubscriber
--
--         , requestDeleteSubscriber $
--             deleteSubscriber
--
--         , requestCreateBudget $
--             createBudget
--
--         , requestCreateNotification $
--             createNotification
--
--           ]

--     , testGroup "response"
--         [ responseDescribeSubscribersForNotification $
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
--         , responseDescribeBudget $
--             describeBudgetResponse
--
--         , responseUpdateSubscriber $
--             updateSubscriberResponse
--
--         , responseDeleteSubscriber $
--             deleteSubscriberResponse
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

requestDescribeSubscribersForNotification :: DescribeSubscribersForNotification -> TestTree
requestDescribeSubscribersForNotification = req
    "DescribeSubscribersForNotification"
    "fixture/DescribeSubscribersForNotification.yaml"

requestDescribeNotificationsForBudget :: DescribeNotificationsForBudget -> TestTree
requestDescribeNotificationsForBudget = req
    "DescribeNotificationsForBudget"
    "fixture/DescribeNotificationsForBudget.yaml"

requestDescribeBudgets :: DescribeBudgets -> TestTree
requestDescribeBudgets = req
    "DescribeBudgets"
    "fixture/DescribeBudgets.yaml"

requestCreateSubscriber :: CreateSubscriber -> TestTree
requestCreateSubscriber = req
    "CreateSubscriber"
    "fixture/CreateSubscriber.yaml"

requestUpdateBudget :: UpdateBudget -> TestTree
requestUpdateBudget = req
    "UpdateBudget"
    "fixture/UpdateBudget.yaml"

requestDeleteBudget :: DeleteBudget -> TestTree
requestDeleteBudget = req
    "DeleteBudget"
    "fixture/DeleteBudget.yaml"

requestDeleteNotification :: DeleteNotification -> TestTree
requestDeleteNotification = req
    "DeleteNotification"
    "fixture/DeleteNotification.yaml"

requestUpdateNotification :: UpdateNotification -> TestTree
requestUpdateNotification = req
    "UpdateNotification"
    "fixture/UpdateNotification.yaml"

requestDescribeBudget :: DescribeBudget -> TestTree
requestDescribeBudget = req
    "DescribeBudget"
    "fixture/DescribeBudget.yaml"

requestUpdateSubscriber :: UpdateSubscriber -> TestTree
requestUpdateSubscriber = req
    "UpdateSubscriber"
    "fixture/UpdateSubscriber.yaml"

requestDeleteSubscriber :: DeleteSubscriber -> TestTree
requestDeleteSubscriber = req
    "DeleteSubscriber"
    "fixture/DeleteSubscriber.yaml"

requestCreateBudget :: CreateBudget -> TestTree
requestCreateBudget = req
    "CreateBudget"
    "fixture/CreateBudget.yaml"

requestCreateNotification :: CreateNotification -> TestTree
requestCreateNotification = req
    "CreateNotification"
    "fixture/CreateNotification.yaml"

-- Responses

responseDescribeSubscribersForNotification :: DescribeSubscribersForNotificationResponse -> TestTree
responseDescribeSubscribersForNotification = res
    "DescribeSubscribersForNotificationResponse"
    "fixture/DescribeSubscribersForNotificationResponse.proto"
    awsBudgets
    (Proxy :: Proxy DescribeSubscribersForNotification)

responseDescribeNotificationsForBudget :: DescribeNotificationsForBudgetResponse -> TestTree
responseDescribeNotificationsForBudget = res
    "DescribeNotificationsForBudgetResponse"
    "fixture/DescribeNotificationsForBudgetResponse.proto"
    awsBudgets
    (Proxy :: Proxy DescribeNotificationsForBudget)

responseDescribeBudgets :: DescribeBudgetsResponse -> TestTree
responseDescribeBudgets = res
    "DescribeBudgetsResponse"
    "fixture/DescribeBudgetsResponse.proto"
    awsBudgets
    (Proxy :: Proxy DescribeBudgets)

responseCreateSubscriber :: CreateSubscriberResponse -> TestTree
responseCreateSubscriber = res
    "CreateSubscriberResponse"
    "fixture/CreateSubscriberResponse.proto"
    awsBudgets
    (Proxy :: Proxy CreateSubscriber)

responseUpdateBudget :: UpdateBudgetResponse -> TestTree
responseUpdateBudget = res
    "UpdateBudgetResponse"
    "fixture/UpdateBudgetResponse.proto"
    awsBudgets
    (Proxy :: Proxy UpdateBudget)

responseDeleteBudget :: DeleteBudgetResponse -> TestTree
responseDeleteBudget = res
    "DeleteBudgetResponse"
    "fixture/DeleteBudgetResponse.proto"
    awsBudgets
    (Proxy :: Proxy DeleteBudget)

responseDeleteNotification :: DeleteNotificationResponse -> TestTree
responseDeleteNotification = res
    "DeleteNotificationResponse"
    "fixture/DeleteNotificationResponse.proto"
    awsBudgets
    (Proxy :: Proxy DeleteNotification)

responseUpdateNotification :: UpdateNotificationResponse -> TestTree
responseUpdateNotification = res
    "UpdateNotificationResponse"
    "fixture/UpdateNotificationResponse.proto"
    awsBudgets
    (Proxy :: Proxy UpdateNotification)

responseDescribeBudget :: DescribeBudgetResponse -> TestTree
responseDescribeBudget = res
    "DescribeBudgetResponse"
    "fixture/DescribeBudgetResponse.proto"
    awsBudgets
    (Proxy :: Proxy DescribeBudget)

responseUpdateSubscriber :: UpdateSubscriberResponse -> TestTree
responseUpdateSubscriber = res
    "UpdateSubscriberResponse"
    "fixture/UpdateSubscriberResponse.proto"
    awsBudgets
    (Proxy :: Proxy UpdateSubscriber)

responseDeleteSubscriber :: DeleteSubscriberResponse -> TestTree
responseDeleteSubscriber = res
    "DeleteSubscriberResponse"
    "fixture/DeleteSubscriberResponse.proto"
    awsBudgets
    (Proxy :: Proxy DeleteSubscriber)

responseCreateBudget :: CreateBudgetResponse -> TestTree
responseCreateBudget = res
    "CreateBudgetResponse"
    "fixture/CreateBudgetResponse.proto"
    awsBudgets
    (Proxy :: Proxy CreateBudget)

responseCreateNotification :: CreateNotificationResponse -> TestTree
responseCreateNotification = res
    "CreateNotificationResponse"
    "fixture/CreateNotificationResponse.proto"
    awsBudgets
    (Proxy :: Proxy CreateNotification)
