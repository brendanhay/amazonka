{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.CreateBudget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a budget and, if included, notifications and subscribers.
--
--
module Network.AWS.Budgets.CreateBudget
    (
    -- * Creating a Request
      createBudget
    , CreateBudget
    -- * Request Lenses
    , cbNotificationsWithSubscribers
    , cbAccountId
    , cbBudget

    -- * Destructuring the Response
    , createBudgetResponse
    , CreateBudgetResponse
    -- * Response Lenses
    , cbrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of CreateBudget
--
--
--
-- /See:/ 'createBudget' smart constructor.
data CreateBudget = CreateBudget'
  { _cbNotificationsWithSubscribers :: !(Maybe [NotificationWithSubscribers])
  , _cbAccountId                    :: !Text
  , _cbBudget                       :: !Budget
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBudget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbNotificationsWithSubscribers' - A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to ten email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
--
-- * 'cbAccountId' - The @accountId@ that is associated with the budget.
--
-- * 'cbBudget' - The budget object that you want to create.
createBudget
    :: Text -- ^ 'cbAccountId'
    -> Budget -- ^ 'cbBudget'
    -> CreateBudget
createBudget pAccountId_ pBudget_ =
  CreateBudget'
    { _cbNotificationsWithSubscribers = Nothing
    , _cbAccountId = pAccountId_
    , _cbBudget = pBudget_
    }


-- | A notification that you want to associate with a budget. A budget can have up to five notifications, and each notification can have one SNS subscriber and up to ten email subscribers. If you include notifications and subscribers in your @CreateBudget@ call, AWS creates the notifications and subscribers for you.
cbNotificationsWithSubscribers :: Lens' CreateBudget [NotificationWithSubscribers]
cbNotificationsWithSubscribers = lens _cbNotificationsWithSubscribers (\ s a -> s{_cbNotificationsWithSubscribers = a}) . _Default . _Coerce

-- | The @accountId@ that is associated with the budget.
cbAccountId :: Lens' CreateBudget Text
cbAccountId = lens _cbAccountId (\ s a -> s{_cbAccountId = a})

-- | The budget object that you want to create.
cbBudget :: Lens' CreateBudget Budget
cbBudget = lens _cbBudget (\ s a -> s{_cbBudget = a})

instance AWSRequest CreateBudget where
        type Rs CreateBudget = CreateBudgetResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 CreateBudgetResponse' <$> (pure (fromEnum s)))

instance Hashable CreateBudget where

instance NFData CreateBudget where

instance ToHeaders CreateBudget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.CreateBudget" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateBudget where
        toJSON CreateBudget'{..}
          = object
              (catMaybes
                 [("NotificationsWithSubscribers" .=) <$>
                    _cbNotificationsWithSubscribers,
                  Just ("AccountId" .= _cbAccountId),
                  Just ("Budget" .= _cbBudget)])

instance ToPath CreateBudget where
        toPath = const "/"

instance ToQuery CreateBudget where
        toQuery = const mempty

-- | Response of CreateBudget
--
--
--
-- /See:/ 'createBudgetResponse' smart constructor.
newtype CreateBudgetResponse = CreateBudgetResponse'
  { _cbrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateBudgetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbrsResponseStatus' - -- | The response status code.
createBudgetResponse
    :: Int -- ^ 'cbrsResponseStatus'
    -> CreateBudgetResponse
createBudgetResponse pResponseStatus_ =
  CreateBudgetResponse' {_cbrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cbrsResponseStatus :: Lens' CreateBudgetResponse Int
cbrsResponseStatus = lens _cbrsResponseStatus (\ s a -> s{_cbrsResponseStatus = a})

instance NFData CreateBudgetResponse where
