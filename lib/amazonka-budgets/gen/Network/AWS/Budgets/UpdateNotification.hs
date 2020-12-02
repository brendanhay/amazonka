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
-- Module      : Network.AWS.Budgets.UpdateNotification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notification.
--
--
module Network.AWS.Budgets.UpdateNotification
    (
    -- * Creating a Request
      updateNotification
    , UpdateNotification
    -- * Request Lenses
    , unAccountId
    , unBudgetName
    , unOldNotification
    , unNewNotification

    -- * Destructuring the Response
    , updateNotificationResponse
    , UpdateNotificationResponse
    -- * Response Lenses
    , unrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of UpdateNotification
--
--
--
-- /See:/ 'updateNotification' smart constructor.
data UpdateNotification = UpdateNotification'
  { _unAccountId       :: !Text
  , _unBudgetName      :: !Text
  , _unOldNotification :: !Notification
  , _unNewNotification :: !Notification
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unAccountId' - The @accountId@ that is associated with the budget whose notification you want to update.
--
-- * 'unBudgetName' - The name of the budget whose notification you want to update.
--
-- * 'unOldNotification' - The previous notification associated with a budget.
--
-- * 'unNewNotification' - The updated notification to be associated with a budget.
updateNotification
    :: Text -- ^ 'unAccountId'
    -> Text -- ^ 'unBudgetName'
    -> Notification -- ^ 'unOldNotification'
    -> Notification -- ^ 'unNewNotification'
    -> UpdateNotification
updateNotification pAccountId_ pBudgetName_ pOldNotification_ pNewNotification_ =
  UpdateNotification'
    { _unAccountId = pAccountId_
    , _unBudgetName = pBudgetName_
    , _unOldNotification = pOldNotification_
    , _unNewNotification = pNewNotification_
    }


-- | The @accountId@ that is associated with the budget whose notification you want to update.
unAccountId :: Lens' UpdateNotification Text
unAccountId = lens _unAccountId (\ s a -> s{_unAccountId = a})

-- | The name of the budget whose notification you want to update.
unBudgetName :: Lens' UpdateNotification Text
unBudgetName = lens _unBudgetName (\ s a -> s{_unBudgetName = a})

-- | The previous notification associated with a budget.
unOldNotification :: Lens' UpdateNotification Notification
unOldNotification = lens _unOldNotification (\ s a -> s{_unOldNotification = a})

-- | The updated notification to be associated with a budget.
unNewNotification :: Lens' UpdateNotification Notification
unNewNotification = lens _unNewNotification (\ s a -> s{_unNewNotification = a})

instance AWSRequest UpdateNotification where
        type Rs UpdateNotification =
             UpdateNotificationResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateNotificationResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateNotification where

instance NFData UpdateNotification where

instance ToHeaders UpdateNotification where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.UpdateNotification" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateNotification where
        toJSON UpdateNotification'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _unAccountId),
                  Just ("BudgetName" .= _unBudgetName),
                  Just ("OldNotification" .= _unOldNotification),
                  Just ("NewNotification" .= _unNewNotification)])

instance ToPath UpdateNotification where
        toPath = const "/"

instance ToQuery UpdateNotification where
        toQuery = const mempty

-- | Response of UpdateNotification
--
--
--
-- /See:/ 'updateNotificationResponse' smart constructor.
newtype UpdateNotificationResponse = UpdateNotificationResponse'
  { _unrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unrsResponseStatus' - -- | The response status code.
updateNotificationResponse
    :: Int -- ^ 'unrsResponseStatus'
    -> UpdateNotificationResponse
updateNotificationResponse pResponseStatus_ =
  UpdateNotificationResponse' {_unrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
unrsResponseStatus :: Lens' UpdateNotificationResponse Int
unrsResponseStatus = lens _unrsResponseStatus (\ s a -> s{_unrsResponseStatus = a})

instance NFData UpdateNotificationResponse where
