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
-- Module      : Network.AWS.Budgets.UpdateSubscriber
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscriber.
--
--
module Network.AWS.Budgets.UpdateSubscriber
    (
    -- * Creating a Request
      updateSubscriber
    , UpdateSubscriber
    -- * Request Lenses
    , usAccountId
    , usBudgetName
    , usNotification
    , usOldSubscriber
    , usNewSubscriber

    -- * Destructuring the Response
    , updateSubscriberResponse
    , UpdateSubscriberResponse
    -- * Response Lenses
    , usrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of UpdateSubscriber
--
--
--
-- /See:/ 'updateSubscriber' smart constructor.
data UpdateSubscriber = UpdateSubscriber'
  { _usAccountId     :: !Text
  , _usBudgetName    :: !Text
  , _usNotification  :: !Notification
  , _usOldSubscriber :: !Subscriber
  , _usNewSubscriber :: !Subscriber
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usAccountId' - The @accountId@ that is associated with the budget whose subscriber you want to update.
--
-- * 'usBudgetName' - The name of the budget whose subscriber you want to update.
--
-- * 'usNotification' - The notification whose subscriber you want to update.
--
-- * 'usOldSubscriber' - The previous subscriber associated with a budget notification.
--
-- * 'usNewSubscriber' - The updated subscriber associated with a budget notification.
updateSubscriber
    :: Text -- ^ 'usAccountId'
    -> Text -- ^ 'usBudgetName'
    -> Notification -- ^ 'usNotification'
    -> Subscriber -- ^ 'usOldSubscriber'
    -> Subscriber -- ^ 'usNewSubscriber'
    -> UpdateSubscriber
updateSubscriber pAccountId_ pBudgetName_ pNotification_ pOldSubscriber_ pNewSubscriber_ =
  UpdateSubscriber'
    { _usAccountId = pAccountId_
    , _usBudgetName = pBudgetName_
    , _usNotification = pNotification_
    , _usOldSubscriber = pOldSubscriber_
    , _usNewSubscriber = pNewSubscriber_
    }


-- | The @accountId@ that is associated with the budget whose subscriber you want to update.
usAccountId :: Lens' UpdateSubscriber Text
usAccountId = lens _usAccountId (\ s a -> s{_usAccountId = a})

-- | The name of the budget whose subscriber you want to update.
usBudgetName :: Lens' UpdateSubscriber Text
usBudgetName = lens _usBudgetName (\ s a -> s{_usBudgetName = a})

-- | The notification whose subscriber you want to update.
usNotification :: Lens' UpdateSubscriber Notification
usNotification = lens _usNotification (\ s a -> s{_usNotification = a})

-- | The previous subscriber associated with a budget notification.
usOldSubscriber :: Lens' UpdateSubscriber Subscriber
usOldSubscriber = lens _usOldSubscriber (\ s a -> s{_usOldSubscriber = a})

-- | The updated subscriber associated with a budget notification.
usNewSubscriber :: Lens' UpdateSubscriber Subscriber
usNewSubscriber = lens _usNewSubscriber (\ s a -> s{_usNewSubscriber = a})

instance AWSRequest UpdateSubscriber where
        type Rs UpdateSubscriber = UpdateSubscriberResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateSubscriberResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateSubscriber where

instance NFData UpdateSubscriber where

instance ToHeaders UpdateSubscriber where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.UpdateSubscriber" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSubscriber where
        toJSON UpdateSubscriber'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _usAccountId),
                  Just ("BudgetName" .= _usBudgetName),
                  Just ("Notification" .= _usNotification),
                  Just ("OldSubscriber" .= _usOldSubscriber),
                  Just ("NewSubscriber" .= _usNewSubscriber)])

instance ToPath UpdateSubscriber where
        toPath = const "/"

instance ToQuery UpdateSubscriber where
        toQuery = const mempty

-- | Response of UpdateSubscriber
--
--
--
-- /See:/ 'updateSubscriberResponse' smart constructor.
newtype UpdateSubscriberResponse = UpdateSubscriberResponse'
  { _usrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubscriberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateSubscriberResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateSubscriberResponse
updateSubscriberResponse pResponseStatus_ =
  UpdateSubscriberResponse' {_usrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSubscriberResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateSubscriberResponse where
