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
-- Module      : Network.AWS.Budgets.DeleteSubscriber
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscriber.
--
--
-- __Deleting the last subscriber to a notification also deletes the notification.__
--
module Network.AWS.Budgets.DeleteSubscriber
    (
    -- * Creating a Request
      deleteSubscriber
    , DeleteSubscriber
    -- * Request Lenses
    , dsAccountId
    , dsBudgetName
    , dsNotification
    , dsSubscriber

    -- * Destructuring the Response
    , deleteSubscriberResponse
    , DeleteSubscriberResponse
    -- * Response Lenses
    , dsrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of DeleteSubscriber
--
--
--
-- /See:/ 'deleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { _dsAccountId    :: !Text
  , _dsBudgetName   :: !Text
  , _dsNotification :: !Notification
  , _dsSubscriber   :: !Subscriber
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsAccountId' - The @accountId@ that is associated with the budget whose subscriber you want to delete.
--
-- * 'dsBudgetName' - The name of the budget whose subscriber you want to delete.
--
-- * 'dsNotification' - The notification whose subscriber you want to delete.
--
-- * 'dsSubscriber' - The subscriber that you want to delete.
deleteSubscriber
    :: Text -- ^ 'dsAccountId'
    -> Text -- ^ 'dsBudgetName'
    -> Notification -- ^ 'dsNotification'
    -> Subscriber -- ^ 'dsSubscriber'
    -> DeleteSubscriber
deleteSubscriber pAccountId_ pBudgetName_ pNotification_ pSubscriber_ =
  DeleteSubscriber'
    { _dsAccountId = pAccountId_
    , _dsBudgetName = pBudgetName_
    , _dsNotification = pNotification_
    , _dsSubscriber = pSubscriber_
    }


-- | The @accountId@ that is associated with the budget whose subscriber you want to delete.
dsAccountId :: Lens' DeleteSubscriber Text
dsAccountId = lens _dsAccountId (\ s a -> s{_dsAccountId = a})

-- | The name of the budget whose subscriber you want to delete.
dsBudgetName :: Lens' DeleteSubscriber Text
dsBudgetName = lens _dsBudgetName (\ s a -> s{_dsBudgetName = a})

-- | The notification whose subscriber you want to delete.
dsNotification :: Lens' DeleteSubscriber Notification
dsNotification = lens _dsNotification (\ s a -> s{_dsNotification = a})

-- | The subscriber that you want to delete.
dsSubscriber :: Lens' DeleteSubscriber Subscriber
dsSubscriber = lens _dsSubscriber (\ s a -> s{_dsSubscriber = a})

instance AWSRequest DeleteSubscriber where
        type Rs DeleteSubscriber = DeleteSubscriberResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSubscriberResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteSubscriber where

instance NFData DeleteSubscriber where

instance ToHeaders DeleteSubscriber where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.DeleteSubscriber" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSubscriber where
        toJSON DeleteSubscriber'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _dsAccountId),
                  Just ("BudgetName" .= _dsBudgetName),
                  Just ("Notification" .= _dsNotification),
                  Just ("Subscriber" .= _dsSubscriber)])

instance ToPath DeleteSubscriber where
        toPath = const "/"

instance ToQuery DeleteSubscriber where
        toQuery = const mempty

-- | Response of DeleteSubscriber
--
--
--
-- /See:/ 'deleteSubscriberResponse' smart constructor.
newtype DeleteSubscriberResponse = DeleteSubscriberResponse'
  { _dsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSubscriberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteSubscriberResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteSubscriberResponse
deleteSubscriberResponse pResponseStatus_ =
  DeleteSubscriberResponse' {_dsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSubscriberResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteSubscriberResponse where
