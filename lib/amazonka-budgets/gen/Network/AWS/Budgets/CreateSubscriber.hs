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
-- Module      : Network.AWS.Budgets.CreateSubscriber
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscriber. You must create the associated budget and notification before you create the subscriber.
--
--
module Network.AWS.Budgets.CreateSubscriber
    (
    -- * Creating a Request
      createSubscriber
    , CreateSubscriber
    -- * Request Lenses
    , csAccountId
    , csBudgetName
    , csNotification
    , csSubscriber

    -- * Destructuring the Response
    , createSubscriberResponse
    , CreateSubscriberResponse
    -- * Response Lenses
    , csrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of CreateSubscriber
--
--
--
-- /See:/ 'createSubscriber' smart constructor.
data CreateSubscriber = CreateSubscriber'
  { _csAccountId    :: !Text
  , _csBudgetName   :: !Text
  , _csNotification :: !Notification
  , _csSubscriber   :: !Subscriber
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriber' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csAccountId' - The @accountId@ associated with the budget that you want to create a subscriber for.
--
-- * 'csBudgetName' - The name of the budget that you want to subscribe to. Budget names must be unique within an account.
--
-- * 'csNotification' - The notification that you want to create a subscriber for.
--
-- * 'csSubscriber' - The subscriber that you want to associate with a budget notification.
createSubscriber
    :: Text -- ^ 'csAccountId'
    -> Text -- ^ 'csBudgetName'
    -> Notification -- ^ 'csNotification'
    -> Subscriber -- ^ 'csSubscriber'
    -> CreateSubscriber
createSubscriber pAccountId_ pBudgetName_ pNotification_ pSubscriber_ =
  CreateSubscriber'
    { _csAccountId = pAccountId_
    , _csBudgetName = pBudgetName_
    , _csNotification = pNotification_
    , _csSubscriber = pSubscriber_
    }


-- | The @accountId@ associated with the budget that you want to create a subscriber for.
csAccountId :: Lens' CreateSubscriber Text
csAccountId = lens _csAccountId (\ s a -> s{_csAccountId = a})

-- | The name of the budget that you want to subscribe to. Budget names must be unique within an account.
csBudgetName :: Lens' CreateSubscriber Text
csBudgetName = lens _csBudgetName (\ s a -> s{_csBudgetName = a})

-- | The notification that you want to create a subscriber for.
csNotification :: Lens' CreateSubscriber Notification
csNotification = lens _csNotification (\ s a -> s{_csNotification = a})

-- | The subscriber that you want to associate with a budget notification.
csSubscriber :: Lens' CreateSubscriber Subscriber
csSubscriber = lens _csSubscriber (\ s a -> s{_csSubscriber = a})

instance AWSRequest CreateSubscriber where
        type Rs CreateSubscriber = CreateSubscriberResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 CreateSubscriberResponse' <$> (pure (fromEnum s)))

instance Hashable CreateSubscriber where

instance NFData CreateSubscriber where

instance ToHeaders CreateSubscriber where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.CreateSubscriber" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateSubscriber where
        toJSON CreateSubscriber'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _csAccountId),
                  Just ("BudgetName" .= _csBudgetName),
                  Just ("Notification" .= _csNotification),
                  Just ("Subscriber" .= _csSubscriber)])

instance ToPath CreateSubscriber where
        toPath = const "/"

instance ToQuery CreateSubscriber where
        toQuery = const mempty

-- | Response of CreateSubscriber
--
--
--
-- /See:/ 'createSubscriberResponse' smart constructor.
newtype CreateSubscriberResponse = CreateSubscriberResponse'
  { _csrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateSubscriberResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsResponseStatus' - -- | The response status code.
createSubscriberResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateSubscriberResponse
createSubscriberResponse pResponseStatus_ =
  CreateSubscriberResponse' {_csrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateSubscriberResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateSubscriberResponse where
