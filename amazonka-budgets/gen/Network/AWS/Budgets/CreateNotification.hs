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
-- Module      : Network.AWS.Budgets.CreateNotification
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a new Notification with subscribers for a budget
module Network.AWS.Budgets.CreateNotification
    (
    -- * Creating a Request
      createNotification
    , CreateNotification
    -- * Request Lenses
    , cnAccountId
    , cnBudgetName
    , cnNotification
    , cnSubscribers

    -- * Destructuring the Response
    , createNotificationResponse
    , CreateNotificationResponse
    -- * Response Lenses
    , cnrsResponseStatus
    ) where

import Network.AWS.Budgets.Types
import Network.AWS.Budgets.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request of CreateNotification
--
-- /See:/ 'createNotification' smart constructor.
data CreateNotification = CreateNotification'
  { _cnAccountId    :: !Text
  , _cnBudgetName   :: !Text
  , _cnNotification :: !Notification
  , _cnSubscribers  :: !(List1 Subscriber)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnAccountId' - Undocumented member.
--
-- * 'cnBudgetName' - Undocumented member.
--
-- * 'cnNotification' - Undocumented member.
--
-- * 'cnSubscribers' - Undocumented member.
createNotification
    :: Text -- ^ 'cnAccountId'
    -> Text -- ^ 'cnBudgetName'
    -> Notification -- ^ 'cnNotification'
    -> NonEmpty Subscriber -- ^ 'cnSubscribers'
    -> CreateNotification
createNotification pAccountId_ pBudgetName_ pNotification_ pSubscribers_ =
  CreateNotification'
  { _cnAccountId = pAccountId_
  , _cnBudgetName = pBudgetName_
  , _cnNotification = pNotification_
  , _cnSubscribers = _List1 # pSubscribers_
  }


-- | Undocumented member.
cnAccountId :: Lens' CreateNotification Text
cnAccountId = lens _cnAccountId (\ s a -> s{_cnAccountId = a});

-- | Undocumented member.
cnBudgetName :: Lens' CreateNotification Text
cnBudgetName = lens _cnBudgetName (\ s a -> s{_cnBudgetName = a});

-- | Undocumented member.
cnNotification :: Lens' CreateNotification Notification
cnNotification = lens _cnNotification (\ s a -> s{_cnNotification = a});

-- | Undocumented member.
cnSubscribers :: Lens' CreateNotification (NonEmpty Subscriber)
cnSubscribers = lens _cnSubscribers (\ s a -> s{_cnSubscribers = a}) . _List1;

instance AWSRequest CreateNotification where
        type Rs CreateNotification =
             CreateNotificationResponse
        request = postJSON budgets
        response
          = receiveEmpty
              (\ s h x ->
                 CreateNotificationResponse' <$> (pure (fromEnum s)))

instance Hashable CreateNotification where

instance NFData CreateNotification where

instance ToHeaders CreateNotification where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSBudgetServiceGateway.CreateNotification" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNotification where
        toJSON CreateNotification'{..}
          = object
              (catMaybes
                 [Just ("AccountId" .= _cnAccountId),
                  Just ("BudgetName" .= _cnBudgetName),
                  Just ("Notification" .= _cnNotification),
                  Just ("Subscribers" .= _cnSubscribers)])

instance ToPath CreateNotification where
        toPath = const "/"

instance ToQuery CreateNotification where
        toQuery = const mempty

-- | Response of CreateNotification
--
-- /See:/ 'createNotificationResponse' smart constructor.
newtype CreateNotificationResponse = CreateNotificationResponse'
  { _cnrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnrsResponseStatus' - -- | The response status code.
createNotificationResponse
    :: Int -- ^ 'cnrsResponseStatus'
    -> CreateNotificationResponse
createNotificationResponse pResponseStatus_ =
  CreateNotificationResponse' {_cnrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cnrsResponseStatus :: Lens' CreateNotificationResponse Int
cnrsResponseStatus = lens _cnrsResponseStatus (\ s a -> s{_cnrsResponseStatus = a});

instance NFData CreateNotificationResponse where
