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
-- Module      : Network.AWS.MechanicalTurk.SendTestEventNotification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @SendTestEventNotification@ operation causes Amazon Mechanical Turk to send a notification message as if a HIT event occurred, according to the provided notification specification. This allows you to test notifications without setting up notifications for a real HIT type and trying to trigger them using the website. When you call this operation, the service attempts to send the test notification immediately.
--
--
module Network.AWS.MechanicalTurk.SendTestEventNotification
    (
    -- * Creating a Request
      sendTestEventNotification
    , SendTestEventNotification
    -- * Request Lenses
    , stenNotification
    , stenTestEventType

    -- * Destructuring the Response
    , sendTestEventNotificationResponse
    , SendTestEventNotificationResponse
    -- * Response Lenses
    , stenrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'sendTestEventNotification' smart constructor.
data SendTestEventNotification = SendTestEventNotification'
  { _stenNotification  :: !NotificationSpecification
  , _stenTestEventType :: !EventType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendTestEventNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stenNotification' - The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type.
--
-- * 'stenTestEventType' - The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event.
sendTestEventNotification
    :: NotificationSpecification -- ^ 'stenNotification'
    -> EventType -- ^ 'stenTestEventType'
    -> SendTestEventNotification
sendTestEventNotification pNotification_ pTestEventType_ =
  SendTestEventNotification'
    {_stenNotification = pNotification_, _stenTestEventType = pTestEventType_}


-- | The notification specification to test. This value is identical to the value you would provide to the UpdateNotificationSettings operation when you establish the notification specification for a HIT type.
stenNotification :: Lens' SendTestEventNotification NotificationSpecification
stenNotification = lens _stenNotification (\ s a -> s{_stenNotification = a})

-- | The event to simulate to test the notification specification. This event is included in the test message even if the notification specification does not include the event type. The notification specification does not filter out the test event.
stenTestEventType :: Lens' SendTestEventNotification EventType
stenTestEventType = lens _stenTestEventType (\ s a -> s{_stenTestEventType = a})

instance AWSRequest SendTestEventNotification where
        type Rs SendTestEventNotification =
             SendTestEventNotificationResponse
        request = postJSON mechanicalTurk
        response
          = receiveEmpty
              (\ s h x ->
                 SendTestEventNotificationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SendTestEventNotification where

instance NFData SendTestEventNotification where

instance ToHeaders SendTestEventNotification where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.SendTestEventNotification"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendTestEventNotification where
        toJSON SendTestEventNotification'{..}
          = object
              (catMaybes
                 [Just ("Notification" .= _stenNotification),
                  Just ("TestEventType" .= _stenTestEventType)])

instance ToPath SendTestEventNotification where
        toPath = const "/"

instance ToQuery SendTestEventNotification where
        toQuery = const mempty

-- | /See:/ 'sendTestEventNotificationResponse' smart constructor.
newtype SendTestEventNotificationResponse = SendTestEventNotificationResponse'
  { _stenrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendTestEventNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stenrsResponseStatus' - -- | The response status code.
sendTestEventNotificationResponse
    :: Int -- ^ 'stenrsResponseStatus'
    -> SendTestEventNotificationResponse
sendTestEventNotificationResponse pResponseStatus_ =
  SendTestEventNotificationResponse' {_stenrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
stenrsResponseStatus :: Lens' SendTestEventNotificationResponse Int
stenrsResponseStatus = lens _stenrsResponseStatus (\ s a -> s{_stenrsResponseStatus = a})

instance NFData SendTestEventNotificationResponse
         where
