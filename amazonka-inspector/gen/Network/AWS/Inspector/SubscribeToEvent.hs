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
-- Module      : Network.AWS.Inspector.SubscribeToEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the process of sending Amazon Simple Notification Service (SNS) notifications about a specified event to a specified SNS topic.
--
--
module Network.AWS.Inspector.SubscribeToEvent
    (
    -- * Creating a Request
      subscribeToEvent
    , SubscribeToEvent
    -- * Request Lenses
    , steResourceARN
    , steEvent
    , steTopicARN

    -- * Destructuring the Response
    , subscribeToEventResponse
    , SubscribeToEventResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'subscribeToEvent' smart constructor.
data SubscribeToEvent = SubscribeToEvent'
  { _steResourceARN :: !Text
  , _steEvent       :: !InspectorEvent
  , _steTopicARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'steResourceARN' - The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
--
-- * 'steEvent' - The event for which you want to receive SNS notifications.
--
-- * 'steTopicARN' - The ARN of the SNS topic to which the SNS notifications are sent.
subscribeToEvent
    :: Text -- ^ 'steResourceARN'
    -> InspectorEvent -- ^ 'steEvent'
    -> Text -- ^ 'steTopicARN'
    -> SubscribeToEvent
subscribeToEvent pResourceARN_ pEvent_ pTopicARN_ =
  SubscribeToEvent'
    { _steResourceARN = pResourceARN_
    , _steEvent = pEvent_
    , _steTopicARN = pTopicARN_
    }


-- | The ARN of the assessment template that is used during the event for which you want to receive SNS notifications.
steResourceARN :: Lens' SubscribeToEvent Text
steResourceARN = lens _steResourceARN (\ s a -> s{_steResourceARN = a})

-- | The event for which you want to receive SNS notifications.
steEvent :: Lens' SubscribeToEvent InspectorEvent
steEvent = lens _steEvent (\ s a -> s{_steEvent = a})

-- | The ARN of the SNS topic to which the SNS notifications are sent.
steTopicARN :: Lens' SubscribeToEvent Text
steTopicARN = lens _steTopicARN (\ s a -> s{_steTopicARN = a})

instance AWSRequest SubscribeToEvent where
        type Rs SubscribeToEvent = SubscribeToEventResponse
        request = postJSON inspector
        response = receiveNull SubscribeToEventResponse'

instance Hashable SubscribeToEvent where

instance NFData SubscribeToEvent where

instance ToHeaders SubscribeToEvent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.SubscribeToEvent" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SubscribeToEvent where
        toJSON SubscribeToEvent'{..}
          = object
              (catMaybes
                 [Just ("resourceArn" .= _steResourceARN),
                  Just ("event" .= _steEvent),
                  Just ("topicArn" .= _steTopicARN)])

instance ToPath SubscribeToEvent where
        toPath = const "/"

instance ToQuery SubscribeToEvent where
        toQuery = const mempty

-- | /See:/ 'subscribeToEventResponse' smart constructor.
data SubscribeToEventResponse =
  SubscribeToEventResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubscribeToEventResponse' with the minimum fields required to make a request.
--
subscribeToEventResponse
    :: SubscribeToEventResponse
subscribeToEventResponse = SubscribeToEventResponse'


instance NFData SubscribeToEventResponse where
