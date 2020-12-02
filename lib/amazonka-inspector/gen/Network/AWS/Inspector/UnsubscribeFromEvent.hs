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
-- Module      : Network.AWS.Inspector.UnsubscribeFromEvent
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the process of sending Amazon Simple Notification Service (SNS) notifications about a specified event to a specified SNS topic.
--
--
module Network.AWS.Inspector.UnsubscribeFromEvent
    (
    -- * Creating a Request
      unsubscribeFromEvent
    , UnsubscribeFromEvent
    -- * Request Lenses
    , ufeResourceARN
    , ufeEvent
    , ufeTopicARN

    -- * Destructuring the Response
    , unsubscribeFromEventResponse
    , UnsubscribeFromEventResponse
    ) where

import Network.AWS.Inspector.Types
import Network.AWS.Inspector.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'unsubscribeFromEvent' smart constructor.
data UnsubscribeFromEvent = UnsubscribeFromEvent'
  { _ufeResourceARN :: !Text
  , _ufeEvent       :: !InspectorEvent
  , _ufeTopicARN    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsubscribeFromEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufeResourceARN' - The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
--
-- * 'ufeEvent' - The event for which you want to stop receiving SNS notifications.
--
-- * 'ufeTopicARN' - The ARN of the SNS topic to which SNS notifications are sent.
unsubscribeFromEvent
    :: Text -- ^ 'ufeResourceARN'
    -> InspectorEvent -- ^ 'ufeEvent'
    -> Text -- ^ 'ufeTopicARN'
    -> UnsubscribeFromEvent
unsubscribeFromEvent pResourceARN_ pEvent_ pTopicARN_ =
  UnsubscribeFromEvent'
    { _ufeResourceARN = pResourceARN_
    , _ufeEvent = pEvent_
    , _ufeTopicARN = pTopicARN_
    }


-- | The ARN of the assessment template that is used during the event for which you want to stop receiving SNS notifications.
ufeResourceARN :: Lens' UnsubscribeFromEvent Text
ufeResourceARN = lens _ufeResourceARN (\ s a -> s{_ufeResourceARN = a})

-- | The event for which you want to stop receiving SNS notifications.
ufeEvent :: Lens' UnsubscribeFromEvent InspectorEvent
ufeEvent = lens _ufeEvent (\ s a -> s{_ufeEvent = a})

-- | The ARN of the SNS topic to which SNS notifications are sent.
ufeTopicARN :: Lens' UnsubscribeFromEvent Text
ufeTopicARN = lens _ufeTopicARN (\ s a -> s{_ufeTopicARN = a})

instance AWSRequest UnsubscribeFromEvent where
        type Rs UnsubscribeFromEvent =
             UnsubscribeFromEventResponse
        request = postJSON inspector
        response = receiveNull UnsubscribeFromEventResponse'

instance Hashable UnsubscribeFromEvent where

instance NFData UnsubscribeFromEvent where

instance ToHeaders UnsubscribeFromEvent where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.UnsubscribeFromEvent" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UnsubscribeFromEvent where
        toJSON UnsubscribeFromEvent'{..}
          = object
              (catMaybes
                 [Just ("resourceArn" .= _ufeResourceARN),
                  Just ("event" .= _ufeEvent),
                  Just ("topicArn" .= _ufeTopicARN)])

instance ToPath UnsubscribeFromEvent where
        toPath = const "/"

instance ToQuery UnsubscribeFromEvent where
        toQuery = const mempty

-- | /See:/ 'unsubscribeFromEventResponse' smart constructor.
data UnsubscribeFromEventResponse =
  UnsubscribeFromEventResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UnsubscribeFromEventResponse' with the minimum fields required to make a request.
--
unsubscribeFromEventResponse
    :: UnsubscribeFromEventResponse
unsubscribeFromEventResponse = UnsubscribeFromEventResponse'


instance NFData UnsubscribeFromEventResponse where
