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
-- Module      : Network.AWS.CloudWatchEvents.CreateEventBus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new event bus within your account. This can be a custom event bus which you can use to receive events from your own custom applications and services, or it can be a partner event bus which can be matched to a partner event source.
--
--
module Network.AWS.CloudWatchEvents.CreateEventBus
    (
    -- * Creating a Request
      createEventBus
    , CreateEventBus
    -- * Request Lenses
    , cebEventSourceName
    , cebName

    -- * Destructuring the Response
    , createEventBusResponse
    , CreateEventBusResponse
    -- * Response Lenses
    , cebrsEventBusARN
    , cebrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEventBus' smart constructor.
data CreateEventBus = CreateEventBus'
  { _cebEventSourceName :: !(Maybe Text)
  , _cebName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEventBus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cebEventSourceName' - If you're creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
--
-- * 'cebName' - The name of the new event bus.  The names of custom event buses can't contain the @/@ character. You can't use the name @default@ for a custom event bus because this name is already used for your account's default event bus. If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to. This name will include the @/@ character.
createEventBus
    :: Text -- ^ 'cebName'
    -> CreateEventBus
createEventBus pName_ =
  CreateEventBus' {_cebEventSourceName = Nothing, _cebName = pName_}


-- | If you're creating a partner event bus, this specifies the partner event source that the new event bus will be matched with.
cebEventSourceName :: Lens' CreateEventBus (Maybe Text)
cebEventSourceName = lens _cebEventSourceName (\ s a -> s{_cebEventSourceName = a})

-- | The name of the new event bus.  The names of custom event buses can't contain the @/@ character. You can't use the name @default@ for a custom event bus because this name is already used for your account's default event bus. If this is a partner event bus, the name must exactly match the name of the partner event source that this event bus is matched to. This name will include the @/@ character.
cebName :: Lens' CreateEventBus Text
cebName = lens _cebName (\ s a -> s{_cebName = a})

instance AWSRequest CreateEventBus where
        type Rs CreateEventBus = CreateEventBusResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 CreateEventBusResponse' <$>
                   (x .?> "EventBusArn") <*> (pure (fromEnum s)))

instance Hashable CreateEventBus where

instance NFData CreateEventBus where

instance ToHeaders CreateEventBus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.CreateEventBus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEventBus where
        toJSON CreateEventBus'{..}
          = object
              (catMaybes
                 [("EventSourceName" .=) <$> _cebEventSourceName,
                  Just ("Name" .= _cebName)])

instance ToPath CreateEventBus where
        toPath = const "/"

instance ToQuery CreateEventBus where
        toQuery = const mempty

-- | /See:/ 'createEventBusResponse' smart constructor.
data CreateEventBusResponse = CreateEventBusResponse'
  { _cebrsEventBusARN    :: !(Maybe Text)
  , _cebrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEventBusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cebrsEventBusARN' - The ARN of the new event bus.
--
-- * 'cebrsResponseStatus' - -- | The response status code.
createEventBusResponse
    :: Int -- ^ 'cebrsResponseStatus'
    -> CreateEventBusResponse
createEventBusResponse pResponseStatus_ =
  CreateEventBusResponse'
    {_cebrsEventBusARN = Nothing, _cebrsResponseStatus = pResponseStatus_}


-- | The ARN of the new event bus.
cebrsEventBusARN :: Lens' CreateEventBusResponse (Maybe Text)
cebrsEventBusARN = lens _cebrsEventBusARN (\ s a -> s{_cebrsEventBusARN = a})

-- | -- | The response status code.
cebrsResponseStatus :: Lens' CreateEventBusResponse Int
cebrsResponseStatus = lens _cebrsResponseStatus (\ s a -> s{_cebrsResponseStatus = a})

instance NFData CreateEventBusResponse where
