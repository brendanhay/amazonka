{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import Network.AWS.Pinpoint.Types.MessageResult
import Network.AWS.Prelude

-- | Provides information about the results of a request to send a message to an endpoint address.
--
--
--
-- /See:/ 'messageResponse' smart constructor.
data MessageResponse = MessageResponse'
  { _mRequestId ::
      !(Maybe Text),
    _mResult :: !(Maybe (Map Text (MessageResult))),
    _mEndpointResult ::
      !(Maybe (Map Text (EndpointMessageResult))),
    _mApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mRequestId' - The identifier for the original request that the message was delivered for.
--
-- * 'mResult' - A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
--
-- * 'mEndpointResult' - A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
--
-- * 'mApplicationId' - The unique identifier for the application that was used to send the message.
messageResponse ::
  -- | 'mApplicationId'
  Text ->
  MessageResponse
messageResponse pApplicationId_ =
  MessageResponse'
    { _mRequestId = Nothing,
      _mResult = Nothing,
      _mEndpointResult = Nothing,
      _mApplicationId = pApplicationId_
    }

-- | The identifier for the original request that the message was delivered for.
mRequestId :: Lens' MessageResponse (Maybe Text)
mRequestId = lens _mRequestId (\s a -> s {_mRequestId = a})

-- | A map that contains a multipart response for each address (email address, phone number, or push notification token) that the message was sent to. In the map, the address is the key and the result is the value.
mResult :: Lens' MessageResponse (HashMap Text (MessageResult))
mResult = lens _mResult (\s a -> s {_mResult = a}) . _Default . _Map

-- | A map that contains a multipart response for each address that the message was sent to. In the map, the endpoint ID is the key and the result is the value.
mEndpointResult :: Lens' MessageResponse (HashMap Text (EndpointMessageResult))
mEndpointResult = lens _mEndpointResult (\s a -> s {_mEndpointResult = a}) . _Default . _Map

-- | The unique identifier for the application that was used to send the message.
mApplicationId :: Lens' MessageResponse Text
mApplicationId = lens _mApplicationId (\s a -> s {_mApplicationId = a})

instance FromJSON MessageResponse where
  parseJSON =
    withObject
      "MessageResponse"
      ( \x ->
          MessageResponse'
            <$> (x .:? "RequestId")
            <*> (x .:? "Result" .!= mempty)
            <*> (x .:? "EndpointResult" .!= mempty)
            <*> (x .: "ApplicationId")
      )

instance Hashable MessageResponse

instance NFData MessageResponse
