{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SendUsersMessageResponse where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.EndpointMessageResult
import Network.AWS.Prelude

-- | Provides information about which users and endpoints a message was sent to.
--
--
--
-- /See:/ 'sendUsersMessageResponse' smart constructor.
data SendUsersMessageResponse = SendUsersMessageResponse'
  { _sumRequestId ::
      !(Maybe Text),
    _sumResult ::
      !( Maybe
           ( Map
               Text
               ( Map
                   Text
                   (EndpointMessageResult)
               )
           )
       ),
    _sumApplicationId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendUsersMessageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumRequestId' - The unique identifier that was assigned to the message request.
--
-- * 'sumResult' - An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
--
-- * 'sumApplicationId' - The unique identifier for the application that was used to send the message.
sendUsersMessageResponse ::
  -- | 'sumApplicationId'
  Text ->
  SendUsersMessageResponse
sendUsersMessageResponse pApplicationId_ =
  SendUsersMessageResponse'
    { _sumRequestId = Nothing,
      _sumResult = Nothing,
      _sumApplicationId = pApplicationId_
    }

-- | The unique identifier that was assigned to the message request.
sumRequestId :: Lens' SendUsersMessageResponse (Maybe Text)
sumRequestId = lens _sumRequestId (\s a -> s {_sumRequestId = a})

-- | An object that indicates which endpoints the message was sent to, for each user. The object lists user IDs and, for each user ID, provides the endpoint IDs that the message was sent to. For each endpoint ID, it provides an EndpointMessageResult object.
sumResult :: Lens' SendUsersMessageResponse (HashMap Text (HashMap Text (EndpointMessageResult)))
sumResult = lens _sumResult (\s a -> s {_sumResult = a}) . _Default . _Map

-- | The unique identifier for the application that was used to send the message.
sumApplicationId :: Lens' SendUsersMessageResponse Text
sumApplicationId = lens _sumApplicationId (\s a -> s {_sumApplicationId = a})

instance FromJSON SendUsersMessageResponse where
  parseJSON =
    withObject
      "SendUsersMessageResponse"
      ( \x ->
          SendUsersMessageResponse'
            <$> (x .:? "RequestId")
            <*> (x .:? "Result" .!= mempty)
            <*> (x .: "ApplicationId")
      )

instance Hashable SendUsersMessageResponse

instance NFData SendUsersMessageResponse
