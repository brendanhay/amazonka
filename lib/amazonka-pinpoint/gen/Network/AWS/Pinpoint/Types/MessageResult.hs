{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageResult where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DeliveryStatus
import Network.AWS.Prelude

-- | Provides information about the results of sending a message directly to an endpoint address.
--
--
--
-- /See:/ 'messageResult' smart constructor.
data MessageResult = MessageResult'
  { _mrStatusMessage ::
      !(Maybe Text),
    _mrUpdatedToken :: !(Maybe Text),
    _mrMessageId :: !(Maybe Text),
    _mrDeliveryStatus :: !DeliveryStatus,
    _mrStatusCode :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrStatusMessage' - The status message for delivering the message.
--
-- * 'mrUpdatedToken' - For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- * 'mrMessageId' - The unique identifier for the message that was sent.
--
-- * 'mrDeliveryStatus' - The delivery status of the message. Possible values are:     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.     * OPT_OUT - The user who's associated with the endpoint address has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint address. Amazon Pinpoint won't attempt to send the message again.     * SUCCESSFUL - The message was successfully delivered to the endpoint address.     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint address.     * TIMEOUT - The message couldn't be sent within the timeout period.     * UNKNOWN_FAILURE - An unknown error occurred.
--
-- * 'mrStatusCode' - The downstream service status code for delivering the message.
messageResult ::
  -- | 'mrDeliveryStatus'
  DeliveryStatus ->
  -- | 'mrStatusCode'
  Int ->
  MessageResult
messageResult pDeliveryStatus_ pStatusCode_ =
  MessageResult'
    { _mrStatusMessage = Nothing,
      _mrUpdatedToken = Nothing,
      _mrMessageId = Nothing,
      _mrDeliveryStatus = pDeliveryStatus_,
      _mrStatusCode = pStatusCode_
    }

-- | The status message for delivering the message.
mrStatusMessage :: Lens' MessageResult (Maybe Text)
mrStatusMessage = lens _mrStatusMessage (\s a -> s {_mrStatusMessage = a})

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
mrUpdatedToken :: Lens' MessageResult (Maybe Text)
mrUpdatedToken = lens _mrUpdatedToken (\s a -> s {_mrUpdatedToken = a})

-- | The unique identifier for the message that was sent.
mrMessageId :: Lens' MessageResult (Maybe Text)
mrMessageId = lens _mrMessageId (\s a -> s {_mrMessageId = a})

-- | The delivery status of the message. Possible values are:     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.     * OPT_OUT - The user who's associated with the endpoint address has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint address. Amazon Pinpoint won't attempt to send the message again.     * SUCCESSFUL - The message was successfully delivered to the endpoint address.     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint address.     * TIMEOUT - The message couldn't be sent within the timeout period.     * UNKNOWN_FAILURE - An unknown error occurred.
mrDeliveryStatus :: Lens' MessageResult DeliveryStatus
mrDeliveryStatus = lens _mrDeliveryStatus (\s a -> s {_mrDeliveryStatus = a})

-- | The downstream service status code for delivering the message.
mrStatusCode :: Lens' MessageResult Int
mrStatusCode = lens _mrStatusCode (\s a -> s {_mrStatusCode = a})

instance FromJSON MessageResult where
  parseJSON =
    withObject
      "MessageResult"
      ( \x ->
          MessageResult'
            <$> (x .:? "StatusMessage")
            <*> (x .:? "UpdatedToken")
            <*> (x .:? "MessageId")
            <*> (x .: "DeliveryStatus")
            <*> (x .: "StatusCode")
      )

instance Hashable MessageResult

instance NFData MessageResult
