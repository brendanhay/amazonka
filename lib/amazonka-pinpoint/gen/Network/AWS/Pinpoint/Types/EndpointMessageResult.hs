{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointMessageResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointMessageResult where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DeliveryStatus
import Network.AWS.Prelude

-- | Provides information about the delivery status and results of sending a message directly to an endpoint.
--
--
--
-- /See:/ 'endpointMessageResult' smart constructor.
data EndpointMessageResult = EndpointMessageResult'
  { _emrAddress ::
      !(Maybe Text),
    _emrStatusMessage :: !(Maybe Text),
    _emrUpdatedToken :: !(Maybe Text),
    _emrMessageId :: !(Maybe Text),
    _emrDeliveryStatus :: !DeliveryStatus,
    _emrStatusCode :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointMessageResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emrAddress' - The endpoint address that the message was delivered to.
--
-- * 'emrStatusMessage' - The status message for delivering the message.
--
-- * 'emrUpdatedToken' - For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
--
-- * 'emrMessageId' - The unique identifier for the message that was sent.
--
-- * 'emrDeliveryStatus' - The delivery status of the message. Possible values are:     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.     * OPT_OUT - The user who's associated with the endpoint has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint. Amazon Pinpoint won't attempt to send the message again.     * SUCCESSFUL - The message was successfully delivered to the endpoint.     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint.     * TIMEOUT - The message couldn't be sent within the timeout period.     * UNKNOWN_FAILURE - An unknown error occurred.
--
-- * 'emrStatusCode' - The downstream service status code for delivering the message.
endpointMessageResult ::
  -- | 'emrDeliveryStatus'
  DeliveryStatus ->
  -- | 'emrStatusCode'
  Int ->
  EndpointMessageResult
endpointMessageResult pDeliveryStatus_ pStatusCode_ =
  EndpointMessageResult'
    { _emrAddress = Nothing,
      _emrStatusMessage = Nothing,
      _emrUpdatedToken = Nothing,
      _emrMessageId = Nothing,
      _emrDeliveryStatus = pDeliveryStatus_,
      _emrStatusCode = pStatusCode_
    }

-- | The endpoint address that the message was delivered to.
emrAddress :: Lens' EndpointMessageResult (Maybe Text)
emrAddress = lens _emrAddress (\s a -> s {_emrAddress = a})

-- | The status message for delivering the message.
emrStatusMessage :: Lens' EndpointMessageResult (Maybe Text)
emrStatusMessage = lens _emrStatusMessage (\s a -> s {_emrStatusMessage = a})

-- | For push notifications that are sent through the GCM channel, specifies whether the endpoint's device registration token was updated as part of delivering the message.
emrUpdatedToken :: Lens' EndpointMessageResult (Maybe Text)
emrUpdatedToken = lens _emrUpdatedToken (\s a -> s {_emrUpdatedToken = a})

-- | The unique identifier for the message that was sent.
emrMessageId :: Lens' EndpointMessageResult (Maybe Text)
emrMessageId = lens _emrMessageId (\s a -> s {_emrMessageId = a})

-- | The delivery status of the message. Possible values are:     * DUPLICATE - The endpoint address is a duplicate of another endpoint address. Amazon Pinpoint won't attempt to send the message again.     * OPT_OUT - The user who's associated with the endpoint has opted out of receiving messages from you. Amazon Pinpoint won't attempt to send the message again.     * PERMANENT_FAILURE - An error occurred when delivering the message to the endpoint. Amazon Pinpoint won't attempt to send the message again.     * SUCCESSFUL - The message was successfully delivered to the endpoint.     * TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint won't attempt to send the message again.     * THROTTLED - Amazon Pinpoint throttled the operation to send the message to the endpoint.     * TIMEOUT - The message couldn't be sent within the timeout period.     * UNKNOWN_FAILURE - An unknown error occurred.
emrDeliveryStatus :: Lens' EndpointMessageResult DeliveryStatus
emrDeliveryStatus = lens _emrDeliveryStatus (\s a -> s {_emrDeliveryStatus = a})

-- | The downstream service status code for delivering the message.
emrStatusCode :: Lens' EndpointMessageResult Int
emrStatusCode = lens _emrStatusCode (\s a -> s {_emrStatusCode = a})

instance FromJSON EndpointMessageResult where
  parseJSON =
    withObject
      "EndpointMessageResult"
      ( \x ->
          EndpointMessageResult'
            <$> (x .:? "Address")
            <*> (x .:? "StatusMessage")
            <*> (x .:? "UpdatedToken")
            <*> (x .:? "MessageId")
            <*> (x .: "DeliveryStatus")
            <*> (x .: "StatusCode")
      )

instance Hashable EndpointMessageResult

instance NFData EndpointMessageResult
