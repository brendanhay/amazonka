{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointMessageResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointMessageResult where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.DeliveryStatus
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the delivery status and results of sending a
-- message directly to an endpoint.
--
-- /See:/ 'newEndpointMessageResult' smart constructor.
data EndpointMessageResult = EndpointMessageResult'
  { -- | The status message for delivering the message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | For push notifications that are sent through the GCM channel, specifies
    -- whether the endpoint\'s device registration token was updated as part of
    -- delivering the message.
    updatedToken :: Prelude.Maybe Prelude.Text,
    -- | The endpoint address that the message was delivered to.
    address :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the message that was sent.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The delivery status of the message. Possible values are:
    --
    -- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
    --     address. Amazon Pinpoint won\'t attempt to send the message again.
    --
    -- -   OPT_OUT - The user who\'s associated with the endpoint has opted out
    --     of receiving messages from you. Amazon Pinpoint won\'t attempt to
    --     send the message again.
    --
    -- -   PERMANENT_FAILURE - An error occurred when delivering the message to
    --     the endpoint. Amazon Pinpoint won\'t attempt to send the message
    --     again.
    --
    -- -   SUCCESSFUL - The message was successfully delivered to the endpoint.
    --
    -- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
    --     won\'t attempt to send the message again.
    --
    -- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
    --     message to the endpoint.
    --
    -- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
    --
    -- -   UNKNOWN_FAILURE - An unknown error occurred.
    deliveryStatus :: DeliveryStatus,
    -- | The downstream service status code for delivering the message.
    statusCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EndpointMessageResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'endpointMessageResult_statusMessage' - The status message for delivering the message.
--
-- 'updatedToken', 'endpointMessageResult_updatedToken' - For push notifications that are sent through the GCM channel, specifies
-- whether the endpoint\'s device registration token was updated as part of
-- delivering the message.
--
-- 'address', 'endpointMessageResult_address' - The endpoint address that the message was delivered to.
--
-- 'messageId', 'endpointMessageResult_messageId' - The unique identifier for the message that was sent.
--
-- 'deliveryStatus', 'endpointMessageResult_deliveryStatus' - The delivery status of the message. Possible values are:
--
-- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
--     address. Amazon Pinpoint won\'t attempt to send the message again.
--
-- -   OPT_OUT - The user who\'s associated with the endpoint has opted out
--     of receiving messages from you. Amazon Pinpoint won\'t attempt to
--     send the message again.
--
-- -   PERMANENT_FAILURE - An error occurred when delivering the message to
--     the endpoint. Amazon Pinpoint won\'t attempt to send the message
--     again.
--
-- -   SUCCESSFUL - The message was successfully delivered to the endpoint.
--
-- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
--     won\'t attempt to send the message again.
--
-- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
--     message to the endpoint.
--
-- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
--
-- -   UNKNOWN_FAILURE - An unknown error occurred.
--
-- 'statusCode', 'endpointMessageResult_statusCode' - The downstream service status code for delivering the message.
newEndpointMessageResult ::
  -- | 'deliveryStatus'
  DeliveryStatus ->
  -- | 'statusCode'
  Prelude.Int ->
  EndpointMessageResult
newEndpointMessageResult
  pDeliveryStatus_
  pStatusCode_ =
    EndpointMessageResult'
      { statusMessage =
          Prelude.Nothing,
        updatedToken = Prelude.Nothing,
        address = Prelude.Nothing,
        messageId = Prelude.Nothing,
        deliveryStatus = pDeliveryStatus_,
        statusCode = pStatusCode_
      }

-- | The status message for delivering the message.
endpointMessageResult_statusMessage :: Lens.Lens' EndpointMessageResult (Prelude.Maybe Prelude.Text)
endpointMessageResult_statusMessage = Lens.lens (\EndpointMessageResult' {statusMessage} -> statusMessage) (\s@EndpointMessageResult' {} a -> s {statusMessage = a} :: EndpointMessageResult)

-- | For push notifications that are sent through the GCM channel, specifies
-- whether the endpoint\'s device registration token was updated as part of
-- delivering the message.
endpointMessageResult_updatedToken :: Lens.Lens' EndpointMessageResult (Prelude.Maybe Prelude.Text)
endpointMessageResult_updatedToken = Lens.lens (\EndpointMessageResult' {updatedToken} -> updatedToken) (\s@EndpointMessageResult' {} a -> s {updatedToken = a} :: EndpointMessageResult)

-- | The endpoint address that the message was delivered to.
endpointMessageResult_address :: Lens.Lens' EndpointMessageResult (Prelude.Maybe Prelude.Text)
endpointMessageResult_address = Lens.lens (\EndpointMessageResult' {address} -> address) (\s@EndpointMessageResult' {} a -> s {address = a} :: EndpointMessageResult)

-- | The unique identifier for the message that was sent.
endpointMessageResult_messageId :: Lens.Lens' EndpointMessageResult (Prelude.Maybe Prelude.Text)
endpointMessageResult_messageId = Lens.lens (\EndpointMessageResult' {messageId} -> messageId) (\s@EndpointMessageResult' {} a -> s {messageId = a} :: EndpointMessageResult)

-- | The delivery status of the message. Possible values are:
--
-- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
--     address. Amazon Pinpoint won\'t attempt to send the message again.
--
-- -   OPT_OUT - The user who\'s associated with the endpoint has opted out
--     of receiving messages from you. Amazon Pinpoint won\'t attempt to
--     send the message again.
--
-- -   PERMANENT_FAILURE - An error occurred when delivering the message to
--     the endpoint. Amazon Pinpoint won\'t attempt to send the message
--     again.
--
-- -   SUCCESSFUL - The message was successfully delivered to the endpoint.
--
-- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
--     won\'t attempt to send the message again.
--
-- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
--     message to the endpoint.
--
-- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
--
-- -   UNKNOWN_FAILURE - An unknown error occurred.
endpointMessageResult_deliveryStatus :: Lens.Lens' EndpointMessageResult DeliveryStatus
endpointMessageResult_deliveryStatus = Lens.lens (\EndpointMessageResult' {deliveryStatus} -> deliveryStatus) (\s@EndpointMessageResult' {} a -> s {deliveryStatus = a} :: EndpointMessageResult)

-- | The downstream service status code for delivering the message.
endpointMessageResult_statusCode :: Lens.Lens' EndpointMessageResult Prelude.Int
endpointMessageResult_statusCode = Lens.lens (\EndpointMessageResult' {statusCode} -> statusCode) (\s@EndpointMessageResult' {} a -> s {statusCode = a} :: EndpointMessageResult)

instance Prelude.FromJSON EndpointMessageResult where
  parseJSON =
    Prelude.withObject
      "EndpointMessageResult"
      ( \x ->
          EndpointMessageResult'
            Prelude.<$> (x Prelude..:? "StatusMessage")
            Prelude.<*> (x Prelude..:? "UpdatedToken")
            Prelude.<*> (x Prelude..:? "Address")
            Prelude.<*> (x Prelude..:? "MessageId")
            Prelude.<*> (x Prelude..: "DeliveryStatus")
            Prelude.<*> (x Prelude..: "StatusCode")
      )

instance Prelude.Hashable EndpointMessageResult

instance Prelude.NFData EndpointMessageResult
