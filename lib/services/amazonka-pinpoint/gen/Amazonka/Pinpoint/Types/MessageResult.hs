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
-- Module      : Amazonka.Pinpoint.Types.MessageResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MessageResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.DeliveryStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the results of sending a message directly to
-- an endpoint address.
--
-- /See:/ 'newMessageResult' smart constructor.
data MessageResult = MessageResult'
  { -- | For push notifications that are sent through the GCM channel, specifies
    -- whether the endpoint\'s device registration token was updated as part of
    -- delivering the message.
    updatedToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the message that was sent.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The status message for delivering the message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The delivery status of the message. Possible values are:
    --
    -- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
    --     address. Amazon Pinpoint won\'t attempt to send the message again.
    --
    -- -   OPT_OUT - The user who\'s associated with the endpoint address has
    --     opted out of receiving messages from you. Amazon Pinpoint won\'t
    --     attempt to send the message again.
    --
    -- -   PERMANENT_FAILURE - An error occurred when delivering the message to
    --     the endpoint address. Amazon Pinpoint won\'t attempt to send the
    --     message again.
    --
    -- -   SUCCESSFUL - The message was successfully delivered to the endpoint
    --     address.
    --
    -- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
    --     won\'t attempt to send the message again.
    --
    -- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
    --     message to the endpoint address.
    --
    -- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
    --
    -- -   UNKNOWN_FAILURE - An unknown error occurred.
    deliveryStatus :: DeliveryStatus,
    -- | The downstream service status code for delivering the message.
    statusCode :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updatedToken', 'messageResult_updatedToken' - For push notifications that are sent through the GCM channel, specifies
-- whether the endpoint\'s device registration token was updated as part of
-- delivering the message.
--
-- 'messageId', 'messageResult_messageId' - The unique identifier for the message that was sent.
--
-- 'statusMessage', 'messageResult_statusMessage' - The status message for delivering the message.
--
-- 'deliveryStatus', 'messageResult_deliveryStatus' - The delivery status of the message. Possible values are:
--
-- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
--     address. Amazon Pinpoint won\'t attempt to send the message again.
--
-- -   OPT_OUT - The user who\'s associated with the endpoint address has
--     opted out of receiving messages from you. Amazon Pinpoint won\'t
--     attempt to send the message again.
--
-- -   PERMANENT_FAILURE - An error occurred when delivering the message to
--     the endpoint address. Amazon Pinpoint won\'t attempt to send the
--     message again.
--
-- -   SUCCESSFUL - The message was successfully delivered to the endpoint
--     address.
--
-- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
--     won\'t attempt to send the message again.
--
-- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
--     message to the endpoint address.
--
-- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
--
-- -   UNKNOWN_FAILURE - An unknown error occurred.
--
-- 'statusCode', 'messageResult_statusCode' - The downstream service status code for delivering the message.
newMessageResult ::
  -- | 'deliveryStatus'
  DeliveryStatus ->
  -- | 'statusCode'
  Prelude.Int ->
  MessageResult
newMessageResult pDeliveryStatus_ pStatusCode_ =
  MessageResult'
    { updatedToken = Prelude.Nothing,
      messageId = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      deliveryStatus = pDeliveryStatus_,
      statusCode = pStatusCode_
    }

-- | For push notifications that are sent through the GCM channel, specifies
-- whether the endpoint\'s device registration token was updated as part of
-- delivering the message.
messageResult_updatedToken :: Lens.Lens' MessageResult (Prelude.Maybe Prelude.Text)
messageResult_updatedToken = Lens.lens (\MessageResult' {updatedToken} -> updatedToken) (\s@MessageResult' {} a -> s {updatedToken = a} :: MessageResult)

-- | The unique identifier for the message that was sent.
messageResult_messageId :: Lens.Lens' MessageResult (Prelude.Maybe Prelude.Text)
messageResult_messageId = Lens.lens (\MessageResult' {messageId} -> messageId) (\s@MessageResult' {} a -> s {messageId = a} :: MessageResult)

-- | The status message for delivering the message.
messageResult_statusMessage :: Lens.Lens' MessageResult (Prelude.Maybe Prelude.Text)
messageResult_statusMessage = Lens.lens (\MessageResult' {statusMessage} -> statusMessage) (\s@MessageResult' {} a -> s {statusMessage = a} :: MessageResult)

-- | The delivery status of the message. Possible values are:
--
-- -   DUPLICATE - The endpoint address is a duplicate of another endpoint
--     address. Amazon Pinpoint won\'t attempt to send the message again.
--
-- -   OPT_OUT - The user who\'s associated with the endpoint address has
--     opted out of receiving messages from you. Amazon Pinpoint won\'t
--     attempt to send the message again.
--
-- -   PERMANENT_FAILURE - An error occurred when delivering the message to
--     the endpoint address. Amazon Pinpoint won\'t attempt to send the
--     message again.
--
-- -   SUCCESSFUL - The message was successfully delivered to the endpoint
--     address.
--
-- -   TEMPORARY_FAILURE - A temporary error occurred. Amazon Pinpoint
--     won\'t attempt to send the message again.
--
-- -   THROTTLED - Amazon Pinpoint throttled the operation to send the
--     message to the endpoint address.
--
-- -   TIMEOUT - The message couldn\'t be sent within the timeout period.
--
-- -   UNKNOWN_FAILURE - An unknown error occurred.
messageResult_deliveryStatus :: Lens.Lens' MessageResult DeliveryStatus
messageResult_deliveryStatus = Lens.lens (\MessageResult' {deliveryStatus} -> deliveryStatus) (\s@MessageResult' {} a -> s {deliveryStatus = a} :: MessageResult)

-- | The downstream service status code for delivering the message.
messageResult_statusCode :: Lens.Lens' MessageResult Prelude.Int
messageResult_statusCode = Lens.lens (\MessageResult' {statusCode} -> statusCode) (\s@MessageResult' {} a -> s {statusCode = a} :: MessageResult)

instance Core.FromJSON MessageResult where
  parseJSON =
    Core.withObject
      "MessageResult"
      ( \x ->
          MessageResult'
            Prelude.<$> (x Core..:? "UpdatedToken")
            Prelude.<*> (x Core..:? "MessageId")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..: "DeliveryStatus")
            Prelude.<*> (x Core..: "StatusCode")
      )

instance Prelude.Hashable MessageResult where
  hashWithSalt _salt MessageResult' {..} =
    _salt `Prelude.hashWithSalt` updatedToken
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` deliveryStatus
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData MessageResult where
  rnf MessageResult' {..} =
    Prelude.rnf updatedToken
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf deliveryStatus
      `Prelude.seq` Prelude.rnf statusCode
