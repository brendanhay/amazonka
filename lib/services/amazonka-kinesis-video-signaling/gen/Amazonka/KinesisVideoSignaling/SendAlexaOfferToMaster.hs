{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.KinesisVideoSignaling.SendAlexaOfferToMaster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API allows you to connect WebRTC-enabled devices with Alexa display
-- devices. When invoked, it sends the Alexa Session Description Protocol
-- (SDP) offer to the master peer. The offer is delivered as soon as the
-- master is connected to the specified signaling channel. This API returns
-- the SDP answer from the connected master. If the master is not connected
-- to the signaling channel, redelivery requests are made until the message
-- expires.
module Amazonka.KinesisVideoSignaling.SendAlexaOfferToMaster
  ( -- * Creating a Request
    SendAlexaOfferToMaster (..),
    newSendAlexaOfferToMaster,

    -- * Request Lenses
    sendAlexaOfferToMaster_channelARN,
    sendAlexaOfferToMaster_senderClientId,
    sendAlexaOfferToMaster_messagePayload,

    -- * Destructuring the Response
    SendAlexaOfferToMasterResponse (..),
    newSendAlexaOfferToMasterResponse,

    -- * Response Lenses
    sendAlexaOfferToMasterResponse_answer,
    sendAlexaOfferToMasterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisVideoSignaling.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSendAlexaOfferToMaster' smart constructor.
data SendAlexaOfferToMaster = SendAlexaOfferToMaster'
  { -- | The ARN of the signaling channel by which Alexa and the master peer
    -- communicate.
    channelARN :: Prelude.Text,
    -- | The unique identifier for the sender client.
    senderClientId :: Prelude.Text,
    -- | The base64-encoded SDP offer content.
    messagePayload :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendAlexaOfferToMaster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelARN', 'sendAlexaOfferToMaster_channelARN' - The ARN of the signaling channel by which Alexa and the master peer
-- communicate.
--
-- 'senderClientId', 'sendAlexaOfferToMaster_senderClientId' - The unique identifier for the sender client.
--
-- 'messagePayload', 'sendAlexaOfferToMaster_messagePayload' - The base64-encoded SDP offer content.
newSendAlexaOfferToMaster ::
  -- | 'channelARN'
  Prelude.Text ->
  -- | 'senderClientId'
  Prelude.Text ->
  -- | 'messagePayload'
  Prelude.Text ->
  SendAlexaOfferToMaster
newSendAlexaOfferToMaster
  pChannelARN_
  pSenderClientId_
  pMessagePayload_ =
    SendAlexaOfferToMaster'
      { channelARN = pChannelARN_,
        senderClientId = pSenderClientId_,
        messagePayload = pMessagePayload_
      }

-- | The ARN of the signaling channel by which Alexa and the master peer
-- communicate.
sendAlexaOfferToMaster_channelARN :: Lens.Lens' SendAlexaOfferToMaster Prelude.Text
sendAlexaOfferToMaster_channelARN = Lens.lens (\SendAlexaOfferToMaster' {channelARN} -> channelARN) (\s@SendAlexaOfferToMaster' {} a -> s {channelARN = a} :: SendAlexaOfferToMaster)

-- | The unique identifier for the sender client.
sendAlexaOfferToMaster_senderClientId :: Lens.Lens' SendAlexaOfferToMaster Prelude.Text
sendAlexaOfferToMaster_senderClientId = Lens.lens (\SendAlexaOfferToMaster' {senderClientId} -> senderClientId) (\s@SendAlexaOfferToMaster' {} a -> s {senderClientId = a} :: SendAlexaOfferToMaster)

-- | The base64-encoded SDP offer content.
sendAlexaOfferToMaster_messagePayload :: Lens.Lens' SendAlexaOfferToMaster Prelude.Text
sendAlexaOfferToMaster_messagePayload = Lens.lens (\SendAlexaOfferToMaster' {messagePayload} -> messagePayload) (\s@SendAlexaOfferToMaster' {} a -> s {messagePayload = a} :: SendAlexaOfferToMaster)

instance Core.AWSRequest SendAlexaOfferToMaster where
  type
    AWSResponse SendAlexaOfferToMaster =
      SendAlexaOfferToMasterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SendAlexaOfferToMasterResponse'
            Prelude.<$> (x Data..?> "Answer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SendAlexaOfferToMaster where
  hashWithSalt _salt SendAlexaOfferToMaster' {..} =
    _salt `Prelude.hashWithSalt` channelARN
      `Prelude.hashWithSalt` senderClientId
      `Prelude.hashWithSalt` messagePayload

instance Prelude.NFData SendAlexaOfferToMaster where
  rnf SendAlexaOfferToMaster' {..} =
    Prelude.rnf channelARN
      `Prelude.seq` Prelude.rnf senderClientId
      `Prelude.seq` Prelude.rnf messagePayload

instance Data.ToHeaders SendAlexaOfferToMaster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SendAlexaOfferToMaster where
  toJSON SendAlexaOfferToMaster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ChannelARN" Data..= channelARN),
            Prelude.Just
              ("SenderClientId" Data..= senderClientId),
            Prelude.Just
              ("MessagePayload" Data..= messagePayload)
          ]
      )

instance Data.ToPath SendAlexaOfferToMaster where
  toPath =
    Prelude.const "/v1/send-alexa-offer-to-master"

instance Data.ToQuery SendAlexaOfferToMaster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSendAlexaOfferToMasterResponse' smart constructor.
data SendAlexaOfferToMasterResponse = SendAlexaOfferToMasterResponse'
  { -- | The base64-encoded SDP answer content.
    answer :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SendAlexaOfferToMasterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'answer', 'sendAlexaOfferToMasterResponse_answer' - The base64-encoded SDP answer content.
--
-- 'httpStatus', 'sendAlexaOfferToMasterResponse_httpStatus' - The response's http status code.
newSendAlexaOfferToMasterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SendAlexaOfferToMasterResponse
newSendAlexaOfferToMasterResponse pHttpStatus_ =
  SendAlexaOfferToMasterResponse'
    { answer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base64-encoded SDP answer content.
sendAlexaOfferToMasterResponse_answer :: Lens.Lens' SendAlexaOfferToMasterResponse (Prelude.Maybe Prelude.Text)
sendAlexaOfferToMasterResponse_answer = Lens.lens (\SendAlexaOfferToMasterResponse' {answer} -> answer) (\s@SendAlexaOfferToMasterResponse' {} a -> s {answer = a} :: SendAlexaOfferToMasterResponse)

-- | The response's http status code.
sendAlexaOfferToMasterResponse_httpStatus :: Lens.Lens' SendAlexaOfferToMasterResponse Prelude.Int
sendAlexaOfferToMasterResponse_httpStatus = Lens.lens (\SendAlexaOfferToMasterResponse' {httpStatus} -> httpStatus) (\s@SendAlexaOfferToMasterResponse' {} a -> s {httpStatus = a} :: SendAlexaOfferToMasterResponse)

instance
  Prelude.NFData
    SendAlexaOfferToMasterResponse
  where
  rnf SendAlexaOfferToMasterResponse' {..} =
    Prelude.rnf answer
      `Prelude.seq` Prelude.rnf httpStatus
