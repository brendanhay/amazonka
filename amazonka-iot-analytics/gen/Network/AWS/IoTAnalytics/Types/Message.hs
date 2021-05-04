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
-- Module      : Network.AWS.IoTAnalytics.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.Message where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a message.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | The ID you want to assign to the message. Each @messageId@ must be
    -- unique within each batch sent.
    messageId :: Prelude.Text,
    -- | The payload of the message. This can be a JSON string or a
    -- base64-encoded string representing binary data, in which case you must
    -- decode it by means of a pipeline activity.
    payload :: Prelude.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Message' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'message_messageId' - The ID you want to assign to the message. Each @messageId@ must be
-- unique within each batch sent.
--
-- 'payload', 'message_payload' - The payload of the message. This can be a JSON string or a
-- base64-encoded string representing binary data, in which case you must
-- decode it by means of a pipeline activity.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newMessage ::
  -- | 'messageId'
  Prelude.Text ->
  -- | 'payload'
  Prelude.ByteString ->
  Message
newMessage pMessageId_ pPayload_ =
  Message'
    { messageId = pMessageId_,
      payload = Prelude._Base64 Lens.# pPayload_
    }

-- | The ID you want to assign to the message. Each @messageId@ must be
-- unique within each batch sent.
message_messageId :: Lens.Lens' Message Prelude.Text
message_messageId = Lens.lens (\Message' {messageId} -> messageId) (\s@Message' {} a -> s {messageId = a} :: Message)

-- | The payload of the message. This can be a JSON string or a
-- base64-encoded string representing binary data, in which case you must
-- decode it by means of a pipeline activity.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
message_payload :: Lens.Lens' Message Prelude.ByteString
message_payload = Lens.lens (\Message' {payload} -> payload) (\s@Message' {} a -> s {payload = a} :: Message) Prelude.. Prelude._Base64

instance Prelude.Hashable Message

instance Prelude.NFData Message

instance Prelude.ToJSON Message where
  toJSON Message' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("messageId" Prelude..= messageId),
            Prelude.Just ("payload" Prelude..= payload)
          ]
      )
