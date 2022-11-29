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
-- Module      : Amazonka.IoTAnalytics.Types.Message
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.Message where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    payload :: Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
      payload = Core._Base64 Lens.# pPayload_
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
message_payload = Lens.lens (\Message' {payload} -> payload) (\s@Message' {} a -> s {payload = a} :: Message) Prelude.. Core._Base64

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` payload

instance Prelude.NFData Message where
  rnf Message' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf payload

instance Core.ToJSON Message where
  toJSON Message' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("messageId" Core..= messageId),
            Prelude.Just ("payload" Core..= payload)
          ]
      )
