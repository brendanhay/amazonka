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
-- Module      : Amazonka.PinpointEmail.Types.Message
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.Message where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types.Body
import Amazonka.PinpointEmail.Types.Content
import qualified Amazonka.Prelude as Prelude

-- | Represents the email message that you\'re sending. The @Message@ object
-- consists of a subject line and a message body.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | The subject line of the email. The subject line can only contain 7-bit
    -- ASCII characters. However, you can specify non-ASCII characters in the
    -- subject line by using encoded-word syntax, as described in
    -- <https://tools.ietf.org/html/rfc2047 RFC 2047>.
    subject :: Content,
    -- | The body of the message. You can specify an HTML version of the message,
    -- a text-only version of the message, or both.
    body :: Body
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
-- 'subject', 'message_subject' - The subject line of the email. The subject line can only contain 7-bit
-- ASCII characters. However, you can specify non-ASCII characters in the
-- subject line by using encoded-word syntax, as described in
-- <https://tools.ietf.org/html/rfc2047 RFC 2047>.
--
-- 'body', 'message_body' - The body of the message. You can specify an HTML version of the message,
-- a text-only version of the message, or both.
newMessage ::
  -- | 'subject'
  Content ->
  -- | 'body'
  Body ->
  Message
newMessage pSubject_ pBody_ =
  Message' {subject = pSubject_, body = pBody_}

-- | The subject line of the email. The subject line can only contain 7-bit
-- ASCII characters. However, you can specify non-ASCII characters in the
-- subject line by using encoded-word syntax, as described in
-- <https://tools.ietf.org/html/rfc2047 RFC 2047>.
message_subject :: Lens.Lens' Message Content
message_subject = Lens.lens (\Message' {subject} -> subject) (\s@Message' {} a -> s {subject = a} :: Message)

-- | The body of the message. You can specify an HTML version of the message,
-- a text-only version of the message, or both.
message_body :: Lens.Lens' Message Body
message_body = Lens.lens (\Message' {body} -> body) (\s@Message' {} a -> s {body = a} :: Message)

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` body

instance Prelude.NFData Message where
  rnf Message' {..} =
    Prelude.rnf subject `Prelude.seq` Prelude.rnf body

instance Data.ToJSON Message where
  toJSON Message' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Subject" Data..= subject),
            Prelude.Just ("Body" Data..= body)
          ]
      )
