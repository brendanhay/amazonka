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
-- Module      : Network.AWS.SES.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Message where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.Body
import Network.AWS.SES.Types.Content

-- | Represents the message to be sent, composed of a subject and a body.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | The subject of the message: A short summary of the content, which will
    -- appear in the recipient\'s inbox.
    subject :: Content,
    -- | The message body.
    body :: Body
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
-- 'subject', 'message_subject' - The subject of the message: A short summary of the content, which will
-- appear in the recipient\'s inbox.
--
-- 'body', 'message_body' - The message body.
newMessage ::
  -- | 'subject'
  Content ->
  -- | 'body'
  Body ->
  Message
newMessage pSubject_ pBody_ =
  Message' {subject = pSubject_, body = pBody_}

-- | The subject of the message: A short summary of the content, which will
-- appear in the recipient\'s inbox.
message_subject :: Lens.Lens' Message Content
message_subject = Lens.lens (\Message' {subject} -> subject) (\s@Message' {} a -> s {subject = a} :: Message)

-- | The message body.
message_body :: Lens.Lens' Message Body
message_body = Lens.lens (\Message' {body} -> body) (\s@Message' {} a -> s {body = a} :: Message)

instance Prelude.Hashable Message

instance Prelude.NFData Message

instance Prelude.ToQuery Message where
  toQuery Message' {..} =
    Prelude.mconcat
      [ "Subject" Prelude.=: subject,
        "Body" Prelude.=: body
      ]
