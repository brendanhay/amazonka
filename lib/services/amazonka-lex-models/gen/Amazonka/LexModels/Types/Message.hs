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
-- Module      : Amazonka.LexModels.Types.Message
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.Message where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types.ContentType
import qualified Amazonka.Prelude as Prelude

-- | The message object that provides the message text and its type.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | Identifies the message group that the message belongs to. When a group
    -- is assigned to a message, Amazon Lex returns one message from each group
    -- in the response.
    groupNumber :: Prelude.Maybe Prelude.Natural,
    -- | The content type of the message string.
    contentType :: ContentType,
    -- | The text of the message.
    content :: Prelude.Text
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
-- 'groupNumber', 'message_groupNumber' - Identifies the message group that the message belongs to. When a group
-- is assigned to a message, Amazon Lex returns one message from each group
-- in the response.
--
-- 'contentType', 'message_contentType' - The content type of the message string.
--
-- 'content', 'message_content' - The text of the message.
newMessage ::
  -- | 'contentType'
  ContentType ->
  -- | 'content'
  Prelude.Text ->
  Message
newMessage pContentType_ pContent_ =
  Message'
    { groupNumber = Prelude.Nothing,
      contentType = pContentType_,
      content = pContent_
    }

-- | Identifies the message group that the message belongs to. When a group
-- is assigned to a message, Amazon Lex returns one message from each group
-- in the response.
message_groupNumber :: Lens.Lens' Message (Prelude.Maybe Prelude.Natural)
message_groupNumber = Lens.lens (\Message' {groupNumber} -> groupNumber) (\s@Message' {} a -> s {groupNumber = a} :: Message)

-- | The content type of the message string.
message_contentType :: Lens.Lens' Message ContentType
message_contentType = Lens.lens (\Message' {contentType} -> contentType) (\s@Message' {} a -> s {contentType = a} :: Message)

-- | The text of the message.
message_content :: Lens.Lens' Message Prelude.Text
message_content = Lens.lens (\Message' {content} -> content) (\s@Message' {} a -> s {content = a} :: Message)

instance Core.FromJSON Message where
  parseJSON =
    Core.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Core..:? "groupNumber")
            Prelude.<*> (x Core..: "contentType")
            Prelude.<*> (x Core..: "content")
      )

instance Prelude.Hashable Message where
  hashWithSalt _salt Message' {..} =
    _salt `Prelude.hashWithSalt` groupNumber
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` content

instance Prelude.NFData Message where
  rnf Message' {..} =
    Prelude.rnf groupNumber
      `Prelude.seq` Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf content

instance Core.ToJSON Message where
  toJSON Message' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("groupNumber" Core..=) Prelude.<$> groupNumber,
            Prelude.Just ("contentType" Core..= contentType),
            Prelude.Just ("content" Core..= content)
          ]
      )
