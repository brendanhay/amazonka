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
-- Module      : Network.AWS.LexModels.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Message where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ContentType

-- | The message object that provides the message text and its type.
--
-- /See:/ 'newMessage' smart constructor.
data Message = Message'
  { -- | Identifies the message group that the message belongs to. When a group
    -- is assigned to a message, Amazon Lex returns one message from each group
    -- in the response.
    groupNumber :: Core.Maybe Core.Natural,
    -- | The content type of the message string.
    contentType :: ContentType,
    -- | The text of the message.
    content :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Message
newMessage pContentType_ pContent_ =
  Message'
    { groupNumber = Core.Nothing,
      contentType = pContentType_,
      content = pContent_
    }

-- | Identifies the message group that the message belongs to. When a group
-- is assigned to a message, Amazon Lex returns one message from each group
-- in the response.
message_groupNumber :: Lens.Lens' Message (Core.Maybe Core.Natural)
message_groupNumber = Lens.lens (\Message' {groupNumber} -> groupNumber) (\s@Message' {} a -> s {groupNumber = a} :: Message)

-- | The content type of the message string.
message_contentType :: Lens.Lens' Message ContentType
message_contentType = Lens.lens (\Message' {contentType} -> contentType) (\s@Message' {} a -> s {contentType = a} :: Message)

-- | The text of the message.
message_content :: Lens.Lens' Message Core.Text
message_content = Lens.lens (\Message' {content} -> content) (\s@Message' {} a -> s {content = a} :: Message)

instance Core.FromJSON Message where
  parseJSON =
    Core.withObject
      "Message"
      ( \x ->
          Message'
            Core.<$> (x Core..:? "groupNumber")
            Core.<*> (x Core..: "contentType")
            Core.<*> (x Core..: "content")
      )

instance Core.Hashable Message

instance Core.NFData Message

instance Core.ToJSON Message where
  toJSON Message' {..} =
    Core.object
      ( Core.catMaybes
          [ ("groupNumber" Core..=) Core.<$> groupNumber,
            Core.Just ("contentType" Core..= contentType),
            Core.Just ("content" Core..= content)
          ]
      )
