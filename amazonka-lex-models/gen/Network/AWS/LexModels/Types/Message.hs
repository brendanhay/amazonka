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
-- Module      : Network.AWS.LexModels.Types.Message
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Message where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ContentType
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON Message where
  parseJSON =
    Prelude.withObject
      "Message"
      ( \x ->
          Message'
            Prelude.<$> (x Prelude..:? "groupNumber")
            Prelude.<*> (x Prelude..: "contentType")
            Prelude.<*> (x Prelude..: "content")
      )

instance Prelude.Hashable Message

instance Prelude.NFData Message

instance Prelude.ToJSON Message where
  toJSON Message' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("groupNumber" Prelude..=) Prelude.<$> groupNumber,
            Prelude.Just ("contentType" Prelude..= contentType),
            Prelude.Just ("content" Prelude..= content)
          ]
      )
