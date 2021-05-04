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
-- Module      : Network.AWS.Connect.Types.ChatMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ChatMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A chat message.
--
-- /See:/ 'newChatMessage' smart constructor.
data ChatMessage = ChatMessage'
  { -- | The type of the content. Supported types are text and plain.
    contentType :: Prelude.Text,
    -- | The content of the chat message.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ChatMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'chatMessage_contentType' - The type of the content. Supported types are text and plain.
--
-- 'content', 'chatMessage_content' - The content of the chat message.
newChatMessage ::
  -- | 'contentType'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  ChatMessage
newChatMessage pContentType_ pContent_ =
  ChatMessage'
    { contentType = pContentType_,
      content = pContent_
    }

-- | The type of the content. Supported types are text and plain.
chatMessage_contentType :: Lens.Lens' ChatMessage Prelude.Text
chatMessage_contentType = Lens.lens (\ChatMessage' {contentType} -> contentType) (\s@ChatMessage' {} a -> s {contentType = a} :: ChatMessage)

-- | The content of the chat message.
chatMessage_content :: Lens.Lens' ChatMessage Prelude.Text
chatMessage_content = Lens.lens (\ChatMessage' {content} -> content) (\s@ChatMessage' {} a -> s {content = a} :: ChatMessage)

instance Prelude.Hashable ChatMessage

instance Prelude.NFData ChatMessage

instance Prelude.ToJSON ChatMessage where
  toJSON ChatMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContentType" Prelude..= contentType),
            Prelude.Just ("Content" Prelude..= content)
          ]
      )
