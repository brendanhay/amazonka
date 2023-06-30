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
-- Module      : Amazonka.Connect.Types.ChatMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ChatMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A chat message.
--
-- /See:/ 'newChatMessage' smart constructor.
data ChatMessage = ChatMessage'
  { -- | The type of the content. Supported types are @text\/plain@,
    -- @text\/markdown@, and @application\/json@.
    contentType :: Prelude.Text,
    -- | The content of the chat message.
    --
    -- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
    --     Minimum of 1, Maximum of 1024.
    --
    -- -   For @application\/json@, the Length Constraints are Minimum of 1,
    --     Maximum of 12000.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChatMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'chatMessage_contentType' - The type of the content. Supported types are @text\/plain@,
-- @text\/markdown@, and @application\/json@.
--
-- 'content', 'chatMessage_content' - The content of the chat message.
--
-- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
--     Minimum of 1, Maximum of 1024.
--
-- -   For @application\/json@, the Length Constraints are Minimum of 1,
--     Maximum of 12000.
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

-- | The type of the content. Supported types are @text\/plain@,
-- @text\/markdown@, and @application\/json@.
chatMessage_contentType :: Lens.Lens' ChatMessage Prelude.Text
chatMessage_contentType = Lens.lens (\ChatMessage' {contentType} -> contentType) (\s@ChatMessage' {} a -> s {contentType = a} :: ChatMessage)

-- | The content of the chat message.
--
-- -   For @text\/plain@ and @text\/markdown@, the Length Constraints are
--     Minimum of 1, Maximum of 1024.
--
-- -   For @application\/json@, the Length Constraints are Minimum of 1,
--     Maximum of 12000.
chatMessage_content :: Lens.Lens' ChatMessage Prelude.Text
chatMessage_content = Lens.lens (\ChatMessage' {content} -> content) (\s@ChatMessage' {} a -> s {content = a} :: ChatMessage)

instance Prelude.Hashable ChatMessage where
  hashWithSalt _salt ChatMessage' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` content

instance Prelude.NFData ChatMessage where
  rnf ChatMessage' {..} =
    Prelude.rnf contentType
      `Prelude.seq` Prelude.rnf content

instance Data.ToJSON ChatMessage where
  toJSON ChatMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContentType" Data..= contentType),
            Prelude.Just ("Content" Data..= content)
          ]
      )
