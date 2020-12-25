{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ChatMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ChatMessage
  ( ChatMessage (..),

    -- * Smart constructor
    mkChatMessage,

    -- * Lenses
    cmContentType,
    cmContent,
  )
where

import qualified Network.AWS.Connect.Types.ChatContentType as Types
import qualified Network.AWS.Connect.Types.Content as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A chat message.
--
-- /See:/ 'mkChatMessage' smart constructor.
data ChatMessage = ChatMessage'
  { -- | The type of the content. Supported types are text/plain.
    contentType :: Types.ChatContentType,
    -- | The content of the chat message.
    content :: Types.Content
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChatMessage' value with any optional fields omitted.
mkChatMessage ::
  -- | 'contentType'
  Types.ChatContentType ->
  -- | 'content'
  Types.Content ->
  ChatMessage
mkChatMessage contentType content =
  ChatMessage' {contentType, content}

-- | The type of the content. Supported types are text/plain.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContentType :: Lens.Lens' ChatMessage Types.ChatContentType
cmContentType = Lens.field @"contentType"
{-# DEPRECATED cmContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The content of the chat message.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContent :: Lens.Lens' ChatMessage Types.Content
cmContent = Lens.field @"content"
{-# DEPRECATED cmContent "Use generic-lens or generic-optics with 'content' instead." #-}

instance Core.FromJSON ChatMessage where
  toJSON ChatMessage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContentType" Core..= contentType),
            Core.Just ("Content" Core..= content)
          ]
      )
