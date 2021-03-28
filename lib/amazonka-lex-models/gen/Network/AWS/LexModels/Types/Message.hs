{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Message
  ( Message (..)
  -- * Smart constructor
  , mkMessage
  -- * Lenses
  , mContentType
  , mContent
  , mGroupNumber
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.ContentString as Types
import qualified Network.AWS.LexModels.Types.ContentType as Types
import qualified Network.AWS.Prelude as Core

-- | The message object that provides the message text and its type.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { contentType :: Types.ContentType
    -- ^ The content type of the message string.
  , content :: Types.ContentString
    -- ^ The text of the message.
  , groupNumber :: Core.Maybe Core.Natural
    -- ^ Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Message' value with any optional fields omitted.
mkMessage
    :: Types.ContentType -- ^ 'contentType'
    -> Types.ContentString -- ^ 'content'
    -> Message
mkMessage contentType content
  = Message'{contentType, content, groupNumber = Core.Nothing}

-- | The content type of the message string.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mContentType :: Lens.Lens' Message Types.ContentType
mContentType = Lens.field @"contentType"
{-# INLINEABLE mContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The text of the message.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mContent :: Lens.Lens' Message Types.ContentString
mContent = Lens.field @"content"
{-# INLINEABLE mContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
--
-- /Note:/ Consider using 'groupNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGroupNumber :: Lens.Lens' Message (Core.Maybe Core.Natural)
mGroupNumber = Lens.field @"groupNumber"
{-# INLINEABLE mGroupNumber #-}
{-# DEPRECATED groupNumber "Use generic-lens or generic-optics with 'groupNumber' instead"  #-}

instance Core.FromJSON Message where
        toJSON Message{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("contentType" Core..= contentType),
                  Core.Just ("content" Core..= content),
                  ("groupNumber" Core..=) Core.<$> groupNumber])

instance Core.FromJSON Message where
        parseJSON
          = Core.withObject "Message" Core.$
              \ x ->
                Message' Core.<$>
                  (x Core..: "contentType") Core.<*> x Core..: "content" Core.<*>
                    x Core..:? "groupNumber"
