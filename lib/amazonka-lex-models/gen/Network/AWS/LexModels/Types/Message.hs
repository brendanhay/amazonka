{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Message
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Message
  ( Message (..),

    -- * Smart constructor
    mkMessage,

    -- * Lenses
    mContent,
    mGroupNumber,
    mContentType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ContentType
import qualified Network.AWS.Prelude as Lude

-- | The message object that provides the message text and its type.
--
-- /See:/ 'mkMessage' smart constructor.
data Message = Message'
  { -- | The text of the message.
    content :: Lude.Text,
    -- | Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
    groupNumber :: Lude.Maybe Lude.Natural,
    -- | The content type of the message string.
    contentType :: ContentType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Message' with the minimum fields required to make a request.
--
-- * 'content' - The text of the message.
-- * 'groupNumber' - Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
-- * 'contentType' - The content type of the message string.
mkMessage ::
  -- | 'content'
  Lude.Text ->
  -- | 'contentType'
  ContentType ->
  Message
mkMessage pContent_ pContentType_ =
  Message'
    { content = pContent_,
      groupNumber = Lude.Nothing,
      contentType = pContentType_
    }

-- | The text of the message.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mContent :: Lens.Lens' Message Lude.Text
mContent = Lens.lens (content :: Message -> Lude.Text) (\s a -> s {content = a} :: Message)
{-# DEPRECATED mContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | Identifies the message group that the message belongs to. When a group is assigned to a message, Amazon Lex returns one message from each group in the response.
--
-- /Note:/ Consider using 'groupNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGroupNumber :: Lens.Lens' Message (Lude.Maybe Lude.Natural)
mGroupNumber = Lens.lens (groupNumber :: Message -> Lude.Maybe Lude.Natural) (\s a -> s {groupNumber = a} :: Message)
{-# DEPRECATED mGroupNumber "Use generic-lens or generic-optics with 'groupNumber' instead." #-}

-- | The content type of the message string.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mContentType :: Lens.Lens' Message ContentType
mContentType = Lens.lens (contentType :: Message -> ContentType) (\s a -> s {contentType = a} :: Message)
{-# DEPRECATED mContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

instance Lude.FromJSON Message where
  parseJSON =
    Lude.withObject
      "Message"
      ( \x ->
          Message'
            Lude.<$> (x Lude..: "content")
            Lude.<*> (x Lude..:? "groupNumber")
            Lude.<*> (x Lude..: "contentType")
      )

instance Lude.ToJSON Message where
  toJSON Message' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("content" Lude..= content),
            ("groupNumber" Lude..=) Lude.<$> groupNumber,
            Lude.Just ("contentType" Lude..= contentType)
          ]
      )
