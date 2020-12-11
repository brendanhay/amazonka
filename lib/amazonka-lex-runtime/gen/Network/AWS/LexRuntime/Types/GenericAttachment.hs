-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.GenericAttachment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.GenericAttachment
  ( GenericAttachment (..),

    -- * Smart constructor
    mkGenericAttachment,

    -- * Lenses
    gaButtons,
    gaSubTitle,
    gaImageURL,
    gaAttachmentLinkURL,
    gaTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.Button
import qualified Network.AWS.Prelude as Lude

-- | Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text.
--
-- /See:/ 'mkGenericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { buttons ::
      Lude.Maybe [Button],
    subTitle :: Lude.Maybe Lude.Text,
    imageURL :: Lude.Maybe Lude.Text,
    attachmentLinkURL :: Lude.Maybe Lude.Text,
    title :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GenericAttachment' with the minimum fields required to make a request.
--
-- * 'attachmentLinkURL' - The URL of an attachment to the response card.
-- * 'buttons' - The list of options to show to the user.
-- * 'imageURL' - The URL of an image that is displayed to the user.
-- * 'subTitle' - The subtitle shown below the title.
-- * 'title' - The title of the option.
mkGenericAttachment ::
  GenericAttachment
mkGenericAttachment =
  GenericAttachment'
    { buttons = Lude.Nothing,
      subTitle = Lude.Nothing,
      imageURL = Lude.Nothing,
      attachmentLinkURL = Lude.Nothing,
      title = Lude.Nothing
    }

-- | The list of options to show to the user.
--
-- /Note:/ Consider using 'buttons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaButtons :: Lens.Lens' GenericAttachment (Lude.Maybe [Button])
gaButtons = Lens.lens (buttons :: GenericAttachment -> Lude.Maybe [Button]) (\s a -> s {buttons = a} :: GenericAttachment)
{-# DEPRECATED gaButtons "Use generic-lens or generic-optics with 'buttons' instead." #-}

-- | The subtitle shown below the title.
--
-- /Note:/ Consider using 'subTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaSubTitle :: Lens.Lens' GenericAttachment (Lude.Maybe Lude.Text)
gaSubTitle = Lens.lens (subTitle :: GenericAttachment -> Lude.Maybe Lude.Text) (\s a -> s {subTitle = a} :: GenericAttachment)
{-# DEPRECATED gaSubTitle "Use generic-lens or generic-optics with 'subTitle' instead." #-}

-- | The URL of an image that is displayed to the user.
--
-- /Note:/ Consider using 'imageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaImageURL :: Lens.Lens' GenericAttachment (Lude.Maybe Lude.Text)
gaImageURL = Lens.lens (imageURL :: GenericAttachment -> Lude.Maybe Lude.Text) (\s a -> s {imageURL = a} :: GenericAttachment)
{-# DEPRECATED gaImageURL "Use generic-lens or generic-optics with 'imageURL' instead." #-}

-- | The URL of an attachment to the response card.
--
-- /Note:/ Consider using 'attachmentLinkURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttachmentLinkURL :: Lens.Lens' GenericAttachment (Lude.Maybe Lude.Text)
gaAttachmentLinkURL = Lens.lens (attachmentLinkURL :: GenericAttachment -> Lude.Maybe Lude.Text) (\s a -> s {attachmentLinkURL = a} :: GenericAttachment)
{-# DEPRECATED gaAttachmentLinkURL "Use generic-lens or generic-optics with 'attachmentLinkURL' instead." #-}

-- | The title of the option.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTitle :: Lens.Lens' GenericAttachment (Lude.Maybe Lude.Text)
gaTitle = Lens.lens (title :: GenericAttachment -> Lude.Maybe Lude.Text) (\s a -> s {title = a} :: GenericAttachment)
{-# DEPRECATED gaTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Lude.FromJSON GenericAttachment where
  parseJSON =
    Lude.withObject
      "GenericAttachment"
      ( \x ->
          GenericAttachment'
            Lude.<$> (x Lude..:? "buttons" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "subTitle")
            Lude.<*> (x Lude..:? "imageUrl")
            Lude.<*> (x Lude..:? "attachmentLinkUrl")
            Lude.<*> (x Lude..:? "title")
      )
