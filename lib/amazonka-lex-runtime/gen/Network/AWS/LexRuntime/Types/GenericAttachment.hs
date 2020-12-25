{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    gaAttachmentLinkUrl,
    gaButtons,
    gaImageUrl,
    gaSubTitle,
    gaTitle,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.AttachmentLinkUrl as Types
import qualified Network.AWS.LexRuntime.Types.Button as Types
import qualified Network.AWS.LexRuntime.Types.ImageUrl as Types
import qualified Network.AWS.LexRuntime.Types.StringWithLength as Types
import qualified Network.AWS.Prelude as Core

-- | Represents an option rendered to the user when a prompt is shown. It could be an image, a button, a link, or text.
--
-- /See:/ 'mkGenericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { -- | The URL of an attachment to the response card.
    attachmentLinkUrl :: Core.Maybe Types.AttachmentLinkUrl,
    -- | The list of options to show to the user.
    buttons :: Core.Maybe [Types.Button],
    -- | The URL of an image that is displayed to the user.
    imageUrl :: Core.Maybe Types.ImageUrl,
    -- | The subtitle shown below the title.
    subTitle :: Core.Maybe Types.StringWithLength,
    -- | The title of the option.
    title :: Core.Maybe Types.StringWithLength
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GenericAttachment' value with any optional fields omitted.
mkGenericAttachment ::
  GenericAttachment
mkGenericAttachment =
  GenericAttachment'
    { attachmentLinkUrl = Core.Nothing,
      buttons = Core.Nothing,
      imageUrl = Core.Nothing,
      subTitle = Core.Nothing,
      title = Core.Nothing
    }

-- | The URL of an attachment to the response card.
--
-- /Note:/ Consider using 'attachmentLinkUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAttachmentLinkUrl :: Lens.Lens' GenericAttachment (Core.Maybe Types.AttachmentLinkUrl)
gaAttachmentLinkUrl = Lens.field @"attachmentLinkUrl"
{-# DEPRECATED gaAttachmentLinkUrl "Use generic-lens or generic-optics with 'attachmentLinkUrl' instead." #-}

-- | The list of options to show to the user.
--
-- /Note:/ Consider using 'buttons' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaButtons :: Lens.Lens' GenericAttachment (Core.Maybe [Types.Button])
gaButtons = Lens.field @"buttons"
{-# DEPRECATED gaButtons "Use generic-lens or generic-optics with 'buttons' instead." #-}

-- | The URL of an image that is displayed to the user.
--
-- /Note:/ Consider using 'imageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaImageUrl :: Lens.Lens' GenericAttachment (Core.Maybe Types.ImageUrl)
gaImageUrl = Lens.field @"imageUrl"
{-# DEPRECATED gaImageUrl "Use generic-lens or generic-optics with 'imageUrl' instead." #-}

-- | The subtitle shown below the title.
--
-- /Note:/ Consider using 'subTitle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaSubTitle :: Lens.Lens' GenericAttachment (Core.Maybe Types.StringWithLength)
gaSubTitle = Lens.field @"subTitle"
{-# DEPRECATED gaSubTitle "Use generic-lens or generic-optics with 'subTitle' instead." #-}

-- | The title of the option.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaTitle :: Lens.Lens' GenericAttachment (Core.Maybe Types.StringWithLength)
gaTitle = Lens.field @"title"
{-# DEPRECATED gaTitle "Use generic-lens or generic-optics with 'title' instead." #-}

instance Core.FromJSON GenericAttachment where
  parseJSON =
    Core.withObject "GenericAttachment" Core.$
      \x ->
        GenericAttachment'
          Core.<$> (x Core..:? "attachmentLinkUrl")
          Core.<*> (x Core..:? "buttons")
          Core.<*> (x Core..:? "imageUrl")
          Core.<*> (x Core..:? "subTitle")
          Core.<*> (x Core..:? "title")
