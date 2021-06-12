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
-- Module      : Network.AWS.LexRuntime.Types.GenericAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.GenericAttachment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.Button

-- | Represents an option rendered to the user when a prompt is shown. It
-- could be an image, a button, a link, or text.
--
-- /See:/ 'newGenericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { -- | The title of the option.
    title :: Core.Maybe Core.Text,
    -- | The list of options to show to the user.
    buttons :: Core.Maybe [Button],
    -- | The URL of an attachment to the response card.
    attachmentLinkUrl :: Core.Maybe Core.Text,
    -- | The URL of an image that is displayed to the user.
    imageUrl :: Core.Maybe Core.Text,
    -- | The subtitle shown below the title.
    subTitle :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GenericAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'title', 'genericAttachment_title' - The title of the option.
--
-- 'buttons', 'genericAttachment_buttons' - The list of options to show to the user.
--
-- 'attachmentLinkUrl', 'genericAttachment_attachmentLinkUrl' - The URL of an attachment to the response card.
--
-- 'imageUrl', 'genericAttachment_imageUrl' - The URL of an image that is displayed to the user.
--
-- 'subTitle', 'genericAttachment_subTitle' - The subtitle shown below the title.
newGenericAttachment ::
  GenericAttachment
newGenericAttachment =
  GenericAttachment'
    { title = Core.Nothing,
      buttons = Core.Nothing,
      attachmentLinkUrl = Core.Nothing,
      imageUrl = Core.Nothing,
      subTitle = Core.Nothing
    }

-- | The title of the option.
genericAttachment_title :: Lens.Lens' GenericAttachment (Core.Maybe Core.Text)
genericAttachment_title = Lens.lens (\GenericAttachment' {title} -> title) (\s@GenericAttachment' {} a -> s {title = a} :: GenericAttachment)

-- | The list of options to show to the user.
genericAttachment_buttons :: Lens.Lens' GenericAttachment (Core.Maybe [Button])
genericAttachment_buttons = Lens.lens (\GenericAttachment' {buttons} -> buttons) (\s@GenericAttachment' {} a -> s {buttons = a} :: GenericAttachment) Core.. Lens.mapping Lens._Coerce

-- | The URL of an attachment to the response card.
genericAttachment_attachmentLinkUrl :: Lens.Lens' GenericAttachment (Core.Maybe Core.Text)
genericAttachment_attachmentLinkUrl = Lens.lens (\GenericAttachment' {attachmentLinkUrl} -> attachmentLinkUrl) (\s@GenericAttachment' {} a -> s {attachmentLinkUrl = a} :: GenericAttachment)

-- | The URL of an image that is displayed to the user.
genericAttachment_imageUrl :: Lens.Lens' GenericAttachment (Core.Maybe Core.Text)
genericAttachment_imageUrl = Lens.lens (\GenericAttachment' {imageUrl} -> imageUrl) (\s@GenericAttachment' {} a -> s {imageUrl = a} :: GenericAttachment)

-- | The subtitle shown below the title.
genericAttachment_subTitle :: Lens.Lens' GenericAttachment (Core.Maybe Core.Text)
genericAttachment_subTitle = Lens.lens (\GenericAttachment' {subTitle} -> subTitle) (\s@GenericAttachment' {} a -> s {subTitle = a} :: GenericAttachment)

instance Core.FromJSON GenericAttachment where
  parseJSON =
    Core.withObject
      "GenericAttachment"
      ( \x ->
          GenericAttachment'
            Core.<$> (x Core..:? "title")
            Core.<*> (x Core..:? "buttons" Core..!= Core.mempty)
            Core.<*> (x Core..:? "attachmentLinkUrl")
            Core.<*> (x Core..:? "imageUrl")
            Core.<*> (x Core..:? "subTitle")
      )

instance Core.Hashable GenericAttachment

instance Core.NFData GenericAttachment
