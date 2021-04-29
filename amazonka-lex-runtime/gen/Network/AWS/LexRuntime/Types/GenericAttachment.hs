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
-- Module      : Network.AWS.LexRuntime.Types.GenericAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.GenericAttachment where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexRuntime.Types.Button
import qualified Network.AWS.Prelude as Prelude

-- | Represents an option rendered to the user when a prompt is shown. It
-- could be an image, a button, a link, or text.
--
-- /See:/ 'newGenericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { -- | The title of the option.
    title :: Prelude.Maybe Prelude.Text,
    -- | The list of options to show to the user.
    buttons :: Prelude.Maybe [Button],
    -- | The URL of an attachment to the response card.
    attachmentLinkUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image that is displayed to the user.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The subtitle shown below the title.
    subTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { title = Prelude.Nothing,
      buttons = Prelude.Nothing,
      attachmentLinkUrl = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      subTitle = Prelude.Nothing
    }

-- | The title of the option.
genericAttachment_title :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_title = Lens.lens (\GenericAttachment' {title} -> title) (\s@GenericAttachment' {} a -> s {title = a} :: GenericAttachment)

-- | The list of options to show to the user.
genericAttachment_buttons :: Lens.Lens' GenericAttachment (Prelude.Maybe [Button])
genericAttachment_buttons = Lens.lens (\GenericAttachment' {buttons} -> buttons) (\s@GenericAttachment' {} a -> s {buttons = a} :: GenericAttachment) Prelude.. Lens.mapping Prelude._Coerce

-- | The URL of an attachment to the response card.
genericAttachment_attachmentLinkUrl :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_attachmentLinkUrl = Lens.lens (\GenericAttachment' {attachmentLinkUrl} -> attachmentLinkUrl) (\s@GenericAttachment' {} a -> s {attachmentLinkUrl = a} :: GenericAttachment)

-- | The URL of an image that is displayed to the user.
genericAttachment_imageUrl :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_imageUrl = Lens.lens (\GenericAttachment' {imageUrl} -> imageUrl) (\s@GenericAttachment' {} a -> s {imageUrl = a} :: GenericAttachment)

-- | The subtitle shown below the title.
genericAttachment_subTitle :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_subTitle = Lens.lens (\GenericAttachment' {subTitle} -> subTitle) (\s@GenericAttachment' {} a -> s {subTitle = a} :: GenericAttachment)

instance Prelude.FromJSON GenericAttachment where
  parseJSON =
    Prelude.withObject
      "GenericAttachment"
      ( \x ->
          GenericAttachment'
            Prelude.<$> (x Prelude..:? "title")
            Prelude.<*> (x Prelude..:? "buttons" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "attachmentLinkUrl")
            Prelude.<*> (x Prelude..:? "imageUrl")
            Prelude.<*> (x Prelude..:? "subTitle")
      )

instance Prelude.Hashable GenericAttachment

instance Prelude.NFData GenericAttachment
