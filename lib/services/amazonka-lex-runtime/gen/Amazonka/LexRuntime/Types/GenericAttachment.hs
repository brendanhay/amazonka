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
-- Module      : Amazonka.LexRuntime.Types.GenericAttachment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.GenericAttachment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LexRuntime.Types.Button
import qualified Amazonka.Prelude as Prelude

-- | Represents an option rendered to the user when a prompt is shown. It
-- could be an image, a button, a link, or text.
--
-- /See:/ 'newGenericAttachment' smart constructor.
data GenericAttachment = GenericAttachment'
  { -- | The list of options to show to the user.
    buttons :: Prelude.Maybe [Button],
    -- | The subtitle shown below the title.
    subTitle :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image that is displayed to the user.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL of an attachment to the response card.
    attachmentLinkUrl :: Prelude.Maybe Prelude.Text,
    -- | The title of the option.
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenericAttachment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buttons', 'genericAttachment_buttons' - The list of options to show to the user.
--
-- 'subTitle', 'genericAttachment_subTitle' - The subtitle shown below the title.
--
-- 'imageUrl', 'genericAttachment_imageUrl' - The URL of an image that is displayed to the user.
--
-- 'attachmentLinkUrl', 'genericAttachment_attachmentLinkUrl' - The URL of an attachment to the response card.
--
-- 'title', 'genericAttachment_title' - The title of the option.
newGenericAttachment ::
  GenericAttachment
newGenericAttachment =
  GenericAttachment'
    { buttons = Prelude.Nothing,
      subTitle = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      attachmentLinkUrl = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The list of options to show to the user.
genericAttachment_buttons :: Lens.Lens' GenericAttachment (Prelude.Maybe [Button])
genericAttachment_buttons = Lens.lens (\GenericAttachment' {buttons} -> buttons) (\s@GenericAttachment' {} a -> s {buttons = a} :: GenericAttachment) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle shown below the title.
genericAttachment_subTitle :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_subTitle = Lens.lens (\GenericAttachment' {subTitle} -> subTitle) (\s@GenericAttachment' {} a -> s {subTitle = a} :: GenericAttachment)

-- | The URL of an image that is displayed to the user.
genericAttachment_imageUrl :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_imageUrl = Lens.lens (\GenericAttachment' {imageUrl} -> imageUrl) (\s@GenericAttachment' {} a -> s {imageUrl = a} :: GenericAttachment)

-- | The URL of an attachment to the response card.
genericAttachment_attachmentLinkUrl :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_attachmentLinkUrl = Lens.lens (\GenericAttachment' {attachmentLinkUrl} -> attachmentLinkUrl) (\s@GenericAttachment' {} a -> s {attachmentLinkUrl = a} :: GenericAttachment)

-- | The title of the option.
genericAttachment_title :: Lens.Lens' GenericAttachment (Prelude.Maybe Prelude.Text)
genericAttachment_title = Lens.lens (\GenericAttachment' {title} -> title) (\s@GenericAttachment' {} a -> s {title = a} :: GenericAttachment)

instance Core.FromJSON GenericAttachment where
  parseJSON =
    Core.withObject
      "GenericAttachment"
      ( \x ->
          GenericAttachment'
            Prelude.<$> (x Core..:? "buttons" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subTitle")
            Prelude.<*> (x Core..:? "imageUrl")
            Prelude.<*> (x Core..:? "attachmentLinkUrl")
            Prelude.<*> (x Core..:? "title")
      )

instance Prelude.Hashable GenericAttachment where
  hashWithSalt _salt GenericAttachment' {..} =
    _salt `Prelude.hashWithSalt` buttons
      `Prelude.hashWithSalt` subTitle
      `Prelude.hashWithSalt` imageUrl
      `Prelude.hashWithSalt` attachmentLinkUrl
      `Prelude.hashWithSalt` title

instance Prelude.NFData GenericAttachment where
  rnf GenericAttachment' {..} =
    Prelude.rnf buttons
      `Prelude.seq` Prelude.rnf subTitle
      `Prelude.seq` Prelude.rnf imageUrl
      `Prelude.seq` Prelude.rnf attachmentLinkUrl
      `Prelude.seq` Prelude.rnf title
