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
-- Module      : Network.AWS.LexV2Models.Types.ImageResponseCard
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.ImageResponseCard where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.Button
import qualified Network.AWS.Prelude as Prelude

-- | A card that is shown to the user by a messaging platform. You define the
-- contents of the card, the card is displayed by the platform.
--
-- When you use a response card, the response from the user is constrained
-- to the text associated with a button on the card.
--
-- /See:/ 'newImageResponseCard' smart constructor.
data ImageResponseCard = ImageResponseCard'
  { -- | A list of buttons that should be displayed on the response card. The
    -- arrangement of the buttons is determined by the platform that displays
    -- the button.
    buttons :: Prelude.Maybe [Button],
    -- | The subtitle to display on the response card. The format of the subtitle
    -- is determined by the platform displaying the response card.
    subtitle :: Prelude.Maybe Prelude.Text,
    -- | The URL of an image to display on the response card. The image URL must
    -- be publicly available so that the platform displaying the response card
    -- has access to the image.
    imageUrl :: Prelude.Maybe Prelude.Text,
    -- | The title to display on the response card. The format of the title is
    -- determined by the platform displaying the response card.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageResponseCard' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buttons', 'imageResponseCard_buttons' - A list of buttons that should be displayed on the response card. The
-- arrangement of the buttons is determined by the platform that displays
-- the button.
--
-- 'subtitle', 'imageResponseCard_subtitle' - The subtitle to display on the response card. The format of the subtitle
-- is determined by the platform displaying the response card.
--
-- 'imageUrl', 'imageResponseCard_imageUrl' - The URL of an image to display on the response card. The image URL must
-- be publicly available so that the platform displaying the response card
-- has access to the image.
--
-- 'title', 'imageResponseCard_title' - The title to display on the response card. The format of the title is
-- determined by the platform displaying the response card.
newImageResponseCard ::
  -- | 'title'
  Prelude.Text ->
  ImageResponseCard
newImageResponseCard pTitle_ =
  ImageResponseCard'
    { buttons = Prelude.Nothing,
      subtitle = Prelude.Nothing,
      imageUrl = Prelude.Nothing,
      title = pTitle_
    }

-- | A list of buttons that should be displayed on the response card. The
-- arrangement of the buttons is determined by the platform that displays
-- the button.
imageResponseCard_buttons :: Lens.Lens' ImageResponseCard (Prelude.Maybe [Button])
imageResponseCard_buttons = Lens.lens (\ImageResponseCard' {buttons} -> buttons) (\s@ImageResponseCard' {} a -> s {buttons = a} :: ImageResponseCard) Prelude.. Lens.mapping Lens.coerced

-- | The subtitle to display on the response card. The format of the subtitle
-- is determined by the platform displaying the response card.
imageResponseCard_subtitle :: Lens.Lens' ImageResponseCard (Prelude.Maybe Prelude.Text)
imageResponseCard_subtitle = Lens.lens (\ImageResponseCard' {subtitle} -> subtitle) (\s@ImageResponseCard' {} a -> s {subtitle = a} :: ImageResponseCard)

-- | The URL of an image to display on the response card. The image URL must
-- be publicly available so that the platform displaying the response card
-- has access to the image.
imageResponseCard_imageUrl :: Lens.Lens' ImageResponseCard (Prelude.Maybe Prelude.Text)
imageResponseCard_imageUrl = Lens.lens (\ImageResponseCard' {imageUrl} -> imageUrl) (\s@ImageResponseCard' {} a -> s {imageUrl = a} :: ImageResponseCard)

-- | The title to display on the response card. The format of the title is
-- determined by the platform displaying the response card.
imageResponseCard_title :: Lens.Lens' ImageResponseCard Prelude.Text
imageResponseCard_title = Lens.lens (\ImageResponseCard' {title} -> title) (\s@ImageResponseCard' {} a -> s {title = a} :: ImageResponseCard)

instance Core.FromJSON ImageResponseCard where
  parseJSON =
    Core.withObject
      "ImageResponseCard"
      ( \x ->
          ImageResponseCard'
            Prelude.<$> (x Core..:? "buttons" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "subtitle")
            Prelude.<*> (x Core..:? "imageUrl")
            Prelude.<*> (x Core..: "title")
      )

instance Prelude.Hashable ImageResponseCard

instance Prelude.NFData ImageResponseCard

instance Core.ToJSON ImageResponseCard where
  toJSON ImageResponseCard' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("buttons" Core..=) Prelude.<$> buttons,
            ("subtitle" Core..=) Prelude.<$> subtitle,
            ("imageUrl" Core..=) Prelude.<$> imageUrl,
            Prelude.Just ("title" Core..= title)
          ]
      )
