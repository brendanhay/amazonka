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
-- Module      : Amazonka.QuickSight.Types.TextConditionalFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TextConditionalFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor
import Amazonka.QuickSight.Types.ConditionalFormattingIcon

-- | The conditional formatting for the text.
--
-- /See:/ 'newTextConditionalFormat' smart constructor.
data TextConditionalFormat = TextConditionalFormat'
  { -- | The conditional formatting for the text background color.
    backgroundColor :: Prelude.Maybe ConditionalFormattingColor,
    -- | The conditional formatting for the icon.
    icon :: Prelude.Maybe ConditionalFormattingIcon,
    -- | The conditional formatting for the text color.
    textColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextConditionalFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundColor', 'textConditionalFormat_backgroundColor' - The conditional formatting for the text background color.
--
-- 'icon', 'textConditionalFormat_icon' - The conditional formatting for the icon.
--
-- 'textColor', 'textConditionalFormat_textColor' - The conditional formatting for the text color.
newTextConditionalFormat ::
  TextConditionalFormat
newTextConditionalFormat =
  TextConditionalFormat'
    { backgroundColor =
        Prelude.Nothing,
      icon = Prelude.Nothing,
      textColor = Prelude.Nothing
    }

-- | The conditional formatting for the text background color.
textConditionalFormat_backgroundColor :: Lens.Lens' TextConditionalFormat (Prelude.Maybe ConditionalFormattingColor)
textConditionalFormat_backgroundColor = Lens.lens (\TextConditionalFormat' {backgroundColor} -> backgroundColor) (\s@TextConditionalFormat' {} a -> s {backgroundColor = a} :: TextConditionalFormat)

-- | The conditional formatting for the icon.
textConditionalFormat_icon :: Lens.Lens' TextConditionalFormat (Prelude.Maybe ConditionalFormattingIcon)
textConditionalFormat_icon = Lens.lens (\TextConditionalFormat' {icon} -> icon) (\s@TextConditionalFormat' {} a -> s {icon = a} :: TextConditionalFormat)

-- | The conditional formatting for the text color.
textConditionalFormat_textColor :: Lens.Lens' TextConditionalFormat (Prelude.Maybe ConditionalFormattingColor)
textConditionalFormat_textColor = Lens.lens (\TextConditionalFormat' {textColor} -> textColor) (\s@TextConditionalFormat' {} a -> s {textColor = a} :: TextConditionalFormat)

instance Data.FromJSON TextConditionalFormat where
  parseJSON =
    Data.withObject
      "TextConditionalFormat"
      ( \x ->
          TextConditionalFormat'
            Prelude.<$> (x Data..:? "BackgroundColor")
            Prelude.<*> (x Data..:? "Icon")
            Prelude.<*> (x Data..:? "TextColor")
      )

instance Prelude.Hashable TextConditionalFormat where
  hashWithSalt _salt TextConditionalFormat' {..} =
    _salt
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` icon
      `Prelude.hashWithSalt` textColor

instance Prelude.NFData TextConditionalFormat where
  rnf TextConditionalFormat' {..} =
    Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf icon
      `Prelude.seq` Prelude.rnf textColor

instance Data.ToJSON TextConditionalFormat where
  toJSON TextConditionalFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundColor" Data..=)
              Prelude.<$> backgroundColor,
            ("Icon" Data..=) Prelude.<$> icon,
            ("TextColor" Data..=) Prelude.<$> textColor
          ]
      )
