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
-- Module      : Amazonka.QuickSight.Types.KPIPrimaryValueConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIPrimaryValueConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor
import Amazonka.QuickSight.Types.ConditionalFormattingIcon

-- | The conditional formatting for the primary value of a KPI visual.
--
-- /See:/ 'newKPIPrimaryValueConditionalFormatting' smart constructor.
data KPIPrimaryValueConditionalFormatting = KPIPrimaryValueConditionalFormatting'
  { -- | The conditional formatting of the primary value\'s icon.
    icon :: Prelude.Maybe ConditionalFormattingIcon,
    -- | The conditional formatting of the primary value\'s text color.
    textColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIPrimaryValueConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'icon', 'kPIPrimaryValueConditionalFormatting_icon' - The conditional formatting of the primary value\'s icon.
--
-- 'textColor', 'kPIPrimaryValueConditionalFormatting_textColor' - The conditional formatting of the primary value\'s text color.
newKPIPrimaryValueConditionalFormatting ::
  KPIPrimaryValueConditionalFormatting
newKPIPrimaryValueConditionalFormatting =
  KPIPrimaryValueConditionalFormatting'
    { icon =
        Prelude.Nothing,
      textColor = Prelude.Nothing
    }

-- | The conditional formatting of the primary value\'s icon.
kPIPrimaryValueConditionalFormatting_icon :: Lens.Lens' KPIPrimaryValueConditionalFormatting (Prelude.Maybe ConditionalFormattingIcon)
kPIPrimaryValueConditionalFormatting_icon = Lens.lens (\KPIPrimaryValueConditionalFormatting' {icon} -> icon) (\s@KPIPrimaryValueConditionalFormatting' {} a -> s {icon = a} :: KPIPrimaryValueConditionalFormatting)

-- | The conditional formatting of the primary value\'s text color.
kPIPrimaryValueConditionalFormatting_textColor :: Lens.Lens' KPIPrimaryValueConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
kPIPrimaryValueConditionalFormatting_textColor = Lens.lens (\KPIPrimaryValueConditionalFormatting' {textColor} -> textColor) (\s@KPIPrimaryValueConditionalFormatting' {} a -> s {textColor = a} :: KPIPrimaryValueConditionalFormatting)

instance
  Data.FromJSON
    KPIPrimaryValueConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "KPIPrimaryValueConditionalFormatting"
      ( \x ->
          KPIPrimaryValueConditionalFormatting'
            Prelude.<$> (x Data..:? "Icon")
            Prelude.<*> (x Data..:? "TextColor")
      )

instance
  Prelude.Hashable
    KPIPrimaryValueConditionalFormatting
  where
  hashWithSalt
    _salt
    KPIPrimaryValueConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` icon
        `Prelude.hashWithSalt` textColor

instance
  Prelude.NFData
    KPIPrimaryValueConditionalFormatting
  where
  rnf KPIPrimaryValueConditionalFormatting' {..} =
    Prelude.rnf icon
      `Prelude.seq` Prelude.rnf textColor

instance
  Data.ToJSON
    KPIPrimaryValueConditionalFormatting
  where
  toJSON KPIPrimaryValueConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Icon" Data..=) Prelude.<$> icon,
            ("TextColor" Data..=) Prelude.<$> textColor
          ]
      )
