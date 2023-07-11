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
-- Module      : Amazonka.QuickSight.Types.KPIProgressBarConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIProgressBarConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ConditionalFormattingColor

-- | The conditional formatting for the progress bar of a KPI visual.
--
-- /See:/ 'newKPIProgressBarConditionalFormatting' smart constructor.
data KPIProgressBarConditionalFormatting = KPIProgressBarConditionalFormatting'
  { -- | The conditional formatting of the progress bar\'s foreground color.
    foregroundColor :: Prelude.Maybe ConditionalFormattingColor
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIProgressBarConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'foregroundColor', 'kPIProgressBarConditionalFormatting_foregroundColor' - The conditional formatting of the progress bar\'s foreground color.
newKPIProgressBarConditionalFormatting ::
  KPIProgressBarConditionalFormatting
newKPIProgressBarConditionalFormatting =
  KPIProgressBarConditionalFormatting'
    { foregroundColor =
        Prelude.Nothing
    }

-- | The conditional formatting of the progress bar\'s foreground color.
kPIProgressBarConditionalFormatting_foregroundColor :: Lens.Lens' KPIProgressBarConditionalFormatting (Prelude.Maybe ConditionalFormattingColor)
kPIProgressBarConditionalFormatting_foregroundColor = Lens.lens (\KPIProgressBarConditionalFormatting' {foregroundColor} -> foregroundColor) (\s@KPIProgressBarConditionalFormatting' {} a -> s {foregroundColor = a} :: KPIProgressBarConditionalFormatting)

instance
  Data.FromJSON
    KPIProgressBarConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "KPIProgressBarConditionalFormatting"
      ( \x ->
          KPIProgressBarConditionalFormatting'
            Prelude.<$> (x Data..:? "ForegroundColor")
      )

instance
  Prelude.Hashable
    KPIProgressBarConditionalFormatting
  where
  hashWithSalt
    _salt
    KPIProgressBarConditionalFormatting' {..} =
      _salt `Prelude.hashWithSalt` foregroundColor

instance
  Prelude.NFData
    KPIProgressBarConditionalFormatting
  where
  rnf KPIProgressBarConditionalFormatting' {..} =
    Prelude.rnf foregroundColor

instance
  Data.ToJSON
    KPIProgressBarConditionalFormatting
  where
  toJSON KPIProgressBarConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForegroundColor" Data..=)
              Prelude.<$> foregroundColor
          ]
      )
