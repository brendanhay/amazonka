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
-- Module      : Amazonka.QuickSight.Types.KPIConditionalFormattingOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIConditionalFormattingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.KPIPrimaryValueConditionalFormatting
import Amazonka.QuickSight.Types.KPIProgressBarConditionalFormatting

-- | The conditional formatting options of a KPI visual.
--
-- /See:/ 'newKPIConditionalFormattingOption' smart constructor.
data KPIConditionalFormattingOption = KPIConditionalFormattingOption'
  { -- | The conditional formatting for the primary value of a KPI visual.
    primaryValue :: Prelude.Maybe KPIPrimaryValueConditionalFormatting,
    -- | The conditional formatting for the progress bar of a KPI visual.
    progressBar :: Prelude.Maybe KPIProgressBarConditionalFormatting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIConditionalFormattingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryValue', 'kPIConditionalFormattingOption_primaryValue' - The conditional formatting for the primary value of a KPI visual.
--
-- 'progressBar', 'kPIConditionalFormattingOption_progressBar' - The conditional formatting for the progress bar of a KPI visual.
newKPIConditionalFormattingOption ::
  KPIConditionalFormattingOption
newKPIConditionalFormattingOption =
  KPIConditionalFormattingOption'
    { primaryValue =
        Prelude.Nothing,
      progressBar = Prelude.Nothing
    }

-- | The conditional formatting for the primary value of a KPI visual.
kPIConditionalFormattingOption_primaryValue :: Lens.Lens' KPIConditionalFormattingOption (Prelude.Maybe KPIPrimaryValueConditionalFormatting)
kPIConditionalFormattingOption_primaryValue = Lens.lens (\KPIConditionalFormattingOption' {primaryValue} -> primaryValue) (\s@KPIConditionalFormattingOption' {} a -> s {primaryValue = a} :: KPIConditionalFormattingOption)

-- | The conditional formatting for the progress bar of a KPI visual.
kPIConditionalFormattingOption_progressBar :: Lens.Lens' KPIConditionalFormattingOption (Prelude.Maybe KPIProgressBarConditionalFormatting)
kPIConditionalFormattingOption_progressBar = Lens.lens (\KPIConditionalFormattingOption' {progressBar} -> progressBar) (\s@KPIConditionalFormattingOption' {} a -> s {progressBar = a} :: KPIConditionalFormattingOption)

instance Data.FromJSON KPIConditionalFormattingOption where
  parseJSON =
    Data.withObject
      "KPIConditionalFormattingOption"
      ( \x ->
          KPIConditionalFormattingOption'
            Prelude.<$> (x Data..:? "PrimaryValue")
            Prelude.<*> (x Data..:? "ProgressBar")
      )

instance
  Prelude.Hashable
    KPIConditionalFormattingOption
  where
  hashWithSalt
    _salt
    KPIConditionalFormattingOption' {..} =
      _salt
        `Prelude.hashWithSalt` primaryValue
        `Prelude.hashWithSalt` progressBar

instance
  Prelude.NFData
    KPIConditionalFormattingOption
  where
  rnf KPIConditionalFormattingOption' {..} =
    Prelude.rnf primaryValue `Prelude.seq`
      Prelude.rnf progressBar

instance Data.ToJSON KPIConditionalFormattingOption where
  toJSON KPIConditionalFormattingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PrimaryValue" Data..=) Prelude.<$> primaryValue,
            ("ProgressBar" Data..=) Prelude.<$> progressBar
          ]
      )
