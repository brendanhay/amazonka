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
-- Module      : Amazonka.QuickSight.Types.PivotTableConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PivotTableConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PivotTableConditionalFormattingOption

-- | The conditional formatting for a @PivotTableVisual@.
--
-- /See:/ 'newPivotTableConditionalFormatting' smart constructor.
data PivotTableConditionalFormatting = PivotTableConditionalFormatting'
  { -- | Conditional formatting options for a @PivotTableVisual@.
    conditionalFormattingOptions :: Prelude.Maybe [PivotTableConditionalFormattingOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PivotTableConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalFormattingOptions', 'pivotTableConditionalFormatting_conditionalFormattingOptions' - Conditional formatting options for a @PivotTableVisual@.
newPivotTableConditionalFormatting ::
  PivotTableConditionalFormatting
newPivotTableConditionalFormatting =
  PivotTableConditionalFormatting'
    { conditionalFormattingOptions =
        Prelude.Nothing
    }

-- | Conditional formatting options for a @PivotTableVisual@.
pivotTableConditionalFormatting_conditionalFormattingOptions :: Lens.Lens' PivotTableConditionalFormatting (Prelude.Maybe [PivotTableConditionalFormattingOption])
pivotTableConditionalFormatting_conditionalFormattingOptions = Lens.lens (\PivotTableConditionalFormatting' {conditionalFormattingOptions} -> conditionalFormattingOptions) (\s@PivotTableConditionalFormatting' {} a -> s {conditionalFormattingOptions = a} :: PivotTableConditionalFormatting) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    PivotTableConditionalFormatting
  where
  parseJSON =
    Data.withObject
      "PivotTableConditionalFormatting"
      ( \x ->
          PivotTableConditionalFormatting'
            Prelude.<$> ( x Data..:? "ConditionalFormattingOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    PivotTableConditionalFormatting
  where
  hashWithSalt
    _salt
    PivotTableConditionalFormatting' {..} =
      _salt
        `Prelude.hashWithSalt` conditionalFormattingOptions

instance
  Prelude.NFData
    PivotTableConditionalFormatting
  where
  rnf PivotTableConditionalFormatting' {..} =
    Prelude.rnf conditionalFormattingOptions

instance Data.ToJSON PivotTableConditionalFormatting where
  toJSON PivotTableConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalFormattingOptions" Data..=)
              Prelude.<$> conditionalFormattingOptions
          ]
      )
