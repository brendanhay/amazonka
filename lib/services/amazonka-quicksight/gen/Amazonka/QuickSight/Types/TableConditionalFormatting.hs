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
-- Module      : Amazonka.QuickSight.Types.TableConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableConditionalFormattingOption

-- | The conditional formatting for a @PivotTableVisual@.
--
-- /See:/ 'newTableConditionalFormatting' smart constructor.
data TableConditionalFormatting = TableConditionalFormatting'
  { -- | Conditional formatting options for a @PivotTableVisual@.
    conditionalFormattingOptions :: Prelude.Maybe [TableConditionalFormattingOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalFormattingOptions', 'tableConditionalFormatting_conditionalFormattingOptions' - Conditional formatting options for a @PivotTableVisual@.
newTableConditionalFormatting ::
  TableConditionalFormatting
newTableConditionalFormatting =
  TableConditionalFormatting'
    { conditionalFormattingOptions =
        Prelude.Nothing
    }

-- | Conditional formatting options for a @PivotTableVisual@.
tableConditionalFormatting_conditionalFormattingOptions :: Lens.Lens' TableConditionalFormatting (Prelude.Maybe [TableConditionalFormattingOption])
tableConditionalFormatting_conditionalFormattingOptions = Lens.lens (\TableConditionalFormatting' {conditionalFormattingOptions} -> conditionalFormattingOptions) (\s@TableConditionalFormatting' {} a -> s {conditionalFormattingOptions = a} :: TableConditionalFormatting) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON TableConditionalFormatting where
  parseJSON =
    Data.withObject
      "TableConditionalFormatting"
      ( \x ->
          TableConditionalFormatting'
            Prelude.<$> ( x Data..:? "ConditionalFormattingOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TableConditionalFormatting where
  hashWithSalt _salt TableConditionalFormatting' {..} =
    _salt
      `Prelude.hashWithSalt` conditionalFormattingOptions

instance Prelude.NFData TableConditionalFormatting where
  rnf TableConditionalFormatting' {..} =
    Prelude.rnf conditionalFormattingOptions

instance Data.ToJSON TableConditionalFormatting where
  toJSON TableConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalFormattingOptions" Data..=)
              Prelude.<$> conditionalFormattingOptions
          ]
      )
