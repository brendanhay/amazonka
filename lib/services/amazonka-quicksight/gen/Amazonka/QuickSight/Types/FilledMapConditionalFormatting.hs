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
-- Module      : Amazonka.QuickSight.Types.FilledMapConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilledMapConditionalFormattingOption

-- | The conditional formatting of a @FilledMapVisual@.
--
-- /See:/ 'newFilledMapConditionalFormatting' smart constructor.
data FilledMapConditionalFormatting = FilledMapConditionalFormatting'
  { -- | Conditional formatting options of a @FilledMapVisual@.
    conditionalFormattingOptions :: [FilledMapConditionalFormattingOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalFormattingOptions', 'filledMapConditionalFormatting_conditionalFormattingOptions' - Conditional formatting options of a @FilledMapVisual@.
newFilledMapConditionalFormatting ::
  FilledMapConditionalFormatting
newFilledMapConditionalFormatting =
  FilledMapConditionalFormatting'
    { conditionalFormattingOptions =
        Prelude.mempty
    }

-- | Conditional formatting options of a @FilledMapVisual@.
filledMapConditionalFormatting_conditionalFormattingOptions :: Lens.Lens' FilledMapConditionalFormatting [FilledMapConditionalFormattingOption]
filledMapConditionalFormatting_conditionalFormattingOptions = Lens.lens (\FilledMapConditionalFormatting' {conditionalFormattingOptions} -> conditionalFormattingOptions) (\s@FilledMapConditionalFormatting' {} a -> s {conditionalFormattingOptions = a} :: FilledMapConditionalFormatting) Prelude.. Lens.coerced

instance Data.FromJSON FilledMapConditionalFormatting where
  parseJSON =
    Data.withObject
      "FilledMapConditionalFormatting"
      ( \x ->
          FilledMapConditionalFormatting'
            Prelude.<$> ( x
                            Data..:? "ConditionalFormattingOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    FilledMapConditionalFormatting
  where
  hashWithSalt
    _salt
    FilledMapConditionalFormatting' {..} =
      _salt
        `Prelude.hashWithSalt` conditionalFormattingOptions

instance
  Prelude.NFData
    FilledMapConditionalFormatting
  where
  rnf FilledMapConditionalFormatting' {..} =
    Prelude.rnf conditionalFormattingOptions

instance Data.ToJSON FilledMapConditionalFormatting where
  toJSON FilledMapConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConditionalFormattingOptions"
                  Data..= conditionalFormattingOptions
              )
          ]
      )
