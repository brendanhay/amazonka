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
-- Module      : Amazonka.QuickSight.Types.FilledMapConditionalFormattingOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapConditionalFormattingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilledMapShapeConditionalFormatting

-- | Conditional formatting options of a @FilledMapVisual@.
--
-- /See:/ 'newFilledMapConditionalFormattingOption' smart constructor.
data FilledMapConditionalFormattingOption = FilledMapConditionalFormattingOption'
  { -- | The conditional formatting that determines the shape of the filled map.
    shape :: FilledMapShapeConditionalFormatting
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapConditionalFormattingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shape', 'filledMapConditionalFormattingOption_shape' - The conditional formatting that determines the shape of the filled map.
newFilledMapConditionalFormattingOption ::
  -- | 'shape'
  FilledMapShapeConditionalFormatting ->
  FilledMapConditionalFormattingOption
newFilledMapConditionalFormattingOption pShape_ =
  FilledMapConditionalFormattingOption'
    { shape =
        pShape_
    }

-- | The conditional formatting that determines the shape of the filled map.
filledMapConditionalFormattingOption_shape :: Lens.Lens' FilledMapConditionalFormattingOption FilledMapShapeConditionalFormatting
filledMapConditionalFormattingOption_shape = Lens.lens (\FilledMapConditionalFormattingOption' {shape} -> shape) (\s@FilledMapConditionalFormattingOption' {} a -> s {shape = a} :: FilledMapConditionalFormattingOption)

instance
  Data.FromJSON
    FilledMapConditionalFormattingOption
  where
  parseJSON =
    Data.withObject
      "FilledMapConditionalFormattingOption"
      ( \x ->
          FilledMapConditionalFormattingOption'
            Prelude.<$> (x Data..: "Shape")
      )

instance
  Prelude.Hashable
    FilledMapConditionalFormattingOption
  where
  hashWithSalt
    _salt
    FilledMapConditionalFormattingOption' {..} =
      _salt `Prelude.hashWithSalt` shape

instance
  Prelude.NFData
    FilledMapConditionalFormattingOption
  where
  rnf FilledMapConditionalFormattingOption' {..} =
    Prelude.rnf shape

instance
  Data.ToJSON
    FilledMapConditionalFormattingOption
  where
  toJSON FilledMapConditionalFormattingOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Shape" Data..= shape)]
      )
