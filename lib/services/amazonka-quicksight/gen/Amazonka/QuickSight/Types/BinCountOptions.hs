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
-- Module      : Amazonka.QuickSight.Types.BinCountOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BinCountOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options that determine the bin count of a histogram.
--
-- /See:/ 'newBinCountOptions' smart constructor.
data BinCountOptions = BinCountOptions'
  { -- | The options that determine the bin count value.
    value :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BinCountOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'binCountOptions_value' - The options that determine the bin count value.
newBinCountOptions ::
  BinCountOptions
newBinCountOptions =
  BinCountOptions' {value = Prelude.Nothing}

-- | The options that determine the bin count value.
binCountOptions_value :: Lens.Lens' BinCountOptions (Prelude.Maybe Prelude.Natural)
binCountOptions_value = Lens.lens (\BinCountOptions' {value} -> value) (\s@BinCountOptions' {} a -> s {value = a} :: BinCountOptions)

instance Data.FromJSON BinCountOptions where
  parseJSON =
    Data.withObject
      "BinCountOptions"
      ( \x ->
          BinCountOptions' Prelude.<$> (x Data..:? "Value")
      )

instance Prelude.Hashable BinCountOptions where
  hashWithSalt _salt BinCountOptions' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData BinCountOptions where
  rnf BinCountOptions' {..} = Prelude.rnf value

instance Data.ToJSON BinCountOptions where
  toJSON BinCountOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Value" Data..=) Prelude.<$> value]
      )
