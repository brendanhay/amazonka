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
-- Module      : Amazonka.QuickSight.Types.BinWidthOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BinWidthOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The options that determine the bin width of a histogram.
--
-- /See:/ 'newBinWidthOptions' smart constructor.
data BinWidthOptions = BinWidthOptions'
  { -- | The options that determine the bin count limit.
    binCountLimit :: Prelude.Maybe Prelude.Natural,
    -- | The options that determine the bin width value.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BinWidthOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binCountLimit', 'binWidthOptions_binCountLimit' - The options that determine the bin count limit.
--
-- 'value', 'binWidthOptions_value' - The options that determine the bin width value.
newBinWidthOptions ::
  BinWidthOptions
newBinWidthOptions =
  BinWidthOptions'
    { binCountLimit = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The options that determine the bin count limit.
binWidthOptions_binCountLimit :: Lens.Lens' BinWidthOptions (Prelude.Maybe Prelude.Natural)
binWidthOptions_binCountLimit = Lens.lens (\BinWidthOptions' {binCountLimit} -> binCountLimit) (\s@BinWidthOptions' {} a -> s {binCountLimit = a} :: BinWidthOptions)

-- | The options that determine the bin width value.
binWidthOptions_value :: Lens.Lens' BinWidthOptions (Prelude.Maybe Prelude.Double)
binWidthOptions_value = Lens.lens (\BinWidthOptions' {value} -> value) (\s@BinWidthOptions' {} a -> s {value = a} :: BinWidthOptions)

instance Data.FromJSON BinWidthOptions where
  parseJSON =
    Data.withObject
      "BinWidthOptions"
      ( \x ->
          BinWidthOptions'
            Prelude.<$> (x Data..:? "BinCountLimit")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable BinWidthOptions where
  hashWithSalt _salt BinWidthOptions' {..} =
    _salt
      `Prelude.hashWithSalt` binCountLimit
      `Prelude.hashWithSalt` value

instance Prelude.NFData BinWidthOptions where
  rnf BinWidthOptions' {..} =
    Prelude.rnf binCountLimit
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON BinWidthOptions where
  toJSON BinWidthOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BinCountLimit" Data..=) Prelude.<$> binCountLimit,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
