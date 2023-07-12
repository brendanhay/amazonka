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
-- Module      : Amazonka.XRay.Types.HistogramEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.HistogramEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An entry in a histogram for a statistic. A histogram maps the range of
-- observed values on the X axis, and the prevalence of each value on the Y
-- axis.
--
-- /See:/ 'newHistogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { -- | The prevalence of the entry.
    count :: Prelude.Maybe Prelude.Int,
    -- | The value of the entry.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistogramEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'histogramEntry_count' - The prevalence of the entry.
--
-- 'value', 'histogramEntry_value' - The value of the entry.
newHistogramEntry ::
  HistogramEntry
newHistogramEntry =
  HistogramEntry'
    { count = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The prevalence of the entry.
histogramEntry_count :: Lens.Lens' HistogramEntry (Prelude.Maybe Prelude.Int)
histogramEntry_count = Lens.lens (\HistogramEntry' {count} -> count) (\s@HistogramEntry' {} a -> s {count = a} :: HistogramEntry)

-- | The value of the entry.
histogramEntry_value :: Lens.Lens' HistogramEntry (Prelude.Maybe Prelude.Double)
histogramEntry_value = Lens.lens (\HistogramEntry' {value} -> value) (\s@HistogramEntry' {} a -> s {value = a} :: HistogramEntry)

instance Data.FromJSON HistogramEntry where
  parseJSON =
    Data.withObject
      "HistogramEntry"
      ( \x ->
          HistogramEntry'
            Prelude.<$> (x Data..:? "Count")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable HistogramEntry where
  hashWithSalt _salt HistogramEntry' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` value

instance Prelude.NFData HistogramEntry where
  rnf HistogramEntry' {..} =
    Prelude.rnf count `Prelude.seq` Prelude.rnf value
