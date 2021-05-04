{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.Types.HistogramEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.HistogramEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An entry in a histogram for a statistic. A histogram maps the range of
-- observed values on the X axis, and the prevalence of each value on the Y
-- axis.
--
-- /See:/ 'newHistogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { -- | The value of the entry.
    value :: Prelude.Maybe Prelude.Double,
    -- | The prevalence of the entry.
    count :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HistogramEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'histogramEntry_value' - The value of the entry.
--
-- 'count', 'histogramEntry_count' - The prevalence of the entry.
newHistogramEntry ::
  HistogramEntry
newHistogramEntry =
  HistogramEntry'
    { value = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The value of the entry.
histogramEntry_value :: Lens.Lens' HistogramEntry (Prelude.Maybe Prelude.Double)
histogramEntry_value = Lens.lens (\HistogramEntry' {value} -> value) (\s@HistogramEntry' {} a -> s {value = a} :: HistogramEntry)

-- | The prevalence of the entry.
histogramEntry_count :: Lens.Lens' HistogramEntry (Prelude.Maybe Prelude.Int)
histogramEntry_count = Lens.lens (\HistogramEntry' {count} -> count) (\s@HistogramEntry' {} a -> s {count = a} :: HistogramEntry)

instance Prelude.FromJSON HistogramEntry where
  parseJSON =
    Prelude.withObject
      "HistogramEntry"
      ( \x ->
          HistogramEntry'
            Prelude.<$> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "Count")
      )

instance Prelude.Hashable HistogramEntry

instance Prelude.NFData HistogramEntry
