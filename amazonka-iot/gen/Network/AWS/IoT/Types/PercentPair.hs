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
-- Module      : Network.AWS.IoT.Types.PercentPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PercentPair where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the percentile and percentile value.
--
-- /See:/ 'newPercentPair' smart constructor.
data PercentPair = PercentPair'
  { -- | The percentile.
    percent :: Prelude.Maybe Prelude.Double,
    -- | The value of the percentile.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PercentPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percent', 'percentPair_percent' - The percentile.
--
-- 'value', 'percentPair_value' - The value of the percentile.
newPercentPair ::
  PercentPair
newPercentPair =
  PercentPair'
    { percent = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The percentile.
percentPair_percent :: Lens.Lens' PercentPair (Prelude.Maybe Prelude.Double)
percentPair_percent = Lens.lens (\PercentPair' {percent} -> percent) (\s@PercentPair' {} a -> s {percent = a} :: PercentPair)

-- | The value of the percentile.
percentPair_value :: Lens.Lens' PercentPair (Prelude.Maybe Prelude.Double)
percentPair_value = Lens.lens (\PercentPair' {value} -> value) (\s@PercentPair' {} a -> s {value = a} :: PercentPair)

instance Prelude.FromJSON PercentPair where
  parseJSON =
    Prelude.withObject
      "PercentPair"
      ( \x ->
          PercentPair'
            Prelude.<$> (x Prelude..:? "percent")
            Prelude.<*> (x Prelude..:? "value")
      )

instance Prelude.Hashable PercentPair

instance Prelude.NFData PercentPair
