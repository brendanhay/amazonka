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
-- Module      : Amazonka.IoT.Types.PercentPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.PercentPair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the percentile and percentile value.
--
-- /See:/ 'newPercentPair' smart constructor.
data PercentPair = PercentPair'
  { -- | The value of the percentile.
    value :: Prelude.Maybe Prelude.Double,
    -- | The percentile.
    percent :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PercentPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'percentPair_value' - The value of the percentile.
--
-- 'percent', 'percentPair_percent' - The percentile.
newPercentPair ::
  PercentPair
newPercentPair =
  PercentPair'
    { value = Prelude.Nothing,
      percent = Prelude.Nothing
    }

-- | The value of the percentile.
percentPair_value :: Lens.Lens' PercentPair (Prelude.Maybe Prelude.Double)
percentPair_value = Lens.lens (\PercentPair' {value} -> value) (\s@PercentPair' {} a -> s {value = a} :: PercentPair)

-- | The percentile.
percentPair_percent :: Lens.Lens' PercentPair (Prelude.Maybe Prelude.Double)
percentPair_percent = Lens.lens (\PercentPair' {percent} -> percent) (\s@PercentPair' {} a -> s {percent = a} :: PercentPair)

instance Core.FromJSON PercentPair where
  parseJSON =
    Core.withObject
      "PercentPair"
      ( \x ->
          PercentPair'
            Prelude.<$> (x Core..:? "value")
            Prelude.<*> (x Core..:? "percent")
      )

instance Prelude.Hashable PercentPair where
  hashWithSalt salt' PercentPair' {..} =
    salt' `Prelude.hashWithSalt` percent
      `Prelude.hashWithSalt` value

instance Prelude.NFData PercentPair where
  rnf PercentPair' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf percent
