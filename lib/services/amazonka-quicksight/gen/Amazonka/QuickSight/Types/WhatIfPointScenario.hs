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
-- Module      : Amazonka.QuickSight.Types.WhatIfPointScenario
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WhatIfPointScenario where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the forecast to meet the target for a particular date.
--
-- /See:/ 'newWhatIfPointScenario' smart constructor.
data WhatIfPointScenario = WhatIfPointScenario'
  { -- | The date that you need the forecast results for.
    date :: Data.POSIX,
    -- | The target value that you want to meet for the provided date.
    value :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WhatIfPointScenario' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'date', 'whatIfPointScenario_date' - The date that you need the forecast results for.
--
-- 'value', 'whatIfPointScenario_value' - The target value that you want to meet for the provided date.
newWhatIfPointScenario ::
  -- | 'date'
  Prelude.UTCTime ->
  -- | 'value'
  Prelude.Double ->
  WhatIfPointScenario
newWhatIfPointScenario pDate_ pValue_ =
  WhatIfPointScenario'
    { date =
        Data._Time Lens.# pDate_,
      value = pValue_
    }

-- | The date that you need the forecast results for.
whatIfPointScenario_date :: Lens.Lens' WhatIfPointScenario Prelude.UTCTime
whatIfPointScenario_date = Lens.lens (\WhatIfPointScenario' {date} -> date) (\s@WhatIfPointScenario' {} a -> s {date = a} :: WhatIfPointScenario) Prelude.. Data._Time

-- | The target value that you want to meet for the provided date.
whatIfPointScenario_value :: Lens.Lens' WhatIfPointScenario Prelude.Double
whatIfPointScenario_value = Lens.lens (\WhatIfPointScenario' {value} -> value) (\s@WhatIfPointScenario' {} a -> s {value = a} :: WhatIfPointScenario)

instance Data.FromJSON WhatIfPointScenario where
  parseJSON =
    Data.withObject
      "WhatIfPointScenario"
      ( \x ->
          WhatIfPointScenario'
            Prelude.<$> (x Data..: "Date") Prelude.<*> (x Data..: "Value")
      )

instance Prelude.Hashable WhatIfPointScenario where
  hashWithSalt _salt WhatIfPointScenario' {..} =
    _salt `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` value

instance Prelude.NFData WhatIfPointScenario where
  rnf WhatIfPointScenario' {..} =
    Prelude.rnf date `Prelude.seq` Prelude.rnf value

instance Data.ToJSON WhatIfPointScenario where
  toJSON WhatIfPointScenario' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Date" Data..= date),
            Prelude.Just ("Value" Data..= value)
          ]
      )
