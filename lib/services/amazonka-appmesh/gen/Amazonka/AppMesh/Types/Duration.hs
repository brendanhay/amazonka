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
-- Module      : Amazonka.AppMesh.Types.Duration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.Duration where

import Amazonka.AppMesh.Types.DurationUnit
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a duration of time.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | A unit of time.
    unit :: Prelude.Maybe DurationUnit,
    -- | A number of time units.
    value :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Duration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'duration_unit' - A unit of time.
--
-- 'value', 'duration_value' - A number of time units.
newDuration ::
  Duration
newDuration =
  Duration'
    { unit = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A unit of time.
duration_unit :: Lens.Lens' Duration (Prelude.Maybe DurationUnit)
duration_unit = Lens.lens (\Duration' {unit} -> unit) (\s@Duration' {} a -> s {unit = a} :: Duration)

-- | A number of time units.
duration_value :: Lens.Lens' Duration (Prelude.Maybe Prelude.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

instance Core.FromJSON Duration where
  parseJSON =
    Core.withObject
      "Duration"
      ( \x ->
          Duration'
            Prelude.<$> (x Core..:? "unit") Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable Duration where
  hashWithSalt _salt Duration' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Duration where
  rnf Duration' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Duration where
  toJSON Duration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unit" Core..=) Prelude.<$> unit,
            ("value" Core..=) Prelude.<$> value
          ]
      )
