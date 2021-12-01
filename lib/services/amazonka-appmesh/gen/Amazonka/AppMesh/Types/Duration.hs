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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.Duration where

import Amazonka.AppMesh.Types.DurationUnit
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a duration of time.
--
-- /See:/ 'newDuration' smart constructor.
data Duration = Duration'
  { -- | A number of time units.
    value :: Prelude.Maybe Prelude.Natural,
    -- | A unit of time.
    unit :: Prelude.Maybe DurationUnit
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
-- 'value', 'duration_value' - A number of time units.
--
-- 'unit', 'duration_unit' - A unit of time.
newDuration ::
  Duration
newDuration =
  Duration'
    { value = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | A number of time units.
duration_value :: Lens.Lens' Duration (Prelude.Maybe Prelude.Natural)
duration_value = Lens.lens (\Duration' {value} -> value) (\s@Duration' {} a -> s {value = a} :: Duration)

-- | A unit of time.
duration_unit :: Lens.Lens' Duration (Prelude.Maybe DurationUnit)
duration_unit = Lens.lens (\Duration' {unit} -> unit) (\s@Duration' {} a -> s {unit = a} :: Duration)

instance Core.FromJSON Duration where
  parseJSON =
    Core.withObject
      "Duration"
      ( \x ->
          Duration'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "unit")
      )

instance Prelude.Hashable Duration where
  hashWithSalt salt' Duration' {..} =
    salt' `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Duration where
  rnf Duration' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf unit

instance Core.ToJSON Duration where
  toJSON Duration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("unit" Core..=) Prelude.<$> unit
          ]
      )
