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
-- Module      : Amazonka.ECS.Types.Scale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Scale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ScaleUnit
import qualified Amazonka.Prelude as Prelude

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- /See:/ 'newScale' smart constructor.
data Scale = Scale'
  { -- | The unit of measure for the scale value.
    unit :: Prelude.Maybe ScaleUnit,
    -- | The value, specified as a percent total of a service\'s @desiredCount@,
    -- to scale the task set. Accepted values are numbers between 0 and 100.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unit', 'scale_unit' - The unit of measure for the scale value.
--
-- 'value', 'scale_value' - The value, specified as a percent total of a service\'s @desiredCount@,
-- to scale the task set. Accepted values are numbers between 0 and 100.
newScale ::
  Scale
newScale =
  Scale'
    { unit = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The unit of measure for the scale value.
scale_unit :: Lens.Lens' Scale (Prelude.Maybe ScaleUnit)
scale_unit = Lens.lens (\Scale' {unit} -> unit) (\s@Scale' {} a -> s {unit = a} :: Scale)

-- | The value, specified as a percent total of a service\'s @desiredCount@,
-- to scale the task set. Accepted values are numbers between 0 and 100.
scale_value :: Lens.Lens' Scale (Prelude.Maybe Prelude.Double)
scale_value = Lens.lens (\Scale' {value} -> value) (\s@Scale' {} a -> s {value = a} :: Scale)

instance Data.FromJSON Scale where
  parseJSON =
    Data.withObject
      "Scale"
      ( \x ->
          Scale'
            Prelude.<$> (x Data..:? "unit") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Scale where
  hashWithSalt _salt Scale' {..} =
    _salt `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` value

instance Prelude.NFData Scale where
  rnf Scale' {..} =
    Prelude.rnf unit `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Scale where
  toJSON Scale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("unit" Data..=) Prelude.<$> unit,
            ("value" Data..=) Prelude.<$> value
          ]
      )
