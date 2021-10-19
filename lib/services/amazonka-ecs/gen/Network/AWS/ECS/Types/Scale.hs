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
-- Module      : Network.AWS.ECS.Types.Scale
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Scale where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.ScaleUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- /See:/ 'newScale' smart constructor.
data Scale = Scale'
  { -- | The value, specified as a percent total of a service\'s @desiredCount@,
    -- to scale the task set. Accepted values are numbers between 0 and 100.
    value :: Prelude.Maybe Prelude.Double,
    -- | The unit of measure for the scale value.
    unit :: Prelude.Maybe ScaleUnit
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
-- 'value', 'scale_value' - The value, specified as a percent total of a service\'s @desiredCount@,
-- to scale the task set. Accepted values are numbers between 0 and 100.
--
-- 'unit', 'scale_unit' - The unit of measure for the scale value.
newScale ::
  Scale
newScale =
  Scale'
    { value = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The value, specified as a percent total of a service\'s @desiredCount@,
-- to scale the task set. Accepted values are numbers between 0 and 100.
scale_value :: Lens.Lens' Scale (Prelude.Maybe Prelude.Double)
scale_value = Lens.lens (\Scale' {value} -> value) (\s@Scale' {} a -> s {value = a} :: Scale)

-- | The unit of measure for the scale value.
scale_unit :: Lens.Lens' Scale (Prelude.Maybe ScaleUnit)
scale_unit = Lens.lens (\Scale' {unit} -> unit) (\s@Scale' {} a -> s {unit = a} :: Scale)

instance Core.FromJSON Scale where
  parseJSON =
    Core.withObject
      "Scale"
      ( \x ->
          Scale'
            Prelude.<$> (x Core..:? "value") Prelude.<*> (x Core..:? "unit")
      )

instance Prelude.Hashable Scale

instance Prelude.NFData Scale

instance Core.ToJSON Scale where
  toJSON Scale' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("unit" Core..=) Prelude.<$> unit
          ]
      )
