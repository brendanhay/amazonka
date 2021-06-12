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

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- /See:/ 'newScale' smart constructor.
data Scale = Scale'
  { -- | The unit of measure for the scale value.
    unit :: Core.Maybe ScaleUnit,
    -- | The value, specified as a percent total of a service\'s @desiredCount@,
    -- to scale the task set. Accepted values are numbers between 0 and 100.
    value :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Scale' {unit = Core.Nothing, value = Core.Nothing}

-- | The unit of measure for the scale value.
scale_unit :: Lens.Lens' Scale (Core.Maybe ScaleUnit)
scale_unit = Lens.lens (\Scale' {unit} -> unit) (\s@Scale' {} a -> s {unit = a} :: Scale)

-- | The value, specified as a percent total of a service\'s @desiredCount@,
-- to scale the task set. Accepted values are numbers between 0 and 100.
scale_value :: Lens.Lens' Scale (Core.Maybe Core.Double)
scale_value = Lens.lens (\Scale' {value} -> value) (\s@Scale' {} a -> s {value = a} :: Scale)

instance Core.FromJSON Scale where
  parseJSON =
    Core.withObject
      "Scale"
      ( \x ->
          Scale'
            Core.<$> (x Core..:? "unit") Core.<*> (x Core..:? "value")
      )

instance Core.Hashable Scale

instance Core.NFData Scale

instance Core.ToJSON Scale where
  toJSON Scale' {..} =
    Core.object
      ( Core.catMaybes
          [ ("unit" Core..=) Core.<$> unit,
            ("value" Core..=) Core.<$> value
          ]
      )
