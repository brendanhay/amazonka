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
-- Module      : Amazonka.QuickSight.Types.ArcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ArcThicknessOptions

-- | The arc configuration of a @GaugeChartVisual@.
--
-- /See:/ 'newArcConfiguration' smart constructor.
data ArcConfiguration = ArcConfiguration'
  { -- | The option that determines the arc angle of a @GaugeChartVisual@.
    arcAngle :: Prelude.Maybe Prelude.Double,
    -- | The options that determine the arc thickness of a @GaugeChartVisual@.
    arcThickness :: Prelude.Maybe ArcThicknessOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arcAngle', 'arcConfiguration_arcAngle' - The option that determines the arc angle of a @GaugeChartVisual@.
--
-- 'arcThickness', 'arcConfiguration_arcThickness' - The options that determine the arc thickness of a @GaugeChartVisual@.
newArcConfiguration ::
  ArcConfiguration
newArcConfiguration =
  ArcConfiguration'
    { arcAngle = Prelude.Nothing,
      arcThickness = Prelude.Nothing
    }

-- | The option that determines the arc angle of a @GaugeChartVisual@.
arcConfiguration_arcAngle :: Lens.Lens' ArcConfiguration (Prelude.Maybe Prelude.Double)
arcConfiguration_arcAngle = Lens.lens (\ArcConfiguration' {arcAngle} -> arcAngle) (\s@ArcConfiguration' {} a -> s {arcAngle = a} :: ArcConfiguration)

-- | The options that determine the arc thickness of a @GaugeChartVisual@.
arcConfiguration_arcThickness :: Lens.Lens' ArcConfiguration (Prelude.Maybe ArcThicknessOptions)
arcConfiguration_arcThickness = Lens.lens (\ArcConfiguration' {arcThickness} -> arcThickness) (\s@ArcConfiguration' {} a -> s {arcThickness = a} :: ArcConfiguration)

instance Data.FromJSON ArcConfiguration where
  parseJSON =
    Data.withObject
      "ArcConfiguration"
      ( \x ->
          ArcConfiguration'
            Prelude.<$> (x Data..:? "ArcAngle")
            Prelude.<*> (x Data..:? "ArcThickness")
      )

instance Prelude.Hashable ArcConfiguration where
  hashWithSalt _salt ArcConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` arcAngle
      `Prelude.hashWithSalt` arcThickness

instance Prelude.NFData ArcConfiguration where
  rnf ArcConfiguration' {..} =
    Prelude.rnf arcAngle
      `Prelude.seq` Prelude.rnf arcThickness

instance Data.ToJSON ArcConfiguration where
  toJSON ArcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ArcAngle" Data..=) Prelude.<$> arcAngle,
            ("ArcThickness" Data..=) Prelude.<$> arcThickness
          ]
      )
