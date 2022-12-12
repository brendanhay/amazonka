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
-- Module      : Amazonka.QuickSight.Types.ArcOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ArcThickness

-- | The options that determine the arc thickness of a @GaugeChartVisual@.
--
-- /See:/ 'newArcOptions' smart constructor.
data ArcOptions = ArcOptions'
  { -- | The arc thickness of a @GaugeChartVisual@.
    arcThickness :: Prelude.Maybe ArcThickness
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ArcOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arcThickness', 'arcOptions_arcThickness' - The arc thickness of a @GaugeChartVisual@.
newArcOptions ::
  ArcOptions
newArcOptions =
  ArcOptions' {arcThickness = Prelude.Nothing}

-- | The arc thickness of a @GaugeChartVisual@.
arcOptions_arcThickness :: Lens.Lens' ArcOptions (Prelude.Maybe ArcThickness)
arcOptions_arcThickness = Lens.lens (\ArcOptions' {arcThickness} -> arcThickness) (\s@ArcOptions' {} a -> s {arcThickness = a} :: ArcOptions)

instance Data.FromJSON ArcOptions where
  parseJSON =
    Data.withObject
      "ArcOptions"
      ( \x ->
          ArcOptions' Prelude.<$> (x Data..:? "ArcThickness")
      )

instance Prelude.Hashable ArcOptions where
  hashWithSalt _salt ArcOptions' {..} =
    _salt `Prelude.hashWithSalt` arcThickness

instance Prelude.NFData ArcOptions where
  rnf ArcOptions' {..} = Prelude.rnf arcThickness

instance Data.ToJSON ArcOptions where
  toJSON ArcOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ArcThickness" Data..=) Prelude.<$> arcThickness]
      )
