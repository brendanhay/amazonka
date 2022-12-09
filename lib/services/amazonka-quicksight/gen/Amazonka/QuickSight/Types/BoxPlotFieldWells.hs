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
-- Module      : Amazonka.QuickSight.Types.BoxPlotFieldWells
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotFieldWells where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.BoxPlotAggregatedFieldWells

-- | The field wells of a @BoxPlotVisual@.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newBoxPlotFieldWells' smart constructor.
data BoxPlotFieldWells = BoxPlotFieldWells'
  { -- | The aggregated field wells of a box plot.
    boxPlotAggregatedFieldWells :: Prelude.Maybe BoxPlotAggregatedFieldWells
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotFieldWells' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boxPlotAggregatedFieldWells', 'boxPlotFieldWells_boxPlotAggregatedFieldWells' - The aggregated field wells of a box plot.
newBoxPlotFieldWells ::
  BoxPlotFieldWells
newBoxPlotFieldWells =
  BoxPlotFieldWells'
    { boxPlotAggregatedFieldWells =
        Prelude.Nothing
    }

-- | The aggregated field wells of a box plot.
boxPlotFieldWells_boxPlotAggregatedFieldWells :: Lens.Lens' BoxPlotFieldWells (Prelude.Maybe BoxPlotAggregatedFieldWells)
boxPlotFieldWells_boxPlotAggregatedFieldWells = Lens.lens (\BoxPlotFieldWells' {boxPlotAggregatedFieldWells} -> boxPlotAggregatedFieldWells) (\s@BoxPlotFieldWells' {} a -> s {boxPlotAggregatedFieldWells = a} :: BoxPlotFieldWells)

instance Data.FromJSON BoxPlotFieldWells where
  parseJSON =
    Data.withObject
      "BoxPlotFieldWells"
      ( \x ->
          BoxPlotFieldWells'
            Prelude.<$> (x Data..:? "BoxPlotAggregatedFieldWells")
      )

instance Prelude.Hashable BoxPlotFieldWells where
  hashWithSalt _salt BoxPlotFieldWells' {..} =
    _salt
      `Prelude.hashWithSalt` boxPlotAggregatedFieldWells

instance Prelude.NFData BoxPlotFieldWells where
  rnf BoxPlotFieldWells' {..} =
    Prelude.rnf boxPlotAggregatedFieldWells

instance Data.ToJSON BoxPlotFieldWells where
  toJSON BoxPlotFieldWells' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BoxPlotAggregatedFieldWells" Data..=)
              Prelude.<$> boxPlotAggregatedFieldWells
          ]
      )
