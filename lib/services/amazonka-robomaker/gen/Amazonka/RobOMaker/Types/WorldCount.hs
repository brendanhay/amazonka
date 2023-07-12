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
-- Module      : Amazonka.RobOMaker.Types.WorldCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The number of worlds that will be created. You can configure the number
-- of unique floorplans and the number of unique interiors for each floor
-- plan. For example, if you want 1 world with 20 unique interiors, you set
-- @floorplanCount = 1@ and @interiorCountPerFloorplan = 20@. This will
-- result in 20 worlds (@floorplanCount@ * @interiorCountPerFloorplan)@.
--
-- If you set @floorplanCount = 4@ and @interiorCountPerFloorplan = 5@,
-- there will be 20 worlds with 5 unique floor plans.
--
-- /See:/ 'newWorldCount' smart constructor.
data WorldCount = WorldCount'
  { -- | The number of unique floorplans.
    floorplanCount :: Prelude.Maybe Prelude.Int,
    -- | The number of unique interiors per floorplan.
    interiorCountPerFloorplan :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'floorplanCount', 'worldCount_floorplanCount' - The number of unique floorplans.
--
-- 'interiorCountPerFloorplan', 'worldCount_interiorCountPerFloorplan' - The number of unique interiors per floorplan.
newWorldCount ::
  WorldCount
newWorldCount =
  WorldCount'
    { floorplanCount = Prelude.Nothing,
      interiorCountPerFloorplan = Prelude.Nothing
    }

-- | The number of unique floorplans.
worldCount_floorplanCount :: Lens.Lens' WorldCount (Prelude.Maybe Prelude.Int)
worldCount_floorplanCount = Lens.lens (\WorldCount' {floorplanCount} -> floorplanCount) (\s@WorldCount' {} a -> s {floorplanCount = a} :: WorldCount)

-- | The number of unique interiors per floorplan.
worldCount_interiorCountPerFloorplan :: Lens.Lens' WorldCount (Prelude.Maybe Prelude.Int)
worldCount_interiorCountPerFloorplan = Lens.lens (\WorldCount' {interiorCountPerFloorplan} -> interiorCountPerFloorplan) (\s@WorldCount' {} a -> s {interiorCountPerFloorplan = a} :: WorldCount)

instance Data.FromJSON WorldCount where
  parseJSON =
    Data.withObject
      "WorldCount"
      ( \x ->
          WorldCount'
            Prelude.<$> (x Data..:? "floorplanCount")
            Prelude.<*> (x Data..:? "interiorCountPerFloorplan")
      )

instance Prelude.Hashable WorldCount where
  hashWithSalt _salt WorldCount' {..} =
    _salt
      `Prelude.hashWithSalt` floorplanCount
      `Prelude.hashWithSalt` interiorCountPerFloorplan

instance Prelude.NFData WorldCount where
  rnf WorldCount' {..} =
    Prelude.rnf floorplanCount
      `Prelude.seq` Prelude.rnf interiorCountPerFloorplan

instance Data.ToJSON WorldCount where
  toJSON WorldCount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("floorplanCount" Data..=)
              Prelude.<$> floorplanCount,
            ("interiorCountPerFloorplan" Data..=)
              Prelude.<$> interiorCountPerFloorplan
          ]
      )
