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
-- Module      : Amazonka.GroundStation.Types.GroundStationData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.GroundStationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the ground station data.
--
-- /See:/ 'newGroundStationData' smart constructor.
data GroundStationData = GroundStationData'
  { -- | Name of a ground station.
    groundStationName :: Prelude.Maybe Prelude.Text,
    -- | Ground station Region.
    region :: Prelude.Maybe Prelude.Text,
    -- | UUID of a ground station.
    groundStationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroundStationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groundStationName', 'groundStationData_groundStationName' - Name of a ground station.
--
-- 'region', 'groundStationData_region' - Ground station Region.
--
-- 'groundStationId', 'groundStationData_groundStationId' - UUID of a ground station.
newGroundStationData ::
  GroundStationData
newGroundStationData =
  GroundStationData'
    { groundStationName =
        Prelude.Nothing,
      region = Prelude.Nothing,
      groundStationId = Prelude.Nothing
    }

-- | Name of a ground station.
groundStationData_groundStationName :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_groundStationName = Lens.lens (\GroundStationData' {groundStationName} -> groundStationName) (\s@GroundStationData' {} a -> s {groundStationName = a} :: GroundStationData)

-- | Ground station Region.
groundStationData_region :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_region = Lens.lens (\GroundStationData' {region} -> region) (\s@GroundStationData' {} a -> s {region = a} :: GroundStationData)

-- | UUID of a ground station.
groundStationData_groundStationId :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_groundStationId = Lens.lens (\GroundStationData' {groundStationId} -> groundStationId) (\s@GroundStationData' {} a -> s {groundStationId = a} :: GroundStationData)

instance Data.FromJSON GroundStationData where
  parseJSON =
    Data.withObject
      "GroundStationData"
      ( \x ->
          GroundStationData'
            Prelude.<$> (x Data..:? "groundStationName")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "groundStationId")
      )

instance Prelude.Hashable GroundStationData where
  hashWithSalt _salt GroundStationData' {..} =
    _salt `Prelude.hashWithSalt` groundStationName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` groundStationId

instance Prelude.NFData GroundStationData where
  rnf GroundStationData' {..} =
    Prelude.rnf groundStationName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf groundStationId
