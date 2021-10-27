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
-- Module      : Network.AWS.GroundStation.Types.GroundStationData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GroundStation.Types.GroundStationData where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the ground station data.
--
-- /See:/ 'newGroundStationData' smart constructor.
data GroundStationData = GroundStationData'
  { -- | UUID of a ground station.
    groundStationId :: Prelude.Maybe Prelude.Text,
    -- | Name of a ground station.
    groundStationName :: Prelude.Maybe Prelude.Text,
    -- | Ground station Region.
    region :: Prelude.Maybe Prelude.Text
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
-- 'groundStationId', 'groundStationData_groundStationId' - UUID of a ground station.
--
-- 'groundStationName', 'groundStationData_groundStationName' - Name of a ground station.
--
-- 'region', 'groundStationData_region' - Ground station Region.
newGroundStationData ::
  GroundStationData
newGroundStationData =
  GroundStationData'
    { groundStationId =
        Prelude.Nothing,
      groundStationName = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | UUID of a ground station.
groundStationData_groundStationId :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_groundStationId = Lens.lens (\GroundStationData' {groundStationId} -> groundStationId) (\s@GroundStationData' {} a -> s {groundStationId = a} :: GroundStationData)

-- | Name of a ground station.
groundStationData_groundStationName :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_groundStationName = Lens.lens (\GroundStationData' {groundStationName} -> groundStationName) (\s@GroundStationData' {} a -> s {groundStationName = a} :: GroundStationData)

-- | Ground station Region.
groundStationData_region :: Lens.Lens' GroundStationData (Prelude.Maybe Prelude.Text)
groundStationData_region = Lens.lens (\GroundStationData' {region} -> region) (\s@GroundStationData' {} a -> s {region = a} :: GroundStationData)

instance Core.FromJSON GroundStationData where
  parseJSON =
    Core.withObject
      "GroundStationData"
      ( \x ->
          GroundStationData'
            Prelude.<$> (x Core..:? "groundStationId")
            Prelude.<*> (x Core..:? "groundStationName")
            Prelude.<*> (x Core..:? "region")
      )

instance Prelude.Hashable GroundStationData

instance Prelude.NFData GroundStationData
