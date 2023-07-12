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
-- Module      : Amazonka.GroundStation.Types.SatelliteListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SatelliteListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.EphemerisMetaData
import qualified Amazonka.Prelude as Prelude

-- | Item in a list of satellites.
--
-- /See:/ 'newSatelliteListItem' smart constructor.
data SatelliteListItem = SatelliteListItem'
  { -- | The current ephemeris being used to compute the trajectory of the
    -- satellite.
    currentEphemeris :: Prelude.Maybe EphemerisMetaData,
    -- | A list of ground stations to which the satellite is on-boarded.
    groundStations :: Prelude.Maybe [Prelude.Text],
    -- | NORAD satellite ID number.
    noradSatelliteID :: Prelude.Maybe Prelude.Natural,
    -- | ARN of a satellite.
    satelliteArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a satellite.
    satelliteId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SatelliteListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentEphemeris', 'satelliteListItem_currentEphemeris' - The current ephemeris being used to compute the trajectory of the
-- satellite.
--
-- 'groundStations', 'satelliteListItem_groundStations' - A list of ground stations to which the satellite is on-boarded.
--
-- 'noradSatelliteID', 'satelliteListItem_noradSatelliteID' - NORAD satellite ID number.
--
-- 'satelliteArn', 'satelliteListItem_satelliteArn' - ARN of a satellite.
--
-- 'satelliteId', 'satelliteListItem_satelliteId' - UUID of a satellite.
newSatelliteListItem ::
  SatelliteListItem
newSatelliteListItem =
  SatelliteListItem'
    { currentEphemeris =
        Prelude.Nothing,
      groundStations = Prelude.Nothing,
      noradSatelliteID = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      satelliteId = Prelude.Nothing
    }

-- | The current ephemeris being used to compute the trajectory of the
-- satellite.
satelliteListItem_currentEphemeris :: Lens.Lens' SatelliteListItem (Prelude.Maybe EphemerisMetaData)
satelliteListItem_currentEphemeris = Lens.lens (\SatelliteListItem' {currentEphemeris} -> currentEphemeris) (\s@SatelliteListItem' {} a -> s {currentEphemeris = a} :: SatelliteListItem)

-- | A list of ground stations to which the satellite is on-boarded.
satelliteListItem_groundStations :: Lens.Lens' SatelliteListItem (Prelude.Maybe [Prelude.Text])
satelliteListItem_groundStations = Lens.lens (\SatelliteListItem' {groundStations} -> groundStations) (\s@SatelliteListItem' {} a -> s {groundStations = a} :: SatelliteListItem) Prelude.. Lens.mapping Lens.coerced

-- | NORAD satellite ID number.
satelliteListItem_noradSatelliteID :: Lens.Lens' SatelliteListItem (Prelude.Maybe Prelude.Natural)
satelliteListItem_noradSatelliteID = Lens.lens (\SatelliteListItem' {noradSatelliteID} -> noradSatelliteID) (\s@SatelliteListItem' {} a -> s {noradSatelliteID = a} :: SatelliteListItem)

-- | ARN of a satellite.
satelliteListItem_satelliteArn :: Lens.Lens' SatelliteListItem (Prelude.Maybe Prelude.Text)
satelliteListItem_satelliteArn = Lens.lens (\SatelliteListItem' {satelliteArn} -> satelliteArn) (\s@SatelliteListItem' {} a -> s {satelliteArn = a} :: SatelliteListItem)

-- | UUID of a satellite.
satelliteListItem_satelliteId :: Lens.Lens' SatelliteListItem (Prelude.Maybe Prelude.Text)
satelliteListItem_satelliteId = Lens.lens (\SatelliteListItem' {satelliteId} -> satelliteId) (\s@SatelliteListItem' {} a -> s {satelliteId = a} :: SatelliteListItem)

instance Data.FromJSON SatelliteListItem where
  parseJSON =
    Data.withObject
      "SatelliteListItem"
      ( \x ->
          SatelliteListItem'
            Prelude.<$> (x Data..:? "currentEphemeris")
            Prelude.<*> (x Data..:? "groundStations" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "noradSatelliteID")
            Prelude.<*> (x Data..:? "satelliteArn")
            Prelude.<*> (x Data..:? "satelliteId")
      )

instance Prelude.Hashable SatelliteListItem where
  hashWithSalt _salt SatelliteListItem' {..} =
    _salt
      `Prelude.hashWithSalt` currentEphemeris
      `Prelude.hashWithSalt` groundStations
      `Prelude.hashWithSalt` noradSatelliteID
      `Prelude.hashWithSalt` satelliteArn
      `Prelude.hashWithSalt` satelliteId

instance Prelude.NFData SatelliteListItem where
  rnf SatelliteListItem' {..} =
    Prelude.rnf currentEphemeris
      `Prelude.seq` Prelude.rnf groundStations
      `Prelude.seq` Prelude.rnf noradSatelliteID
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf satelliteId
