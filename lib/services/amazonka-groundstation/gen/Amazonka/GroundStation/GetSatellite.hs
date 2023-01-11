{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GroundStation.GetSatellite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a satellite.
module Amazonka.GroundStation.GetSatellite
  ( -- * Creating a Request
    GetSatellite (..),
    newGetSatellite,

    -- * Request Lenses
    getSatellite_satelliteId,

    -- * Destructuring the Response
    GetSatelliteResponse (..),
    newGetSatelliteResponse,

    -- * Response Lenses
    getSatelliteResponse_currentEphemeris,
    getSatelliteResponse_groundStations,
    getSatelliteResponse_noradSatelliteID,
    getSatelliteResponse_satelliteArn,
    getSatelliteResponse_satelliteId,
    getSatelliteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newGetSatellite' smart constructor.
data GetSatellite = GetSatellite'
  { -- | UUID of a satellite.
    satelliteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSatellite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'satelliteId', 'getSatellite_satelliteId' - UUID of a satellite.
newGetSatellite ::
  -- | 'satelliteId'
  Prelude.Text ->
  GetSatellite
newGetSatellite pSatelliteId_ =
  GetSatellite' {satelliteId = pSatelliteId_}

-- | UUID of a satellite.
getSatellite_satelliteId :: Lens.Lens' GetSatellite Prelude.Text
getSatellite_satelliteId = Lens.lens (\GetSatellite' {satelliteId} -> satelliteId) (\s@GetSatellite' {} a -> s {satelliteId = a} :: GetSatellite)

instance Core.AWSRequest GetSatellite where
  type AWSResponse GetSatellite = GetSatelliteResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSatelliteResponse'
            Prelude.<$> (x Data..?> "currentEphemeris")
            Prelude.<*> (x Data..?> "groundStations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "noradSatelliteID")
            Prelude.<*> (x Data..?> "satelliteArn")
            Prelude.<*> (x Data..?> "satelliteId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSatellite where
  hashWithSalt _salt GetSatellite' {..} =
    _salt `Prelude.hashWithSalt` satelliteId

instance Prelude.NFData GetSatellite where
  rnf GetSatellite' {..} = Prelude.rnf satelliteId

instance Data.ToHeaders GetSatellite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSatellite where
  toPath GetSatellite' {..} =
    Prelude.mconcat
      ["/satellite/", Data.toBS satelliteId]

instance Data.ToQuery GetSatellite where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetSatelliteResponse' smart constructor.
data GetSatelliteResponse = GetSatelliteResponse'
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
    satelliteId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSatelliteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentEphemeris', 'getSatelliteResponse_currentEphemeris' - The current ephemeris being used to compute the trajectory of the
-- satellite.
--
-- 'groundStations', 'getSatelliteResponse_groundStations' - A list of ground stations to which the satellite is on-boarded.
--
-- 'noradSatelliteID', 'getSatelliteResponse_noradSatelliteID' - NORAD satellite ID number.
--
-- 'satelliteArn', 'getSatelliteResponse_satelliteArn' - ARN of a satellite.
--
-- 'satelliteId', 'getSatelliteResponse_satelliteId' - UUID of a satellite.
--
-- 'httpStatus', 'getSatelliteResponse_httpStatus' - The response's http status code.
newGetSatelliteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSatelliteResponse
newGetSatelliteResponse pHttpStatus_ =
  GetSatelliteResponse'
    { currentEphemeris =
        Prelude.Nothing,
      groundStations = Prelude.Nothing,
      noradSatelliteID = Prelude.Nothing,
      satelliteArn = Prelude.Nothing,
      satelliteId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current ephemeris being used to compute the trajectory of the
-- satellite.
getSatelliteResponse_currentEphemeris :: Lens.Lens' GetSatelliteResponse (Prelude.Maybe EphemerisMetaData)
getSatelliteResponse_currentEphemeris = Lens.lens (\GetSatelliteResponse' {currentEphemeris} -> currentEphemeris) (\s@GetSatelliteResponse' {} a -> s {currentEphemeris = a} :: GetSatelliteResponse)

-- | A list of ground stations to which the satellite is on-boarded.
getSatelliteResponse_groundStations :: Lens.Lens' GetSatelliteResponse (Prelude.Maybe [Prelude.Text])
getSatelliteResponse_groundStations = Lens.lens (\GetSatelliteResponse' {groundStations} -> groundStations) (\s@GetSatelliteResponse' {} a -> s {groundStations = a} :: GetSatelliteResponse) Prelude.. Lens.mapping Lens.coerced

-- | NORAD satellite ID number.
getSatelliteResponse_noradSatelliteID :: Lens.Lens' GetSatelliteResponse (Prelude.Maybe Prelude.Natural)
getSatelliteResponse_noradSatelliteID = Lens.lens (\GetSatelliteResponse' {noradSatelliteID} -> noradSatelliteID) (\s@GetSatelliteResponse' {} a -> s {noradSatelliteID = a} :: GetSatelliteResponse)

-- | ARN of a satellite.
getSatelliteResponse_satelliteArn :: Lens.Lens' GetSatelliteResponse (Prelude.Maybe Prelude.Text)
getSatelliteResponse_satelliteArn = Lens.lens (\GetSatelliteResponse' {satelliteArn} -> satelliteArn) (\s@GetSatelliteResponse' {} a -> s {satelliteArn = a} :: GetSatelliteResponse)

-- | UUID of a satellite.
getSatelliteResponse_satelliteId :: Lens.Lens' GetSatelliteResponse (Prelude.Maybe Prelude.Text)
getSatelliteResponse_satelliteId = Lens.lens (\GetSatelliteResponse' {satelliteId} -> satelliteId) (\s@GetSatelliteResponse' {} a -> s {satelliteId = a} :: GetSatelliteResponse)

-- | The response's http status code.
getSatelliteResponse_httpStatus :: Lens.Lens' GetSatelliteResponse Prelude.Int
getSatelliteResponse_httpStatus = Lens.lens (\GetSatelliteResponse' {httpStatus} -> httpStatus) (\s@GetSatelliteResponse' {} a -> s {httpStatus = a} :: GetSatelliteResponse)

instance Prelude.NFData GetSatelliteResponse where
  rnf GetSatelliteResponse' {..} =
    Prelude.rnf currentEphemeris
      `Prelude.seq` Prelude.rnf groundStations
      `Prelude.seq` Prelude.rnf noradSatelliteID
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf satelliteId
      `Prelude.seq` Prelude.rnf httpStatus
