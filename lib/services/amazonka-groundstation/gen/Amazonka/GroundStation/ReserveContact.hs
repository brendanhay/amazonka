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
-- Module      : Amazonka.GroundStation.ReserveContact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Reserves a contact using specified parameters.
module Amazonka.GroundStation.ReserveContact
  ( -- * Creating a Request
    ReserveContact (..),
    newReserveContact,

    -- * Request Lenses
    reserveContact_tags,
    reserveContact_endTime,
    reserveContact_groundStation,
    reserveContact_missionProfileArn,
    reserveContact_satelliteArn,
    reserveContact_startTime,

    -- * Destructuring the Response
    ContactIdResponse (..),
    newContactIdResponse,

    -- * Response Lenses
    contactIdResponse_contactId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newReserveContact' smart constructor.
data ReserveContact = ReserveContact'
  { -- | Tags assigned to a contact.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | End time of a contact in UTC.
    endTime :: Core.POSIX,
    -- | Name of a ground station.
    groundStation :: Prelude.Text,
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Text,
    -- | ARN of a satellite
    satelliteArn :: Prelude.Text,
    -- | Start time of a contact in UTC.
    startTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReserveContact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'reserveContact_tags' - Tags assigned to a contact.
--
-- 'endTime', 'reserveContact_endTime' - End time of a contact in UTC.
--
-- 'groundStation', 'reserveContact_groundStation' - Name of a ground station.
--
-- 'missionProfileArn', 'reserveContact_missionProfileArn' - ARN of a mission profile.
--
-- 'satelliteArn', 'reserveContact_satelliteArn' - ARN of a satellite
--
-- 'startTime', 'reserveContact_startTime' - Start time of a contact in UTC.
newReserveContact ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'groundStation'
  Prelude.Text ->
  -- | 'missionProfileArn'
  Prelude.Text ->
  -- | 'satelliteArn'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  ReserveContact
newReserveContact
  pEndTime_
  pGroundStation_
  pMissionProfileArn_
  pSatelliteArn_
  pStartTime_ =
    ReserveContact'
      { tags = Prelude.Nothing,
        endTime = Core._Time Lens.# pEndTime_,
        groundStation = pGroundStation_,
        missionProfileArn = pMissionProfileArn_,
        satelliteArn = pSatelliteArn_,
        startTime = Core._Time Lens.# pStartTime_
      }

-- | Tags assigned to a contact.
reserveContact_tags :: Lens.Lens' ReserveContact (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
reserveContact_tags = Lens.lens (\ReserveContact' {tags} -> tags) (\s@ReserveContact' {} a -> s {tags = a} :: ReserveContact) Prelude.. Lens.mapping Lens.coerced

-- | End time of a contact in UTC.
reserveContact_endTime :: Lens.Lens' ReserveContact Prelude.UTCTime
reserveContact_endTime = Lens.lens (\ReserveContact' {endTime} -> endTime) (\s@ReserveContact' {} a -> s {endTime = a} :: ReserveContact) Prelude.. Core._Time

-- | Name of a ground station.
reserveContact_groundStation :: Lens.Lens' ReserveContact Prelude.Text
reserveContact_groundStation = Lens.lens (\ReserveContact' {groundStation} -> groundStation) (\s@ReserveContact' {} a -> s {groundStation = a} :: ReserveContact)

-- | ARN of a mission profile.
reserveContact_missionProfileArn :: Lens.Lens' ReserveContact Prelude.Text
reserveContact_missionProfileArn = Lens.lens (\ReserveContact' {missionProfileArn} -> missionProfileArn) (\s@ReserveContact' {} a -> s {missionProfileArn = a} :: ReserveContact)

-- | ARN of a satellite
reserveContact_satelliteArn :: Lens.Lens' ReserveContact Prelude.Text
reserveContact_satelliteArn = Lens.lens (\ReserveContact' {satelliteArn} -> satelliteArn) (\s@ReserveContact' {} a -> s {satelliteArn = a} :: ReserveContact)

-- | Start time of a contact in UTC.
reserveContact_startTime :: Lens.Lens' ReserveContact Prelude.UTCTime
reserveContact_startTime = Lens.lens (\ReserveContact' {startTime} -> startTime) (\s@ReserveContact' {} a -> s {startTime = a} :: ReserveContact) Prelude.. Core._Time

instance Core.AWSRequest ReserveContact where
  type AWSResponse ReserveContact = ContactIdResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable ReserveContact where
  hashWithSalt _salt ReserveContact' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` groundStation
      `Prelude.hashWithSalt` missionProfileArn
      `Prelude.hashWithSalt` satelliteArn
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ReserveContact where
  rnf ReserveContact' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf groundStation
      `Prelude.seq` Prelude.rnf missionProfileArn
      `Prelude.seq` Prelude.rnf satelliteArn
      `Prelude.seq` Prelude.rnf startTime

instance Core.ToHeaders ReserveContact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ReserveContact where
  toJSON ReserveContact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("endTime" Core..= endTime),
            Prelude.Just ("groundStation" Core..= groundStation),
            Prelude.Just
              ("missionProfileArn" Core..= missionProfileArn),
            Prelude.Just ("satelliteArn" Core..= satelliteArn),
            Prelude.Just ("startTime" Core..= startTime)
          ]
      )

instance Core.ToPath ReserveContact where
  toPath = Prelude.const "/contact"

instance Core.ToQuery ReserveContact where
  toQuery = Prelude.const Prelude.mempty
