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
-- Module      : Amazonka.GroundStation.UpdateMissionProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a mission profile.
--
-- Updating a mission profile will not update the execution parameters for
-- existing future contacts.
module Amazonka.GroundStation.UpdateMissionProfile
  ( -- * Creating a Request
    UpdateMissionProfile (..),
    newUpdateMissionProfile,

    -- * Request Lenses
    updateMissionProfile_trackingConfigArn,
    updateMissionProfile_contactPrePassDurationSeconds,
    updateMissionProfile_contactPostPassDurationSeconds,
    updateMissionProfile_name,
    updateMissionProfile_dataflowEdges,
    updateMissionProfile_minimumViableContactDurationSeconds,
    updateMissionProfile_missionProfileId,

    -- * Destructuring the Response
    MissionProfileIdResponse (..),
    newMissionProfileIdResponse,

    -- * Response Lenses
    missionProfileIdResponse_missionProfileId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newUpdateMissionProfile' smart constructor.
data UpdateMissionProfile = UpdateMissionProfile'
  { -- | ARN of a tracking @Config@.
    trackingConfigArn :: Prelude.Maybe Prelude.Text,
    -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    contactPrePassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    contactPostPassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Name of a mission profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
    -- @Config@ and a /to/ @Config@.
    dataflowEdges :: Prelude.Maybe [Prelude.NonEmpty Prelude.Text],
    -- | Smallest amount of time in seconds that you’d like to see for an
    -- available contact. AWS Ground Station will not present you with contacts
    -- shorter than this duration.
    minimumViableContactDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | UUID of a mission profile.
    missionProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMissionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingConfigArn', 'updateMissionProfile_trackingConfigArn' - ARN of a tracking @Config@.
--
-- 'contactPrePassDurationSeconds', 'updateMissionProfile_contactPrePassDurationSeconds' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'contactPostPassDurationSeconds', 'updateMissionProfile_contactPostPassDurationSeconds' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'name', 'updateMissionProfile_name' - Name of a mission profile.
--
-- 'dataflowEdges', 'updateMissionProfile_dataflowEdges' - A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
-- @Config@ and a /to/ @Config@.
--
-- 'minimumViableContactDurationSeconds', 'updateMissionProfile_minimumViableContactDurationSeconds' - Smallest amount of time in seconds that you’d like to see for an
-- available contact. AWS Ground Station will not present you with contacts
-- shorter than this duration.
--
-- 'missionProfileId', 'updateMissionProfile_missionProfileId' - UUID of a mission profile.
newUpdateMissionProfile ::
  -- | 'missionProfileId'
  Prelude.Text ->
  UpdateMissionProfile
newUpdateMissionProfile pMissionProfileId_ =
  UpdateMissionProfile'
    { trackingConfigArn =
        Prelude.Nothing,
      contactPrePassDurationSeconds = Prelude.Nothing,
      contactPostPassDurationSeconds = Prelude.Nothing,
      name = Prelude.Nothing,
      dataflowEdges = Prelude.Nothing,
      minimumViableContactDurationSeconds =
        Prelude.Nothing,
      missionProfileId = pMissionProfileId_
    }

-- | ARN of a tracking @Config@.
updateMissionProfile_trackingConfigArn :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe Prelude.Text)
updateMissionProfile_trackingConfigArn = Lens.lens (\UpdateMissionProfile' {trackingConfigArn} -> trackingConfigArn) (\s@UpdateMissionProfile' {} a -> s {trackingConfigArn = a} :: UpdateMissionProfile)

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
updateMissionProfile_contactPrePassDurationSeconds :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe Prelude.Natural)
updateMissionProfile_contactPrePassDurationSeconds = Lens.lens (\UpdateMissionProfile' {contactPrePassDurationSeconds} -> contactPrePassDurationSeconds) (\s@UpdateMissionProfile' {} a -> s {contactPrePassDurationSeconds = a} :: UpdateMissionProfile)

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
updateMissionProfile_contactPostPassDurationSeconds :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe Prelude.Natural)
updateMissionProfile_contactPostPassDurationSeconds = Lens.lens (\UpdateMissionProfile' {contactPostPassDurationSeconds} -> contactPostPassDurationSeconds) (\s@UpdateMissionProfile' {} a -> s {contactPostPassDurationSeconds = a} :: UpdateMissionProfile)

-- | Name of a mission profile.
updateMissionProfile_name :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe Prelude.Text)
updateMissionProfile_name = Lens.lens (\UpdateMissionProfile' {name} -> name) (\s@UpdateMissionProfile' {} a -> s {name = a} :: UpdateMissionProfile)

-- | A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
-- @Config@ and a /to/ @Config@.
updateMissionProfile_dataflowEdges :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe [Prelude.NonEmpty Prelude.Text])
updateMissionProfile_dataflowEdges = Lens.lens (\UpdateMissionProfile' {dataflowEdges} -> dataflowEdges) (\s@UpdateMissionProfile' {} a -> s {dataflowEdges = a} :: UpdateMissionProfile) Prelude.. Lens.mapping Lens.coerced

-- | Smallest amount of time in seconds that you’d like to see for an
-- available contact. AWS Ground Station will not present you with contacts
-- shorter than this duration.
updateMissionProfile_minimumViableContactDurationSeconds :: Lens.Lens' UpdateMissionProfile (Prelude.Maybe Prelude.Natural)
updateMissionProfile_minimumViableContactDurationSeconds = Lens.lens (\UpdateMissionProfile' {minimumViableContactDurationSeconds} -> minimumViableContactDurationSeconds) (\s@UpdateMissionProfile' {} a -> s {minimumViableContactDurationSeconds = a} :: UpdateMissionProfile)

-- | UUID of a mission profile.
updateMissionProfile_missionProfileId :: Lens.Lens' UpdateMissionProfile Prelude.Text
updateMissionProfile_missionProfileId = Lens.lens (\UpdateMissionProfile' {missionProfileId} -> missionProfileId) (\s@UpdateMissionProfile' {} a -> s {missionProfileId = a} :: UpdateMissionProfile)

instance Core.AWSRequest UpdateMissionProfile where
  type
    AWSResponse UpdateMissionProfile =
      MissionProfileIdResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateMissionProfile where
  hashWithSalt _salt UpdateMissionProfile' {..} =
    _salt `Prelude.hashWithSalt` trackingConfigArn
      `Prelude.hashWithSalt` contactPrePassDurationSeconds
      `Prelude.hashWithSalt` contactPostPassDurationSeconds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dataflowEdges
      `Prelude.hashWithSalt` minimumViableContactDurationSeconds
      `Prelude.hashWithSalt` missionProfileId

instance Prelude.NFData UpdateMissionProfile where
  rnf UpdateMissionProfile' {..} =
    Prelude.rnf trackingConfigArn
      `Prelude.seq` Prelude.rnf contactPrePassDurationSeconds
      `Prelude.seq` Prelude.rnf contactPostPassDurationSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf dataflowEdges
      `Prelude.seq` Prelude.rnf minimumViableContactDurationSeconds
      `Prelude.seq` Prelude.rnf missionProfileId

instance Core.ToHeaders UpdateMissionProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMissionProfile where
  toJSON UpdateMissionProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("trackingConfigArn" Core..=)
              Prelude.<$> trackingConfigArn,
            ("contactPrePassDurationSeconds" Core..=)
              Prelude.<$> contactPrePassDurationSeconds,
            ("contactPostPassDurationSeconds" Core..=)
              Prelude.<$> contactPostPassDurationSeconds,
            ("name" Core..=) Prelude.<$> name,
            ("dataflowEdges" Core..=) Prelude.<$> dataflowEdges,
            ("minimumViableContactDurationSeconds" Core..=)
              Prelude.<$> minimumViableContactDurationSeconds
          ]
      )

instance Core.ToPath UpdateMissionProfile where
  toPath UpdateMissionProfile' {..} =
    Prelude.mconcat
      ["/missionprofile/", Core.toBS missionProfileId]

instance Core.ToQuery UpdateMissionProfile where
  toQuery = Prelude.const Prelude.mempty
