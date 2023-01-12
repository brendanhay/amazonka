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
-- Module      : Amazonka.GroundStation.DeleteMissionProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a mission profile.
module Amazonka.GroundStation.DeleteMissionProfile
  ( -- * Creating a Request
    DeleteMissionProfile (..),
    newDeleteMissionProfile,

    -- * Request Lenses
    deleteMissionProfile_missionProfileId,

    -- * Destructuring the Response
    MissionProfileIdResponse (..),
    newMissionProfileIdResponse,

    -- * Response Lenses
    missionProfileIdResponse_missionProfileId,
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
-- /See:/ 'newDeleteMissionProfile' smart constructor.
data DeleteMissionProfile = DeleteMissionProfile'
  { -- | UUID of a mission profile.
    missionProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteMissionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missionProfileId', 'deleteMissionProfile_missionProfileId' - UUID of a mission profile.
newDeleteMissionProfile ::
  -- | 'missionProfileId'
  Prelude.Text ->
  DeleteMissionProfile
newDeleteMissionProfile pMissionProfileId_ =
  DeleteMissionProfile'
    { missionProfileId =
        pMissionProfileId_
    }

-- | UUID of a mission profile.
deleteMissionProfile_missionProfileId :: Lens.Lens' DeleteMissionProfile Prelude.Text
deleteMissionProfile_missionProfileId = Lens.lens (\DeleteMissionProfile' {missionProfileId} -> missionProfileId) (\s@DeleteMissionProfile' {} a -> s {missionProfileId = a} :: DeleteMissionProfile)

instance Core.AWSRequest DeleteMissionProfile where
  type
    AWSResponse DeleteMissionProfile =
      MissionProfileIdResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DeleteMissionProfile where
  hashWithSalt _salt DeleteMissionProfile' {..} =
    _salt `Prelude.hashWithSalt` missionProfileId

instance Prelude.NFData DeleteMissionProfile where
  rnf DeleteMissionProfile' {..} =
    Prelude.rnf missionProfileId

instance Data.ToHeaders DeleteMissionProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteMissionProfile where
  toPath DeleteMissionProfile' {..} =
    Prelude.mconcat
      ["/missionprofile/", Data.toBS missionProfileId]

instance Data.ToQuery DeleteMissionProfile where
  toQuery = Prelude.const Prelude.mempty
