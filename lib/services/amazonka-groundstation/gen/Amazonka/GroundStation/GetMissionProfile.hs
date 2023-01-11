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
-- Module      : Amazonka.GroundStation.GetMissionProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a mission profile.
module Amazonka.GroundStation.GetMissionProfile
  ( -- * Creating a Request
    GetMissionProfile (..),
    newGetMissionProfile,

    -- * Request Lenses
    getMissionProfile_missionProfileId,

    -- * Destructuring the Response
    GetMissionProfileResponse (..),
    newGetMissionProfileResponse,

    -- * Response Lenses
    getMissionProfileResponse_contactPostPassDurationSeconds,
    getMissionProfileResponse_contactPrePassDurationSeconds,
    getMissionProfileResponse_dataflowEdges,
    getMissionProfileResponse_minimumViableContactDurationSeconds,
    getMissionProfileResponse_missionProfileArn,
    getMissionProfileResponse_missionProfileId,
    getMissionProfileResponse_name,
    getMissionProfileResponse_region,
    getMissionProfileResponse_tags,
    getMissionProfileResponse_trackingConfigArn,
    getMissionProfileResponse_httpStatus,
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
-- /See:/ 'newGetMissionProfile' smart constructor.
data GetMissionProfile = GetMissionProfile'
  { -- | UUID of a mission profile.
    missionProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMissionProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'missionProfileId', 'getMissionProfile_missionProfileId' - UUID of a mission profile.
newGetMissionProfile ::
  -- | 'missionProfileId'
  Prelude.Text ->
  GetMissionProfile
newGetMissionProfile pMissionProfileId_ =
  GetMissionProfile'
    { missionProfileId =
        pMissionProfileId_
    }

-- | UUID of a mission profile.
getMissionProfile_missionProfileId :: Lens.Lens' GetMissionProfile Prelude.Text
getMissionProfile_missionProfileId = Lens.lens (\GetMissionProfile' {missionProfileId} -> missionProfileId) (\s@GetMissionProfile' {} a -> s {missionProfileId = a} :: GetMissionProfile)

instance Core.AWSRequest GetMissionProfile where
  type
    AWSResponse GetMissionProfile =
      GetMissionProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMissionProfileResponse'
            Prelude.<$> (x Data..?> "contactPostPassDurationSeconds")
            Prelude.<*> (x Data..?> "contactPrePassDurationSeconds")
            Prelude.<*> (x Data..?> "dataflowEdges" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "minimumViableContactDurationSeconds")
            Prelude.<*> (x Data..?> "missionProfileArn")
            Prelude.<*> (x Data..?> "missionProfileId")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "region")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "trackingConfigArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetMissionProfile where
  hashWithSalt _salt GetMissionProfile' {..} =
    _salt `Prelude.hashWithSalt` missionProfileId

instance Prelude.NFData GetMissionProfile where
  rnf GetMissionProfile' {..} =
    Prelude.rnf missionProfileId

instance Data.ToHeaders GetMissionProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetMissionProfile where
  toPath GetMissionProfile' {..} =
    Prelude.mconcat
      ["/missionprofile/", Data.toBS missionProfileId]

instance Data.ToQuery GetMissionProfile where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newGetMissionProfileResponse' smart constructor.
data GetMissionProfileResponse = GetMissionProfileResponse'
  { -- | Amount of time after a contact ends that you’d like to receive a
    -- CloudWatch event indicating the pass has finished.
    contactPostPassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | Amount of time prior to contact start you’d like to receive a CloudWatch
    -- event indicating an upcoming pass.
    contactPrePassDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
    -- @Config@ and a /to/ @Config@.
    dataflowEdges :: Prelude.Maybe [Prelude.NonEmpty Prelude.Text],
    -- | Smallest amount of time in seconds that you’d like to see for an
    -- available contact. AWS Ground Station will not present you with contacts
    -- shorter than this duration.
    minimumViableContactDurationSeconds :: Prelude.Maybe Prelude.Natural,
    -- | ARN of a mission profile.
    missionProfileArn :: Prelude.Maybe Prelude.Text,
    -- | UUID of a mission profile.
    missionProfileId :: Prelude.Maybe Prelude.Text,
    -- | Name of a mission profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | Region of a mission profile.
    region :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to a mission profile.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | ARN of a tracking @Config@.
    trackingConfigArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMissionProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactPostPassDurationSeconds', 'getMissionProfileResponse_contactPostPassDurationSeconds' - Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
--
-- 'contactPrePassDurationSeconds', 'getMissionProfileResponse_contactPrePassDurationSeconds' - Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
--
-- 'dataflowEdges', 'getMissionProfileResponse_dataflowEdges' - A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
-- @Config@ and a /to/ @Config@.
--
-- 'minimumViableContactDurationSeconds', 'getMissionProfileResponse_minimumViableContactDurationSeconds' - Smallest amount of time in seconds that you’d like to see for an
-- available contact. AWS Ground Station will not present you with contacts
-- shorter than this duration.
--
-- 'missionProfileArn', 'getMissionProfileResponse_missionProfileArn' - ARN of a mission profile.
--
-- 'missionProfileId', 'getMissionProfileResponse_missionProfileId' - UUID of a mission profile.
--
-- 'name', 'getMissionProfileResponse_name' - Name of a mission profile.
--
-- 'region', 'getMissionProfileResponse_region' - Region of a mission profile.
--
-- 'tags', 'getMissionProfileResponse_tags' - Tags assigned to a mission profile.
--
-- 'trackingConfigArn', 'getMissionProfileResponse_trackingConfigArn' - ARN of a tracking @Config@.
--
-- 'httpStatus', 'getMissionProfileResponse_httpStatus' - The response's http status code.
newGetMissionProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMissionProfileResponse
newGetMissionProfileResponse pHttpStatus_ =
  GetMissionProfileResponse'
    { contactPostPassDurationSeconds =
        Prelude.Nothing,
      contactPrePassDurationSeconds = Prelude.Nothing,
      dataflowEdges = Prelude.Nothing,
      minimumViableContactDurationSeconds =
        Prelude.Nothing,
      missionProfileArn = Prelude.Nothing,
      missionProfileId = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing,
      tags = Prelude.Nothing,
      trackingConfigArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amount of time after a contact ends that you’d like to receive a
-- CloudWatch event indicating the pass has finished.
getMissionProfileResponse_contactPostPassDurationSeconds :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Natural)
getMissionProfileResponse_contactPostPassDurationSeconds = Lens.lens (\GetMissionProfileResponse' {contactPostPassDurationSeconds} -> contactPostPassDurationSeconds) (\s@GetMissionProfileResponse' {} a -> s {contactPostPassDurationSeconds = a} :: GetMissionProfileResponse)

-- | Amount of time prior to contact start you’d like to receive a CloudWatch
-- event indicating an upcoming pass.
getMissionProfileResponse_contactPrePassDurationSeconds :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Natural)
getMissionProfileResponse_contactPrePassDurationSeconds = Lens.lens (\GetMissionProfileResponse' {contactPrePassDurationSeconds} -> contactPrePassDurationSeconds) (\s@GetMissionProfileResponse' {} a -> s {contactPrePassDurationSeconds = a} :: GetMissionProfileResponse)

-- | A list of lists of ARNs. Each list of ARNs is an edge, with a /from/
-- @Config@ and a /to/ @Config@.
getMissionProfileResponse_dataflowEdges :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe [Prelude.NonEmpty Prelude.Text])
getMissionProfileResponse_dataflowEdges = Lens.lens (\GetMissionProfileResponse' {dataflowEdges} -> dataflowEdges) (\s@GetMissionProfileResponse' {} a -> s {dataflowEdges = a} :: GetMissionProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | Smallest amount of time in seconds that you’d like to see for an
-- available contact. AWS Ground Station will not present you with contacts
-- shorter than this duration.
getMissionProfileResponse_minimumViableContactDurationSeconds :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Natural)
getMissionProfileResponse_minimumViableContactDurationSeconds = Lens.lens (\GetMissionProfileResponse' {minimumViableContactDurationSeconds} -> minimumViableContactDurationSeconds) (\s@GetMissionProfileResponse' {} a -> s {minimumViableContactDurationSeconds = a} :: GetMissionProfileResponse)

-- | ARN of a mission profile.
getMissionProfileResponse_missionProfileArn :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Text)
getMissionProfileResponse_missionProfileArn = Lens.lens (\GetMissionProfileResponse' {missionProfileArn} -> missionProfileArn) (\s@GetMissionProfileResponse' {} a -> s {missionProfileArn = a} :: GetMissionProfileResponse)

-- | UUID of a mission profile.
getMissionProfileResponse_missionProfileId :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Text)
getMissionProfileResponse_missionProfileId = Lens.lens (\GetMissionProfileResponse' {missionProfileId} -> missionProfileId) (\s@GetMissionProfileResponse' {} a -> s {missionProfileId = a} :: GetMissionProfileResponse)

-- | Name of a mission profile.
getMissionProfileResponse_name :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Text)
getMissionProfileResponse_name = Lens.lens (\GetMissionProfileResponse' {name} -> name) (\s@GetMissionProfileResponse' {} a -> s {name = a} :: GetMissionProfileResponse)

-- | Region of a mission profile.
getMissionProfileResponse_region :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Text)
getMissionProfileResponse_region = Lens.lens (\GetMissionProfileResponse' {region} -> region) (\s@GetMissionProfileResponse' {} a -> s {region = a} :: GetMissionProfileResponse)

-- | Tags assigned to a mission profile.
getMissionProfileResponse_tags :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getMissionProfileResponse_tags = Lens.lens (\GetMissionProfileResponse' {tags} -> tags) (\s@GetMissionProfileResponse' {} a -> s {tags = a} :: GetMissionProfileResponse) Prelude.. Lens.mapping Lens.coerced

-- | ARN of a tracking @Config@.
getMissionProfileResponse_trackingConfigArn :: Lens.Lens' GetMissionProfileResponse (Prelude.Maybe Prelude.Text)
getMissionProfileResponse_trackingConfigArn = Lens.lens (\GetMissionProfileResponse' {trackingConfigArn} -> trackingConfigArn) (\s@GetMissionProfileResponse' {} a -> s {trackingConfigArn = a} :: GetMissionProfileResponse)

-- | The response's http status code.
getMissionProfileResponse_httpStatus :: Lens.Lens' GetMissionProfileResponse Prelude.Int
getMissionProfileResponse_httpStatus = Lens.lens (\GetMissionProfileResponse' {httpStatus} -> httpStatus) (\s@GetMissionProfileResponse' {} a -> s {httpStatus = a} :: GetMissionProfileResponse)

instance Prelude.NFData GetMissionProfileResponse where
  rnf GetMissionProfileResponse' {..} =
    Prelude.rnf contactPostPassDurationSeconds
      `Prelude.seq` Prelude.rnf contactPrePassDurationSeconds
      `Prelude.seq` Prelude.rnf dataflowEdges
      `Prelude.seq` Prelude.rnf minimumViableContactDurationSeconds
      `Prelude.seq` Prelude.rnf missionProfileArn
      `Prelude.seq` Prelude.rnf missionProfileId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf trackingConfigArn
      `Prelude.seq` Prelude.rnf httpStatus
