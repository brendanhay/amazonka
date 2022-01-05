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
-- Module      : Amazonka.Nimble.GetLaunchProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a launch profile.
module Amazonka.Nimble.GetLaunchProfile
  ( -- * Creating a Request
    GetLaunchProfile (..),
    newGetLaunchProfile,

    -- * Request Lenses
    getLaunchProfile_studioId,
    getLaunchProfile_launchProfileId,

    -- * Destructuring the Response
    GetLaunchProfileResponse (..),
    newGetLaunchProfileResponse,

    -- * Response Lenses
    getLaunchProfileResponse_launchProfile,
    getLaunchProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfile' smart constructor.
data GetLaunchProfile = GetLaunchProfile'
  { -- | The studio ID.
    studioId :: Prelude.Text,
    -- | The launch profile ID.
    launchProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'getLaunchProfile_studioId' - The studio ID.
--
-- 'launchProfileId', 'getLaunchProfile_launchProfileId' - The launch profile ID.
newGetLaunchProfile ::
  -- | 'studioId'
  Prelude.Text ->
  -- | 'launchProfileId'
  Prelude.Text ->
  GetLaunchProfile
newGetLaunchProfile pStudioId_ pLaunchProfileId_ =
  GetLaunchProfile'
    { studioId = pStudioId_,
      launchProfileId = pLaunchProfileId_
    }

-- | The studio ID.
getLaunchProfile_studioId :: Lens.Lens' GetLaunchProfile Prelude.Text
getLaunchProfile_studioId = Lens.lens (\GetLaunchProfile' {studioId} -> studioId) (\s@GetLaunchProfile' {} a -> s {studioId = a} :: GetLaunchProfile)

-- | The launch profile ID.
getLaunchProfile_launchProfileId :: Lens.Lens' GetLaunchProfile Prelude.Text
getLaunchProfile_launchProfileId = Lens.lens (\GetLaunchProfile' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfile' {} a -> s {launchProfileId = a} :: GetLaunchProfile)

instance Core.AWSRequest GetLaunchProfile where
  type
    AWSResponse GetLaunchProfile =
      GetLaunchProfileResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileResponse'
            Prelude.<$> (x Core..?> "launchProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfile where
  hashWithSalt _salt GetLaunchProfile' {..} =
    _salt `Prelude.hashWithSalt` studioId
      `Prelude.hashWithSalt` launchProfileId

instance Prelude.NFData GetLaunchProfile where
  rnf GetLaunchProfile' {..} =
    Prelude.rnf studioId
      `Prelude.seq` Prelude.rnf launchProfileId

instance Core.ToHeaders GetLaunchProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLaunchProfile where
  toPath GetLaunchProfile' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Core.toBS studioId,
        "/launch-profiles/",
        Core.toBS launchProfileId
      ]

instance Core.ToQuery GetLaunchProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchProfileResponse' smart constructor.
data GetLaunchProfileResponse = GetLaunchProfileResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchProfile', 'getLaunchProfileResponse_launchProfile' - The launch profile.
--
-- 'httpStatus', 'getLaunchProfileResponse_httpStatus' - The response's http status code.
newGetLaunchProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchProfileResponse
newGetLaunchProfileResponse pHttpStatus_ =
  GetLaunchProfileResponse'
    { launchProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The launch profile.
getLaunchProfileResponse_launchProfile :: Lens.Lens' GetLaunchProfileResponse (Prelude.Maybe LaunchProfile)
getLaunchProfileResponse_launchProfile = Lens.lens (\GetLaunchProfileResponse' {launchProfile} -> launchProfile) (\s@GetLaunchProfileResponse' {} a -> s {launchProfile = a} :: GetLaunchProfileResponse)

-- | The response's http status code.
getLaunchProfileResponse_httpStatus :: Lens.Lens' GetLaunchProfileResponse Prelude.Int
getLaunchProfileResponse_httpStatus = Lens.lens (\GetLaunchProfileResponse' {httpStatus} -> httpStatus) (\s@GetLaunchProfileResponse' {} a -> s {httpStatus = a} :: GetLaunchProfileResponse)

instance Prelude.NFData GetLaunchProfileResponse where
  rnf GetLaunchProfileResponse' {..} =
    Prelude.rnf launchProfile
      `Prelude.seq` Prelude.rnf httpStatus
