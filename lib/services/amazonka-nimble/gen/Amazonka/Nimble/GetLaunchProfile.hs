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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    getLaunchProfile_launchProfileId,
    getLaunchProfile_studioId,

    -- * Destructuring the Response
    GetLaunchProfileResponse (..),
    newGetLaunchProfileResponse,

    -- * Response Lenses
    getLaunchProfileResponse_launchProfile,
    getLaunchProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchProfile' smart constructor.
data GetLaunchProfile = GetLaunchProfile'
  { -- | The ID of the launch profile used to control access from the streaming
    -- session.
    launchProfileId :: Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
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
-- 'launchProfileId', 'getLaunchProfile_launchProfileId' - The ID of the launch profile used to control access from the streaming
-- session.
--
-- 'studioId', 'getLaunchProfile_studioId' - The studio ID.
newGetLaunchProfile ::
  -- | 'launchProfileId'
  Prelude.Text ->
  -- | 'studioId'
  Prelude.Text ->
  GetLaunchProfile
newGetLaunchProfile pLaunchProfileId_ pStudioId_ =
  GetLaunchProfile'
    { launchProfileId =
        pLaunchProfileId_,
      studioId = pStudioId_
    }

-- | The ID of the launch profile used to control access from the streaming
-- session.
getLaunchProfile_launchProfileId :: Lens.Lens' GetLaunchProfile Prelude.Text
getLaunchProfile_launchProfileId = Lens.lens (\GetLaunchProfile' {launchProfileId} -> launchProfileId) (\s@GetLaunchProfile' {} a -> s {launchProfileId = a} :: GetLaunchProfile)

-- | The studio ID.
getLaunchProfile_studioId :: Lens.Lens' GetLaunchProfile Prelude.Text
getLaunchProfile_studioId = Lens.lens (\GetLaunchProfile' {studioId} -> studioId) (\s@GetLaunchProfile' {} a -> s {studioId = a} :: GetLaunchProfile)

instance Core.AWSRequest GetLaunchProfile where
  type
    AWSResponse GetLaunchProfile =
      GetLaunchProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchProfileResponse'
            Prelude.<$> (x Data..?> "launchProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunchProfile where
  hashWithSalt _salt GetLaunchProfile' {..} =
    _salt `Prelude.hashWithSalt` launchProfileId
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData GetLaunchProfile where
  rnf GetLaunchProfile' {..} =
    Prelude.rnf launchProfileId
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders GetLaunchProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLaunchProfile where
  toPath GetLaunchProfile' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/launch-profiles/",
        Data.toBS launchProfileId
      ]

instance Data.ToQuery GetLaunchProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchProfileResponse' smart constructor.
data GetLaunchProfileResponse = GetLaunchProfileResponse'
  { -- | The launch profile.
    launchProfile :: Prelude.Maybe LaunchProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
