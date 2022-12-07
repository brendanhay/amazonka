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
-- Module      : Amazonka.Evidently.GetLaunch
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details about one launch. You must already know the launch
-- name. To retrieve a list of launches in your account, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_ListLaunches.html ListLaunches>.
module Amazonka.Evidently.GetLaunch
  ( -- * Creating a Request
    GetLaunch (..),
    newGetLaunch,

    -- * Request Lenses
    getLaunch_launch,
    getLaunch_project,

    -- * Destructuring the Response
    GetLaunchResponse (..),
    newGetLaunchResponse,

    -- * Response Lenses
    getLaunchResponse_launch,
    getLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunch' smart constructor.
data GetLaunch = GetLaunch'
  { -- | The name of the launch that you want to see the details of.
    launch :: Prelude.Text,
    -- | The name or ARN of the project that contains the launch.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launch', 'getLaunch_launch' - The name of the launch that you want to see the details of.
--
-- 'project', 'getLaunch_project' - The name or ARN of the project that contains the launch.
newGetLaunch ::
  -- | 'launch'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  GetLaunch
newGetLaunch pLaunch_ pProject_ =
  GetLaunch' {launch = pLaunch_, project = pProject_}

-- | The name of the launch that you want to see the details of.
getLaunch_launch :: Lens.Lens' GetLaunch Prelude.Text
getLaunch_launch = Lens.lens (\GetLaunch' {launch} -> launch) (\s@GetLaunch' {} a -> s {launch = a} :: GetLaunch)

-- | The name or ARN of the project that contains the launch.
getLaunch_project :: Lens.Lens' GetLaunch Prelude.Text
getLaunch_project = Lens.lens (\GetLaunch' {project} -> project) (\s@GetLaunch' {} a -> s {project = a} :: GetLaunch)

instance Core.AWSRequest GetLaunch where
  type AWSResponse GetLaunch = GetLaunchResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLaunchResponse'
            Prelude.<$> (x Data..?> "launch")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLaunch where
  hashWithSalt _salt GetLaunch' {..} =
    _salt `Prelude.hashWithSalt` launch
      `Prelude.hashWithSalt` project

instance Prelude.NFData GetLaunch where
  rnf GetLaunch' {..} =
    Prelude.rnf launch
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders GetLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLaunch where
  toPath GetLaunch' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/launches/",
        Data.toBS launch
      ]

instance Data.ToQuery GetLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLaunchResponse' smart constructor.
data GetLaunchResponse = GetLaunchResponse'
  { -- | A structure containing the configuration details of the launch.
    launch :: Prelude.Maybe Launch,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launch', 'getLaunchResponse_launch' - A structure containing the configuration details of the launch.
--
-- 'httpStatus', 'getLaunchResponse_httpStatus' - The response's http status code.
newGetLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLaunchResponse
newGetLaunchResponse pHttpStatus_ =
  GetLaunchResponse'
    { launch = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure containing the configuration details of the launch.
getLaunchResponse_launch :: Lens.Lens' GetLaunchResponse (Prelude.Maybe Launch)
getLaunchResponse_launch = Lens.lens (\GetLaunchResponse' {launch} -> launch) (\s@GetLaunchResponse' {} a -> s {launch = a} :: GetLaunchResponse)

-- | The response's http status code.
getLaunchResponse_httpStatus :: Lens.Lens' GetLaunchResponse Prelude.Int
getLaunchResponse_httpStatus = Lens.lens (\GetLaunchResponse' {httpStatus} -> httpStatus) (\s@GetLaunchResponse' {} a -> s {httpStatus = a} :: GetLaunchResponse)

instance Prelude.NFData GetLaunchResponse where
  rnf GetLaunchResponse' {..} =
    Prelude.rnf launch
      `Prelude.seq` Prelude.rnf httpStatus
