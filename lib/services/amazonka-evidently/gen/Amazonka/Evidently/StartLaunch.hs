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
-- Module      : Amazonka.Evidently.StartLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing launch. To create a launch, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_CreateLaunch.html CreateLaunch>.
module Amazonka.Evidently.StartLaunch
  ( -- * Creating a Request
    StartLaunch (..),
    newStartLaunch,

    -- * Request Lenses
    startLaunch_launch,
    startLaunch_project,

    -- * Destructuring the Response
    StartLaunchResponse (..),
    newStartLaunchResponse,

    -- * Response Lenses
    startLaunchResponse_httpStatus,
    startLaunchResponse_launch,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartLaunch' smart constructor.
data StartLaunch = StartLaunch'
  { -- | The name of the launch to start.
    launch :: Prelude.Text,
    -- | The name or ARN of the project that contains the launch to start.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launch', 'startLaunch_launch' - The name of the launch to start.
--
-- 'project', 'startLaunch_project' - The name or ARN of the project that contains the launch to start.
newStartLaunch ::
  -- | 'launch'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  StartLaunch
newStartLaunch pLaunch_ pProject_ =
  StartLaunch'
    { launch = pLaunch_,
      project = pProject_
    }

-- | The name of the launch to start.
startLaunch_launch :: Lens.Lens' StartLaunch Prelude.Text
startLaunch_launch = Lens.lens (\StartLaunch' {launch} -> launch) (\s@StartLaunch' {} a -> s {launch = a} :: StartLaunch)

-- | The name or ARN of the project that contains the launch to start.
startLaunch_project :: Lens.Lens' StartLaunch Prelude.Text
startLaunch_project = Lens.lens (\StartLaunch' {project} -> project) (\s@StartLaunch' {} a -> s {project = a} :: StartLaunch)

instance Core.AWSRequest StartLaunch where
  type AWSResponse StartLaunch = StartLaunchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartLaunchResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "launch")
      )

instance Prelude.Hashable StartLaunch where
  hashWithSalt _salt StartLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` launch
      `Prelude.hashWithSalt` project

instance Prelude.NFData StartLaunch where
  rnf StartLaunch' {..} =
    Prelude.rnf launch
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders StartLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartLaunch where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartLaunch where
  toPath StartLaunch' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/launches/",
        Data.toBS launch,
        "/start"
      ]

instance Data.ToQuery StartLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartLaunchResponse' smart constructor.
data StartLaunchResponse = StartLaunchResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains information about the launch that was started.
    launch :: Launch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startLaunchResponse_httpStatus' - The response's http status code.
--
-- 'launch', 'startLaunchResponse_launch' - A structure that contains information about the launch that was started.
newStartLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'launch'
  Launch ->
  StartLaunchResponse
newStartLaunchResponse pHttpStatus_ pLaunch_ =
  StartLaunchResponse'
    { httpStatus = pHttpStatus_,
      launch = pLaunch_
    }

-- | The response's http status code.
startLaunchResponse_httpStatus :: Lens.Lens' StartLaunchResponse Prelude.Int
startLaunchResponse_httpStatus = Lens.lens (\StartLaunchResponse' {httpStatus} -> httpStatus) (\s@StartLaunchResponse' {} a -> s {httpStatus = a} :: StartLaunchResponse)

-- | A structure that contains information about the launch that was started.
startLaunchResponse_launch :: Lens.Lens' StartLaunchResponse Launch
startLaunchResponse_launch = Lens.lens (\StartLaunchResponse' {launch} -> launch) (\s@StartLaunchResponse' {} a -> s {launch = a} :: StartLaunchResponse)

instance Prelude.NFData StartLaunchResponse where
  rnf StartLaunchResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf launch
