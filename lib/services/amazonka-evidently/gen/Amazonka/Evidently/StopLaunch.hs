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
-- Module      : Amazonka.Evidently.StopLaunch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a launch that is currently running. After you stop a launch, you
-- will not be able to resume it or restart it. Also, it will not be
-- evaluated as a rule for traffic allocation, and the traffic that was
-- allocated to the launch will instead be available to the feature\'s
-- experiment, if there is one. Otherwise, all traffic will be served the
-- default variation after the launch is stopped.
module Amazonka.Evidently.StopLaunch
  ( -- * Creating a Request
    StopLaunch (..),
    newStopLaunch,

    -- * Request Lenses
    stopLaunch_desiredState,
    stopLaunch_reason,
    stopLaunch_launch,
    stopLaunch_project,

    -- * Destructuring the Response
    StopLaunchResponse (..),
    newStopLaunchResponse,

    -- * Response Lenses
    stopLaunchResponse_endedTime,
    stopLaunchResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopLaunch' smart constructor.
data StopLaunch = StopLaunch'
  { -- | Specify whether to consider the launch as @COMPLETED@ or @CANCELLED@
    -- after it stops.
    desiredState :: Prelude.Maybe LaunchStopDesiredState,
    -- | A string that describes why you are stopping the launch.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch to stop.
    launch :: Prelude.Text,
    -- | The name or ARN of the project that contains the launch that you want to
    -- stop.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopLaunch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredState', 'stopLaunch_desiredState' - Specify whether to consider the launch as @COMPLETED@ or @CANCELLED@
-- after it stops.
--
-- 'reason', 'stopLaunch_reason' - A string that describes why you are stopping the launch.
--
-- 'launch', 'stopLaunch_launch' - The name of the launch to stop.
--
-- 'project', 'stopLaunch_project' - The name or ARN of the project that contains the launch that you want to
-- stop.
newStopLaunch ::
  -- | 'launch'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  StopLaunch
newStopLaunch pLaunch_ pProject_ =
  StopLaunch'
    { desiredState = Prelude.Nothing,
      reason = Prelude.Nothing,
      launch = pLaunch_,
      project = pProject_
    }

-- | Specify whether to consider the launch as @COMPLETED@ or @CANCELLED@
-- after it stops.
stopLaunch_desiredState :: Lens.Lens' StopLaunch (Prelude.Maybe LaunchStopDesiredState)
stopLaunch_desiredState = Lens.lens (\StopLaunch' {desiredState} -> desiredState) (\s@StopLaunch' {} a -> s {desiredState = a} :: StopLaunch)

-- | A string that describes why you are stopping the launch.
stopLaunch_reason :: Lens.Lens' StopLaunch (Prelude.Maybe Prelude.Text)
stopLaunch_reason = Lens.lens (\StopLaunch' {reason} -> reason) (\s@StopLaunch' {} a -> s {reason = a} :: StopLaunch)

-- | The name of the launch to stop.
stopLaunch_launch :: Lens.Lens' StopLaunch Prelude.Text
stopLaunch_launch = Lens.lens (\StopLaunch' {launch} -> launch) (\s@StopLaunch' {} a -> s {launch = a} :: StopLaunch)

-- | The name or ARN of the project that contains the launch that you want to
-- stop.
stopLaunch_project :: Lens.Lens' StopLaunch Prelude.Text
stopLaunch_project = Lens.lens (\StopLaunch' {project} -> project) (\s@StopLaunch' {} a -> s {project = a} :: StopLaunch)

instance Core.AWSRequest StopLaunch where
  type AWSResponse StopLaunch = StopLaunchResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopLaunchResponse'
            Prelude.<$> (x Data..?> "endedTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopLaunch where
  hashWithSalt _salt StopLaunch' {..} =
    _salt
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` launch
      `Prelude.hashWithSalt` project

instance Prelude.NFData StopLaunch where
  rnf StopLaunch' {..} =
    Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf launch
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders StopLaunch where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopLaunch where
  toJSON StopLaunch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("desiredState" Data..=) Prelude.<$> desiredState,
            ("reason" Data..=) Prelude.<$> reason
          ]
      )

instance Data.ToPath StopLaunch where
  toPath StopLaunch' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS project,
        "/launches/",
        Data.toBS launch,
        "/cancel"
      ]

instance Data.ToQuery StopLaunch where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopLaunchResponse' smart constructor.
data StopLaunchResponse = StopLaunchResponse'
  { -- | The date and time that the launch stopped.
    endedTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopLaunchResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endedTime', 'stopLaunchResponse_endedTime' - The date and time that the launch stopped.
--
-- 'httpStatus', 'stopLaunchResponse_httpStatus' - The response's http status code.
newStopLaunchResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopLaunchResponse
newStopLaunchResponse pHttpStatus_ =
  StopLaunchResponse'
    { endedTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the launch stopped.
stopLaunchResponse_endedTime :: Lens.Lens' StopLaunchResponse (Prelude.Maybe Prelude.UTCTime)
stopLaunchResponse_endedTime = Lens.lens (\StopLaunchResponse' {endedTime} -> endedTime) (\s@StopLaunchResponse' {} a -> s {endedTime = a} :: StopLaunchResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
stopLaunchResponse_httpStatus :: Lens.Lens' StopLaunchResponse Prelude.Int
stopLaunchResponse_httpStatus = Lens.lens (\StopLaunchResponse' {httpStatus} -> httpStatus) (\s@StopLaunchResponse' {} a -> s {httpStatus = a} :: StopLaunchResponse)

instance Prelude.NFData StopLaunchResponse where
  rnf StopLaunchResponse' {..} =
    Prelude.rnf endedTime
      `Prelude.seq` Prelude.rnf httpStatus
