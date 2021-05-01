{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskRunnerHeartbeat@ every 15 minutes to
-- indicate that they are operational. If the AWS Data Pipeline Task Runner
-- is launched on a resource managed by AWS Data Pipeline, the web service
-- can use this call to detect when the task runner application has failed
-- and restart a new instance.
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
  ( -- * Creating a Request
    ReportTaskRunnerHeartbeat (..),
    newReportTaskRunnerHeartbeat,

    -- * Request Lenses
    reportTaskRunnerHeartbeat_hostname,
    reportTaskRunnerHeartbeat_workerGroup,
    reportTaskRunnerHeartbeat_taskrunnerId,

    -- * Destructuring the Response
    ReportTaskRunnerHeartbeatResponse (..),
    newReportTaskRunnerHeartbeatResponse,

    -- * Response Lenses
    reportTaskRunnerHeartbeatResponse_httpStatus,
    reportTaskRunnerHeartbeatResponse_terminate,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ReportTaskRunnerHeartbeat.
--
-- /See:/ 'newReportTaskRunnerHeartbeat' smart constructor.
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat'
  { -- | The public DNS name of the task runner.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The type of task the task runner is configured to accept and process.
    -- The worker group is set as a field on objects in the pipeline when they
    -- are created. You can only specify a single value for @workerGroup@.
    -- There are no wildcard values permitted in @workerGroup@; the string must
    -- be an exact, case-sensitive, match.
    workerGroup :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task runner. This value should be unique across your AWS
    -- account. In the case of AWS Data Pipeline Task Runner launched on a
    -- resource managed by AWS Data Pipeline, the web service provides a unique
    -- identifier when it launches the application. If you have written a
    -- custom task runner, you should assign a unique identifier for the task
    -- runner.
    taskrunnerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReportTaskRunnerHeartbeat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'reportTaskRunnerHeartbeat_hostname' - The public DNS name of the task runner.
--
-- 'workerGroup', 'reportTaskRunnerHeartbeat_workerGroup' - The type of task the task runner is configured to accept and process.
-- The worker group is set as a field on objects in the pipeline when they
-- are created. You can only specify a single value for @workerGroup@.
-- There are no wildcard values permitted in @workerGroup@; the string must
-- be an exact, case-sensitive, match.
--
-- 'taskrunnerId', 'reportTaskRunnerHeartbeat_taskrunnerId' - The ID of the task runner. This value should be unique across your AWS
-- account. In the case of AWS Data Pipeline Task Runner launched on a
-- resource managed by AWS Data Pipeline, the web service provides a unique
-- identifier when it launches the application. If you have written a
-- custom task runner, you should assign a unique identifier for the task
-- runner.
newReportTaskRunnerHeartbeat ::
  -- | 'taskrunnerId'
  Prelude.Text ->
  ReportTaskRunnerHeartbeat
newReportTaskRunnerHeartbeat pTaskrunnerId_ =
  ReportTaskRunnerHeartbeat'
    { hostname =
        Prelude.Nothing,
      workerGroup = Prelude.Nothing,
      taskrunnerId = pTaskrunnerId_
    }

-- | The public DNS name of the task runner.
reportTaskRunnerHeartbeat_hostname :: Lens.Lens' ReportTaskRunnerHeartbeat (Prelude.Maybe Prelude.Text)
reportTaskRunnerHeartbeat_hostname = Lens.lens (\ReportTaskRunnerHeartbeat' {hostname} -> hostname) (\s@ReportTaskRunnerHeartbeat' {} a -> s {hostname = a} :: ReportTaskRunnerHeartbeat)

-- | The type of task the task runner is configured to accept and process.
-- The worker group is set as a field on objects in the pipeline when they
-- are created. You can only specify a single value for @workerGroup@.
-- There are no wildcard values permitted in @workerGroup@; the string must
-- be an exact, case-sensitive, match.
reportTaskRunnerHeartbeat_workerGroup :: Lens.Lens' ReportTaskRunnerHeartbeat (Prelude.Maybe Prelude.Text)
reportTaskRunnerHeartbeat_workerGroup = Lens.lens (\ReportTaskRunnerHeartbeat' {workerGroup} -> workerGroup) (\s@ReportTaskRunnerHeartbeat' {} a -> s {workerGroup = a} :: ReportTaskRunnerHeartbeat)

-- | The ID of the task runner. This value should be unique across your AWS
-- account. In the case of AWS Data Pipeline Task Runner launched on a
-- resource managed by AWS Data Pipeline, the web service provides a unique
-- identifier when it launches the application. If you have written a
-- custom task runner, you should assign a unique identifier for the task
-- runner.
reportTaskRunnerHeartbeat_taskrunnerId :: Lens.Lens' ReportTaskRunnerHeartbeat Prelude.Text
reportTaskRunnerHeartbeat_taskrunnerId = Lens.lens (\ReportTaskRunnerHeartbeat' {taskrunnerId} -> taskrunnerId) (\s@ReportTaskRunnerHeartbeat' {} a -> s {taskrunnerId = a} :: ReportTaskRunnerHeartbeat)

instance Prelude.AWSRequest ReportTaskRunnerHeartbeat where
  type
    Rs ReportTaskRunnerHeartbeat =
      ReportTaskRunnerHeartbeatResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ReportTaskRunnerHeartbeatResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "terminate")
      )

instance Prelude.Hashable ReportTaskRunnerHeartbeat

instance Prelude.NFData ReportTaskRunnerHeartbeat

instance Prelude.ToHeaders ReportTaskRunnerHeartbeat where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DataPipeline.ReportTaskRunnerHeartbeat" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ReportTaskRunnerHeartbeat where
  toJSON ReportTaskRunnerHeartbeat' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("hostname" Prelude..=) Prelude.<$> hostname,
            ("workerGroup" Prelude..=) Prelude.<$> workerGroup,
            Prelude.Just
              ("taskrunnerId" Prelude..= taskrunnerId)
          ]
      )

instance Prelude.ToPath ReportTaskRunnerHeartbeat where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ReportTaskRunnerHeartbeat where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the output of ReportTaskRunnerHeartbeat.
--
-- /See:/ 'newReportTaskRunnerHeartbeatResponse' smart constructor.
data ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether the calling task runner should terminate.
    terminate :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReportTaskRunnerHeartbeatResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'reportTaskRunnerHeartbeatResponse_httpStatus' - The response's http status code.
--
-- 'terminate', 'reportTaskRunnerHeartbeatResponse_terminate' - Indicates whether the calling task runner should terminate.
newReportTaskRunnerHeartbeatResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'terminate'
  Prelude.Bool ->
  ReportTaskRunnerHeartbeatResponse
newReportTaskRunnerHeartbeatResponse
  pHttpStatus_
  pTerminate_ =
    ReportTaskRunnerHeartbeatResponse'
      { httpStatus =
          pHttpStatus_,
        terminate = pTerminate_
      }

-- | The response's http status code.
reportTaskRunnerHeartbeatResponse_httpStatus :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Prelude.Int
reportTaskRunnerHeartbeatResponse_httpStatus = Lens.lens (\ReportTaskRunnerHeartbeatResponse' {httpStatus} -> httpStatus) (\s@ReportTaskRunnerHeartbeatResponse' {} a -> s {httpStatus = a} :: ReportTaskRunnerHeartbeatResponse)

-- | Indicates whether the calling task runner should terminate.
reportTaskRunnerHeartbeatResponse_terminate :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Prelude.Bool
reportTaskRunnerHeartbeatResponse_terminate = Lens.lens (\ReportTaskRunnerHeartbeatResponse' {terminate} -> terminate) (\s@ReportTaskRunnerHeartbeatResponse' {} a -> s {terminate = a} :: ReportTaskRunnerHeartbeatResponse)

instance
  Prelude.NFData
    ReportTaskRunnerHeartbeatResponse
