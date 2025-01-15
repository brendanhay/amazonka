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
-- Module      : Amazonka.DataBrew.StopJobRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a particular run of a job.
module Amazonka.DataBrew.StopJobRun
  ( -- * Creating a Request
    StopJobRun (..),
    newStopJobRun,

    -- * Request Lenses
    stopJobRun_name,
    stopJobRun_runId,

    -- * Destructuring the Response
    StopJobRunResponse (..),
    newStopJobRunResponse,

    -- * Response Lenses
    stopJobRunResponse_httpStatus,
    stopJobRunResponse_runId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopJobRun' smart constructor.
data StopJobRun = StopJobRun'
  { -- | The name of the job to be stopped.
    name :: Prelude.Text,
    -- | The ID of the job run to be stopped.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJobRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopJobRun_name' - The name of the job to be stopped.
--
-- 'runId', 'stopJobRun_runId' - The ID of the job run to be stopped.
newStopJobRun ::
  -- | 'name'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  StopJobRun
newStopJobRun pName_ pRunId_ =
  StopJobRun' {name = pName_, runId = pRunId_}

-- | The name of the job to be stopped.
stopJobRun_name :: Lens.Lens' StopJobRun Prelude.Text
stopJobRun_name = Lens.lens (\StopJobRun' {name} -> name) (\s@StopJobRun' {} a -> s {name = a} :: StopJobRun)

-- | The ID of the job run to be stopped.
stopJobRun_runId :: Lens.Lens' StopJobRun Prelude.Text
stopJobRun_runId = Lens.lens (\StopJobRun' {runId} -> runId) (\s@StopJobRun' {} a -> s {runId = a} :: StopJobRun)

instance Core.AWSRequest StopJobRun where
  type AWSResponse StopJobRun = StopJobRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StopJobRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "RunId")
      )

instance Prelude.Hashable StopJobRun where
  hashWithSalt _salt StopJobRun' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` runId

instance Prelude.NFData StopJobRun where
  rnf StopJobRun' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders StopJobRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopJobRun where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopJobRun where
  toPath StopJobRun' {..} =
    Prelude.mconcat
      [ "/jobs/",
        Data.toBS name,
        "/jobRun/",
        Data.toBS runId,
        "/stopJobRun"
      ]

instance Data.ToQuery StopJobRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopJobRunResponse' smart constructor.
data StopJobRunResponse = StopJobRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the job run that you stopped.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopJobRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopJobRunResponse_httpStatus' - The response's http status code.
--
-- 'runId', 'stopJobRunResponse_runId' - The ID of the job run that you stopped.
newStopJobRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'runId'
  Prelude.Text ->
  StopJobRunResponse
newStopJobRunResponse pHttpStatus_ pRunId_ =
  StopJobRunResponse'
    { httpStatus = pHttpStatus_,
      runId = pRunId_
    }

-- | The response's http status code.
stopJobRunResponse_httpStatus :: Lens.Lens' StopJobRunResponse Prelude.Int
stopJobRunResponse_httpStatus = Lens.lens (\StopJobRunResponse' {httpStatus} -> httpStatus) (\s@StopJobRunResponse' {} a -> s {httpStatus = a} :: StopJobRunResponse)

-- | The ID of the job run that you stopped.
stopJobRunResponse_runId :: Lens.Lens' StopJobRunResponse Prelude.Text
stopJobRunResponse_runId = Lens.lens (\StopJobRunResponse' {runId} -> runId) (\s@StopJobRunResponse' {} a -> s {runId = a} :: StopJobRunResponse)

instance Prelude.NFData StopJobRunResponse where
  rnf StopJobRunResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf runId
