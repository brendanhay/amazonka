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
-- Module      : Amazonka.RobOMaker.CancelSimulationJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified simulation job.
module Amazonka.RobOMaker.CancelSimulationJob
  ( -- * Creating a Request
    CancelSimulationJob (..),
    newCancelSimulationJob,

    -- * Request Lenses
    cancelSimulationJob_job,

    -- * Destructuring the Response
    CancelSimulationJobResponse (..),
    newCancelSimulationJobResponse,

    -- * Response Lenses
    cancelSimulationJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCancelSimulationJob' smart constructor.
data CancelSimulationJob = CancelSimulationJob'
  { -- | The simulation job ARN to cancel.
    job :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSimulationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'cancelSimulationJob_job' - The simulation job ARN to cancel.
newCancelSimulationJob ::
  -- | 'job'
  Prelude.Text ->
  CancelSimulationJob
newCancelSimulationJob pJob_ =
  CancelSimulationJob' {job = pJob_}

-- | The simulation job ARN to cancel.
cancelSimulationJob_job :: Lens.Lens' CancelSimulationJob Prelude.Text
cancelSimulationJob_job = Lens.lens (\CancelSimulationJob' {job} -> job) (\s@CancelSimulationJob' {} a -> s {job = a} :: CancelSimulationJob)

instance Core.AWSRequest CancelSimulationJob where
  type
    AWSResponse CancelSimulationJob =
      CancelSimulationJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelSimulationJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSimulationJob where
  hashWithSalt _salt CancelSimulationJob' {..} =
    _salt `Prelude.hashWithSalt` job

instance Prelude.NFData CancelSimulationJob where
  rnf CancelSimulationJob' {..} = Prelude.rnf job

instance Data.ToHeaders CancelSimulationJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelSimulationJob where
  toJSON CancelSimulationJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("job" Data..= job)]
      )

instance Data.ToPath CancelSimulationJob where
  toPath = Prelude.const "/cancelSimulationJob"

instance Data.ToQuery CancelSimulationJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelSimulationJobResponse' smart constructor.
data CancelSimulationJobResponse = CancelSimulationJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSimulationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelSimulationJobResponse_httpStatus' - The response's http status code.
newCancelSimulationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelSimulationJobResponse
newCancelSimulationJobResponse pHttpStatus_ =
  CancelSimulationJobResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
cancelSimulationJobResponse_httpStatus :: Lens.Lens' CancelSimulationJobResponse Prelude.Int
cancelSimulationJobResponse_httpStatus = Lens.lens (\CancelSimulationJobResponse' {httpStatus} -> httpStatus) (\s@CancelSimulationJobResponse' {} a -> s {httpStatus = a} :: CancelSimulationJobResponse)

instance Prelude.NFData CancelSimulationJobResponse where
  rnf CancelSimulationJobResponse' {..} =
    Prelude.rnf httpStatus
