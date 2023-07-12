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
-- Module      : Amazonka.ElasticTranscoder.ReadJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ReadJob operation returns detailed information about a job.
module Amazonka.ElasticTranscoder.ReadJob
  ( -- * Creating a Request
    ReadJob (..),
    newReadJob,

    -- * Request Lenses
    readJob_id,

    -- * Destructuring the Response
    ReadJobResponse (..),
    newReadJobResponse,

    -- * Response Lenses
    readJobResponse_httpStatus,
    readJobResponse_job,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @ReadJobRequest@ structure.
--
-- /See:/ 'newReadJob' smart constructor.
data ReadJob = ReadJob'
  { -- | The identifier of the job for which you want to get detailed
    -- information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'readJob_id' - The identifier of the job for which you want to get detailed
-- information.
newReadJob ::
  -- | 'id'
  Prelude.Text ->
  ReadJob
newReadJob pId_ = ReadJob' {id = pId_}

-- | The identifier of the job for which you want to get detailed
-- information.
readJob_id :: Lens.Lens' ReadJob Prelude.Text
readJob_id = Lens.lens (\ReadJob' {id} -> id) (\s@ReadJob' {} a -> s {id = a} :: ReadJob)

instance Core.AWSRequest ReadJob where
  type AWSResponse ReadJob = ReadJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ReadJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Job")
      )

instance Prelude.Hashable ReadJob where
  hashWithSalt _salt ReadJob' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ReadJob where
  rnf ReadJob' {..} = Prelude.rnf id

instance Data.ToHeaders ReadJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ReadJob where
  toPath ReadJob' {..} =
    Prelude.mconcat ["/2012-09-25/jobs/", Data.toBS id]

instance Data.ToQuery ReadJob where
  toQuery = Prelude.const Prelude.mempty

-- | The @ReadJobResponse@ structure.
--
-- /See:/ 'newReadJobResponse' smart constructor.
data ReadJobResponse = ReadJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A section of the response body that provides information about the job.
    job :: Job
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReadJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'readJobResponse_httpStatus' - The response's http status code.
--
-- 'job', 'readJobResponse_job' - A section of the response body that provides information about the job.
newReadJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'job'
  Job ->
  ReadJobResponse
newReadJobResponse pHttpStatus_ pJob_ =
  ReadJobResponse'
    { httpStatus = pHttpStatus_,
      job = pJob_
    }

-- | The response's http status code.
readJobResponse_httpStatus :: Lens.Lens' ReadJobResponse Prelude.Int
readJobResponse_httpStatus = Lens.lens (\ReadJobResponse' {httpStatus} -> httpStatus) (\s@ReadJobResponse' {} a -> s {httpStatus = a} :: ReadJobResponse)

-- | A section of the response body that provides information about the job.
readJobResponse_job :: Lens.Lens' ReadJobResponse Job
readJobResponse_job = Lens.lens (\ReadJobResponse' {job} -> job) (\s@ReadJobResponse' {} a -> s {job = a} :: ReadJobResponse)

instance Prelude.NFData ReadJobResponse where
  rnf ReadJobResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf job
