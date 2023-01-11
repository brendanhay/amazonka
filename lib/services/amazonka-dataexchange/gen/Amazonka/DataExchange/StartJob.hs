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
-- Module      : Amazonka.DataExchange.StartJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation starts a job.
module Amazonka.DataExchange.StartJob
  ( -- * Creating a Request
    StartJob (..),
    newStartJob,

    -- * Request Lenses
    startJob_jobId,

    -- * Destructuring the Response
    StartJobResponse (..),
    newStartJobResponse,

    -- * Response Lenses
    startJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataExchange.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartJob' smart constructor.
data StartJob = StartJob'
  { -- | The unique identifier for a job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'startJob_jobId' - The unique identifier for a job.
newStartJob ::
  -- | 'jobId'
  Prelude.Text ->
  StartJob
newStartJob pJobId_ = StartJob' {jobId = pJobId_}

-- | The unique identifier for a job.
startJob_jobId :: Lens.Lens' StartJob Prelude.Text
startJob_jobId = Lens.lens (\StartJob' {jobId} -> jobId) (\s@StartJob' {} a -> s {jobId = a} :: StartJob)

instance Core.AWSRequest StartJob where
  type AWSResponse StartJob = StartJobResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartJobResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartJob where
  hashWithSalt _salt StartJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData StartJob where
  rnf StartJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders StartJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartJob where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StartJob where
  toPath StartJob' {..} =
    Prelude.mconcat ["/v1/jobs/", Data.toBS jobId]

instance Data.ToQuery StartJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartJobResponse' smart constructor.
data StartJobResponse = StartJobResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startJobResponse_httpStatus' - The response's http status code.
newStartJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartJobResponse
newStartJobResponse pHttpStatus_ =
  StartJobResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startJobResponse_httpStatus :: Lens.Lens' StartJobResponse Prelude.Int
startJobResponse_httpStatus = Lens.lens (\StartJobResponse' {httpStatus} -> httpStatus) (\s@StartJobResponse' {} a -> s {httpStatus = a} :: StartJobResponse)

instance Prelude.NFData StartJobResponse where
  rnf StartJobResponse' {..} = Prelude.rnf httpStatus
