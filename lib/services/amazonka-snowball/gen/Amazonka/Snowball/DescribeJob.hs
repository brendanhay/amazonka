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
-- Module      : Amazonka.Snowball.DescribeJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information,
-- job status, and other important metadata.
module Amazonka.Snowball.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_jobId,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_jobMetadata,
    describeJobResponse_subJobMetadata,
    describeJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Snowball.Types

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeJob_jobId' - The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
newDescribeJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeJob
newDescribeJob pJobId_ =
  DescribeJob' {jobId = pJobId_}

-- | The automatically generated ID for a job, for example
-- @JID123e4567-e89b-12d3-a456-426655440000@.
describeJob_jobId :: Lens.Lens' DescribeJob Prelude.Text
describeJob_jobId = Lens.lens (\DescribeJob' {jobId} -> jobId) (\s@DescribeJob' {} a -> s {jobId = a} :: DescribeJob)

instance Core.AWSRequest DescribeJob where
  type AWSResponse DescribeJob = DescribeJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Prelude.<$> (x Data..?> "JobMetadata")
            Prelude.<*> (x Data..?> "SubJobMetadata" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJob where
  hashWithSalt _salt DescribeJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeJob where
  rnf DescribeJob' {..} = Prelude.rnf jobId

instance Data.ToHeaders DescribeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSIESnowballJobManagementService.DescribeJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeJob where
  toJSON DescribeJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | Information about a specific job, including shipping information, job
    -- status, and other important metadata.
    jobMetadata :: Prelude.Maybe JobMetadata,
    -- | Information about a specific job part (in the case of an export job),
    -- including shipping information, job status, and other important
    -- metadata.
    subJobMetadata :: Prelude.Maybe [JobMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobMetadata', 'describeJobResponse_jobMetadata' - Information about a specific job, including shipping information, job
-- status, and other important metadata.
--
-- 'subJobMetadata', 'describeJobResponse_subJobMetadata' - Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ =
  DescribeJobResponse'
    { jobMetadata = Prelude.Nothing,
      subJobMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a specific job, including shipping information, job
-- status, and other important metadata.
describeJobResponse_jobMetadata :: Lens.Lens' DescribeJobResponse (Prelude.Maybe JobMetadata)
describeJobResponse_jobMetadata = Lens.lens (\DescribeJobResponse' {jobMetadata} -> jobMetadata) (\s@DescribeJobResponse' {} a -> s {jobMetadata = a} :: DescribeJobResponse)

-- | Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
describeJobResponse_subJobMetadata :: Lens.Lens' DescribeJobResponse (Prelude.Maybe [JobMetadata])
describeJobResponse_subJobMetadata = Lens.lens (\DescribeJobResponse' {subJobMetadata} -> subJobMetadata) (\s@DescribeJobResponse' {} a -> s {subJobMetadata = a} :: DescribeJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Prelude.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

instance Prelude.NFData DescribeJobResponse where
  rnf DescribeJobResponse' {..} =
    Prelude.rnf jobMetadata
      `Prelude.seq` Prelude.rnf subJobMetadata
      `Prelude.seq` Prelude.rnf httpStatus
