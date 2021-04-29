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
-- Module      : Network.AWS.Snowball.DescribeJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific job including shipping information,
-- job status, and other important metadata.
module Network.AWS.Snowball.DescribeJob
  ( -- * Creating a Request
    DescribeJob (..),
    newDescribeJob,

    -- * Request Lenses
    describeJob_jobId,

    -- * Destructuring the Response
    DescribeJobResponse (..),
    newDescribeJobResponse,

    -- * Response Lenses
    describeJobResponse_subJobMetadata,
    describeJobResponse_jobMetadata,
    describeJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Snowball.Types

-- | /See:/ 'newDescribeJob' smart constructor.
data DescribeJob = DescribeJob'
  { -- | The automatically generated ID for a job, for example
    -- @JID123e4567-e89b-12d3-a456-426655440000@.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeJob where
  type Rs DescribeJob = DescribeJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobResponse'
            Prelude.<$> ( x Prelude..?> "SubJobMetadata"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "JobMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJob

instance Prelude.NFData DescribeJob

instance Prelude.ToHeaders DescribeJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSIESnowballJobManagementService.DescribeJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeJob where
  toJSON DescribeJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance Prelude.ToPath DescribeJob where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobResponse' smart constructor.
data DescribeJobResponse = DescribeJobResponse'
  { -- | Information about a specific job part (in the case of an export job),
    -- including shipping information, job status, and other important
    -- metadata.
    subJobMetadata :: Prelude.Maybe [JobMetadata],
    -- | Information about a specific job, including shipping information, job
    -- status, and other important metadata.
    jobMetadata :: Prelude.Maybe JobMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subJobMetadata', 'describeJobResponse_subJobMetadata' - Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
--
-- 'jobMetadata', 'describeJobResponse_jobMetadata' - Information about a specific job, including shipping information, job
-- status, and other important metadata.
--
-- 'httpStatus', 'describeJobResponse_httpStatus' - The response's http status code.
newDescribeJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobResponse
newDescribeJobResponse pHttpStatus_ =
  DescribeJobResponse'
    { subJobMetadata =
        Prelude.Nothing,
      jobMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a specific job part (in the case of an export job),
-- including shipping information, job status, and other important
-- metadata.
describeJobResponse_subJobMetadata :: Lens.Lens' DescribeJobResponse (Prelude.Maybe [JobMetadata])
describeJobResponse_subJobMetadata = Lens.lens (\DescribeJobResponse' {subJobMetadata} -> subJobMetadata) (\s@DescribeJobResponse' {} a -> s {subJobMetadata = a} :: DescribeJobResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about a specific job, including shipping information, job
-- status, and other important metadata.
describeJobResponse_jobMetadata :: Lens.Lens' DescribeJobResponse (Prelude.Maybe JobMetadata)
describeJobResponse_jobMetadata = Lens.lens (\DescribeJobResponse' {jobMetadata} -> jobMetadata) (\s@DescribeJobResponse' {} a -> s {jobMetadata = a} :: DescribeJobResponse)

-- | The response's http status code.
describeJobResponse_httpStatus :: Lens.Lens' DescribeJobResponse Prelude.Int
describeJobResponse_httpStatus = Lens.lens (\DescribeJobResponse' {httpStatus} -> httpStatus) (\s@DescribeJobResponse' {} a -> s {httpStatus = a} :: DescribeJobResponse)

instance Prelude.NFData DescribeJobResponse
