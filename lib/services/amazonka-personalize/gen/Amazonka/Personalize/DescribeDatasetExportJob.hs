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
-- Module      : Amazonka.Personalize.DescribeDatasetExportJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the dataset export job created by
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetExportJob.html CreateDatasetExportJob>,
-- including the export job status.
module Amazonka.Personalize.DescribeDatasetExportJob
  ( -- * Creating a Request
    DescribeDatasetExportJob (..),
    newDescribeDatasetExportJob,

    -- * Request Lenses
    describeDatasetExportJob_datasetExportJobArn,

    -- * Destructuring the Response
    DescribeDatasetExportJobResponse (..),
    newDescribeDatasetExportJobResponse,

    -- * Response Lenses
    describeDatasetExportJobResponse_datasetExportJob,
    describeDatasetExportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatasetExportJob' smart constructor.
data DescribeDatasetExportJob = DescribeDatasetExportJob'
  { -- | The Amazon Resource Name (ARN) of the dataset export job to describe.
    datasetExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetExportJobArn', 'describeDatasetExportJob_datasetExportJobArn' - The Amazon Resource Name (ARN) of the dataset export job to describe.
newDescribeDatasetExportJob ::
  -- | 'datasetExportJobArn'
  Prelude.Text ->
  DescribeDatasetExportJob
newDescribeDatasetExportJob pDatasetExportJobArn_ =
  DescribeDatasetExportJob'
    { datasetExportJobArn =
        pDatasetExportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset export job to describe.
describeDatasetExportJob_datasetExportJobArn :: Lens.Lens' DescribeDatasetExportJob Prelude.Text
describeDatasetExportJob_datasetExportJobArn = Lens.lens (\DescribeDatasetExportJob' {datasetExportJobArn} -> datasetExportJobArn) (\s@DescribeDatasetExportJob' {} a -> s {datasetExportJobArn = a} :: DescribeDatasetExportJob)

instance Core.AWSRequest DescribeDatasetExportJob where
  type
    AWSResponse DescribeDatasetExportJob =
      DescribeDatasetExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetExportJobResponse'
            Prelude.<$> (x Core..?> "datasetExportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatasetExportJob where
  hashWithSalt _salt DescribeDatasetExportJob' {..} =
    _salt `Prelude.hashWithSalt` datasetExportJobArn

instance Prelude.NFData DescribeDatasetExportJob where
  rnf DescribeDatasetExportJob' {..} =
    Prelude.rnf datasetExportJobArn

instance Core.ToHeaders DescribeDatasetExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonPersonalize.DescribeDatasetExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDatasetExportJob where
  toJSON DescribeDatasetExportJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("datasetExportJobArn" Core..= datasetExportJobArn)
          ]
      )

instance Core.ToPath DescribeDatasetExportJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDatasetExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetExportJobResponse' smart constructor.
data DescribeDatasetExportJobResponse = DescribeDatasetExportJobResponse'
  { -- | Information about the dataset export job, including the status.
    --
    -- The status is one of the following values:
    --
    -- -   CREATE PENDING
    --
    -- -   CREATE IN_PROGRESS
    --
    -- -   ACTIVE
    --
    -- -   CREATE FAILED
    datasetExportJob :: Prelude.Maybe DatasetExportJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetExportJob', 'describeDatasetExportJobResponse_datasetExportJob' - Information about the dataset export job, including the status.
--
-- The status is one of the following values:
--
-- -   CREATE PENDING
--
-- -   CREATE IN_PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
--
-- 'httpStatus', 'describeDatasetExportJobResponse_httpStatus' - The response's http status code.
newDescribeDatasetExportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetExportJobResponse
newDescribeDatasetExportJobResponse pHttpStatus_ =
  DescribeDatasetExportJobResponse'
    { datasetExportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the dataset export job, including the status.
--
-- The status is one of the following values:
--
-- -   CREATE PENDING
--
-- -   CREATE IN_PROGRESS
--
-- -   ACTIVE
--
-- -   CREATE FAILED
describeDatasetExportJobResponse_datasetExportJob :: Lens.Lens' DescribeDatasetExportJobResponse (Prelude.Maybe DatasetExportJob)
describeDatasetExportJobResponse_datasetExportJob = Lens.lens (\DescribeDatasetExportJobResponse' {datasetExportJob} -> datasetExportJob) (\s@DescribeDatasetExportJobResponse' {} a -> s {datasetExportJob = a} :: DescribeDatasetExportJobResponse)

-- | The response's http status code.
describeDatasetExportJobResponse_httpStatus :: Lens.Lens' DescribeDatasetExportJobResponse Prelude.Int
describeDatasetExportJobResponse_httpStatus = Lens.lens (\DescribeDatasetExportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetExportJobResponse' {} a -> s {httpStatus = a} :: DescribeDatasetExportJobResponse)

instance
  Prelude.NFData
    DescribeDatasetExportJobResponse
  where
  rnf DescribeDatasetExportJobResponse' {..} =
    Prelude.rnf datasetExportJob
      `Prelude.seq` Prelude.rnf httpStatus
