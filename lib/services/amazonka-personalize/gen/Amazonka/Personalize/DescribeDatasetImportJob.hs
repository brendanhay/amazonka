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
-- Module      : Amazonka.Personalize.DescribeDatasetImportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the dataset import job created by
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_CreateDatasetImportJob.html CreateDatasetImportJob>,
-- including the import job status.
module Amazonka.Personalize.DescribeDatasetImportJob
  ( -- * Creating a Request
    DescribeDatasetImportJob (..),
    newDescribeDatasetImportJob,

    -- * Request Lenses
    describeDatasetImportJob_datasetImportJobArn,

    -- * Destructuring the Response
    DescribeDatasetImportJobResponse (..),
    newDescribeDatasetImportJobResponse,

    -- * Response Lenses
    describeDatasetImportJobResponse_datasetImportJob,
    describeDatasetImportJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Personalize.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDatasetImportJob' smart constructor.
data DescribeDatasetImportJob = DescribeDatasetImportJob'
  { -- | The Amazon Resource Name (ARN) of the dataset import job to describe.
    datasetImportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetImportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJobArn', 'describeDatasetImportJob_datasetImportJobArn' - The Amazon Resource Name (ARN) of the dataset import job to describe.
newDescribeDatasetImportJob ::
  -- | 'datasetImportJobArn'
  Prelude.Text ->
  DescribeDatasetImportJob
newDescribeDatasetImportJob pDatasetImportJobArn_ =
  DescribeDatasetImportJob'
    { datasetImportJobArn =
        pDatasetImportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the dataset import job to describe.
describeDatasetImportJob_datasetImportJobArn :: Lens.Lens' DescribeDatasetImportJob Prelude.Text
describeDatasetImportJob_datasetImportJobArn = Lens.lens (\DescribeDatasetImportJob' {datasetImportJobArn} -> datasetImportJobArn) (\s@DescribeDatasetImportJob' {} a -> s {datasetImportJobArn = a} :: DescribeDatasetImportJob)

instance Core.AWSRequest DescribeDatasetImportJob where
  type
    AWSResponse DescribeDatasetImportJob =
      DescribeDatasetImportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetImportJobResponse'
            Prelude.<$> (x Data..?> "datasetImportJob")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDatasetImportJob where
  hashWithSalt _salt DescribeDatasetImportJob' {..} =
    _salt `Prelude.hashWithSalt` datasetImportJobArn

instance Prelude.NFData DescribeDatasetImportJob where
  rnf DescribeDatasetImportJob' {..} =
    Prelude.rnf datasetImportJobArn

instance Data.ToHeaders DescribeDatasetImportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonPersonalize.DescribeDatasetImportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDatasetImportJob where
  toJSON DescribeDatasetImportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("datasetImportJobArn" Data..= datasetImportJobArn)
          ]
      )

instance Data.ToPath DescribeDatasetImportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDatasetImportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDatasetImportJobResponse' smart constructor.
data DescribeDatasetImportJobResponse = DescribeDatasetImportJobResponse'
  { -- | Information about the dataset import job, including the status.
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
    datasetImportJob :: Prelude.Maybe DatasetImportJob,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDatasetImportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetImportJob', 'describeDatasetImportJobResponse_datasetImportJob' - Information about the dataset import job, including the status.
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
-- 'httpStatus', 'describeDatasetImportJobResponse_httpStatus' - The response's http status code.
newDescribeDatasetImportJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDatasetImportJobResponse
newDescribeDatasetImportJobResponse pHttpStatus_ =
  DescribeDatasetImportJobResponse'
    { datasetImportJob =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the dataset import job, including the status.
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
describeDatasetImportJobResponse_datasetImportJob :: Lens.Lens' DescribeDatasetImportJobResponse (Prelude.Maybe DatasetImportJob)
describeDatasetImportJobResponse_datasetImportJob = Lens.lens (\DescribeDatasetImportJobResponse' {datasetImportJob} -> datasetImportJob) (\s@DescribeDatasetImportJobResponse' {} a -> s {datasetImportJob = a} :: DescribeDatasetImportJobResponse)

-- | The response's http status code.
describeDatasetImportJobResponse_httpStatus :: Lens.Lens' DescribeDatasetImportJobResponse Prelude.Int
describeDatasetImportJobResponse_httpStatus = Lens.lens (\DescribeDatasetImportJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDatasetImportJobResponse' {} a -> s {httpStatus = a} :: DescribeDatasetImportJobResponse)

instance
  Prelude.NFData
    DescribeDatasetImportJobResponse
  where
  rnf DescribeDatasetImportJobResponse' {..} =
    Prelude.rnf datasetImportJob
      `Prelude.seq` Prelude.rnf httpStatus
