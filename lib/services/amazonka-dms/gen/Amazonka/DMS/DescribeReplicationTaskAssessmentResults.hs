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
-- Module      : Amazonka.DMS.DescribeReplicationTaskAssessmentResults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the task assessment results from the Amazon S3 bucket that DMS
-- creates in your Amazon Web Services account. This action always returns
-- the latest results.
--
-- For more information about DMS task assessments, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Creating a task assessment report>
-- in the /Database Migration Service User Guide/.
--
-- This operation returns paginated results.
module Amazonka.DMS.DescribeReplicationTaskAssessmentResults
  ( -- * Creating a Request
    DescribeReplicationTaskAssessmentResults (..),
    newDescribeReplicationTaskAssessmentResults,

    -- * Request Lenses
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResults_maxRecords,
    describeReplicationTaskAssessmentResults_replicationTaskArn,

    -- * Destructuring the Response
    DescribeReplicationTaskAssessmentResultsResponse (..),
    newDescribeReplicationTaskAssessmentResultsResponse,

    -- * Response Lenses
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentResults' smart constructor.
data DescribeReplicationTaskAssessmentResults = DescribeReplicationTaskAssessmentResults'
  { -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    --
    -- Default: 100
    --
    -- Constraints: Minimum 20, maximum 100.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) string that uniquely identifies the task.
    -- When this input parameter is specified, the API returns only one result
    -- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
    replicationTaskArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeReplicationTaskAssessmentResults_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationTaskAssessmentResults_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
--
-- 'replicationTaskArn', 'describeReplicationTaskAssessmentResults_replicationTaskArn' - The Amazon Resource Name (ARN) string that uniquely identifies the task.
-- When this input parameter is specified, the API returns only one result
-- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
newDescribeReplicationTaskAssessmentResults ::
  DescribeReplicationTaskAssessmentResults
newDescribeReplicationTaskAssessmentResults =
  DescribeReplicationTaskAssessmentResults'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      replicationTaskArn =
        Prelude.Nothing
    }

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskAssessmentResults_marker :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResults_marker = Lens.lens (\DescribeReplicationTaskAssessmentResults' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentResults' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentResults)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
--
-- Default: 100
--
-- Constraints: Minimum 20, maximum 100.
describeReplicationTaskAssessmentResults_maxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Prelude.Maybe Prelude.Int)
describeReplicationTaskAssessmentResults_maxRecords = Lens.lens (\DescribeReplicationTaskAssessmentResults' {maxRecords} -> maxRecords) (\s@DescribeReplicationTaskAssessmentResults' {} a -> s {maxRecords = a} :: DescribeReplicationTaskAssessmentResults)

-- | The Amazon Resource Name (ARN) string that uniquely identifies the task.
-- When this input parameter is specified, the API returns only one result
-- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
describeReplicationTaskAssessmentResults_replicationTaskArn :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResults_replicationTaskArn = Lens.lens (\DescribeReplicationTaskAssessmentResults' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeReplicationTaskAssessmentResults' {} a -> s {replicationTaskArn = a} :: DescribeReplicationTaskAssessmentResults)

instance
  Core.AWSPager
    DescribeReplicationTaskAssessmentResults
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeReplicationTaskAssessmentResults_marker
          Lens..~ rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_marker
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeReplicationTaskAssessmentResults
  where
  type
    AWSResponse
      DescribeReplicationTaskAssessmentResults =
      DescribeReplicationTaskAssessmentResultsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentResultsResponse'
            Prelude.<$> ( x Core..?> "ReplicationTaskAssessmentResults"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "Marker")
              Prelude.<*> (x Core..?> "BucketName")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskAssessmentResults
  where
  hashWithSalt
    _salt
    DescribeReplicationTaskAssessmentResults' {..} =
      _salt `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords
        `Prelude.hashWithSalt` replicationTaskArn

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentResults
  where
  rnf DescribeReplicationTaskAssessmentResults' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf replicationTaskArn

instance
  Core.ToHeaders
    DescribeReplicationTaskAssessmentResults
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentResults" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeReplicationTaskAssessmentResults
  where
  toJSON DescribeReplicationTaskAssessmentResults' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords,
            ("ReplicationTaskArn" Core..=)
              Prelude.<$> replicationTaskArn
          ]
      )

instance
  Core.ToPath
    DescribeReplicationTaskAssessmentResults
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReplicationTaskAssessmentResults
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentResultsResponse' smart constructor.
data DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse'
  { -- | The task assessment report.
    replicationTaskAssessmentResults :: Prelude.Maybe [ReplicationTaskAssessmentResult],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | - The Amazon S3 bucket where the task assessment report is located.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentResults', 'describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults' - The task assessment report.
--
-- 'marker', 'describeReplicationTaskAssessmentResultsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'bucketName', 'describeReplicationTaskAssessmentResultsResponse_bucketName' - - The Amazon S3 bucket where the task assessment report is located.
--
-- 'httpStatus', 'describeReplicationTaskAssessmentResultsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskAssessmentResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskAssessmentResultsResponse
newDescribeReplicationTaskAssessmentResultsResponse
  pHttpStatus_ =
    DescribeReplicationTaskAssessmentResultsResponse'
      { replicationTaskAssessmentResults =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        bucketName =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The task assessment report.
describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe [ReplicationTaskAssessmentResult])
describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {replicationTaskAssessmentResults} -> replicationTaskAssessmentResults) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {replicationTaskAssessmentResults = a} :: DescribeReplicationTaskAssessmentResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskAssessmentResultsResponse_marker :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResultsResponse_marker = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentResultsResponse)

-- | - The Amazon S3 bucket where the task assessment report is located.
describeReplicationTaskAssessmentResultsResponse_bucketName :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResultsResponse_bucketName = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {bucketName} -> bucketName) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {bucketName = a} :: DescribeReplicationTaskAssessmentResultsResponse)

-- | The response's http status code.
describeReplicationTaskAssessmentResultsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse Prelude.Int
describeReplicationTaskAssessmentResultsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskAssessmentResultsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentResultsResponse
  where
  rnf
    DescribeReplicationTaskAssessmentResultsResponse' {..} =
      Prelude.rnf replicationTaskAssessmentResults
        `Prelude.seq` Prelude.rnf marker
        `Prelude.seq` Prelude.rnf bucketName
        `Prelude.seq` Prelude.rnf httpStatus
