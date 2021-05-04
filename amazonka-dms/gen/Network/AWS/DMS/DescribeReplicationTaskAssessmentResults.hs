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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the task assessment results from Amazon S3. This action always
-- returns the latest results.
--
-- This operation returns paginated results.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentResults
  ( -- * Creating a Request
    DescribeReplicationTaskAssessmentResults (..),
    newDescribeReplicationTaskAssessmentResults,

    -- * Request Lenses
    describeReplicationTaskAssessmentResults_replicationTaskArn,
    describeReplicationTaskAssessmentResults_marker,
    describeReplicationTaskAssessmentResults_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationTaskAssessmentResultsResponse (..),
    newDescribeReplicationTaskAssessmentResultsResponse,

    -- * Response Lenses
    describeReplicationTaskAssessmentResultsResponse_bucketName,
    describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults,
    describeReplicationTaskAssessmentResultsResponse_marker,
    describeReplicationTaskAssessmentResultsResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentResults' smart constructor.
data DescribeReplicationTaskAssessmentResults = DescribeReplicationTaskAssessmentResults'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the task.
    -- When this input parameter is specified, the API returns only one result
    -- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | An optional pagination token provided by a previous request. If this
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
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskArn', 'describeReplicationTaskAssessmentResults_replicationTaskArn' - The Amazon Resource Name (ARN) string that uniquely identifies the task.
-- When this input parameter is specified, the API returns only one result
-- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
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
newDescribeReplicationTaskAssessmentResults ::
  DescribeReplicationTaskAssessmentResults
newDescribeReplicationTaskAssessmentResults =
  DescribeReplicationTaskAssessmentResults'
    { replicationTaskArn =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the task.
-- When this input parameter is specified, the API returns only one result
-- and ignore the values of the @MaxRecords@ and @Marker@ parameters.
describeReplicationTaskAssessmentResults_replicationTaskArn :: Lens.Lens' DescribeReplicationTaskAssessmentResults (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResults_replicationTaskArn = Lens.lens (\DescribeReplicationTaskAssessmentResults' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeReplicationTaskAssessmentResults' {} a -> s {replicationTaskArn = a} :: DescribeReplicationTaskAssessmentResults)

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

instance
  Pager.AWSPager
    DescribeReplicationTaskAssessmentResults
  where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_marker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& describeReplicationTaskAssessmentResults_marker
          Lens..~ rs
            Lens.^? describeReplicationTaskAssessmentResultsResponse_marker
              Prelude.. Lens._Just

instance
  Prelude.AWSRequest
    DescribeReplicationTaskAssessmentResults
  where
  type
    Rs DescribeReplicationTaskAssessmentResults =
      DescribeReplicationTaskAssessmentResultsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentResultsResponse'
            Prelude.<$> (x Prelude..?> "BucketName")
              Prelude.<*> ( x Prelude..?> "ReplicationTaskAssessmentResults"
                              Prelude..!@ Prelude.mempty
                          )
              Prelude.<*> (x Prelude..?> "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskAssessmentResults

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentResults

instance
  Prelude.ToHeaders
    DescribeReplicationTaskAssessmentResults
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentResults" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribeReplicationTaskAssessmentResults
  where
  toJSON DescribeReplicationTaskAssessmentResults' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ReplicationTaskArn" Prelude..=)
              Prelude.<$> replicationTaskArn,
            ("Marker" Prelude..=) Prelude.<$> marker,
            ("MaxRecords" Prelude..=) Prelude.<$> maxRecords
          ]
      )

instance
  Prelude.ToPath
    DescribeReplicationTaskAssessmentResults
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeReplicationTaskAssessmentResults
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentResultsResponse' smart constructor.
data DescribeReplicationTaskAssessmentResultsResponse = DescribeReplicationTaskAssessmentResultsResponse'
  { -- | - The Amazon S3 bucket where the task assessment report is located.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The task assessment report.
    replicationTaskAssessmentResults :: Prelude.Maybe [ReplicationTaskAssessmentResult],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'describeReplicationTaskAssessmentResultsResponse_bucketName' - - The Amazon S3 bucket where the task assessment report is located.
--
-- 'replicationTaskAssessmentResults', 'describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults' - The task assessment report.
--
-- 'marker', 'describeReplicationTaskAssessmentResultsResponse_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationTaskAssessmentResultsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskAssessmentResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskAssessmentResultsResponse
newDescribeReplicationTaskAssessmentResultsResponse
  pHttpStatus_ =
    DescribeReplicationTaskAssessmentResultsResponse'
      { bucketName =
          Prelude.Nothing,
        replicationTaskAssessmentResults =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | - The Amazon S3 bucket where the task assessment report is located.
describeReplicationTaskAssessmentResultsResponse_bucketName :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResultsResponse_bucketName = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {bucketName} -> bucketName) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {bucketName = a} :: DescribeReplicationTaskAssessmentResultsResponse)

-- | The task assessment report.
describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe [ReplicationTaskAssessmentResult])
describeReplicationTaskAssessmentResultsResponse_replicationTaskAssessmentResults = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {replicationTaskAssessmentResults} -> replicationTaskAssessmentResults) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {replicationTaskAssessmentResults = a} :: DescribeReplicationTaskAssessmentResultsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskAssessmentResultsResponse_marker :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentResultsResponse_marker = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentResultsResponse)

-- | The response's http status code.
describeReplicationTaskAssessmentResultsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskAssessmentResultsResponse Prelude.Int
describeReplicationTaskAssessmentResultsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskAssessmentResultsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskAssessmentResultsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskAssessmentResultsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentResultsResponse
