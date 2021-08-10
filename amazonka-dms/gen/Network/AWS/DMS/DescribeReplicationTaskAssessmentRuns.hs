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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of premigration assessment runs based on filter
-- settings.
--
-- These filter settings can specify a combination of premigration
-- assessment runs, migration tasks, replication instances, and assessment
-- run status values.
--
-- This operation doesn\'t return information about individual assessments.
-- For this information, see the
-- @DescribeReplicationTaskIndividualAssessments@ operation.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
  ( -- * Creating a Request
    DescribeReplicationTaskAssessmentRuns (..),
    newDescribeReplicationTaskAssessmentRuns,

    -- * Request Lenses
    describeReplicationTaskAssessmentRuns_filters,
    describeReplicationTaskAssessmentRuns_marker,
    describeReplicationTaskAssessmentRuns_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationTaskAssessmentRunsResponse (..),
    newDescribeReplicationTaskAssessmentRunsResponse,

    -- * Response Lenses
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentRuns' smart constructor.
data DescribeReplicationTaskAssessmentRuns = DescribeReplicationTaskAssessmentRuns'
  { -- | Filters applied to the premigration assessment runs described in the
    -- form of key-value pairs.
    --
    -- Valid filter names: @replication-task-assessment-run-arn@,
    -- @replication-task-arn@, @replication-instance-arn@, @status@
    filters :: Prelude.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReplicationTaskAssessmentRuns_filters' - Filters applied to the premigration assessment runs described in the
-- form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@,
-- @replication-task-arn@, @replication-instance-arn@, @status@
--
-- 'marker', 'describeReplicationTaskAssessmentRuns_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationTaskAssessmentRuns_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
newDescribeReplicationTaskAssessmentRuns ::
  DescribeReplicationTaskAssessmentRuns
newDescribeReplicationTaskAssessmentRuns =
  DescribeReplicationTaskAssessmentRuns'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to the premigration assessment runs described in the
-- form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@,
-- @replication-task-arn@, @replication-instance-arn@, @status@
describeReplicationTaskAssessmentRuns_filters :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Prelude.Maybe [Filter])
describeReplicationTaskAssessmentRuns_filters = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {filters} -> filters) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {filters = a} :: DescribeReplicationTaskAssessmentRuns) Prelude.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskAssessmentRuns_marker :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentRuns_marker = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentRuns)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeReplicationTaskAssessmentRuns_maxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Prelude.Maybe Prelude.Int)
describeReplicationTaskAssessmentRuns_maxRecords = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {maxRecords} -> maxRecords) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {maxRecords = a} :: DescribeReplicationTaskAssessmentRuns)

instance
  Core.AWSRequest
    DescribeReplicationTaskAssessmentRuns
  where
  type
    AWSResponse
      DescribeReplicationTaskAssessmentRuns =
      DescribeReplicationTaskAssessmentRunsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentRunsResponse'
            Prelude.<$> ( x Core..?> "ReplicationTaskAssessmentRuns"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskAssessmentRuns

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentRuns

instance
  Core.ToHeaders
    DescribeReplicationTaskAssessmentRuns
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns" ::
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
    DescribeReplicationTaskAssessmentRuns
  where
  toJSON DescribeReplicationTaskAssessmentRuns' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("Marker" Core..=) Prelude.<$> marker,
            ("MaxRecords" Core..=) Prelude.<$> maxRecords
          ]
      )

instance
  Core.ToPath
    DescribeReplicationTaskAssessmentRuns
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReplicationTaskAssessmentRuns
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { -- | One or more premigration assessment runs as specified by @Filters@.
    replicationTaskAssessmentRuns :: Prelude.Maybe [ReplicationTaskAssessmentRun],
    -- | A pagination token returned for you to pass to a subsequent request. If
    -- you pass this token as the @Marker@ value in a subsequent request, the
    -- response includes only records beyond the marker, up to the value
    -- specified in the request by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeReplicationTaskAssessmentRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskAssessmentRuns', 'describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns' - One or more premigration assessment runs as specified by @Filters@.
--
-- 'marker', 'describeReplicationTaskAssessmentRunsResponse_marker' - A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationTaskAssessmentRunsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskAssessmentRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskAssessmentRunsResponse
newDescribeReplicationTaskAssessmentRunsResponse
  pHttpStatus_ =
    DescribeReplicationTaskAssessmentRunsResponse'
      { replicationTaskAssessmentRuns =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | One or more premigration assessment runs as specified by @Filters@.
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Prelude.Maybe [ReplicationTaskAssessmentRun])
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {replicationTaskAssessmentRuns} -> replicationTaskAssessmentRuns) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {replicationTaskAssessmentRuns = a} :: DescribeReplicationTaskAssessmentRunsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeReplicationTaskAssessmentRunsResponse_marker :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentRunsResponse_marker = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentRunsResponse)

-- | The response's http status code.
describeReplicationTaskAssessmentRunsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse Prelude.Int
describeReplicationTaskAssessmentRunsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskAssessmentRunsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentRunsResponse
