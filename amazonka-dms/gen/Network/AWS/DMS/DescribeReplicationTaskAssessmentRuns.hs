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
    filters :: Core.Maybe [Filter],
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified @MaxRecords@ value, a pagination token
    -- called a marker is included in the response so that the remaining
    -- results can be retrieved.
    maxRecords :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      marker = Core.Nothing,
      maxRecords = Core.Nothing
    }

-- | Filters applied to the premigration assessment runs described in the
-- form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@,
-- @replication-task-arn@, @replication-instance-arn@, @status@
describeReplicationTaskAssessmentRuns_filters :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe [Filter])
describeReplicationTaskAssessmentRuns_filters = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {filters} -> filters) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {filters = a} :: DescribeReplicationTaskAssessmentRuns) Core.. Lens.mapping Lens._Coerce

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskAssessmentRuns_marker :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe Core.Text)
describeReplicationTaskAssessmentRuns_marker = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentRuns)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeReplicationTaskAssessmentRuns_maxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Core.Maybe Core.Int)
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
            Core.<$> ( x Core..?> "ReplicationTaskAssessmentRuns"
                         Core..!@ Core.mempty
                     )
              Core.<*> (x Core..?> "Marker")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeReplicationTaskAssessmentRuns

instance
  Core.NFData
    DescribeReplicationTaskAssessmentRuns

instance
  Core.ToHeaders
    DescribeReplicationTaskAssessmentRuns
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeReplicationTaskAssessmentRuns
  where
  toJSON DescribeReplicationTaskAssessmentRuns' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxRecords" Core..=) Core.<$> maxRecords
          ]
      )

instance
  Core.ToPath
    DescribeReplicationTaskAssessmentRuns
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeReplicationTaskAssessmentRuns
  where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { -- | One or more premigration assessment runs as specified by @Filters@.
    replicationTaskAssessmentRuns :: Core.Maybe [ReplicationTaskAssessmentRun],
    -- | A pagination token returned for you to pass to a subsequent request. If
    -- you pass this token as the @Marker@ value in a subsequent request, the
    -- response includes only records beyond the marker, up to the value
    -- specified in the request by @MaxRecords@.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeReplicationTaskAssessmentRunsResponse
newDescribeReplicationTaskAssessmentRunsResponse
  pHttpStatus_ =
    DescribeReplicationTaskAssessmentRunsResponse'
      { replicationTaskAssessmentRuns =
          Core.Nothing,
        marker = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | One or more premigration assessment runs as specified by @Filters@.
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Core.Maybe [ReplicationTaskAssessmentRun])
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {replicationTaskAssessmentRuns} -> replicationTaskAssessmentRuns) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {replicationTaskAssessmentRuns = a} :: DescribeReplicationTaskAssessmentRunsResponse) Core.. Lens.mapping Lens._Coerce

-- | A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeReplicationTaskAssessmentRunsResponse_marker :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Core.Maybe Core.Text)
describeReplicationTaskAssessmentRunsResponse_marker = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentRunsResponse)

-- | The response's http status code.
describeReplicationTaskAssessmentRunsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse Core.Int
describeReplicationTaskAssessmentRunsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskAssessmentRunsResponse)

instance
  Core.NFData
    DescribeReplicationTaskAssessmentRunsResponse
