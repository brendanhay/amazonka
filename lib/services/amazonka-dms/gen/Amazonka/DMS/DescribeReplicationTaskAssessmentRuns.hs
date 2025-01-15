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
-- Module      : Amazonka.DMS.DescribeReplicationTaskAssessmentRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.DMS.DescribeReplicationTaskAssessmentRuns
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
    describeReplicationTaskAssessmentRunsResponse_marker,
    describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns,
    describeReplicationTaskAssessmentRunsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
describeReplicationTaskAssessmentRuns_filters = Lens.lens (\DescribeReplicationTaskAssessmentRuns' {filters} -> filters) (\s@DescribeReplicationTaskAssessmentRuns' {} a -> s {filters = a} :: DescribeReplicationTaskAssessmentRuns) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentRunsResponse'
            Prelude.<$> (x Data..?> "Marker")
            Prelude.<*> ( x
                            Data..?> "ReplicationTaskAssessmentRuns"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskAssessmentRuns
  where
  hashWithSalt
    _salt
    DescribeReplicationTaskAssessmentRuns' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentRuns
  where
  rnf DescribeReplicationTaskAssessmentRuns' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeReplicationTaskAssessmentRuns
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeReplicationTaskAssessmentRuns
  where
  toJSON DescribeReplicationTaskAssessmentRuns' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxRecords" Data..=) Prelude.<$> maxRecords
          ]
      )

instance
  Data.ToPath
    DescribeReplicationTaskAssessmentRuns
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeReplicationTaskAssessmentRuns
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { -- | A pagination token returned for you to pass to a subsequent request. If
    -- you pass this token as the @Marker@ value in a subsequent request, the
    -- response includes only records beyond the marker, up to the value
    -- specified in the request by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | One or more premigration assessment runs as specified by @Filters@.
    replicationTaskAssessmentRuns :: Prelude.Maybe [ReplicationTaskAssessmentRun],
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
-- 'marker', 'describeReplicationTaskAssessmentRunsResponse_marker' - A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
--
-- 'replicationTaskAssessmentRuns', 'describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns' - One or more premigration assessment runs as specified by @Filters@.
--
-- 'httpStatus', 'describeReplicationTaskAssessmentRunsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskAssessmentRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskAssessmentRunsResponse
newDescribeReplicationTaskAssessmentRunsResponse
  pHttpStatus_ =
    DescribeReplicationTaskAssessmentRunsResponse'
      { marker =
          Prelude.Nothing,
        replicationTaskAssessmentRuns =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeReplicationTaskAssessmentRunsResponse_marker :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskAssessmentRunsResponse_marker = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {marker} -> marker) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskAssessmentRunsResponse)

-- | One or more premigration assessment runs as specified by @Filters@.
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Prelude.Maybe [ReplicationTaskAssessmentRun])
describeReplicationTaskAssessmentRunsResponse_replicationTaskAssessmentRuns = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {replicationTaskAssessmentRuns} -> replicationTaskAssessmentRuns) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {replicationTaskAssessmentRuns = a} :: DescribeReplicationTaskAssessmentRunsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationTaskAssessmentRunsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse Prelude.Int
describeReplicationTaskAssessmentRunsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskAssessmentRunsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskAssessmentRunsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskAssessmentRunsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskAssessmentRunsResponse
  where
  rnf
    DescribeReplicationTaskAssessmentRunsResponse' {..} =
      Prelude.rnf marker `Prelude.seq`
        Prelude.rnf replicationTaskAssessmentRuns `Prelude.seq`
          Prelude.rnf httpStatus
