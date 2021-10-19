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
-- Module      : Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of individual assessments based on filter
-- settings.
--
-- These filter settings can specify a combination of premigration
-- assessment runs, migration tasks, and assessment status values.
module Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
  ( -- * Creating a Request
    DescribeReplicationTaskIndividualAssessments (..),
    newDescribeReplicationTaskIndividualAssessments,

    -- * Request Lenses
    describeReplicationTaskIndividualAssessments_filters,
    describeReplicationTaskIndividualAssessments_marker,
    describeReplicationTaskIndividualAssessments_maxRecords,

    -- * Destructuring the Response
    DescribeReplicationTaskIndividualAssessmentsResponse (..),
    newDescribeReplicationTaskIndividualAssessmentsResponse,

    -- * Response Lenses
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,
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
-- /See:/ 'newDescribeReplicationTaskIndividualAssessments' smart constructor.
data DescribeReplicationTaskIndividualAssessments = DescribeReplicationTaskIndividualAssessments'
  { -- | Filters applied to the individual assessments described in the form of
    -- key-value pairs.
    --
    -- Valid filter names: @replication-task-assessment-run-arn@,
    -- @replication-task-arn@, @status@
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
-- Create a value of 'DescribeReplicationTaskIndividualAssessments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeReplicationTaskIndividualAssessments_filters' - Filters applied to the individual assessments described in the form of
-- key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@,
-- @replication-task-arn@, @status@
--
-- 'marker', 'describeReplicationTaskIndividualAssessments_marker' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeReplicationTaskIndividualAssessments_maxRecords' - The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
newDescribeReplicationTaskIndividualAssessments ::
  DescribeReplicationTaskIndividualAssessments
newDescribeReplicationTaskIndividualAssessments =
  DescribeReplicationTaskIndividualAssessments'
    { filters =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Filters applied to the individual assessments described in the form of
-- key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@,
-- @replication-task-arn@, @status@
describeReplicationTaskIndividualAssessments_filters :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Prelude.Maybe [Filter])
describeReplicationTaskIndividualAssessments_filters = Lens.lens (\DescribeReplicationTaskIndividualAssessments' {filters} -> filters) (\s@DescribeReplicationTaskIndividualAssessments' {} a -> s {filters = a} :: DescribeReplicationTaskIndividualAssessments) Prelude.. Lens.mapping Lens.coerced

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeReplicationTaskIndividualAssessments_marker :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Prelude.Maybe Prelude.Text)
describeReplicationTaskIndividualAssessments_marker = Lens.lens (\DescribeReplicationTaskIndividualAssessments' {marker} -> marker) (\s@DescribeReplicationTaskIndividualAssessments' {} a -> s {marker = a} :: DescribeReplicationTaskIndividualAssessments)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a pagination token
-- called a marker is included in the response so that the remaining
-- results can be retrieved.
describeReplicationTaskIndividualAssessments_maxRecords :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Prelude.Maybe Prelude.Int)
describeReplicationTaskIndividualAssessments_maxRecords = Lens.lens (\DescribeReplicationTaskIndividualAssessments' {maxRecords} -> maxRecords) (\s@DescribeReplicationTaskIndividualAssessments' {} a -> s {maxRecords = a} :: DescribeReplicationTaskIndividualAssessments)

instance
  Core.AWSRequest
    DescribeReplicationTaskIndividualAssessments
  where
  type
    AWSResponse
      DescribeReplicationTaskIndividualAssessments =
      DescribeReplicationTaskIndividualAssessmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskIndividualAssessmentsResponse'
            Prelude.<$> ( x Core..?> "ReplicationTaskIndividualAssessments"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskIndividualAssessments

instance
  Prelude.NFData
    DescribeReplicationTaskIndividualAssessments

instance
  Core.ToHeaders
    DescribeReplicationTaskIndividualAssessments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeReplicationTaskIndividualAssessments" ::
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
    DescribeReplicationTaskIndividualAssessments
  where
  toJSON
    DescribeReplicationTaskIndividualAssessments' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Filters" Core..=) Prelude.<$> filters,
              ("Marker" Core..=) Prelude.<$> marker,
              ("MaxRecords" Core..=) Prelude.<$> maxRecords
            ]
        )

instance
  Core.ToPath
    DescribeReplicationTaskIndividualAssessments
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeReplicationTaskIndividualAssessments
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskIndividualAssessmentsResponse' smart constructor.
data DescribeReplicationTaskIndividualAssessmentsResponse = DescribeReplicationTaskIndividualAssessmentsResponse'
  { -- | One or more individual assessments as specified by @Filters@.
    replicationTaskIndividualAssessments :: Prelude.Maybe [ReplicationTaskIndividualAssessment],
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
-- Create a value of 'DescribeReplicationTaskIndividualAssessmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTaskIndividualAssessments', 'describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments' - One or more individual assessments as specified by @Filters@.
--
-- 'marker', 'describeReplicationTaskIndividualAssessmentsResponse_marker' - A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
--
-- 'httpStatus', 'describeReplicationTaskIndividualAssessmentsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskIndividualAssessmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskIndividualAssessmentsResponse
newDescribeReplicationTaskIndividualAssessmentsResponse
  pHttpStatus_ =
    DescribeReplicationTaskIndividualAssessmentsResponse'
      { replicationTaskIndividualAssessments =
          Prelude.Nothing,
        marker =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | One or more individual assessments as specified by @Filters@.
describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Prelude.Maybe [ReplicationTaskIndividualAssessment])
describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {replicationTaskIndividualAssessments} -> replicationTaskIndividualAssessments) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {replicationTaskIndividualAssessments = a} :: DescribeReplicationTaskIndividualAssessmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeReplicationTaskIndividualAssessmentsResponse_marker :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskIndividualAssessmentsResponse_marker = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {marker} -> marker) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)

-- | The response's http status code.
describeReplicationTaskIndividualAssessmentsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse Prelude.Int
describeReplicationTaskIndividualAssessmentsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskIndividualAssessmentsResponse
