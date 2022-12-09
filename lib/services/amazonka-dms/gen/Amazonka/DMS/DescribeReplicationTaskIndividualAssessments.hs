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
-- Module      : Amazonka.DMS.DescribeReplicationTaskIndividualAssessments
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.DMS.DescribeReplicationTaskIndividualAssessments
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
    describeReplicationTaskIndividualAssessmentsResponse_marker,
    describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments,
    describeReplicationTaskIndividualAssessmentsResponse_httpStatus,
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskIndividualAssessmentsResponse'
            Prelude.<$> (x Data..?> "Marker")
              Prelude.<*> ( x Data..?> "ReplicationTaskIndividualAssessments"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeReplicationTaskIndividualAssessments
  where
  hashWithSalt
    _salt
    DescribeReplicationTaskIndividualAssessments' {..} =
      _salt `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` maxRecords

instance
  Prelude.NFData
    DescribeReplicationTaskIndividualAssessments
  where
  rnf DescribeReplicationTaskIndividualAssessments' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords

instance
  Data.ToHeaders
    DescribeReplicationTaskIndividualAssessments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeReplicationTaskIndividualAssessments" ::
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
    DescribeReplicationTaskIndividualAssessments
  where
  toJSON
    DescribeReplicationTaskIndividualAssessments' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Filters" Data..=) Prelude.<$> filters,
              ("Marker" Data..=) Prelude.<$> marker,
              ("MaxRecords" Data..=) Prelude.<$> maxRecords
            ]
        )

instance
  Data.ToPath
    DescribeReplicationTaskIndividualAssessments
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeReplicationTaskIndividualAssessments
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeReplicationTaskIndividualAssessmentsResponse' smart constructor.
data DescribeReplicationTaskIndividualAssessmentsResponse = DescribeReplicationTaskIndividualAssessmentsResponse'
  { -- | A pagination token returned for you to pass to a subsequent request. If
    -- you pass this token as the @Marker@ value in a subsequent request, the
    -- response includes only records beyond the marker, up to the value
    -- specified in the request by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | One or more individual assessments as specified by @Filters@.
    replicationTaskIndividualAssessments :: Prelude.Maybe [ReplicationTaskIndividualAssessment],
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
-- 'marker', 'describeReplicationTaskIndividualAssessmentsResponse_marker' - A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
--
-- 'replicationTaskIndividualAssessments', 'describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments' - One or more individual assessments as specified by @Filters@.
--
-- 'httpStatus', 'describeReplicationTaskIndividualAssessmentsResponse_httpStatus' - The response's http status code.
newDescribeReplicationTaskIndividualAssessmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeReplicationTaskIndividualAssessmentsResponse
newDescribeReplicationTaskIndividualAssessmentsResponse
  pHttpStatus_ =
    DescribeReplicationTaskIndividualAssessmentsResponse'
      { marker =
          Prelude.Nothing,
        replicationTaskIndividualAssessments =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | A pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeReplicationTaskIndividualAssessmentsResponse_marker :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Prelude.Maybe Prelude.Text)
describeReplicationTaskIndividualAssessmentsResponse_marker = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {marker} -> marker) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {marker = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)

-- | One or more individual assessments as specified by @Filters@.
describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Prelude.Maybe [ReplicationTaskIndividualAssessment])
describeReplicationTaskIndividualAssessmentsResponse_replicationTaskIndividualAssessments = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {replicationTaskIndividualAssessments} -> replicationTaskIndividualAssessments) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {replicationTaskIndividualAssessments = a} :: DescribeReplicationTaskIndividualAssessmentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeReplicationTaskIndividualAssessmentsResponse_httpStatus :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse Prelude.Int
describeReplicationTaskIndividualAssessmentsResponse_httpStatus = Lens.lens (\DescribeReplicationTaskIndividualAssessmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeReplicationTaskIndividualAssessmentsResponse' {} a -> s {httpStatus = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)

instance
  Prelude.NFData
    DescribeReplicationTaskIndividualAssessmentsResponse
  where
  rnf
    DescribeReplicationTaskIndividualAssessmentsResponse' {..} =
      Prelude.rnf marker
        `Prelude.seq` Prelude.rnf replicationTaskIndividualAssessments
        `Prelude.seq` Prelude.rnf httpStatus
