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
-- Module      : Network.AWS.DMS.DescribeApplicableIndividualAssessments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of individual assessments that you can specify for a new
-- premigration assessment run, given one or more parameters.
--
-- If you specify an existing migration task, this operation provides the
-- default individual assessments you can specify for that task. Otherwise,
-- the specified parameters model elements of a possible migration task on
-- which to base a premigration assessment run.
--
-- To use these migration task modeling parameters, you must specify an
-- existing replication instance, a source database engine, a target
-- database engine, and a migration type. This combination of parameters
-- potentially limits the default individual assessments available for an
-- assessment run created for a corresponding migration task.
--
-- If you specify no parameters, this operation provides a list of all
-- possible individual assessments that you can specify for an assessment
-- run. If you specify any one of the task modeling parameters, you must
-- specify all of them or the operation cannot provide a list of individual
-- assessments. The only parameter that you can specify alone is for an
-- existing migration task. The specified task definition then determines
-- the default list of individual assessments that you can specify in an
-- assessment run for the task.
module Network.AWS.DMS.DescribeApplicableIndividualAssessments
  ( -- * Creating a Request
    DescribeApplicableIndividualAssessments (..),
    newDescribeApplicableIndividualAssessments,

    -- * Request Lenses
    describeApplicableIndividualAssessments_migrationType,
    describeApplicableIndividualAssessments_replicationTaskArn,
    describeApplicableIndividualAssessments_sourceEngineName,
    describeApplicableIndividualAssessments_replicationInstanceArn,
    describeApplicableIndividualAssessments_targetEngineName,
    describeApplicableIndividualAssessments_marker,
    describeApplicableIndividualAssessments_maxRecords,

    -- * Destructuring the Response
    DescribeApplicableIndividualAssessmentsResponse (..),
    newDescribeApplicableIndividualAssessmentsResponse,

    -- * Response Lenses
    describeApplicableIndividualAssessmentsResponse_individualAssessmentNames,
    describeApplicableIndividualAssessmentsResponse_marker,
    describeApplicableIndividualAssessmentsResponse_httpStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeApplicableIndividualAssessments' smart constructor.
data DescribeApplicableIndividualAssessments = DescribeApplicableIndividualAssessments'
  { -- | Name of the migration type that each provided individual assessment must
    -- support.
    migrationType :: Prelude.Maybe MigrationTypeValue,
    -- | Amazon Resource Name (ARN) of a migration task on which you want to base
    -- the default list of individual assessments.
    replicationTaskArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a database engine that the specified replication instance
    -- supports as a source.
    sourceEngineName :: Prelude.Maybe Prelude.Text,
    -- | ARN of a replication instance on which you want to base the default list
    -- of individual assessments.
    replicationInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | Name of a database engine that the specified replication instance
    -- supports as a target.
    targetEngineName :: Prelude.Maybe Prelude.Text,
    -- | Optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- marker, up to the value specified by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of records to include in the response. If more records
    -- exist than the specified @MaxRecords@ value, a pagination token called a
    -- marker is included in the response so that the remaining results can be
    -- retrieved.
    maxRecords :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicableIndividualAssessments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationType', 'describeApplicableIndividualAssessments_migrationType' - Name of the migration type that each provided individual assessment must
-- support.
--
-- 'replicationTaskArn', 'describeApplicableIndividualAssessments_replicationTaskArn' - Amazon Resource Name (ARN) of a migration task on which you want to base
-- the default list of individual assessments.
--
-- 'sourceEngineName', 'describeApplicableIndividualAssessments_sourceEngineName' - Name of a database engine that the specified replication instance
-- supports as a source.
--
-- 'replicationInstanceArn', 'describeApplicableIndividualAssessments_replicationInstanceArn' - ARN of a replication instance on which you want to base the default list
-- of individual assessments.
--
-- 'targetEngineName', 'describeApplicableIndividualAssessments_targetEngineName' - Name of a database engine that the specified replication instance
-- supports as a target.
--
-- 'marker', 'describeApplicableIndividualAssessments_marker' - Optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
--
-- 'maxRecords', 'describeApplicableIndividualAssessments_maxRecords' - Maximum number of records to include in the response. If more records
-- exist than the specified @MaxRecords@ value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
newDescribeApplicableIndividualAssessments ::
  DescribeApplicableIndividualAssessments
newDescribeApplicableIndividualAssessments =
  DescribeApplicableIndividualAssessments'
    { migrationType =
        Prelude.Nothing,
      replicationTaskArn =
        Prelude.Nothing,
      sourceEngineName = Prelude.Nothing,
      replicationInstanceArn =
        Prelude.Nothing,
      targetEngineName = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing
    }

-- | Name of the migration type that each provided individual assessment must
-- support.
describeApplicableIndividualAssessments_migrationType :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe MigrationTypeValue)
describeApplicableIndividualAssessments_migrationType = Lens.lens (\DescribeApplicableIndividualAssessments' {migrationType} -> migrationType) (\s@DescribeApplicableIndividualAssessments' {} a -> s {migrationType = a} :: DescribeApplicableIndividualAssessments)

-- | Amazon Resource Name (ARN) of a migration task on which you want to base
-- the default list of individual assessments.
describeApplicableIndividualAssessments_replicationTaskArn :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessments_replicationTaskArn = Lens.lens (\DescribeApplicableIndividualAssessments' {replicationTaskArn} -> replicationTaskArn) (\s@DescribeApplicableIndividualAssessments' {} a -> s {replicationTaskArn = a} :: DescribeApplicableIndividualAssessments)

-- | Name of a database engine that the specified replication instance
-- supports as a source.
describeApplicableIndividualAssessments_sourceEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessments_sourceEngineName = Lens.lens (\DescribeApplicableIndividualAssessments' {sourceEngineName} -> sourceEngineName) (\s@DescribeApplicableIndividualAssessments' {} a -> s {sourceEngineName = a} :: DescribeApplicableIndividualAssessments)

-- | ARN of a replication instance on which you want to base the default list
-- of individual assessments.
describeApplicableIndividualAssessments_replicationInstanceArn :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessments_replicationInstanceArn = Lens.lens (\DescribeApplicableIndividualAssessments' {replicationInstanceArn} -> replicationInstanceArn) (\s@DescribeApplicableIndividualAssessments' {} a -> s {replicationInstanceArn = a} :: DescribeApplicableIndividualAssessments)

-- | Name of a database engine that the specified replication instance
-- supports as a target.
describeApplicableIndividualAssessments_targetEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessments_targetEngineName = Lens.lens (\DescribeApplicableIndividualAssessments' {targetEngineName} -> targetEngineName) (\s@DescribeApplicableIndividualAssessments' {} a -> s {targetEngineName = a} :: DescribeApplicableIndividualAssessments)

-- | Optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- marker, up to the value specified by @MaxRecords@.
describeApplicableIndividualAssessments_marker :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessments_marker = Lens.lens (\DescribeApplicableIndividualAssessments' {marker} -> marker) (\s@DescribeApplicableIndividualAssessments' {} a -> s {marker = a} :: DescribeApplicableIndividualAssessments)

-- | Maximum number of records to include in the response. If more records
-- exist than the specified @MaxRecords@ value, a pagination token called a
-- marker is included in the response so that the remaining results can be
-- retrieved.
describeApplicableIndividualAssessments_maxRecords :: Lens.Lens' DescribeApplicableIndividualAssessments (Prelude.Maybe Prelude.Int)
describeApplicableIndividualAssessments_maxRecords = Lens.lens (\DescribeApplicableIndividualAssessments' {maxRecords} -> maxRecords) (\s@DescribeApplicableIndividualAssessments' {} a -> s {maxRecords = a} :: DescribeApplicableIndividualAssessments)

instance
  Prelude.AWSRequest
    DescribeApplicableIndividualAssessments
  where
  type
    Rs DescribeApplicableIndividualAssessments =
      DescribeApplicableIndividualAssessmentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeApplicableIndividualAssessmentsResponse'
            Prelude.<$> ( x Prelude..?> "IndividualAssessmentNames"
                            Prelude..!@ Prelude.mempty
                        )
              Prelude.<*> (x Prelude..?> "Marker")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeApplicableIndividualAssessments

instance
  Prelude.NFData
    DescribeApplicableIndividualAssessments

instance
  Prelude.ToHeaders
    DescribeApplicableIndividualAssessments
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonDMSv20160101.DescribeApplicableIndividualAssessments" ::
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
    DescribeApplicableIndividualAssessments
  where
  toJSON DescribeApplicableIndividualAssessments' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MigrationType" Prelude..=)
              Prelude.<$> migrationType,
            ("ReplicationTaskArn" Prelude..=)
              Prelude.<$> replicationTaskArn,
            ("SourceEngineName" Prelude..=)
              Prelude.<$> sourceEngineName,
            ("ReplicationInstanceArn" Prelude..=)
              Prelude.<$> replicationInstanceArn,
            ("TargetEngineName" Prelude..=)
              Prelude.<$> targetEngineName,
            ("Marker" Prelude..=) Prelude.<$> marker,
            ("MaxRecords" Prelude..=) Prelude.<$> maxRecords
          ]
      )

instance
  Prelude.ToPath
    DescribeApplicableIndividualAssessments
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribeApplicableIndividualAssessments
  where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeApplicableIndividualAssessmentsResponse' smart constructor.
data DescribeApplicableIndividualAssessmentsResponse = DescribeApplicableIndividualAssessmentsResponse'
  { -- | List of names for the individual assessments supported by the
    -- premigration assessment run that you start based on the specified
    -- request parameters. For more information on the available individual
    -- assessments, including compatibility with different migration task
    -- configurations, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs>
    -- in the /AWS Database Migration Service User Guide./
    individualAssessmentNames :: Prelude.Maybe [Prelude.Text],
    -- | Pagination token returned for you to pass to a subsequent request. If
    -- you pass this token as the @Marker@ value in a subsequent request, the
    -- response includes only records beyond the marker, up to the value
    -- specified in the request by @MaxRecords@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeApplicableIndividualAssessmentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'individualAssessmentNames', 'describeApplicableIndividualAssessmentsResponse_individualAssessmentNames' - List of names for the individual assessments supported by the
-- premigration assessment run that you start based on the specified
-- request parameters. For more information on the available individual
-- assessments, including compatibility with different migration task
-- configurations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs>
-- in the /AWS Database Migration Service User Guide./
--
-- 'marker', 'describeApplicableIndividualAssessmentsResponse_marker' - Pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
--
-- 'httpStatus', 'describeApplicableIndividualAssessmentsResponse_httpStatus' - The response's http status code.
newDescribeApplicableIndividualAssessmentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeApplicableIndividualAssessmentsResponse
newDescribeApplicableIndividualAssessmentsResponse
  pHttpStatus_ =
    DescribeApplicableIndividualAssessmentsResponse'
      { individualAssessmentNames =
          Prelude.Nothing,
        marker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | List of names for the individual assessments supported by the
-- premigration assessment run that you start based on the specified
-- request parameters. For more information on the available individual
-- assessments, including compatibility with different migration task
-- configurations, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs>
-- in the /AWS Database Migration Service User Guide./
describeApplicableIndividualAssessmentsResponse_individualAssessmentNames :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Prelude.Maybe [Prelude.Text])
describeApplicableIndividualAssessmentsResponse_individualAssessmentNames = Lens.lens (\DescribeApplicableIndividualAssessmentsResponse' {individualAssessmentNames} -> individualAssessmentNames) (\s@DescribeApplicableIndividualAssessmentsResponse' {} a -> s {individualAssessmentNames = a} :: DescribeApplicableIndividualAssessmentsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Pagination token returned for you to pass to a subsequent request. If
-- you pass this token as the @Marker@ value in a subsequent request, the
-- response includes only records beyond the marker, up to the value
-- specified in the request by @MaxRecords@.
describeApplicableIndividualAssessmentsResponse_marker :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Prelude.Maybe Prelude.Text)
describeApplicableIndividualAssessmentsResponse_marker = Lens.lens (\DescribeApplicableIndividualAssessmentsResponse' {marker} -> marker) (\s@DescribeApplicableIndividualAssessmentsResponse' {} a -> s {marker = a} :: DescribeApplicableIndividualAssessmentsResponse)

-- | The response's http status code.
describeApplicableIndividualAssessmentsResponse_httpStatus :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse Prelude.Int
describeApplicableIndividualAssessmentsResponse_httpStatus = Lens.lens (\DescribeApplicableIndividualAssessmentsResponse' {httpStatus} -> httpStatus) (\s@DescribeApplicableIndividualAssessmentsResponse' {} a -> s {httpStatus = a} :: DescribeApplicableIndividualAssessmentsResponse)

instance
  Prelude.NFData
    DescribeApplicableIndividualAssessmentsResponse
