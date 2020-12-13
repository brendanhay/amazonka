{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeApplicableIndividualAssessments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of individual assessments that you can specify for a new premigration assessment run, given one or more parameters.
--
-- If you specify an existing migration task, this operation provides the default individual assessments you can specify for that task. Otherwise, the specified parameters model elements of a possible migration task on which to base a premigration assessment run.
-- To use these migration task modeling parameters, you must specify an existing replication instance, a source database engine, a target database engine, and a migration type. This combination of parameters potentially limits the default individual assessments available for an assessment run created for a corresponding migration task.
-- If you specify no parameters, this operation provides a list of all possible individual assessments that you can specify for an assessment run. If you specify any one of the task modeling parameters, you must specify all of them or the operation cannot provide a list of individual assessments. The only parameter that you can specify alone is for an existing migration task. The specified task definition then determines the default list of individual assessments that you can specify in an assessment run for the task.
module Network.AWS.DMS.DescribeApplicableIndividualAssessments
  ( -- * Creating a request
    DescribeApplicableIndividualAssessments (..),
    mkDescribeApplicableIndividualAssessments,

    -- ** Request lenses
    daiaMigrationType,
    daiaSourceEngineName,
    daiaReplicationTaskARN,
    daiaMarker,
    daiaMaxRecords,
    daiaTargetEngineName,
    daiaReplicationInstanceARN,

    -- * Destructuring the response
    DescribeApplicableIndividualAssessmentsResponse (..),
    mkDescribeApplicableIndividualAssessmentsResponse,

    -- ** Response lenses
    daiarsMarker,
    daiarsIndividualAssessmentNames,
    daiarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeApplicableIndividualAssessments' smart constructor.
data DescribeApplicableIndividualAssessments = DescribeApplicableIndividualAssessments'
  { -- | Name of the migration type that each provided individual assessment must support.
    migrationType :: Lude.Maybe MigrationTypeValue,
    -- | Name of a database engine that the specified replication instance supports as a source.
    sourceEngineName :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (ARN) of a migration task on which you want to base the default list of individual assessments.
    replicationTaskARN :: Lude.Maybe Lude.Text,
    -- | Optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | Maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
    maxRecords :: Lude.Maybe Lude.Int,
    -- | Name of a database engine that the specified replication instance supports as a target.
    targetEngineName :: Lude.Maybe Lude.Text,
    -- | ARN of a replication instance on which you want to base the default list of individual assessments.
    replicationInstanceARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicableIndividualAssessments' with the minimum fields required to make a request.
--
-- * 'migrationType' - Name of the migration type that each provided individual assessment must support.
-- * 'sourceEngineName' - Name of a database engine that the specified replication instance supports as a source.
-- * 'replicationTaskARN' - Amazon Resource Name (ARN) of a migration task on which you want to base the default list of individual assessments.
-- * 'marker' - Optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - Maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
-- * 'targetEngineName' - Name of a database engine that the specified replication instance supports as a target.
-- * 'replicationInstanceARN' - ARN of a replication instance on which you want to base the default list of individual assessments.
mkDescribeApplicableIndividualAssessments ::
  DescribeApplicableIndividualAssessments
mkDescribeApplicableIndividualAssessments =
  DescribeApplicableIndividualAssessments'
    { migrationType =
        Lude.Nothing,
      sourceEngineName = Lude.Nothing,
      replicationTaskARN = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      targetEngineName = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing
    }

-- | Name of the migration type that each provided individual assessment must support.
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMigrationType :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe MigrationTypeValue)
daiaMigrationType = Lens.lens (migrationType :: DescribeApplicableIndividualAssessments -> Lude.Maybe MigrationTypeValue) (\s a -> s {migrationType = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | Name of a database engine that the specified replication instance supports as a source.
--
-- /Note:/ Consider using 'sourceEngineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaSourceEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Text)
daiaSourceEngineName = Lens.lens (sourceEngineName :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {sourceEngineName = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaSourceEngineName "Use generic-lens or generic-optics with 'sourceEngineName' instead." #-}

-- | Amazon Resource Name (ARN) of a migration task on which you want to base the default list of individual assessments.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaReplicationTaskARN :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Text)
daiaReplicationTaskARN = Lens.lens (replicationTaskARN :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskARN = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | Optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMarker :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Text)
daiaMarker = Lens.lens (marker :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaMaxRecords :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Int)
daiaMaxRecords = Lens.lens (maxRecords :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | Name of a database engine that the specified replication instance supports as a target.
--
-- /Note:/ Consider using 'targetEngineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaTargetEngineName :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Text)
daiaTargetEngineName = Lens.lens (targetEngineName :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {targetEngineName = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaTargetEngineName "Use generic-lens or generic-optics with 'targetEngineName' instead." #-}

-- | ARN of a replication instance on which you want to base the default list of individual assessments.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiaReplicationInstanceARN :: Lens.Lens' DescribeApplicableIndividualAssessments (Lude.Maybe Lude.Text)
daiaReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DescribeApplicableIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DescribeApplicableIndividualAssessments)
{-# DEPRECATED daiaReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest DescribeApplicableIndividualAssessments where
  type
    Rs DescribeApplicableIndividualAssessments =
      DescribeApplicableIndividualAssessmentsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeApplicableIndividualAssessmentsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "IndividualAssessmentNames" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeApplicableIndividualAssessments where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeApplicableIndividualAssessments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeApplicableIndividualAssessments where
  toJSON DescribeApplicableIndividualAssessments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MigrationType" Lude..=) Lude.<$> migrationType,
            ("SourceEngineName" Lude..=) Lude.<$> sourceEngineName,
            ("ReplicationTaskArn" Lude..=) Lude.<$> replicationTaskARN,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            ("TargetEngineName" Lude..=) Lude.<$> targetEngineName,
            ("ReplicationInstanceArn" Lude..=)
              Lude.<$> replicationInstanceARN
          ]
      )

instance Lude.ToPath DescribeApplicableIndividualAssessments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeApplicableIndividualAssessments where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeApplicableIndividualAssessmentsResponse' smart constructor.
data DescribeApplicableIndividualAssessmentsResponse = DescribeApplicableIndividualAssessmentsResponse'
  { -- | Pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
    marker :: Lude.Maybe Lude.Text,
    -- | List of names for the individual assessments supported by the premigration assessment run that you start based on the specified request parameters. For more information on the available individual assessments, including compatibility with different migration task configurations, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs> in the /AWS Database Migration Service User Guide./
    individualAssessmentNames :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeApplicableIndividualAssessmentsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - Pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
-- * 'individualAssessmentNames' - List of names for the individual assessments supported by the premigration assessment run that you start based on the specified request parameters. For more information on the available individual assessments, including compatibility with different migration task configurations, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs> in the /AWS Database Migration Service User Guide./
-- * 'responseStatus' - The response status code.
mkDescribeApplicableIndividualAssessmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeApplicableIndividualAssessmentsResponse
mkDescribeApplicableIndividualAssessmentsResponse pResponseStatus_ =
  DescribeApplicableIndividualAssessmentsResponse'
    { marker =
        Lude.Nothing,
      individualAssessmentNames = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarsMarker :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Lude.Maybe Lude.Text)
daiarsMarker = Lens.lens (marker :: DescribeApplicableIndividualAssessmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeApplicableIndividualAssessmentsResponse)
{-# DEPRECATED daiarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | List of names for the individual assessments supported by the premigration assessment run that you start based on the specified request parameters. For more information on the available individual assessments, including compatibility with different migration task configurations, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.AssessmentReport.html Working with premigration assessment runs> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'individualAssessmentNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarsIndividualAssessmentNames :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse (Lude.Maybe [Lude.Text])
daiarsIndividualAssessmentNames = Lens.lens (individualAssessmentNames :: DescribeApplicableIndividualAssessmentsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {individualAssessmentNames = a} :: DescribeApplicableIndividualAssessmentsResponse)
{-# DEPRECATED daiarsIndividualAssessmentNames "Use generic-lens or generic-optics with 'individualAssessmentNames' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daiarsResponseStatus :: Lens.Lens' DescribeApplicableIndividualAssessmentsResponse Lude.Int
daiarsResponseStatus = Lens.lens (responseStatus :: DescribeApplicableIndividualAssessmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeApplicableIndividualAssessmentsResponse)
{-# DEPRECATED daiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
