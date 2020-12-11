{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of individual assessments based on filter settings.
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, and assessment status values.
module Network.AWS.DMS.DescribeReplicationTaskIndividualAssessments
  ( -- * Creating a request
    DescribeReplicationTaskIndividualAssessments (..),
    mkDescribeReplicationTaskIndividualAssessments,

    -- ** Request lenses
    drtiaFilters,
    drtiaMarker,
    drtiaMaxRecords,

    -- * Destructuring the response
    DescribeReplicationTaskIndividualAssessmentsResponse (..),
    mkDescribeReplicationTaskIndividualAssessmentsResponse,

    -- ** Response lenses
    drtiarsReplicationTaskIndividualAssessments,
    drtiarsMarker,
    drtiarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReplicationTaskIndividualAssessments' smart constructor.
data DescribeReplicationTaskIndividualAssessments = DescribeReplicationTaskIndividualAssessments'
  { filters ::
      Lude.Maybe
        [Filter],
    marker ::
      Lude.Maybe
        Lude.Text,
    maxRecords ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationTaskIndividualAssessments' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to the individual assessments described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
mkDescribeReplicationTaskIndividualAssessments ::
  DescribeReplicationTaskIndividualAssessments
mkDescribeReplicationTaskIndividualAssessments =
  DescribeReplicationTaskIndividualAssessments'
    { filters =
        Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to the individual assessments described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @status@
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaFilters :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Lude.Maybe [Filter])
drtiaFilters = Lens.lens (filters :: DescribeReplicationTaskIndividualAssessments -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReplicationTaskIndividualAssessments)
{-# DEPRECATED drtiaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaMarker :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Lude.Maybe Lude.Text)
drtiaMarker = Lens.lens (marker :: DescribeReplicationTaskIndividualAssessments -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskIndividualAssessments)
{-# DEPRECATED drtiaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiaMaxRecords :: Lens.Lens' DescribeReplicationTaskIndividualAssessments (Lude.Maybe Lude.Int)
drtiaMaxRecords = Lens.lens (maxRecords :: DescribeReplicationTaskIndividualAssessments -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationTaskIndividualAssessments)
{-# DEPRECATED drtiaMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance
  Lude.AWSRequest
    DescribeReplicationTaskIndividualAssessments
  where
  type
    Rs DescribeReplicationTaskIndividualAssessments =
      DescribeReplicationTaskIndividualAssessmentsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskIndividualAssessmentsResponse'
            Lude.<$> ( x Lude..?> "ReplicationTaskIndividualAssessments"
                         Lude..!@ Lude.mempty
                     )
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeReplicationTaskIndividualAssessments
  where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationTaskIndividualAssessments" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationTaskIndividualAssessments where
  toJSON DescribeReplicationTaskIndividualAssessments' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationTaskIndividualAssessments where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationTaskIndividualAssessments where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationTaskIndividualAssessmentsResponse' smart constructor.
data DescribeReplicationTaskIndividualAssessmentsResponse = DescribeReplicationTaskIndividualAssessmentsResponse'
  { replicationTaskIndividualAssessments ::
      Lude.Maybe
        [ReplicationTaskIndividualAssessment],
    marker ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeReplicationTaskIndividualAssessmentsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
-- * 'replicationTaskIndividualAssessments' - One or more individual assessments as specified by @Filters@ .
-- * 'responseStatus' - The response status code.
mkDescribeReplicationTaskIndividualAssessmentsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationTaskIndividualAssessmentsResponse
mkDescribeReplicationTaskIndividualAssessmentsResponse
  pResponseStatus_ =
    DescribeReplicationTaskIndividualAssessmentsResponse'
      { replicationTaskIndividualAssessments =
          Lude.Nothing,
        marker = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | One or more individual assessments as specified by @Filters@ .
--
-- /Note:/ Consider using 'replicationTaskIndividualAssessments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarsReplicationTaskIndividualAssessments :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Lude.Maybe [ReplicationTaskIndividualAssessment])
drtiarsReplicationTaskIndividualAssessments = Lens.lens (replicationTaskIndividualAssessments :: DescribeReplicationTaskIndividualAssessmentsResponse -> Lude.Maybe [ReplicationTaskIndividualAssessment]) (\s a -> s {replicationTaskIndividualAssessments = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)
{-# DEPRECATED drtiarsReplicationTaskIndividualAssessments "Use generic-lens or generic-optics with 'replicationTaskIndividualAssessments' instead." #-}

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarsMarker :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse (Lude.Maybe Lude.Text)
drtiarsMarker = Lens.lens (marker :: DescribeReplicationTaskIndividualAssessmentsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)
{-# DEPRECATED drtiarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtiarsResponseStatus :: Lens.Lens' DescribeReplicationTaskIndividualAssessmentsResponse Lude.Int
drtiarsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationTaskIndividualAssessmentsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationTaskIndividualAssessmentsResponse)
{-# DEPRECATED drtiarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
