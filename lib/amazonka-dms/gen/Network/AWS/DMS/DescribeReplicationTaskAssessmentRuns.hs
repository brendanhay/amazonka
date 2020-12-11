{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of premigration assessment runs based on filter settings.
--
-- These filter settings can specify a combination of premigration assessment runs, migration tasks, replication instances, and assessment run status values.
module Network.AWS.DMS.DescribeReplicationTaskAssessmentRuns
  ( -- * Creating a request
    DescribeReplicationTaskAssessmentRuns (..),
    mkDescribeReplicationTaskAssessmentRuns,

    -- ** Request lenses
    drtarFilters,
    drtarMarker,
    drtarMaxRecords,

    -- * Destructuring the response
    DescribeReplicationTaskAssessmentRunsResponse (..),
    mkDescribeReplicationTaskAssessmentRunsResponse,

    -- ** Response lenses
    drtarsrsReplicationTaskAssessmentRuns,
    drtarsrsMarker,
    drtarsrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentRuns' smart constructor.
data DescribeReplicationTaskAssessmentRuns = DescribeReplicationTaskAssessmentRuns'
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

-- | Creates a value of 'DescribeReplicationTaskAssessmentRuns' with the minimum fields required to make a request.
--
-- * 'filters' - Filters applied to the premigration assessment runs described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
mkDescribeReplicationTaskAssessmentRuns ::
  DescribeReplicationTaskAssessmentRuns
mkDescribeReplicationTaskAssessmentRuns =
  DescribeReplicationTaskAssessmentRuns'
    { filters = Lude.Nothing,
      marker = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | Filters applied to the premigration assessment runs described in the form of key-value pairs.
--
-- Valid filter names: @replication-task-assessment-run-arn@ , @replication-task-arn@ , @replication-instance-arn@ , @status@
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarFilters :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Lude.Maybe [Filter])
drtarFilters = Lens.lens (filters :: DescribeReplicationTaskAssessmentRuns -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeReplicationTaskAssessmentRuns)
{-# DEPRECATED drtarFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMarker :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Lude.Maybe Lude.Text)
drtarMarker = Lens.lens (marker :: DescribeReplicationTaskAssessmentRuns -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskAssessmentRuns)
{-# DEPRECATED drtarMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarMaxRecords :: Lens.Lens' DescribeReplicationTaskAssessmentRuns (Lude.Maybe Lude.Int)
drtarMaxRecords = Lens.lens (maxRecords :: DescribeReplicationTaskAssessmentRuns -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationTaskAssessmentRuns)
{-# DEPRECATED drtarMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Lude.AWSRequest DescribeReplicationTaskAssessmentRuns where
  type
    Rs DescribeReplicationTaskAssessmentRuns =
      DescribeReplicationTaskAssessmentRunsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationTaskAssessmentRunsResponse'
            Lude.<$> (x Lude..?> "ReplicationTaskAssessmentRuns" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationTaskAssessmentRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationTaskAssessmentRuns" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationTaskAssessmentRuns where
  toJSON DescribeReplicationTaskAssessmentRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords
          ]
      )

instance Lude.ToPath DescribeReplicationTaskAssessmentRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationTaskAssessmentRuns where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDescribeReplicationTaskAssessmentRunsResponse' smart constructor.
data DescribeReplicationTaskAssessmentRunsResponse = DescribeReplicationTaskAssessmentRunsResponse'
  { replicationTaskAssessmentRuns ::
      Lude.Maybe
        [ReplicationTaskAssessmentRun],
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

-- | Creates a value of 'DescribeReplicationTaskAssessmentRunsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
-- * 'replicationTaskAssessmentRuns' - One or more premigration assessment runs as specified by @Filters@ .
-- * 'responseStatus' - The response status code.
mkDescribeReplicationTaskAssessmentRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationTaskAssessmentRunsResponse
mkDescribeReplicationTaskAssessmentRunsResponse pResponseStatus_ =
  DescribeReplicationTaskAssessmentRunsResponse'
    { replicationTaskAssessmentRuns =
        Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | One or more premigration assessment runs as specified by @Filters@ .
--
-- /Note:/ Consider using 'replicationTaskAssessmentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarsrsReplicationTaskAssessmentRuns :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Lude.Maybe [ReplicationTaskAssessmentRun])
drtarsrsReplicationTaskAssessmentRuns = Lens.lens (replicationTaskAssessmentRuns :: DescribeReplicationTaskAssessmentRunsResponse -> Lude.Maybe [ReplicationTaskAssessmentRun]) (\s a -> s {replicationTaskAssessmentRuns = a} :: DescribeReplicationTaskAssessmentRunsResponse)
{-# DEPRECATED drtarsrsReplicationTaskAssessmentRuns "Use generic-lens or generic-optics with 'replicationTaskAssessmentRuns' instead." #-}

-- | A pagination token returned for you to pass to a subsequent request. If you pass this token as the @Marker@ value in a subsequent request, the response includes only records beyond the marker, up to the value specified in the request by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarsrsMarker :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse (Lude.Maybe Lude.Text)
drtarsrsMarker = Lens.lens (marker :: DescribeReplicationTaskAssessmentRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationTaskAssessmentRunsResponse)
{-# DEPRECATED drtarsrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtarsrsResponseStatus :: Lens.Lens' DescribeReplicationTaskAssessmentRunsResponse Lude.Int
drtarsrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationTaskAssessmentRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationTaskAssessmentRunsResponse)
{-# DEPRECATED drtarsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
