{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the task logs for the specified task.
module Network.AWS.DMS.DescribeReplicationInstanceTaskLogs
  ( -- * Creating a request
    DescribeReplicationInstanceTaskLogs (..),
    mkDescribeReplicationInstanceTaskLogs,

    -- ** Request lenses
    dritlMarker,
    dritlMaxRecords,
    dritlReplicationInstanceARN,

    -- * Destructuring the response
    DescribeReplicationInstanceTaskLogsResponse (..),
    mkDescribeReplicationInstanceTaskLogsResponse,

    -- ** Response lenses
    dritlrsReplicationInstanceTaskLogs,
    dritlrsMarker,
    dritlrsReplicationInstanceARN,
    dritlrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeReplicationInstanceTaskLogs' smart constructor.
data DescribeReplicationInstanceTaskLogs = DescribeReplicationInstanceTaskLogs'
  { marker ::
      Lude.Maybe
        Lude.Text,
    maxRecords ::
      Lude.Maybe Lude.Int,
    replicationInstanceARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationInstanceTaskLogs' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'maxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
mkDescribeReplicationInstanceTaskLogs ::
  -- | 'replicationInstanceARN'
  Lude.Text ->
  DescribeReplicationInstanceTaskLogs
mkDescribeReplicationInstanceTaskLogs pReplicationInstanceARN_ =
  DescribeReplicationInstanceTaskLogs'
    { marker = Lude.Nothing,
      maxRecords = Lude.Nothing,
      replicationInstanceARN = pReplicationInstanceARN_
    }

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlMarker :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Lude.Maybe Lude.Text)
dritlMarker = Lens.lens (marker :: DescribeReplicationInstanceTaskLogs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationInstanceTaskLogs)
{-# DEPRECATED dritlMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.
--
-- Default: 100
-- Constraints: Minimum 20, maximum 100.
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlMaxRecords :: Lens.Lens' DescribeReplicationInstanceTaskLogs (Lude.Maybe Lude.Int)
dritlMaxRecords = Lens.lens (maxRecords :: DescribeReplicationInstanceTaskLogs -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeReplicationInstanceTaskLogs)
{-# DEPRECATED dritlMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlReplicationInstanceARN :: Lens.Lens' DescribeReplicationInstanceTaskLogs Lude.Text
dritlReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DescribeReplicationInstanceTaskLogs -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DescribeReplicationInstanceTaskLogs)
{-# DEPRECATED dritlReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest DescribeReplicationInstanceTaskLogs where
  type
    Rs DescribeReplicationInstanceTaskLogs =
      DescribeReplicationInstanceTaskLogsResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReplicationInstanceTaskLogsResponse'
            Lude.<$> (x Lude..?> "ReplicationInstanceTaskLogs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "ReplicationInstanceArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReplicationInstanceTaskLogs where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.DescribeReplicationInstanceTaskLogs" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReplicationInstanceTaskLogs where
  toJSON DescribeReplicationInstanceTaskLogs' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("MaxRecords" Lude..=) Lude.<$> maxRecords,
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath DescribeReplicationInstanceTaskLogs where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReplicationInstanceTaskLogs where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeReplicationInstanceTaskLogsResponse' smart constructor.
data DescribeReplicationInstanceTaskLogsResponse = DescribeReplicationInstanceTaskLogsResponse'
  { replicationInstanceTaskLogs ::
      Lude.Maybe
        [ReplicationInstanceTaskLog],
    marker ::
      Lude.Maybe
        Lude.Text,
    replicationInstanceARN ::
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReplicationInstanceTaskLogsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the replication instance.
-- * 'replicationInstanceTaskLogs' - An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes).
-- * 'responseStatus' - The response status code.
mkDescribeReplicationInstanceTaskLogsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReplicationInstanceTaskLogsResponse
mkDescribeReplicationInstanceTaskLogsResponse pResponseStatus_ =
  DescribeReplicationInstanceTaskLogsResponse'
    { replicationInstanceTaskLogs =
        Lude.Nothing,
      marker = Lude.Nothing,
      replicationInstanceARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of replication task log metadata. Each member of the array contains the replication task name, ARN, and task log size (in bytes).
--
-- /Note:/ Consider using 'replicationInstanceTaskLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrsReplicationInstanceTaskLogs :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Lude.Maybe [ReplicationInstanceTaskLog])
dritlrsReplicationInstanceTaskLogs = Lens.lens (replicationInstanceTaskLogs :: DescribeReplicationInstanceTaskLogsResponse -> Lude.Maybe [ReplicationInstanceTaskLog]) (\s a -> s {replicationInstanceTaskLogs = a} :: DescribeReplicationInstanceTaskLogsResponse)
{-# DEPRECATED dritlrsReplicationInstanceTaskLogs "Use generic-lens or generic-optics with 'replicationInstanceTaskLogs' instead." #-}

-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrsMarker :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Lude.Maybe Lude.Text)
dritlrsMarker = Lens.lens (marker :: DescribeReplicationInstanceTaskLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeReplicationInstanceTaskLogsResponse)
{-# DEPRECATED dritlrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrsReplicationInstanceARN :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse (Lude.Maybe Lude.Text)
dritlrsReplicationInstanceARN = Lens.lens (replicationInstanceARN :: DescribeReplicationInstanceTaskLogsResponse -> Lude.Maybe Lude.Text) (\s a -> s {replicationInstanceARN = a} :: DescribeReplicationInstanceTaskLogsResponse)
{-# DEPRECATED dritlrsReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dritlrsResponseStatus :: Lens.Lens' DescribeReplicationInstanceTaskLogsResponse Lude.Int
dritlrsResponseStatus = Lens.lens (responseStatus :: DescribeReplicationInstanceTaskLogsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReplicationInstanceTaskLogsResponse)
{-# DEPRECATED dritlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
