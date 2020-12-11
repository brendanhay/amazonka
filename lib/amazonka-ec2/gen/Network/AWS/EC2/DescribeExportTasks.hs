{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified export instance tasks or all of your export instance tasks.
module Network.AWS.EC2.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detFilters,
    detExportTaskIds,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrsExportTasks,
    detrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { filters ::
      Lude.Maybe [Filter],
    exportTaskIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- * 'exportTaskIds' - The export task IDs.
-- * 'filters' - the filters for the export tasks.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { filters = Lude.Nothing,
      exportTaskIds = Lude.Nothing
    }

-- | the filters for the export tasks.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Lude.Maybe [Filter])
detFilters = Lens.lens (filters :: DescribeExportTasks -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeExportTasks)
{-# DEPRECATED detFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The export task IDs.
--
-- /Note:/ Consider using 'exportTaskIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportTaskIds :: Lens.Lens' DescribeExportTasks (Lude.Maybe [Lude.Text])
detExportTaskIds = Lens.lens (exportTaskIds :: DescribeExportTasks -> Lude.Maybe [Lude.Text]) (\s a -> s {exportTaskIds = a} :: DescribeExportTasks)
{-# DEPRECATED detExportTaskIds "Use generic-lens or generic-optics with 'exportTaskIds' instead." #-}

instance Lude.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeExportTasksResponse'
            Lude.<$> ( x Lude..@? "exportTaskSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExportTasks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeExportTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExportTasks where
  toQuery DescribeExportTasks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeExportTasks" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery
          (Lude.toQueryList "ExportTaskId" Lude.<$> exportTaskIds)
      ]

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { exportTasks ::
      Lude.Maybe [ExportTask],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- * 'exportTasks' - Information about the export tasks.
-- * 'responseStatus' - The response status code.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { exportTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the export tasks.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe [ExportTask])
detrsExportTasks = Lens.lens (exportTasks :: DescribeExportTasksResponse -> Lude.Maybe [ExportTask]) (\s a -> s {exportTasks = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeExportTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
