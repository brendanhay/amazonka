{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeLogGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified log groups. You can list all your log groups or filter the results by prefix. The results are ASCII-sorted by log group name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeLogGroups
  ( -- * Creating a request
    DescribeLogGroups (..),
    mkDescribeLogGroups,

    -- ** Request lenses
    dlgLogGroupNamePrefix,
    dlgNextToken,
    dlgLimit,

    -- * Destructuring the response
    DescribeLogGroupsResponse (..),
    mkDescribeLogGroupsResponse,

    -- ** Response lenses
    dlgrsLogGroups,
    dlgrsNextToken,
    dlgrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLogGroups' smart constructor.
data DescribeLogGroups = DescribeLogGroups'
  { logGroupNamePrefix ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLogGroups' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
-- * 'logGroupNamePrefix' - The prefix to match.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeLogGroups ::
  DescribeLogGroups
mkDescribeLogGroups =
  DescribeLogGroups'
    { logGroupNamePrefix = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The prefix to match.
--
-- /Note:/ Consider using 'logGroupNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLogGroupNamePrefix :: Lens.Lens' DescribeLogGroups (Lude.Maybe Lude.Text)
dlgLogGroupNamePrefix = Lens.lens (logGroupNamePrefix :: DescribeLogGroups -> Lude.Maybe Lude.Text) (\s a -> s {logGroupNamePrefix = a} :: DescribeLogGroups)
{-# DEPRECATED dlgLogGroupNamePrefix "Use generic-lens or generic-optics with 'logGroupNamePrefix' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgNextToken :: Lens.Lens' DescribeLogGroups (Lude.Maybe Lude.Text)
dlgNextToken = Lens.lens (nextToken :: DescribeLogGroups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLogGroups)
{-# DEPRECATED dlgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgLimit :: Lens.Lens' DescribeLogGroups (Lude.Maybe Lude.Natural)
dlgLimit = Lens.lens (limit :: DescribeLogGroups -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeLogGroups)
{-# DEPRECATED dlgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeLogGroups where
  page rq rs
    | Page.stop (rs Lens.^. dlgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dlgrsLogGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dlgNextToken Lens..~ rs Lens.^. dlgrsNextToken

instance Lude.AWSRequest DescribeLogGroups where
  type Rs DescribeLogGroups = DescribeLogGroupsResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeLogGroupsResponse'
            Lude.<$> (x Lude..?> "logGroups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLogGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeLogGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeLogGroups where
  toJSON DescribeLogGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("logGroupNamePrefix" Lude..=) Lude.<$> logGroupNamePrefix,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeLogGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLogGroups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeLogGroupsResponse' smart constructor.
data DescribeLogGroupsResponse = DescribeLogGroupsResponse'
  { logGroups ::
      Lude.Maybe [LogGroup],
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeLogGroupsResponse' with the minimum fields required to make a request.
--
-- * 'logGroups' - The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then that log group is set to have its events never expire.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeLogGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLogGroupsResponse
mkDescribeLogGroupsResponse pResponseStatus_ =
  DescribeLogGroupsResponse'
    { logGroups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The log groups.
--
-- If the @retentionInDays@ value if not included for a log group, then that log group is set to have its events never expire.
--
-- /Note:/ Consider using 'logGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsLogGroups :: Lens.Lens' DescribeLogGroupsResponse (Lude.Maybe [LogGroup])
dlgrsLogGroups = Lens.lens (logGroups :: DescribeLogGroupsResponse -> Lude.Maybe [LogGroup]) (\s a -> s {logGroups = a} :: DescribeLogGroupsResponse)
{-# DEPRECATED dlgrsLogGroups "Use generic-lens or generic-optics with 'logGroups' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsNextToken :: Lens.Lens' DescribeLogGroupsResponse (Lude.Maybe Lude.Text)
dlgrsNextToken = Lens.lens (nextToken :: DescribeLogGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeLogGroupsResponse)
{-# DEPRECATED dlgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlgrsResponseStatus :: Lens.Lens' DescribeLogGroupsResponse Lude.Int
dlgrsResponseStatus = Lens.lens (responseStatus :: DescribeLogGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLogGroupsResponse)
{-# DEPRECATED dlgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
