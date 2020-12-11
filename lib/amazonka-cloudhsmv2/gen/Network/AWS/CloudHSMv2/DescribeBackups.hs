{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.DescribeBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about backups of AWS CloudHSM clusters.
--
-- This is a paginated operation, which means that each response might contain only a subset of all the backups. When the response contains only a subset of backups, it includes a @NextToken@ value. Use this value in a subsequent @DescribeBackups@ request to get more backups. When you receive a response with no @NextToken@ (or an empty or null value), that means there are no more backups to get.
--
-- This operation returns paginated results.
module Network.AWS.CloudHSMv2.DescribeBackups
  ( -- * Creating a request
    DescribeBackups (..),
    mkDescribeBackups,

    -- ** Request lenses
    dbSortAscending,
    dbFilters,
    dbNextToken,
    dbMaxResults,

    -- * Destructuring the response
    DescribeBackupsResponse (..),
    mkDescribeBackupsResponse,

    -- ** Response lenses
    dbsrsBackups,
    dbsrsNextToken,
    dbsrsResponseStatus,
  )
where

import Network.AWS.CloudHSMv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { sortAscending ::
      Lude.Maybe Lude.Bool,
    filters ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeBackups' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID).
-- Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation.
-- Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @states@ filter to return only backups that match the specified state.
-- Use the @neverExpires@ filter to return backups filtered by the value in the @neverExpires@ parameter. @True@ returns all backups exempt from the backup retention policy. @False@ returns all backups with a backup retention policy defined at the cluster.
-- * 'maxResults' - The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
-- * 'nextToken' - The @NextToken@ value that you received in the previous response. Use this value to get more backups.
-- * 'sortAscending' - Designates whether or not to sort the return backups by ascending chronological order of generation.
mkDescribeBackups ::
  DescribeBackups
mkDescribeBackups =
  DescribeBackups'
    { sortAscending = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Designates whether or not to sort the return backups by ascending chronological order of generation.
--
-- /Note:/ Consider using 'sortAscending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbSortAscending :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Bool)
dbSortAscending = Lens.lens (sortAscending :: DescribeBackups -> Lude.Maybe Lude.Bool) (\s a -> s {sortAscending = a} :: DescribeBackups)
{-# DEPRECATED dbSortAscending "Use generic-lens or generic-optics with 'sortAscending' instead." #-}

-- | One or more filters to limit the items returned in the response.
--
-- Use the @backupIds@ filter to return only the specified backups. Specify backups by their backup identifier (ID).
-- Use the @sourceBackupIds@ filter to return only the backups created from a source backup. The @sourceBackupID@ of a source backup is returned by the 'CopyBackupToRegion' operation.
-- Use the @clusterIds@ filter to return only the backups for the specified clusters. Specify clusters by their cluster identifier (ID).
-- Use the @states@ filter to return only backups that match the specified state.
-- Use the @neverExpires@ filter to return backups filtered by the value in the @neverExpires@ parameter. @True@ returns all backups exempt from the backup retention policy. @False@ returns all backups with a backup retention policy defined at the cluster.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbFilters :: Lens.Lens' DescribeBackups (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
dbFilters = Lens.lens (filters :: DescribeBackups -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {filters = a} :: DescribeBackups)
{-# DEPRECATED dbFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The @NextToken@ value that you received in the previous response. Use this value to get more backups.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbNextToken :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Text)
dbNextToken = Lens.lens (nextToken :: DescribeBackups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBackups)
{-# DEPRECATED dbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of backups to return in the response. When there are more backups than the number you specify, the response contains a @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbMaxResults :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Natural)
dbMaxResults = Lens.lens (maxResults :: DescribeBackups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBackups)
{-# DEPRECATED dbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeBackups where
  page rq rs
    | Page.stop (rs Lens.^. dbsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dbsrsBackups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dbNextToken Lens..~ rs Lens.^. dbsrsNextToken

instance Lude.AWSRequest DescribeBackups where
  type Rs DescribeBackups = DescribeBackupsResponse
  request = Req.postJSON cloudHSMv2Service
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeBackupsResponse'
            Lude.<$> (x Lude..?> "Backups" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeBackups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("BaldrApiService.DescribeBackups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBackups where
  toJSON DescribeBackups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SortAscending" Lude..=) Lude.<$> sortAscending,
            ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeBackups where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeBackups where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeBackupsResponse' smart constructor.
data DescribeBackupsResponse = DescribeBackupsResponse'
  { backups ::
      Lude.Maybe [Backup],
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

-- | Creates a value of 'DescribeBackupsResponse' with the minimum fields required to make a request.
--
-- * 'backups' - A list of backups.
-- * 'nextToken' - An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
-- * 'responseStatus' - The response status code.
mkDescribeBackupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeBackupsResponse
mkDescribeBackupsResponse pResponseStatus_ =
  DescribeBackupsResponse'
    { backups = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of backups.
--
-- /Note:/ Consider using 'backups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsrsBackups :: Lens.Lens' DescribeBackupsResponse (Lude.Maybe [Backup])
dbsrsBackups = Lens.lens (backups :: DescribeBackupsResponse -> Lude.Maybe [Backup]) (\s a -> s {backups = a} :: DescribeBackupsResponse)
{-# DEPRECATED dbsrsBackups "Use generic-lens or generic-optics with 'backups' instead." #-}

-- | An opaque string that indicates that the response contains only a subset of backups. Use this value in a subsequent @DescribeBackups@ request to get more backups.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsrsNextToken :: Lens.Lens' DescribeBackupsResponse (Lude.Maybe Lude.Text)
dbsrsNextToken = Lens.lens (nextToken :: DescribeBackupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBackupsResponse)
{-# DEPRECATED dbsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbsrsResponseStatus :: Lens.Lens' DescribeBackupsResponse Lude.Int
dbsrsResponseStatus = Lens.lens (responseStatus :: DescribeBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBackupsResponse)
{-# DEPRECATED dbsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
