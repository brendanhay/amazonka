{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeBackups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes backups. The results are ordered by time, with newest backups first. If you do not specify a BackupId or ServerName, the command returns all backups.
--
-- This operation is synchronous.
-- A @ResourceNotFoundException@ is thrown when the backup does not exist. A @ValidationException@ is raised when parameters of the request are not valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeBackups
  ( -- * Creating a request
    DescribeBackups (..),
    mkDescribeBackups,

    -- ** Request lenses
    dServerName,
    dBackupId,
    dNextToken,
    dMaxResults,

    -- * Destructuring the response
    DescribeBackupsResponse (..),
    mkDescribeBackupsResponse,

    -- ** Response lenses
    drsBackups,
    drsNextToken,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeBackups' smart constructor.
data DescribeBackups = DescribeBackups'
  { serverName ::
      Lude.Maybe Lude.Text,
    backupId :: Lude.Maybe Lude.Text,
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
-- * 'backupId' - Describes a single backup.
-- * 'maxResults' - This is not currently implemented for @DescribeBackups@ requests.
-- * 'nextToken' - This is not currently implemented for @DescribeBackups@ requests.
-- * 'serverName' - Returns backups for the server with the specified ServerName.
mkDescribeBackups ::
  DescribeBackups
mkDescribeBackups =
  DescribeBackups'
    { serverName = Lude.Nothing,
      backupId = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Returns backups for the server with the specified ServerName.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dServerName :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Text)
dServerName = Lens.lens (serverName :: DescribeBackups -> Lude.Maybe Lude.Text) (\s a -> s {serverName = a} :: DescribeBackups)
{-# DEPRECATED dServerName "Use generic-lens or generic-optics with 'serverName' instead." #-}

-- | Describes a single backup.
--
-- /Note:/ Consider using 'backupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBackupId :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Text)
dBackupId = Lens.lens (backupId :: DescribeBackups -> Lude.Maybe Lude.Text) (\s a -> s {backupId = a} :: DescribeBackups)
{-# DEPRECATED dBackupId "Use generic-lens or generic-optics with 'backupId' instead." #-}

-- | This is not currently implemented for @DescribeBackups@ requests.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Text)
dNextToken = Lens.lens (nextToken :: DescribeBackups -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBackups)
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | This is not currently implemented for @DescribeBackups@ requests.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeBackups (Lude.Maybe Lude.Natural)
dMaxResults = Lens.lens (maxResults :: DescribeBackups -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeBackups)
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeBackups where
  page rq rs
    | Page.stop (rs Lens.^. drsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. drsBackups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dNextToken Lens..~ rs Lens.^. drsNextToken

instance Lude.AWSRequest DescribeBackups where
  type Rs DescribeBackups = DescribeBackupsResponse
  request = Req.postJSON opsWorksCMService
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
              Lude.=# ("OpsWorksCM_V2016_11_01.DescribeBackups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeBackups where
  toJSON DescribeBackups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ServerName" Lude..=) Lude.<$> serverName,
            ("BackupId" Lude..=) Lude.<$> backupId,
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
-- * 'backups' - Contains the response to a @DescribeBackups@ request.
-- * 'nextToken' - This is not currently implemented for @DescribeBackups@ requests.
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

-- | Contains the response to a @DescribeBackups@ request.
--
-- /Note:/ Consider using 'backups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsBackups :: Lens.Lens' DescribeBackupsResponse (Lude.Maybe [Backup])
drsBackups = Lens.lens (backups :: DescribeBackupsResponse -> Lude.Maybe [Backup]) (\s a -> s {backups = a} :: DescribeBackupsResponse)
{-# DEPRECATED drsBackups "Use generic-lens or generic-optics with 'backups' instead." #-}

-- | This is not currently implemented for @DescribeBackups@ requests.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeBackupsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeBackupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeBackupsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeBackupsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeBackupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeBackupsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
