{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DescribeSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains information about the directory snapshots that belong to this account.
--
-- This operation supports pagination with the use of the /NextToken/ request and response parameters. If more results are available, the /DescribeSnapshots.NextToken/ member contains a token that you pass in the next call to 'DescribeSnapshots' to retrieve the next set of items.
-- You can also specify a maximum number of return results with the /Limit/ parameter.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.DescribeSnapshots
  ( -- * Creating a request
    DescribeSnapshots (..),
    mkDescribeSnapshots,

    -- ** Request lenses
    dsDirectoryId,
    dsNextToken,
    dsSnapshotIds,
    dsLimit,

    -- * Destructuring the response
    DescribeSnapshotsResponse (..),
    mkDescribeSnapshotsResponse,

    -- ** Response lenses
    dssrsNextToken,
    dssrsSnapshots,
    dssrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DescribeSnapshots' operation.
--
-- /See:/ 'mkDescribeSnapshots' smart constructor.
data DescribeSnapshots = DescribeSnapshots'
  { -- | The identifier of the directory for which to retrieve snapshot information.
    directoryId :: Lude.Maybe Lude.Text,
    -- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
    snapshotIds :: Lude.Maybe [Lude.Text],
    -- | The maximum number of objects to return.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshots' with the minimum fields required to make a request.
--
-- * 'directoryId' - The identifier of the directory for which to retrieve snapshot information.
-- * 'nextToken' - The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
-- * 'snapshotIds' - A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
-- * 'limit' - The maximum number of objects to return.
mkDescribeSnapshots ::
  DescribeSnapshots
mkDescribeSnapshots =
  DescribeSnapshots'
    { directoryId = Lude.Nothing,
      nextToken = Lude.Nothing,
      snapshotIds = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | The identifier of the directory for which to retrieve snapshot information.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsDirectoryId :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dsDirectoryId = Lens.lens (directoryId :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: DescribeSnapshots)
{-# DEPRECATED dsDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The /DescribeSnapshotsResult.NextToken/ value from a previous call to 'DescribeSnapshots' . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNextToken :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Text)
dsNextToken = Lens.lens (nextToken :: DescribeSnapshots -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSnapshots)
{-# DEPRECATED dsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of identifiers of the snapshots to obtain the information for. If this member is null or empty, all snapshots are returned using the /Limit/ and /NextToken/ members.
--
-- /Note:/ Consider using 'snapshotIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotIds :: Lens.Lens' DescribeSnapshots (Lude.Maybe [Lude.Text])
dsSnapshotIds = Lens.lens (snapshotIds :: DescribeSnapshots -> Lude.Maybe [Lude.Text]) (\s a -> s {snapshotIds = a} :: DescribeSnapshots)
{-# DEPRECATED dsSnapshotIds "Use generic-lens or generic-optics with 'snapshotIds' instead." #-}

-- | The maximum number of objects to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLimit :: Lens.Lens' DescribeSnapshots (Lude.Maybe Lude.Natural)
dsLimit = Lens.lens (limit :: DescribeSnapshots -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeSnapshots)
{-# DEPRECATED dsLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager DescribeSnapshots where
  page rq rs
    | Page.stop (rs Lens.^. dssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dssrsSnapshots) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dsNextToken Lens..~ rs Lens.^. dssrsNextToken

instance Lude.AWSRequest DescribeSnapshots where
  type Rs DescribeSnapshots = DescribeSnapshotsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSnapshotsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Snapshots" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshots where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DescribeSnapshots" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSnapshots where
  toJSON DescribeSnapshots' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DirectoryId" Lude..=) Lude.<$> directoryId,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("SnapshotIds" Lude..=) Lude.<$> snapshotIds,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath DescribeSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshots where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DescribeSnapshots' operation.
--
-- /See:/ 'mkDescribeSnapshotsResponse' smart constructor.
data DescribeSnapshotsResponse = DescribeSnapshotsResponse'
  { -- | If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The list of 'Snapshot' objects that were retrieved.
    --
    -- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
    snapshots :: Lude.Maybe [Snapshot],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
-- * 'snapshots' - The list of 'Snapshot' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotsResponse
mkDescribeSnapshotsResponse pResponseStatus_ =
  DescribeSnapshotsResponse'
    { nextToken = Lude.Nothing,
      snapshots = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If not null, more results are available. Pass this value in the /NextToken/ member of a subsequent call to 'DescribeSnapshots' .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsNextToken :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe Lude.Text)
dssrsNextToken = Lens.lens (nextToken :: DescribeSnapshotsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of 'Snapshot' objects that were retrieved.
--
-- It is possible that this list contains less than the number of items specified in the /Limit/ member of the request. This occurs if there are less than the requested number of items left to retrieve, or if the limitations of the operation have been exceeded.
--
-- /Note:/ Consider using 'snapshots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsSnapshots :: Lens.Lens' DescribeSnapshotsResponse (Lude.Maybe [Snapshot])
dssrsSnapshots = Lens.lens (snapshots :: DescribeSnapshotsResponse -> Lude.Maybe [Snapshot]) (\s a -> s {snapshots = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsSnapshots "Use generic-lens or generic-optics with 'snapshots' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSnapshotsResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotsResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
