{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of fast snapshot restores for your snapshots.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFastSnapshotRestores
  ( -- * Creating a request
    DescribeFastSnapshotRestores (..),
    mkDescribeFastSnapshotRestores,

    -- ** Request lenses
    dfsrFilters,
    dfsrNextToken,
    dfsrDryRun,
    dfsrMaxResults,

    -- * Destructuring the response
    DescribeFastSnapshotRestoresResponse (..),
    mkDescribeFastSnapshotRestoresResponse,

    -- ** Response lenses
    dfsrsrsFastSnapshotRestores,
    dfsrsrsNextToken,
    dfsrsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFastSnapshotRestores' smart constructor.
data DescribeFastSnapshotRestores = DescribeFastSnapshotRestores'
  { -- | The filters. The possible values are:
    --
    --
    --     * @availability-zone@ : The Availability Zone of the snapshot.
    --
    --
    --     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.
    --
    --
    --     * @snapshot-id@ : The ID of the snapshot.
    --
    --
    --     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
    filters :: Lude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFastSnapshotRestores' with the minimum fields required to make a request.
--
-- * 'filters' - The filters. The possible values are:
--
--
--     * @availability-zone@ : The Availability Zone of the snapshot.
--
--
--     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.
--
--
--     * @snapshot-id@ : The ID of the snapshot.
--
--
--     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
--
--
-- * 'nextToken' - The token for the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
mkDescribeFastSnapshotRestores ::
  DescribeFastSnapshotRestores
mkDescribeFastSnapshotRestores =
  DescribeFastSnapshotRestores'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters. The possible values are:
--
--
--     * @availability-zone@ : The Availability Zone of the snapshot.
--
--
--     * @owner-id@ : The ID of the AWS account that enabled fast snapshot restore on the snapshot.
--
--
--     * @snapshot-id@ : The ID of the snapshot.
--
--
--     * @state@ : The state of fast snapshot restores for the snapshot (@enabling@ | @optimizing@ | @enabled@ | @disabling@ | @disabled@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrFilters :: Lens.Lens' DescribeFastSnapshotRestores (Lude.Maybe [Filter])
dfsrFilters = Lens.lens (filters :: DescribeFastSnapshotRestores -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeFastSnapshotRestores)
{-# DEPRECATED dfsrFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrNextToken :: Lens.Lens' DescribeFastSnapshotRestores (Lude.Maybe Lude.Text)
dfsrNextToken = Lens.lens (nextToken :: DescribeFastSnapshotRestores -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFastSnapshotRestores)
{-# DEPRECATED dfsrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrDryRun :: Lens.Lens' DescribeFastSnapshotRestores (Lude.Maybe Lude.Bool)
dfsrDryRun = Lens.lens (dryRun :: DescribeFastSnapshotRestores -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeFastSnapshotRestores)
{-# DEPRECATED dfsrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrMaxResults :: Lens.Lens' DescribeFastSnapshotRestores (Lude.Maybe Lude.Natural)
dfsrMaxResults = Lens.lens (maxResults :: DescribeFastSnapshotRestores -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeFastSnapshotRestores)
{-# DEPRECATED dfsrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeFastSnapshotRestores where
  page rq rs
    | Page.stop (rs Lens.^. dfsrsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dfsrsrsFastSnapshotRestores) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dfsrNextToken Lens..~ rs Lens.^. dfsrsrsNextToken

instance Lude.AWSRequest DescribeFastSnapshotRestores where
  type
    Rs DescribeFastSnapshotRestores =
      DescribeFastSnapshotRestoresResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeFastSnapshotRestoresResponse'
            Lude.<$> ( x Lude..@? "fastSnapshotRestoreSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeFastSnapshotRestores where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFastSnapshotRestores where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeFastSnapshotRestores where
  toQuery DescribeFastSnapshotRestores' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeFastSnapshotRestores" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeFastSnapshotRestoresResponse' smart constructor.
data DescribeFastSnapshotRestoresResponse = DescribeFastSnapshotRestoresResponse'
  { -- | Information about the state of fast snapshot restores.
    fastSnapshotRestores :: Lude.Maybe [DescribeFastSnapshotRestoreSuccessItem],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- * 'fastSnapshotRestores' - Information about the state of fast snapshot restores.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeFastSnapshotRestoresResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeFastSnapshotRestoresResponse
mkDescribeFastSnapshotRestoresResponse pResponseStatus_ =
  DescribeFastSnapshotRestoresResponse'
    { fastSnapshotRestores =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the state of fast snapshot restores.
--
-- /Note:/ Consider using 'fastSnapshotRestores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsrsFastSnapshotRestores :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Lude.Maybe [DescribeFastSnapshotRestoreSuccessItem])
dfsrsrsFastSnapshotRestores = Lens.lens (fastSnapshotRestores :: DescribeFastSnapshotRestoresResponse -> Lude.Maybe [DescribeFastSnapshotRestoreSuccessItem]) (\s a -> s {fastSnapshotRestores = a} :: DescribeFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrsrsFastSnapshotRestores "Use generic-lens or generic-optics with 'fastSnapshotRestores' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsrsNextToken :: Lens.Lens' DescribeFastSnapshotRestoresResponse (Lude.Maybe Lude.Text)
dfsrsrsNextToken = Lens.lens (nextToken :: DescribeFastSnapshotRestoresResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfsrsrsResponseStatus :: Lens.Lens' DescribeFastSnapshotRestoresResponse Lude.Int
dfsrsrsResponseStatus = Lens.lens (responseStatus :: DescribeFastSnapshotRestoresResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeFastSnapshotRestoresResponse)
{-# DEPRECATED dfsrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
