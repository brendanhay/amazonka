{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeInstanceRefreshes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more instance refreshes.
--
-- You can determine the status of a request by looking at the @Status@ parameter. The following are the possible statuses:
--
--     * @Pending@ - The request was created, but the operation has not started.
--
--
--     * @InProgress@ - The operation is in progress.
--
--
--     * @Successful@ - The operation completed successfully.
--
--
--     * @Failed@ - The operation failed to complete. You can troubleshoot using the status reason and the scaling activities.
--
--
--     * @Cancelling@ - An ongoing operation is being cancelled. Cancellation does not roll back any replacements that have already been completed, but it prevents new replacements from being started.
--
--
--     * @Cancelled@ - The operation is cancelled.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/asg-instance-refresh.html Replacing Auto Scaling Instances Based on an Instance Refresh> .
module Network.AWS.AutoScaling.DescribeInstanceRefreshes
  ( -- * Creating a request
    DescribeInstanceRefreshes (..),
    mkDescribeInstanceRefreshes,

    -- ** Request lenses
    dirNextToken,
    dirMaxRecords,
    dirInstanceRefreshIds,
    dirAutoScalingGroupName,

    -- * Destructuring the response
    DescribeInstanceRefreshesResponse (..),
    mkDescribeInstanceRefreshesResponse,

    -- ** Response lenses
    dirrsNextToken,
    dirrsInstanceRefreshes,
    dirrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceRefreshes' smart constructor.
data DescribeInstanceRefreshes = DescribeInstanceRefreshes'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxRecords :: Lude.Maybe Lude.Int,
    instanceRefreshIds ::
      Lude.Maybe [Lude.Text],
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceRefreshes' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'instanceRefreshIds' - One or more instance refresh IDs.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeInstanceRefreshes ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DescribeInstanceRefreshes
mkDescribeInstanceRefreshes pAutoScalingGroupName_ =
  DescribeInstanceRefreshes'
    { nextToken = Lude.Nothing,
      maxRecords = Lude.Nothing,
      instanceRefreshIds = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirNextToken :: Lens.Lens' DescribeInstanceRefreshes (Lude.Maybe Lude.Text)
dirNextToken = Lens.lens (nextToken :: DescribeInstanceRefreshes -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceRefreshes)
{-# DEPRECATED dirNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirMaxRecords :: Lens.Lens' DescribeInstanceRefreshes (Lude.Maybe Lude.Int)
dirMaxRecords = Lens.lens (maxRecords :: DescribeInstanceRefreshes -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeInstanceRefreshes)
{-# DEPRECATED dirMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

-- | One or more instance refresh IDs.
--
-- /Note:/ Consider using 'instanceRefreshIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirInstanceRefreshIds :: Lens.Lens' DescribeInstanceRefreshes (Lude.Maybe [Lude.Text])
dirInstanceRefreshIds = Lens.lens (instanceRefreshIds :: DescribeInstanceRefreshes -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceRefreshIds = a} :: DescribeInstanceRefreshes)
{-# DEPRECATED dirInstanceRefreshIds "Use generic-lens or generic-optics with 'instanceRefreshIds' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirAutoScalingGroupName :: Lens.Lens' DescribeInstanceRefreshes Lude.Text
dirAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeInstanceRefreshes -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeInstanceRefreshes)
{-# DEPRECATED dirAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DescribeInstanceRefreshes where
  type
    Rs DescribeInstanceRefreshes =
      DescribeInstanceRefreshesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeInstanceRefreshesResult"
      ( \s h x ->
          DescribeInstanceRefreshesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "InstanceRefreshes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceRefreshes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceRefreshes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceRefreshes where
  toQuery DescribeInstanceRefreshes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstanceRefreshes" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "MaxRecords" Lude.=: maxRecords,
        "InstanceRefreshIds"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> instanceRefreshIds),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDescribeInstanceRefreshesResponse' smart constructor.
data DescribeInstanceRefreshesResponse = DescribeInstanceRefreshesResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    instanceRefreshes ::
      Lude.Maybe
        [InstanceRefresh],
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

-- | Creates a value of 'DescribeInstanceRefreshesResponse' with the minimum fields required to make a request.
--
-- * 'instanceRefreshes' - The instance refreshes for the specified group.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceRefreshesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceRefreshesResponse
mkDescribeInstanceRefreshesResponse pResponseStatus_ =
  DescribeInstanceRefreshesResponse'
    { nextToken = Lude.Nothing,
      instanceRefreshes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsNextToken :: Lens.Lens' DescribeInstanceRefreshesResponse (Lude.Maybe Lude.Text)
dirrsNextToken = Lens.lens (nextToken :: DescribeInstanceRefreshesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceRefreshesResponse)
{-# DEPRECATED dirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instance refreshes for the specified group.
--
-- /Note:/ Consider using 'instanceRefreshes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInstanceRefreshes :: Lens.Lens' DescribeInstanceRefreshesResponse (Lude.Maybe [InstanceRefresh])
dirrsInstanceRefreshes = Lens.lens (instanceRefreshes :: DescribeInstanceRefreshesResponse -> Lude.Maybe [InstanceRefresh]) (\s a -> s {instanceRefreshes = a} :: DescribeInstanceRefreshesResponse)
{-# DEPRECATED dirrsInstanceRefreshes "Use generic-lens or generic-optics with 'instanceRefreshes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeInstanceRefreshesResponse Lude.Int
dirrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceRefreshesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceRefreshesResponse)
{-# DEPRECATED dirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
