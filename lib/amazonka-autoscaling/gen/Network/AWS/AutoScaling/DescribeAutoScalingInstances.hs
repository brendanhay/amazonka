{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Auto Scaling instances.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
  ( -- * Creating a request
    DescribeAutoScalingInstances (..),
    mkDescribeAutoScalingInstances,

    -- ** Request lenses
    dasiNextToken,
    dasiInstanceIds,
    dasiMaxRecords,

    -- * Destructuring the response
    DescribeAutoScalingInstancesResponse (..),
    mkDescribeAutoScalingInstancesResponse,

    -- ** Response lenses
    dasirsNextToken,
    dasirsAutoScalingInstances,
    dasirsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAutoScalingInstances' smart constructor.
data DescribeAutoScalingInstances = DescribeAutoScalingInstances'
  { nextToken ::
      Lude.Maybe Lude.Text,
    instanceIds ::
      Lude.Maybe [Lude.Text],
    maxRecords :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutoScalingInstances' with the minimum fields required to make a request.
--
-- * 'instanceIds' - The IDs of the instances. You can specify up to @MaxRecords@ IDs. If you omit this parameter, all Auto Scaling instances are described. If you specify an ID that does not exist, it is ignored with no error.
-- * 'maxRecords' - The maximum number of items to return with this call. The default value is @50@ and the maximum value is @50@ .
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
mkDescribeAutoScalingInstances ::
  DescribeAutoScalingInstances
mkDescribeAutoScalingInstances =
  DescribeAutoScalingInstances'
    { nextToken = Lude.Nothing,
      instanceIds = Lude.Nothing,
      maxRecords = Lude.Nothing
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasiNextToken :: Lens.Lens' DescribeAutoScalingInstances (Lude.Maybe Lude.Text)
dasiNextToken = Lens.lens (nextToken :: DescribeAutoScalingInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutoScalingInstances)
{-# DEPRECATED dasiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The IDs of the instances. You can specify up to @MaxRecords@ IDs. If you omit this parameter, all Auto Scaling instances are described. If you specify an ID that does not exist, it is ignored with no error.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasiInstanceIds :: Lens.Lens' DescribeAutoScalingInstances (Lude.Maybe [Lude.Text])
dasiInstanceIds = Lens.lens (instanceIds :: DescribeAutoScalingInstances -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: DescribeAutoScalingInstances)
{-# DEPRECATED dasiInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The maximum number of items to return with this call. The default value is @50@ and the maximum value is @50@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasiMaxRecords :: Lens.Lens' DescribeAutoScalingInstances (Lude.Maybe Lude.Int)
dasiMaxRecords = Lens.lens (maxRecords :: DescribeAutoScalingInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxRecords = a} :: DescribeAutoScalingInstances)
{-# DEPRECATED dasiMaxRecords "Use generic-lens or generic-optics with 'maxRecords' instead." #-}

instance Page.AWSPager DescribeAutoScalingInstances where
  page rq rs
    | Page.stop (rs Lens.^. dasirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dasirsAutoScalingInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dasiNextToken Lens..~ rs Lens.^. dasirsNextToken

instance Lude.AWSRequest DescribeAutoScalingInstances where
  type
    Rs DescribeAutoScalingInstances =
      DescribeAutoScalingInstancesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeAutoScalingInstancesResult"
      ( \s h x ->
          DescribeAutoScalingInstancesResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "AutoScalingInstances" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAutoScalingInstances where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAutoScalingInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAutoScalingInstances where
  toQuery DescribeAutoScalingInstances' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeAutoScalingInstances" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "NextToken" Lude.=: nextToken,
        "InstanceIds"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> instanceIds),
        "MaxRecords" Lude.=: maxRecords
      ]

-- | /See:/ 'mkDescribeAutoScalingInstancesResponse' smart constructor.
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    autoScalingInstances ::
      Lude.Maybe
        [AutoScalingInstanceDetails],
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

-- | Creates a value of 'DescribeAutoScalingInstancesResponse' with the minimum fields required to make a request.
--
-- * 'autoScalingInstances' - The instances.
-- * 'nextToken' - A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
-- * 'responseStatus' - The response status code.
mkDescribeAutoScalingInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAutoScalingInstancesResponse
mkDescribeAutoScalingInstancesResponse pResponseStatus_ =
  DescribeAutoScalingInstancesResponse'
    { nextToken = Lude.Nothing,
      autoScalingInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasirsNextToken :: Lens.Lens' DescribeAutoScalingInstancesResponse (Lude.Maybe Lude.Text)
dasirsNextToken = Lens.lens (nextToken :: DescribeAutoScalingInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeAutoScalingInstancesResponse)
{-# DEPRECATED dasirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The instances.
--
-- /Note:/ Consider using 'autoScalingInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasirsAutoScalingInstances :: Lens.Lens' DescribeAutoScalingInstancesResponse (Lude.Maybe [AutoScalingInstanceDetails])
dasirsAutoScalingInstances = Lens.lens (autoScalingInstances :: DescribeAutoScalingInstancesResponse -> Lude.Maybe [AutoScalingInstanceDetails]) (\s a -> s {autoScalingInstances = a} :: DescribeAutoScalingInstancesResponse)
{-# DEPRECATED dasirsAutoScalingInstances "Use generic-lens or generic-optics with 'autoScalingInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasirsResponseStatus :: Lens.Lens' DescribeAutoScalingInstancesResponse Lude.Int
dasirsResponseStatus = Lens.lens (responseStatus :: DescribeAutoScalingInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAutoScalingInstancesResponse)
{-# DEPRECATED dasirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
