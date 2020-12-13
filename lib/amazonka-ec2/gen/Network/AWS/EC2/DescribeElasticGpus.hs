{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeElasticGpus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Elastic Graphics accelerator associated with your instances. For more information about Elastic Graphics, see <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon Elastic Graphics> .
module Network.AWS.EC2.DescribeElasticGpus
  ( -- * Creating a request
    DescribeElasticGpus (..),
    mkDescribeElasticGpus,

    -- ** Request lenses
    degFilters,
    degNextToken,
    degDryRun,
    degMaxResults,
    degElasticGpuIds,

    -- * Destructuring the response
    DescribeElasticGpusResponse (..),
    mkDescribeElasticGpusResponse,

    -- ** Response lenses
    degrsElasticGpuSet,
    degrsNextToken,
    degrsMaxResults,
    degrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeElasticGpus' smart constructor.
data DescribeElasticGpus = DescribeElasticGpus'
  { -- | The filters.
    --
    --
    --     * @availability-zone@ - The Availability Zone in which the Elastic Graphics accelerator resides.
    --
    --
    --     * @elastic-gpu-health@ - The status of the Elastic Graphics accelerator (@OK@ | @IMPAIRED@ ).
    --
    --
    --     * @elastic-gpu-state@ - The state of the Elastic Graphics accelerator (@ATTACHED@ ).
    --
    --
    --     * @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for example, @eg1.medium@ .
    --
    --
    --     * @instance-id@ - The ID of the instance to which the Elastic Graphics accelerator is associated.
    filters :: Lude.Maybe [Filter],
    -- | The token to request the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
    maxResults :: Lude.Maybe Lude.Natural,
    -- | The Elastic Graphics accelerator IDs.
    elasticGpuIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticGpus' with the minimum fields required to make a request.
--
-- * 'filters' - The filters.
--
--
--     * @availability-zone@ - The Availability Zone in which the Elastic Graphics accelerator resides.
--
--
--     * @elastic-gpu-health@ - The status of the Elastic Graphics accelerator (@OK@ | @IMPAIRED@ ).
--
--
--     * @elastic-gpu-state@ - The state of the Elastic Graphics accelerator (@ATTACHED@ ).
--
--
--     * @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for example, @eg1.medium@ .
--
--
--     * @instance-id@ - The ID of the instance to which the Elastic Graphics accelerator is associated.
--
--
-- * 'nextToken' - The token to request the next page of results.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
-- * 'elasticGpuIds' - The Elastic Graphics accelerator IDs.
mkDescribeElasticGpus ::
  DescribeElasticGpus
mkDescribeElasticGpus =
  DescribeElasticGpus'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      elasticGpuIds = Lude.Nothing
    }

-- | The filters.
--
--
--     * @availability-zone@ - The Availability Zone in which the Elastic Graphics accelerator resides.
--
--
--     * @elastic-gpu-health@ - The status of the Elastic Graphics accelerator (@OK@ | @IMPAIRED@ ).
--
--
--     * @elastic-gpu-state@ - The state of the Elastic Graphics accelerator (@ATTACHED@ ).
--
--
--     * @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for example, @eg1.medium@ .
--
--
--     * @instance-id@ - The ID of the instance to which the Elastic Graphics accelerator is associated.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degFilters :: Lens.Lens' DescribeElasticGpus (Lude.Maybe [Filter])
degFilters = Lens.lens (filters :: DescribeElasticGpus -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeElasticGpus)
{-# DEPRECATED degFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degNextToken :: Lens.Lens' DescribeElasticGpus (Lude.Maybe Lude.Text)
degNextToken = Lens.lens (nextToken :: DescribeElasticGpus -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeElasticGpus)
{-# DEPRECATED degNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degDryRun :: Lens.Lens' DescribeElasticGpus (Lude.Maybe Lude.Bool)
degDryRun = Lens.lens (dryRun :: DescribeElasticGpus -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeElasticGpus)
{-# DEPRECATED degDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degMaxResults :: Lens.Lens' DescribeElasticGpus (Lude.Maybe Lude.Natural)
degMaxResults = Lens.lens (maxResults :: DescribeElasticGpus -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeElasticGpus)
{-# DEPRECATED degMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The Elastic Graphics accelerator IDs.
--
-- /Note:/ Consider using 'elasticGpuIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degElasticGpuIds :: Lens.Lens' DescribeElasticGpus (Lude.Maybe [Lude.Text])
degElasticGpuIds = Lens.lens (elasticGpuIds :: DescribeElasticGpus -> Lude.Maybe [Lude.Text]) (\s a -> s {elasticGpuIds = a} :: DescribeElasticGpus)
{-# DEPRECATED degElasticGpuIds "Use generic-lens or generic-optics with 'elasticGpuIds' instead." #-}

instance Lude.AWSRequest DescribeElasticGpus where
  type Rs DescribeElasticGpus = DescribeElasticGpusResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeElasticGpusResponse'
            Lude.<$> ( x Lude..@? "elasticGpuSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (x Lude..@? "maxResults")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeElasticGpus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeElasticGpus where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeElasticGpus where
  toQuery DescribeElasticGpus' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeElasticGpus" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        Lude.toQuery
          (Lude.toQueryList "ElasticGpuId" Lude.<$> elasticGpuIds)
      ]

-- | /See:/ 'mkDescribeElasticGpusResponse' smart constructor.
data DescribeElasticGpusResponse = DescribeElasticGpusResponse'
  { -- | Information about the Elastic Graphics accelerators.
    elasticGpuSet :: Lude.Maybe [ElasticGpus],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
    maxResults :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeElasticGpusResponse' with the minimum fields required to make a request.
--
-- * 'elasticGpuSet' - Information about the Elastic Graphics accelerators.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'maxResults' - The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
-- * 'responseStatus' - The response status code.
mkDescribeElasticGpusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeElasticGpusResponse
mkDescribeElasticGpusResponse pResponseStatus_ =
  DescribeElasticGpusResponse'
    { elasticGpuSet = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Elastic Graphics accelerators.
--
-- /Note:/ Consider using 'elasticGpuSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsElasticGpuSet :: Lens.Lens' DescribeElasticGpusResponse (Lude.Maybe [ElasticGpus])
degrsElasticGpuSet = Lens.lens (elasticGpuSet :: DescribeElasticGpusResponse -> Lude.Maybe [ElasticGpus]) (\s a -> s {elasticGpuSet = a} :: DescribeElasticGpusResponse)
{-# DEPRECATED degrsElasticGpuSet "Use generic-lens or generic-optics with 'elasticGpuSet' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsNextToken :: Lens.Lens' DescribeElasticGpusResponse (Lude.Maybe Lude.Text)
degrsNextToken = Lens.lens (nextToken :: DescribeElasticGpusResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeElasticGpusResponse)
{-# DEPRECATED degrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total number of items to return. If the total number of items available is more than the value specified in max-items then a Next-Token will be provided in the output that you can use to resume pagination.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsMaxResults :: Lens.Lens' DescribeElasticGpusResponse (Lude.Maybe Lude.Int)
degrsMaxResults = Lens.lens (maxResults :: DescribeElasticGpusResponse -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeElasticGpusResponse)
{-# DEPRECATED degrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
degrsResponseStatus :: Lens.Lens' DescribeElasticGpusResponse Lude.Int
degrsResponseStatus = Lens.lens (responseStatus :: DescribeElasticGpusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeElasticGpusResponse)
{-# DEPRECATED degrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
