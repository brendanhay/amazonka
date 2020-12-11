{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetCoipPoolUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the allocations from the specified customer-owned address pool.
module Network.AWS.EC2.GetCoipPoolUsage
  ( -- * Creating a request
    GetCoipPoolUsage (..),
    mkGetCoipPoolUsage,

    -- ** Request lenses
    gcpuFilters,
    gcpuNextToken,
    gcpuDryRun,
    gcpuMaxResults,
    gcpuPoolId,

    -- * Destructuring the response
    GetCoipPoolUsageResponse (..),
    mkGetCoipPoolUsageResponse,

    -- ** Response lenses
    gcpursCoipAddressUsages,
    gcpursCoipPoolId,
    gcpursLocalGatewayRouteTableId,
    gcpursResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetCoipPoolUsage' smart constructor.
data GetCoipPoolUsage = GetCoipPoolUsage'
  { filters ::
      Lude.Maybe [Filter],
    nextToken :: Lude.Maybe Lude.Text,
    dryRun :: Lude.Maybe Lude.Bool,
    maxResults :: Lude.Maybe Lude.Natural,
    poolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetCoipPoolUsage' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters. The following are the possible values:
--
--
--     * @coip-address-usage.allocation-id@
--
--
--
--     * @coip-address-usage.aws-account-id@
--
--
--
--     * @coip-address-usage.aws-service@
--
--
--
--     * @coip-address-usage.co-ip@
--
--
-- * 'maxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
-- * 'nextToken' - The token for the next page of results.
-- * 'poolId' - The ID of the address pool.
mkGetCoipPoolUsage ::
  -- | 'poolId'
  Lude.Text ->
  GetCoipPoolUsage
mkGetCoipPoolUsage pPoolId_ =
  GetCoipPoolUsage'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing,
      poolId = pPoolId_
    }

-- | The filters. The following are the possible values:
--
--
--     * @coip-address-usage.allocation-id@
--
--
--
--     * @coip-address-usage.aws-account-id@
--
--
--
--     * @coip-address-usage.aws-service@
--
--
--
--     * @coip-address-usage.co-ip@
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuFilters :: Lens.Lens' GetCoipPoolUsage (Lude.Maybe [Filter])
gcpuFilters = Lens.lens (filters :: GetCoipPoolUsage -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: GetCoipPoolUsage)
{-# DEPRECATED gcpuFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuNextToken :: Lens.Lens' GetCoipPoolUsage (Lude.Maybe Lude.Text)
gcpuNextToken = Lens.lens (nextToken :: GetCoipPoolUsage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetCoipPoolUsage)
{-# DEPRECATED gcpuNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuDryRun :: Lens.Lens' GetCoipPoolUsage (Lude.Maybe Lude.Bool)
gcpuDryRun = Lens.lens (dryRun :: GetCoipPoolUsage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetCoipPoolUsage)
{-# DEPRECATED gcpuDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuMaxResults :: Lens.Lens' GetCoipPoolUsage (Lude.Maybe Lude.Natural)
gcpuMaxResults = Lens.lens (maxResults :: GetCoipPoolUsage -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetCoipPoolUsage)
{-# DEPRECATED gcpuMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpuPoolId :: Lens.Lens' GetCoipPoolUsage Lude.Text
gcpuPoolId = Lens.lens (poolId :: GetCoipPoolUsage -> Lude.Text) (\s a -> s {poolId = a} :: GetCoipPoolUsage)
{-# DEPRECATED gcpuPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

instance Lude.AWSRequest GetCoipPoolUsage where
  type Rs GetCoipPoolUsage = GetCoipPoolUsageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetCoipPoolUsageResponse'
            Lude.<$> ( x Lude..@? "coipAddressUsageSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "coipPoolId")
            Lude.<*> (x Lude..@? "localGatewayRouteTableId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetCoipPoolUsage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetCoipPoolUsage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetCoipPoolUsage where
  toQuery GetCoipPoolUsage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetCoipPoolUsage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults,
        "PoolId" Lude.=: poolId
      ]

-- | /See:/ 'mkGetCoipPoolUsageResponse' smart constructor.
data GetCoipPoolUsageResponse = GetCoipPoolUsageResponse'
  { coipAddressUsages ::
      Lude.Maybe [CoipAddressUsage],
    coipPoolId :: Lude.Maybe Lude.Text,
    localGatewayRouteTableId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetCoipPoolUsageResponse' with the minimum fields required to make a request.
--
-- * 'coipAddressUsages' - Information about the address usage.
-- * 'coipPoolId' - The ID of the customer-owned address pool.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'responseStatus' - The response status code.
mkGetCoipPoolUsageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetCoipPoolUsageResponse
mkGetCoipPoolUsageResponse pResponseStatus_ =
  GetCoipPoolUsageResponse'
    { coipAddressUsages = Lude.Nothing,
      coipPoolId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the address usage.
--
-- /Note:/ Consider using 'coipAddressUsages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpursCoipAddressUsages :: Lens.Lens' GetCoipPoolUsageResponse (Lude.Maybe [CoipAddressUsage])
gcpursCoipAddressUsages = Lens.lens (coipAddressUsages :: GetCoipPoolUsageResponse -> Lude.Maybe [CoipAddressUsage]) (\s a -> s {coipAddressUsages = a} :: GetCoipPoolUsageResponse)
{-# DEPRECATED gcpursCoipAddressUsages "Use generic-lens or generic-optics with 'coipAddressUsages' instead." #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'coipPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpursCoipPoolId :: Lens.Lens' GetCoipPoolUsageResponse (Lude.Maybe Lude.Text)
gcpursCoipPoolId = Lens.lens (coipPoolId :: GetCoipPoolUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {coipPoolId = a} :: GetCoipPoolUsageResponse)
{-# DEPRECATED gcpursCoipPoolId "Use generic-lens or generic-optics with 'coipPoolId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpursLocalGatewayRouteTableId :: Lens.Lens' GetCoipPoolUsageResponse (Lude.Maybe Lude.Text)
gcpursLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: GetCoipPoolUsageResponse -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: GetCoipPoolUsageResponse)
{-# DEPRECATED gcpursLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpursResponseStatus :: Lens.Lens' GetCoipPoolUsageResponse Lude.Int
gcpursResponseStatus = Lens.lens (responseStatus :: GetCoipPoolUsageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetCoipPoolUsageResponse)
{-# DEPRECATED gcpursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
