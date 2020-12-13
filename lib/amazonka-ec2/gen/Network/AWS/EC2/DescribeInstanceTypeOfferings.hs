{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceTypeOfferings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all instance types offered. The results can be filtered by location (Region or Availability Zone). If no location is specified, the instance types offered in the current Region are returned.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceTypeOfferings
  ( -- * Creating a request
    DescribeInstanceTypeOfferings (..),
    mkDescribeInstanceTypeOfferings,

    -- ** Request lenses
    ditoFilters,
    ditoNextToken,
    ditoLocationType,
    ditoDryRun,
    ditoMaxResults,

    -- * Destructuring the response
    DescribeInstanceTypeOfferingsResponse (..),
    mkDescribeInstanceTypeOfferingsResponse,

    -- ** Response lenses
    ditorsInstanceTypeOfferings,
    ditorsNextToken,
    ditorsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceTypeOfferings' smart constructor.
data DescribeInstanceTypeOfferings = DescribeInstanceTypeOfferings'
  { -- | One or more filters. Filter names and values are case-sensitive.
    --
    --
    --     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)
    --
    --
    --     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
    filters :: Lude.Maybe [Filter],
    -- | The token to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The location type.
    locationType :: Lude.Maybe LocationType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceTypeOfferings' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Filter names and values are case-sensitive.
--
--
--     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)
--
--
--     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
--
--
-- * 'nextToken' - The token to retrieve the next page of results.
-- * 'locationType' - The location type.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'maxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
mkDescribeInstanceTypeOfferings ::
  DescribeInstanceTypeOfferings
mkDescribeInstanceTypeOfferings =
  DescribeInstanceTypeOfferings'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      locationType = Lude.Nothing,
      dryRun = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | One or more filters. Filter names and values are case-sensitive.
--
--
--     * @location@ - This depends on the location type. For example, if the location type is @region@ (default), the location is the Region code (for example, @us-east-2@ .)
--
--
--     * @instance-type@ - The instance type. For example, @c5.2xlarge@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoFilters :: Lens.Lens' DescribeInstanceTypeOfferings (Lude.Maybe [Filter])
ditoFilters = Lens.lens (filters :: DescribeInstanceTypeOfferings -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInstanceTypeOfferings)
{-# DEPRECATED ditoFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoNextToken :: Lens.Lens' DescribeInstanceTypeOfferings (Lude.Maybe Lude.Text)
ditoNextToken = Lens.lens (nextToken :: DescribeInstanceTypeOfferings -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceTypeOfferings)
{-# DEPRECATED ditoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The location type.
--
-- /Note:/ Consider using 'locationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoLocationType :: Lens.Lens' DescribeInstanceTypeOfferings (Lude.Maybe LocationType)
ditoLocationType = Lens.lens (locationType :: DescribeInstanceTypeOfferings -> Lude.Maybe LocationType) (\s a -> s {locationType = a} :: DescribeInstanceTypeOfferings)
{-# DEPRECATED ditoLocationType "Use generic-lens or generic-optics with 'locationType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoDryRun :: Lens.Lens' DescribeInstanceTypeOfferings (Lude.Maybe Lude.Bool)
ditoDryRun = Lens.lens (dryRun :: DescribeInstanceTypeOfferings -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceTypeOfferings)
{-# DEPRECATED ditoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the next token value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditoMaxResults :: Lens.Lens' DescribeInstanceTypeOfferings (Lude.Maybe Lude.Natural)
ditoMaxResults = Lens.lens (maxResults :: DescribeInstanceTypeOfferings -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeInstanceTypeOfferings)
{-# DEPRECATED ditoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager DescribeInstanceTypeOfferings where
  page rq rs
    | Page.stop (rs Lens.^. ditorsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ditorsInstanceTypeOfferings) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ditoNextToken Lens..~ rs Lens.^. ditorsNextToken

instance Lude.AWSRequest DescribeInstanceTypeOfferings where
  type
    Rs DescribeInstanceTypeOfferings =
      DescribeInstanceTypeOfferingsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceTypeOfferingsResponse'
            Lude.<$> ( x Lude..@? "instanceTypeOfferingSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceTypeOfferings where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceTypeOfferings where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceTypeOfferings where
  toQuery DescribeInstanceTypeOfferings' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeInstanceTypeOfferings" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "NextToken" Lude.=: nextToken,
        "LocationType" Lude.=: locationType,
        "DryRun" Lude.=: dryRun,
        "MaxResults" Lude.=: maxResults
      ]

-- | /See:/ 'mkDescribeInstanceTypeOfferingsResponse' smart constructor.
data DescribeInstanceTypeOfferingsResponse = DescribeInstanceTypeOfferingsResponse'
  { -- | The instance types offered.
    instanceTypeOfferings :: Lude.Maybe [InstanceTypeOffering],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceTypeOfferingsResponse' with the minimum fields required to make a request.
--
-- * 'instanceTypeOfferings' - The instance types offered.
-- * 'nextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
-- * 'responseStatus' - The response status code.
mkDescribeInstanceTypeOfferingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceTypeOfferingsResponse
mkDescribeInstanceTypeOfferingsResponse pResponseStatus_ =
  DescribeInstanceTypeOfferingsResponse'
    { instanceTypeOfferings =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance types offered.
--
-- /Note:/ Consider using 'instanceTypeOfferings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorsInstanceTypeOfferings :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Lude.Maybe [InstanceTypeOffering])
ditorsInstanceTypeOfferings = Lens.lens (instanceTypeOfferings :: DescribeInstanceTypeOfferingsResponse -> Lude.Maybe [InstanceTypeOffering]) (\s a -> s {instanceTypeOfferings = a} :: DescribeInstanceTypeOfferingsResponse)
{-# DEPRECATED ditorsInstanceTypeOfferings "Use generic-lens or generic-optics with 'instanceTypeOfferings' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorsNextToken :: Lens.Lens' DescribeInstanceTypeOfferingsResponse (Lude.Maybe Lude.Text)
ditorsNextToken = Lens.lens (nextToken :: DescribeInstanceTypeOfferingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInstanceTypeOfferingsResponse)
{-# DEPRECATED ditorsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditorsResponseStatus :: Lens.Lens' DescribeInstanceTypeOfferingsResponse Lude.Int
ditorsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceTypeOfferingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceTypeOfferingsResponse)
{-# DEPRECATED ditorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
