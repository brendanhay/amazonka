{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeRegions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Regions that are enabled for your account, or all Regions.
--
-- For a list of the Regions supported by Amazon EC2, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints> .
-- For information about enabling and disabling Regions for your account, see <https://docs.aws.amazon.com/general/latest/gr/rande-manage.html Managing AWS Regions> in the /AWS General Reference/ .
module Network.AWS.EC2.DescribeRegions
  ( -- * Creating a request
    DescribeRegions (..),
    mkDescribeRegions,

    -- ** Request lenses
    drRegionNames,
    drFilters,
    drAllRegions,
    drDryRun,

    -- * Destructuring the response
    DescribeRegionsResponse (..),
    mkDescribeRegionsResponse,

    -- ** Response lenses
    drrsRegions,
    drrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeRegions' smart constructor.
data DescribeRegions = DescribeRegions'
  { -- | The names of the Regions. You can specify any Regions, whether they are enabled and disabled for your account.
    regionNames :: Lude.Maybe [Lude.Text],
    -- | The filters.
    --
    --
    --     * @endpoint@ - The endpoint of the Region (for example, @ec2.us-east-1.amazonaws.com@ ).
    --
    --
    --     * @opt-in-status@ - The opt-in status of the Region (@opt-in-not-required@ | @opted-in@ | @not-opted-in@ ).
    --
    --
    --     * @region-name@ - The name of the Region (for example, @us-east-1@ ).
    filters :: Lude.Maybe [Filter],
    -- | Indicates whether to display all Regions, including Regions that are disabled for your account.
    allRegions :: Lude.Maybe Lude.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRegions' with the minimum fields required to make a request.
--
-- * 'regionNames' - The names of the Regions. You can specify any Regions, whether they are enabled and disabled for your account.
-- * 'filters' - The filters.
--
--
--     * @endpoint@ - The endpoint of the Region (for example, @ec2.us-east-1.amazonaws.com@ ).
--
--
--     * @opt-in-status@ - The opt-in status of the Region (@opt-in-not-required@ | @opted-in@ | @not-opted-in@ ).
--
--
--     * @region-name@ - The name of the Region (for example, @us-east-1@ ).
--
--
-- * 'allRegions' - Indicates whether to display all Regions, including Regions that are disabled for your account.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeRegions ::
  DescribeRegions
mkDescribeRegions =
  DescribeRegions'
    { regionNames = Lude.Nothing,
      filters = Lude.Nothing,
      allRegions = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The names of the Regions. You can specify any Regions, whether they are enabled and disabled for your account.
--
-- /Note:/ Consider using 'regionNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRegionNames :: Lens.Lens' DescribeRegions (Lude.Maybe [Lude.Text])
drRegionNames = Lens.lens (regionNames :: DescribeRegions -> Lude.Maybe [Lude.Text]) (\s a -> s {regionNames = a} :: DescribeRegions)
{-# DEPRECATED drRegionNames "Use generic-lens or generic-optics with 'regionNames' instead." #-}

-- | The filters.
--
--
--     * @endpoint@ - The endpoint of the Region (for example, @ec2.us-east-1.amazonaws.com@ ).
--
--
--     * @opt-in-status@ - The opt-in status of the Region (@opt-in-not-required@ | @opted-in@ | @not-opted-in@ ).
--
--
--     * @region-name@ - The name of the Region (for example, @us-east-1@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drFilters :: Lens.Lens' DescribeRegions (Lude.Maybe [Filter])
drFilters = Lens.lens (filters :: DescribeRegions -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeRegions)
{-# DEPRECATED drFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Indicates whether to display all Regions, including Regions that are disabled for your account.
--
-- /Note:/ Consider using 'allRegions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drAllRegions :: Lens.Lens' DescribeRegions (Lude.Maybe Lude.Bool)
drAllRegions = Lens.lens (allRegions :: DescribeRegions -> Lude.Maybe Lude.Bool) (\s a -> s {allRegions = a} :: DescribeRegions)
{-# DEPRECATED drAllRegions "Use generic-lens or generic-optics with 'allRegions' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drDryRun :: Lens.Lens' DescribeRegions (Lude.Maybe Lude.Bool)
drDryRun = Lens.lens (dryRun :: DescribeRegions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeRegions)
{-# DEPRECATED drDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeRegions where
  type Rs DescribeRegions = DescribeRegionsResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeRegionsResponse'
            Lude.<$> ( x Lude..@? "regionInfo" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeRegions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeRegions where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeRegions where
  toQuery DescribeRegions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeRegions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "RegionName" Lude.<$> regionNames),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "AllRegions" Lude.=: allRegions,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeRegionsResponse' smart constructor.
data DescribeRegionsResponse = DescribeRegionsResponse'
  { -- | Information about the Regions.
    regions :: Lude.Maybe [RegionInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeRegionsResponse' with the minimum fields required to make a request.
--
-- * 'regions' - Information about the Regions.
-- * 'responseStatus' - The response status code.
mkDescribeRegionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeRegionsResponse
mkDescribeRegionsResponse pResponseStatus_ =
  DescribeRegionsResponse'
    { regions = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Regions.
--
-- /Note:/ Consider using 'regions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRegions :: Lens.Lens' DescribeRegionsResponse (Lude.Maybe [RegionInfo])
drrsRegions = Lens.lens (regions :: DescribeRegionsResponse -> Lude.Maybe [RegionInfo]) (\s a -> s {regions = a} :: DescribeRegionsResponse)
{-# DEPRECATED drrsRegions "Use generic-lens or generic-optics with 'regions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DescribeRegionsResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DescribeRegionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeRegionsResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
