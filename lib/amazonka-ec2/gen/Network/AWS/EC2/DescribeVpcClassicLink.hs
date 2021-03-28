{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVpcClassicLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the ClassicLink status of one or more VPCs.
module Network.AWS.EC2.DescribeVpcClassicLink
    (
    -- * Creating a request
      DescribeVpcClassicLink (..)
    , mkDescribeVpcClassicLink
    -- ** Request lenses
    , dvclDryRun
    , dvclFilters
    , dvclVpcIds

    -- * Destructuring the response
    , DescribeVpcClassicLinkResponse (..)
    , mkDescribeVpcClassicLinkResponse
    -- ** Response lenses
    , dvclrrsVpcs
    , dvclrrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeVpcClassicLink' smart constructor.
data DescribeVpcClassicLink = DescribeVpcClassicLink'
  { dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , filters :: Core.Maybe [Types.Filter]
    -- ^ One or more filters.
--
--
--     * @is-classic-link-enabled@ - Whether the VPC is enabled for ClassicLink (@true@ | @false@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
  , vpcIds :: Core.Maybe [Types.VpcId]
    -- ^ One or more VPCs for which you want to describe the ClassicLink status.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcClassicLink' value with any optional fields omitted.
mkDescribeVpcClassicLink
    :: DescribeVpcClassicLink
mkDescribeVpcClassicLink
  = DescribeVpcClassicLink'{dryRun = Core.Nothing,
                            filters = Core.Nothing, vpcIds = Core.Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclDryRun :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe Core.Bool)
dvclDryRun = Lens.field @"dryRun"
{-# INLINEABLE dvclDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | One or more filters.
--
--
--     * @is-classic-link-enabled@ - Whether the VPC is enabled for ClassicLink (@true@ | @false@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclFilters :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe [Types.Filter])
dvclFilters = Lens.field @"filters"
{-# INLINEABLE dvclFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | One or more VPCs for which you want to describe the ClassicLink status.
--
-- /Note:/ Consider using 'vpcIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclVpcIds :: Lens.Lens' DescribeVpcClassicLink (Core.Maybe [Types.VpcId])
dvclVpcIds = Lens.field @"vpcIds"
{-# INLINEABLE dvclVpcIds #-}
{-# DEPRECATED vpcIds "Use generic-lens or generic-optics with 'vpcIds' instead"  #-}

instance Core.ToQuery DescribeVpcClassicLink where
        toQuery DescribeVpcClassicLink{..}
          = Core.toQueryPair "Action" ("DescribeVpcClassicLink" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<> Core.maybe Core.mempty (Core.toQueryList "Filter") filters
              Core.<> Core.maybe Core.mempty (Core.toQueryList "VpcId") vpcIds

instance Core.ToHeaders DescribeVpcClassicLink where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeVpcClassicLink where
        type Rs DescribeVpcClassicLink = DescribeVpcClassicLinkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 DescribeVpcClassicLinkResponse' Core.<$>
                   (x Core..@? "vpcSet" Core..<@> Core.parseXMLList "item") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeVpcClassicLinkResponse' smart constructor.
data DescribeVpcClassicLinkResponse = DescribeVpcClassicLinkResponse'
  { vpcs :: Core.Maybe [Types.VpcClassicLink]
    -- ^ The ClassicLink status of one or more VPCs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcClassicLinkResponse' value with any optional fields omitted.
mkDescribeVpcClassicLinkResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcClassicLinkResponse
mkDescribeVpcClassicLinkResponse responseStatus
  = DescribeVpcClassicLinkResponse'{vpcs = Core.Nothing,
                                    responseStatus}

-- | The ClassicLink status of one or more VPCs.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrrsVpcs :: Lens.Lens' DescribeVpcClassicLinkResponse (Core.Maybe [Types.VpcClassicLink])
dvclrrsVpcs = Lens.field @"vpcs"
{-# INLINEABLE dvclrrsVpcs #-}
{-# DEPRECATED vpcs "Use generic-lens or generic-optics with 'vpcs' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvclrrsResponseStatus :: Lens.Lens' DescribeVpcClassicLinkResponse Core.Int
dvclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
