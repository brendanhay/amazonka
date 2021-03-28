{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the policies for the specified Auto Scaling group.
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribePolicies
    (
    -- * Creating a request
      DescribePolicies (..)
    , mkDescribePolicies
    -- ** Request lenses
    , dpsAutoScalingGroupName
    , dpsMaxRecords
    , dpsNextToken
    , dpsPolicyNames
    , dpsPolicyTypes

    -- * Destructuring the response
    , DescribePoliciesResponse (..)
    , mkDescribePoliciesResponse
    -- ** Response lenses
    , dprrsNextToken
    , dprrsScalingPolicies
    , dprrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribePolicies' smart constructor.
data DescribePolicies = DescribePolicies'
  { autoScalingGroupName :: Core.Maybe Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , maxRecords :: Core.Maybe Core.Int
    -- ^ The maximum number of items to be returned with each call. The default value is @50@ and the maximum value is @100@ .
  , nextToken :: Core.Maybe Types.XmlString
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  , policyNames :: Core.Maybe [Types.ResourceName]
    -- ^ The names of one or more policies. If you omit this parameter, all policies are described. If a group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
  , policyTypes :: Core.Maybe [Types.XmlStringMaxLen64]
    -- ^ One or more policy types. The valid values are @SimpleScaling@ , @StepScaling@ , and @TargetTrackingScaling@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePolicies' value with any optional fields omitted.
mkDescribePolicies
    :: DescribePolicies
mkDescribePolicies
  = DescribePolicies'{autoScalingGroupName = Core.Nothing,
                      maxRecords = Core.Nothing, nextToken = Core.Nothing,
                      policyNames = Core.Nothing, policyTypes = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsAutoScalingGroupName :: Lens.Lens' DescribePolicies (Core.Maybe Types.ResourceName)
dpsAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dpsAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The maximum number of items to be returned with each call. The default value is @50@ and the maximum value is @100@ .
--
-- /Note:/ Consider using 'maxRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsMaxRecords :: Lens.Lens' DescribePolicies (Core.Maybe Core.Int)
dpsMaxRecords = Lens.field @"maxRecords"
{-# INLINEABLE dpsMaxRecords #-}
{-# DEPRECATED maxRecords "Use generic-lens or generic-optics with 'maxRecords' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsNextToken :: Lens.Lens' DescribePolicies (Core.Maybe Types.XmlString)
dpsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dpsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The names of one or more policies. If you omit this parameter, all policies are described. If a group name is provided, the results are limited to that group. This list is limited to 50 items. If you specify an unknown policy name, it is ignored with no error.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPolicyNames :: Lens.Lens' DescribePolicies (Core.Maybe [Types.ResourceName])
dpsPolicyNames = Lens.field @"policyNames"
{-# INLINEABLE dpsPolicyNames #-}
{-# DEPRECATED policyNames "Use generic-lens or generic-optics with 'policyNames' instead"  #-}

-- | One or more policy types. The valid values are @SimpleScaling@ , @StepScaling@ , and @TargetTrackingScaling@ .
--
-- /Note:/ Consider using 'policyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpsPolicyTypes :: Lens.Lens' DescribePolicies (Core.Maybe [Types.XmlStringMaxLen64])
dpsPolicyTypes = Lens.field @"policyTypes"
{-# INLINEABLE dpsPolicyTypes #-}
{-# DEPRECATED policyTypes "Use generic-lens or generic-optics with 'policyTypes' instead"  #-}

instance Core.ToQuery DescribePolicies where
        toQuery DescribePolicies{..}
          = Core.toQueryPair "Action" ("DescribePolicies" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoScalingGroupName")
                autoScalingGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxRecords") maxRecords
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<>
              Core.toQueryPair "PolicyNames"
                (Core.maybe Core.mempty (Core.toQueryList "member") policyNames)
              Core.<>
              Core.toQueryPair "PolicyTypes"
                (Core.maybe Core.mempty (Core.toQueryList "member") policyTypes)

instance Core.ToHeaders DescribePolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribePolicies where
        type Rs DescribePolicies = DescribePoliciesResponse
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
          = Response.receiveXMLWrapper "DescribePoliciesResult"
              (\ s h x ->
                 DescribePoliciesResponse' Core.<$>
                   (x Core..@? "NextToken") Core.<*>
                     x Core..@? "ScalingPolicies" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribePolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"scalingPolicies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribePoliciesResponse' smart constructor.
data DescribePoliciesResponse = DescribePoliciesResponse'
  { nextToken :: Core.Maybe Types.XmlString
    -- ^ A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
  , scalingPolicies :: Core.Maybe [Types.ScalingPolicy]
    -- ^ The scaling policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribePoliciesResponse' value with any optional fields omitted.
mkDescribePoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribePoliciesResponse
mkDescribePoliciesResponse responseStatus
  = DescribePoliciesResponse'{nextToken = Core.Nothing,
                              scalingPolicies = Core.Nothing, responseStatus}

-- | A string that indicates that the response contains more items than can be returned in a single response. To receive additional items, specify this string for the @NextToken@ value when requesting the next set of items. This value is null when there are no more items to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsNextToken :: Lens.Lens' DescribePoliciesResponse (Core.Maybe Types.XmlString)
dprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The scaling policies.
--
-- /Note:/ Consider using 'scalingPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsScalingPolicies :: Lens.Lens' DescribePoliciesResponse (Core.Maybe [Types.ScalingPolicy])
dprrsScalingPolicies = Lens.field @"scalingPolicies"
{-# INLINEABLE dprrsScalingPolicies #-}
{-# DEPRECATED scalingPolicies "Use generic-lens or generic-optics with 'scalingPolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprrsResponseStatus :: Lens.Lens' DescribePoliciesResponse Core.Int
dprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
