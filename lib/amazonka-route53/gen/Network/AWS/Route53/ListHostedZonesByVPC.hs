{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZonesByVPC
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account or AWS service owns the hosted zones. The @HostedZoneOwner@ structure in the response contains one of the following values:
--
--
--     * An @OwningAccount@ element, which contains the account number of either the current AWS account or another AWS account. Some services, such as AWS Cloud Map, create hosted zones using the current account. 
--
--
--     * An @OwningService@ element, which identifies the AWS service that created and owns the hosted zone. For example, if a hosted zone was created by Amazon Elastic File System (Amazon EFS), the value of @Owner@ is @efs.amazonaws.com@ . 
--
--
module Network.AWS.Route53.ListHostedZonesByVPC
    (
    -- * Creating a request
      ListHostedZonesByVPC (..)
    , mkListHostedZonesByVPC
    -- ** Request lenses
    , lhzbvpcVPCId
    , lhzbvpcVPCRegion
    , lhzbvpcMaxItems
    , lhzbvpcNextToken

    -- * Destructuring the response
    , ListHostedZonesByVPCResponse (..)
    , mkListHostedZonesByVPCResponse
    -- ** Response lenses
    , lhzbvpcrrsHostedZoneSummaries
    , lhzbvpcrrsMaxItems
    , lhzbvpcrrsNextToken
    , lhzbvpcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account created the hosted zones.
--
-- /See:/ 'mkListHostedZonesByVPC' smart constructor.
data ListHostedZonesByVPC = ListHostedZonesByVPC'
  { vPCId :: Types.VPCId
    -- ^ The ID of the Amazon VPC that you want to list hosted zones for.
  , vPCRegion :: Types.VPCRegion
    -- ^ For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in. 
  , maxItems :: Core.Maybe Types.MaxItems
    -- ^ (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request. 
--
-- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
-- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByVPC' value with any optional fields omitted.
mkListHostedZonesByVPC
    :: Types.VPCId -- ^ 'vPCId'
    -> Types.VPCRegion -- ^ 'vPCRegion'
    -> ListHostedZonesByVPC
mkListHostedZonesByVPC vPCId vPCRegion
  = ListHostedZonesByVPC'{vPCId, vPCRegion, maxItems = Core.Nothing,
                          nextToken = Core.Nothing}

-- | The ID of the Amazon VPC that you want to list hosted zones for.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcVPCId :: Lens.Lens' ListHostedZonesByVPC Types.VPCId
lhzbvpcVPCId = Lens.field @"vPCId"
{-# INLINEABLE lhzbvpcVPCId #-}
{-# DEPRECATED vPCId "Use generic-lens or generic-optics with 'vPCId' instead"  #-}

-- | For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in. 
--
-- /Note:/ Consider using 'vPCRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcVPCRegion :: Lens.Lens' ListHostedZonesByVPC Types.VPCRegion
lhzbvpcVPCRegion = Lens.field @"vPCRegion"
{-# INLINEABLE lhzbvpcVPCRegion #-}
{-# DEPRECATED vPCRegion "Use generic-lens or generic-optics with 'vPCRegion' instead"  #-}

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcMaxItems :: Lens.Lens' ListHostedZonesByVPC (Core.Maybe Types.MaxItems)
lhzbvpcMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lhzbvpcMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request. 
--
-- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
-- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcNextToken :: Lens.Lens' ListHostedZonesByVPC (Core.Maybe Types.PaginationToken)
lhzbvpcNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhzbvpcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListHostedZonesByVPC where
        toQuery ListHostedZonesByVPC{..}
          = Core.toQueryPair "vpcid" vPCId Core.<>
              Core.toQueryPair "vpcregion" vPCRegion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxitems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nexttoken") nextToken

instance Core.ToHeaders ListHostedZonesByVPC where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListHostedZonesByVPC where
        type Rs ListHostedZonesByVPC = ListHostedZonesByVPCResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2013-04-01/hostedzonesbyvpc",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListHostedZonesByVPCResponse' Core.<$>
                   (x Core..@ "HostedZoneSummaries" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "HostedZoneSummary")
                     Core.<*> x Core..@ "MaxItems"
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListHostedZonesByVPCResponse' smart constructor.
data ListHostedZonesByVPCResponse = ListHostedZonesByVPCResponse'
  { hostedZoneSummaries :: [Types.HostedZoneSummary]
    -- ^ A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
  , maxItems :: Types.MaxItems
    -- ^ The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByVPCResponse' value with any optional fields omitted.
mkListHostedZonesByVPCResponse
    :: Types.MaxItems -- ^ 'maxItems'
    -> Core.Int -- ^ 'responseStatus'
    -> ListHostedZonesByVPCResponse
mkListHostedZonesByVPCResponse maxItems responseStatus
  = ListHostedZonesByVPCResponse'{hostedZoneSummaries = Core.mempty,
                                  maxItems, nextToken = Core.Nothing, responseStatus}

-- | A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsHostedZoneSummaries :: Lens.Lens' ListHostedZonesByVPCResponse [Types.HostedZoneSummary]
lhzbvpcrrsHostedZoneSummaries = Lens.field @"hostedZoneSummaries"
{-# INLINEABLE lhzbvpcrrsHostedZoneSummaries #-}
{-# DEPRECATED hostedZoneSummaries "Use generic-lens or generic-optics with 'hostedZoneSummaries' instead"  #-}

-- | The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsMaxItems :: Lens.Lens' ListHostedZonesByVPCResponse Types.MaxItems
lhzbvpcrrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lhzbvpcrrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsNextToken :: Lens.Lens' ListHostedZonesByVPCResponse (Core.Maybe Types.PaginationToken)
lhzbvpcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lhzbvpcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsResponseStatus :: Lens.Lens' ListHostedZonesByVPCResponse Core.Int
lhzbvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lhzbvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
