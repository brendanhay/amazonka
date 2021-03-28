{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListVPCAssociationAuthorizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the VPCs that were created by other accounts and that can be associated with a specified hosted zone because you've submitted one or more @CreateVPCAssociationAuthorization@ requests. 
--
-- The response includes a @VPCs@ element with a @VPC@ child element for each VPC that can be associated with the hosted zone.
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListVPCAssociationAuthorizations
    (
    -- * Creating a request
      ListVPCAssociationAuthorizations (..)
    , mkListVPCAssociationAuthorizations
    -- ** Request lenses
    , lvpcaaHostedZoneId
    , lvpcaaMaxResults
    , lvpcaaNextToken

    -- * Destructuring the response
    , ListVPCAssociationAuthorizationsResponse (..)
    , mkListVPCAssociationAuthorizationsResponse
    -- ** Response lenses
    , lvpcaarrsHostedZoneId
    , lvpcaarrsVPCs
    , lvpcaarrsNextToken
    , lvpcaarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains information about that can be associated with your hosted zone.
--
-- /See:/ 'mkListVPCAssociationAuthorizations' smart constructor.
data ListVPCAssociationAuthorizations = ListVPCAssociationAuthorizations'
  { hostedZoneId :: Types.HostedZoneId
    -- ^ The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
  , maxResults :: Core.Maybe Types.MaxResults
    -- ^ /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Route 53 returns up to 50 VPCs per page.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVPCAssociationAuthorizations' value with any optional fields omitted.
mkListVPCAssociationAuthorizations
    :: Types.HostedZoneId -- ^ 'hostedZoneId'
    -> ListVPCAssociationAuthorizations
mkListVPCAssociationAuthorizations hostedZoneId
  = ListVPCAssociationAuthorizations'{hostedZoneId,
                                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaaHostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizations Types.HostedZoneId
lvpcaaHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE lvpcaaHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Route 53 returns up to 50 VPCs per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaaMaxResults :: Lens.Lens' ListVPCAssociationAuthorizations (Core.Maybe Types.MaxResults)
lvpcaaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lvpcaaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaaNextToken :: Lens.Lens' ListVPCAssociationAuthorizations (Core.Maybe Types.PaginationToken)
lvpcaaNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvpcaaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListVPCAssociationAuthorizations where
        toQuery ListVPCAssociationAuthorizations{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxresults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nexttoken") nextToken

instance Core.ToHeaders ListVPCAssociationAuthorizations where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListVPCAssociationAuthorizations where
        type Rs ListVPCAssociationAuthorizations =
             ListVPCAssociationAuthorizationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/hostedzone/" Core.<> Core.toText hostedZoneId Core.<>
                             "/authorizevpcassociation",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListVPCAssociationAuthorizationsResponse' Core.<$>
                   (x Core..@ "HostedZoneId") Core.<*>
                     x Core..@ "VPCs" Core..<@> Core.parseXMLNonEmpty "VPC"
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListVPCAssociationAuthorizations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^. Lens.field @"vPCs") = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListVPCAssociationAuthorizationsResponse' smart constructor.
data ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse'
  { hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that you can associate the listed VPCs with.
  , vPCs :: Core.NonEmpty Types.VPC
    -- ^ The list of VPCs that are authorized to be associated with the specified hosted zone.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVPCAssociationAuthorizationsResponse' value with any optional fields omitted.
mkListVPCAssociationAuthorizationsResponse
    :: Types.ResourceId -- ^ 'hostedZoneId'
    -> Core.NonEmpty Types.VPC -- ^ 'vPCs'
    -> Core.Int -- ^ 'responseStatus'
    -> ListVPCAssociationAuthorizationsResponse
mkListVPCAssociationAuthorizationsResponse hostedZoneId vPCs
  responseStatus
  = ListVPCAssociationAuthorizationsResponse'{hostedZoneId, vPCs,
                                              nextToken = Core.Nothing, responseStatus}

-- | The ID of the hosted zone that you can associate the listed VPCs with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaarrsHostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizationsResponse Types.ResourceId
lvpcaarrsHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE lvpcaarrsHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The list of VPCs that are authorized to be associated with the specified hosted zone.
--
-- /Note:/ Consider using 'vPCs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaarrsVPCs :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Core.NonEmpty Types.VPC)
lvpcaarrsVPCs = Lens.field @"vPCs"
{-# INLINEABLE lvpcaarrsVPCs #-}
{-# DEPRECATED vPCs "Use generic-lens or generic-optics with 'vPCs' instead"  #-}

-- | When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaarrsNextToken :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Core.Maybe Types.PaginationToken)
lvpcaarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lvpcaarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvpcaarrsResponseStatus :: Lens.Lens' ListVPCAssociationAuthorizationsResponse Core.Int
lvpcaarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lvpcaarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
