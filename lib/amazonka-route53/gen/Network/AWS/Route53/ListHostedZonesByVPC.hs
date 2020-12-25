{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
module Network.AWS.Route53.ListHostedZonesByVPC
  ( -- * Creating a request
    ListHostedZonesByVPC (..),
    mkListHostedZonesByVPC,

    -- ** Request lenses
    lhzbvpcVPCId,
    lhzbvpcVPCRegion,
    lhzbvpcMaxItems,
    lhzbvpcNextToken,

    -- * Destructuring the response
    ListHostedZonesByVPCResponse (..),
    mkListHostedZonesByVPCResponse,

    -- ** Response lenses
    lhzbvpcrrsHostedZoneSummaries,
    lhzbvpcrrsMaxItems,
    lhzbvpcrrsNextToken,
    lhzbvpcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account created the hosted zones.
--
-- /See:/ 'mkListHostedZonesByVPC' smart constructor.
data ListHostedZonesByVPC = ListHostedZonesByVPC'
  { -- | The ID of the Amazon VPC that you want to list hosted zones for.
    vPCId :: Types.VPCId,
    -- | For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
    vPCRegion :: Types.VPCRegion,
    -- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
    maxItems :: Core.Maybe Types.MaxItems,
    -- | If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.
    --
    -- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
    -- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByVPC' value with any optional fields omitted.
mkListHostedZonesByVPC ::
  -- | 'vPCId'
  Types.VPCId ->
  -- | 'vPCRegion'
  Types.VPCRegion ->
  ListHostedZonesByVPC
mkListHostedZonesByVPC vPCId vPCRegion =
  ListHostedZonesByVPC'
    { vPCId,
      vPCRegion,
      maxItems = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the Amazon VPC that you want to list hosted zones for.
--
-- /Note:/ Consider using 'vPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcVPCId :: Lens.Lens' ListHostedZonesByVPC Types.VPCId
lhzbvpcVPCId = Lens.field @"vPCId"
{-# DEPRECATED lhzbvpcVPCId "Use generic-lens or generic-optics with 'vPCId' instead." #-}

-- | For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
--
-- /Note:/ Consider using 'vPCRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcVPCRegion :: Lens.Lens' ListHostedZonesByVPC Types.VPCRegion
lhzbvpcVPCRegion = Lens.field @"vPCRegion"
{-# DEPRECATED lhzbvpcVPCRegion "Use generic-lens or generic-optics with 'vPCRegion' instead." #-}

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcMaxItems :: Lens.Lens' ListHostedZonesByVPC (Core.Maybe Types.MaxItems)
lhzbvpcMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzbvpcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.
--
-- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
-- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcNextToken :: Lens.Lens' ListHostedZonesByVPC (Core.Maybe Types.PaginationToken)
lhzbvpcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhzbvpcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListHostedZonesByVPC where
  type Rs ListHostedZonesByVPC = ListHostedZonesByVPCResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/hostedzonesbyvpc",
        Core._rqQuery =
          Core.toQueryValue "vpcid" vPCId
            Core.<> (Core.toQueryValue "vpcregion" vPCRegion)
            Core.<> (Core.toQueryValue "maxitems" Core.<$> maxItems)
            Core.<> (Core.toQueryValue "nexttoken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesByVPCResponse'
            Core.<$> ( x Core..@? "HostedZoneSummaries" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "HostedZoneSummary"
                     )
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListHostedZonesByVPCResponse' smart constructor.
data ListHostedZonesByVPCResponse = ListHostedZonesByVPCResponse'
  { -- | A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
    hostedZoneSummaries :: [Types.HostedZoneSummary],
    -- | The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
    maxItems :: Types.MaxItems,
    -- | The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByVPCResponse' value with any optional fields omitted.
mkListHostedZonesByVPCResponse ::
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListHostedZonesByVPCResponse
mkListHostedZonesByVPCResponse maxItems responseStatus =
  ListHostedZonesByVPCResponse'
    { hostedZoneSummaries = Core.mempty,
      maxItems,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsHostedZoneSummaries :: Lens.Lens' ListHostedZonesByVPCResponse [Types.HostedZoneSummary]
lhzbvpcrrsHostedZoneSummaries = Lens.field @"hostedZoneSummaries"
{-# DEPRECATED lhzbvpcrrsHostedZoneSummaries "Use generic-lens or generic-optics with 'hostedZoneSummaries' instead." #-}

-- | The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsMaxItems :: Lens.Lens' ListHostedZonesByVPCResponse Types.MaxItems
lhzbvpcrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzbvpcrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsNextToken :: Lens.Lens' ListHostedZonesByVPCResponse (Core.Maybe Types.PaginationToken)
lhzbvpcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhzbvpcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvpcrrsResponseStatus :: Lens.Lens' ListHostedZonesByVPCResponse Core.Int
lhzbvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhzbvpcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
