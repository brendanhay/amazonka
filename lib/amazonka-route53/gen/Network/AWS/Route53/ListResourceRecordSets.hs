{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListResourceRecordSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource record sets in a specified hosted zone.
--
-- @ListResourceRecordSets@ returns up to 100 resource record sets at a time in ASCII order, beginning at a position specified by the @name@ and @type@ elements.
-- __Sort order__ 
-- @ListResourceRecordSets@ sorts results first by DNS name with the labels reversed, for example:
-- @com.example.www.@ 
-- Note the trailing dot, which can change the sort order when the record name contains characters that appear before @.@ (decimal 46) in the ASCII table. These characters include the following: @! " # $ % & ' ( ) * + , -@ 
-- When multiple records have the same DNS name, @ListResourceRecordSets@ sorts results by the record type.
-- __Specifying where to start listing records__ 
-- You can use the name and type elements to specify the resource record set that the list begins with:
--
--     * If you do not specify Name or Type
--
--     * The results begin with the first resource record set that the hosted zone contains.
--
--
--     * If you specify Name but not Type
--
--     * The results begin with the first resource record set in the list whose name is greater than or equal to @Name@ .
--
--
--     * If you specify Type but not Name
--
--     * Amazon Route 53 returns the @InvalidInput@ error.
--
--
--     * If you specify both Name and Type
--
--     * The results begin with the first resource record set in the list whose name is greater than or equal to @Name@ , and whose type is greater than or equal to @Type@ .
--
--
-- __Resource record sets that are PENDING__ 
-- This action returns the most current version of the records. This includes records that are @PENDING@ , and that are not yet available on all Route 53 DNS servers.
-- __Changing resource record sets__ 
-- To ensure that you get an accurate listing of the resource record sets for a hosted zone at a point in time, do not submit a @ChangeResourceRecordSets@ request while you're paging through the results of a @ListResourceRecordSets@ request. If you do, some pages may display results without the latest changes while other pages display results with the latest changes.
-- __Displaying the next page of results__ 
-- If a @ListResourceRecordSets@ command returns more than one page of results, the value of @IsTruncated@ is @true@ . To display the next page of results, get the values of @NextRecordName@ , @NextRecordType@ , and @NextRecordIdentifier@ (if any) from the response. Then submit another @ListResourceRecordSets@ request, and specify those values for @StartRecordName@ , @StartRecordType@ , and @StartRecordIdentifier@ .
--
-- This operation returns paginated results.
module Network.AWS.Route53.ListResourceRecordSets
    (
    -- * Creating a request
      ListResourceRecordSets (..)
    , mkListResourceRecordSets
    -- ** Request lenses
    , lrrsHostedZoneId
    , lrrsMaxItems
    , lrrsStartRecordIdentifier
    , lrrsStartRecordName
    , lrrsStartRecordType

    -- * Destructuring the response
    , ListResourceRecordSetsResponse (..)
    , mkListResourceRecordSetsResponse
    -- ** Response lenses
    , lrrsrrsResourceRecordSets
    , lrrsrrsIsTruncated
    , lrrsrrsMaxItems
    , lrrsrrsNextRecordIdentifier
    , lrrsrrsNextRecordName
    , lrrsrrsNextRecordType
    , lrrsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request for the resource record sets that are associated with a specified hosted zone.
--
-- /See:/ 'mkListResourceRecordSets' smart constructor.
data ListResourceRecordSets = ListResourceRecordSets'
  { hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that contains the resource record sets that you want to list.
  , maxItems :: Core.Maybe Types.MaxItems
    -- ^ (Optional) The maximum number of resource records sets to include in the response body for this request. If the response includes more than @maxitems@ resource record sets, the value of the @IsTruncated@ element in the response is @true@ , and the values of the @NextRecordName@ and @NextRecordType@ elements in the response identify the first resource record set in the next group of @maxitems@ resource record sets.
  , startRecordIdentifier :: Core.Maybe Types.StartRecordIdentifier
    -- ^ /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, specify the value of @NextRecordIdentifier@ from the previous response to get the next resource record set that has the current DNS name and type.
  , startRecordName :: Core.Maybe Types.StartRecordName
    -- ^ The first name in the lexicographic ordering of resource record sets that you want to list. If the specified record name doesn't exist, the results begin with the first resource record set that has a name greater than the value of @name@ .
  , startRecordType :: Core.Maybe Types.RecordType
    -- ^ The type of resource record set to begin the record listing from.
--
-- Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@ 
-- Values for weighted, latency, geolocation, and failover resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@ 
-- Values for alias resource record sets: 
--
--     * __API Gateway custom regional API or edge-optimized API__ : A
--
--
--     * __CloudFront distribution__ : A or AAAA
--
--
--     * __Elastic Beanstalk environment that has a regionalized subdomain__ : A
--
--
--     * __Elastic Load Balancing load balancer__ : A | AAAA
--
--
--     * __S3 bucket__ : A
--
--
--     * __VPC interface VPC endpoint__ : A
--
--
--     * __Another resource record set in this hosted zone:__ The type of the resource record set that the alias references.
--
--
-- Constraint: Specifying @type@ without specifying @name@ returns an @InvalidInput@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceRecordSets' value with any optional fields omitted.
mkListResourceRecordSets
    :: Types.ResourceId -- ^ 'hostedZoneId'
    -> ListResourceRecordSets
mkListResourceRecordSets hostedZoneId
  = ListResourceRecordSets'{hostedZoneId, maxItems = Core.Nothing,
                            startRecordIdentifier = Core.Nothing,
                            startRecordName = Core.Nothing, startRecordType = Core.Nothing}

-- | The ID of the hosted zone that contains the resource record sets that you want to list.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsHostedZoneId :: Lens.Lens' ListResourceRecordSets Types.ResourceId
lrrsHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE lrrsHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | (Optional) The maximum number of resource records sets to include in the response body for this request. If the response includes more than @maxitems@ resource record sets, the value of the @IsTruncated@ element in the response is @true@ , and the values of the @NextRecordName@ and @NextRecordType@ elements in the response identify the first resource record set in the next group of @maxitems@ resource record sets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsMaxItems :: Lens.Lens' ListResourceRecordSets (Core.Maybe Types.MaxItems)
lrrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lrrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, specify the value of @NextRecordIdentifier@ from the previous response to get the next resource record set that has the current DNS name and type.
--
-- /Note:/ Consider using 'startRecordIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsStartRecordIdentifier :: Lens.Lens' ListResourceRecordSets (Core.Maybe Types.StartRecordIdentifier)
lrrsStartRecordIdentifier = Lens.field @"startRecordIdentifier"
{-# INLINEABLE lrrsStartRecordIdentifier #-}
{-# DEPRECATED startRecordIdentifier "Use generic-lens or generic-optics with 'startRecordIdentifier' instead"  #-}

-- | The first name in the lexicographic ordering of resource record sets that you want to list. If the specified record name doesn't exist, the results begin with the first resource record set that has a name greater than the value of @name@ .
--
-- /Note:/ Consider using 'startRecordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsStartRecordName :: Lens.Lens' ListResourceRecordSets (Core.Maybe Types.StartRecordName)
lrrsStartRecordName = Lens.field @"startRecordName"
{-# INLINEABLE lrrsStartRecordName #-}
{-# DEPRECATED startRecordName "Use generic-lens or generic-optics with 'startRecordName' instead"  #-}

-- | The type of resource record set to begin the record listing from.
--
-- Valid values for basic resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @NS@ | @PTR@ | @SOA@ | @SPF@ | @SRV@ | @TXT@ 
-- Values for weighted, latency, geolocation, and failover resource record sets: @A@ | @AAAA@ | @CAA@ | @CNAME@ | @MX@ | @NAPTR@ | @PTR@ | @SPF@ | @SRV@ | @TXT@ 
-- Values for alias resource record sets: 
--
--     * __API Gateway custom regional API or edge-optimized API__ : A
--
--
--     * __CloudFront distribution__ : A or AAAA
--
--
--     * __Elastic Beanstalk environment that has a regionalized subdomain__ : A
--
--
--     * __Elastic Load Balancing load balancer__ : A | AAAA
--
--
--     * __S3 bucket__ : A
--
--
--     * __VPC interface VPC endpoint__ : A
--
--
--     * __Another resource record set in this hosted zone:__ The type of the resource record set that the alias references.
--
--
-- Constraint: Specifying @type@ without specifying @name@ returns an @InvalidInput@ error.
--
-- /Note:/ Consider using 'startRecordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsStartRecordType :: Lens.Lens' ListResourceRecordSets (Core.Maybe Types.RecordType)
lrrsStartRecordType = Lens.field @"startRecordType"
{-# INLINEABLE lrrsStartRecordType #-}
{-# DEPRECATED startRecordType "Use generic-lens or generic-optics with 'startRecordType' instead"  #-}

instance Core.ToQuery ListResourceRecordSets where
        toQuery ListResourceRecordSets{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxitems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "identifier")
                startRecordIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "name") startRecordName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "type") startRecordType

instance Core.ToHeaders ListResourceRecordSets where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListResourceRecordSets where
        type Rs ListResourceRecordSets = ListResourceRecordSetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/hostedzone/" Core.<> Core.toText hostedZoneId Core.<>
                             "/rrset",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListResourceRecordSetsResponse' Core.<$>
                   (x Core..@ "ResourceRecordSets" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "ResourceRecordSet")
                     Core.<*> x Core..@ "IsTruncated"
                     Core.<*> x Core..@ "MaxItems"
                     Core.<*> x Core..@? "NextRecordIdentifier"
                     Core.<*> x Core..@? "NextRecordName"
                     Core.<*> x Core..@? "NextRecordType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListResourceRecordSets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"isTruncated") = Core.Nothing
          | Core.isNothing (rs Lens.^. Lens.field @"nextRecordName") Core.&&
              Core.isNothing (rs Lens.^. Lens.field @"nextRecordType")
              Core.&&
              Core.isNothing (rs Lens.^. Lens.field @"nextRecordIdentifier")
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"startRecordName" Lens..~
                   rs Lens.^. Lens.field @"nextRecordName"
                 Core.&
                 Lens.field @"startRecordType" Lens..~
                   rs Lens.^. Lens.field @"nextRecordType"
                 Core.&
                 Lens.field @"startRecordIdentifier" Lens..~
                   rs Lens.^. Lens.field @"nextRecordIdentifier")

-- | A complex type that contains list information for the resource record set.
--
-- /See:/ 'mkListResourceRecordSetsResponse' smart constructor.
data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse'
  { resourceRecordSets :: [Types.ResourceRecordSet]
    -- ^ Information about multiple resource record sets.
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether more resource record sets remain to be listed. If your results were truncated, you can make a follow-up pagination request by using the @NextRecordName@ element.
  , maxItems :: Types.MaxItems
    -- ^ The maximum number of records you requested.
  , nextRecordIdentifier :: Core.Maybe Types.ResourceRecordSetIdentifier
    -- ^ /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, the value of @SetIdentifier@ for the next resource record set that has the current DNS name and type.
--
-- For information about routing policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy> in the /Amazon Route 53 Developer Guide/ .
  , nextRecordName :: Core.Maybe Types.NextRecordName
    -- ^ If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true. 
  , nextRecordType :: Core.Maybe Types.RecordType
    -- ^ If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListResourceRecordSetsResponse' value with any optional fields omitted.
mkListResourceRecordSetsResponse
    :: Core.Bool -- ^ 'isTruncated'
    -> Types.MaxItems -- ^ 'maxItems'
    -> Core.Int -- ^ 'responseStatus'
    -> ListResourceRecordSetsResponse
mkListResourceRecordSetsResponse isTruncated maxItems
  responseStatus
  = ListResourceRecordSetsResponse'{resourceRecordSets = Core.mempty,
                                    isTruncated, maxItems, nextRecordIdentifier = Core.Nothing,
                                    nextRecordName = Core.Nothing, nextRecordType = Core.Nothing,
                                    responseStatus}

-- | Information about multiple resource record sets.
--
-- /Note:/ Consider using 'resourceRecordSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsResourceRecordSets :: Lens.Lens' ListResourceRecordSetsResponse [Types.ResourceRecordSet]
lrrsrrsResourceRecordSets = Lens.field @"resourceRecordSets"
{-# INLINEABLE lrrsrrsResourceRecordSets #-}
{-# DEPRECATED resourceRecordSets "Use generic-lens or generic-optics with 'resourceRecordSets' instead"  #-}

-- | A flag that indicates whether more resource record sets remain to be listed. If your results were truncated, you can make a follow-up pagination request by using the @NextRecordName@ element.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsIsTruncated :: Lens.Lens' ListResourceRecordSetsResponse Core.Bool
lrrsrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE lrrsrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The maximum number of records you requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsMaxItems :: Lens.Lens' ListResourceRecordSetsResponse Types.MaxItems
lrrsrrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lrrsrrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, the value of @SetIdentifier@ for the next resource record set that has the current DNS name and type.
--
-- For information about routing policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy> in the /Amazon Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'nextRecordIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsNextRecordIdentifier :: Lens.Lens' ListResourceRecordSetsResponse (Core.Maybe Types.ResourceRecordSetIdentifier)
lrrsrrsNextRecordIdentifier = Lens.field @"nextRecordIdentifier"
{-# INLINEABLE lrrsrrsNextRecordIdentifier #-}
{-# DEPRECATED nextRecordIdentifier "Use generic-lens or generic-optics with 'nextRecordIdentifier' instead"  #-}

-- | If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true. 
--
-- /Note:/ Consider using 'nextRecordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsNextRecordName :: Lens.Lens' ListResourceRecordSetsResponse (Core.Maybe Types.NextRecordName)
lrrsrrsNextRecordName = Lens.field @"nextRecordName"
{-# INLINEABLE lrrsrrsNextRecordName #-}
{-# DEPRECATED nextRecordName "Use generic-lens or generic-optics with 'nextRecordName' instead"  #-}

-- | If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true. 
--
-- /Note:/ Consider using 'nextRecordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsNextRecordType :: Lens.Lens' ListResourceRecordSetsResponse (Core.Maybe Types.RecordType)
lrrsrrsNextRecordType = Lens.field @"nextRecordType"
{-# INLINEABLE lrrsrrsNextRecordType #-}
{-# DEPRECATED nextRecordType "Use generic-lens or generic-optics with 'nextRecordType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrrsResponseStatus :: Lens.Lens' ListResourceRecordSetsResponse Core.Int
lrrsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
