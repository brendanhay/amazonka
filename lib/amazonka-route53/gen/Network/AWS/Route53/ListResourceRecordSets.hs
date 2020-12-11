{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListResourceRecordSets (..),
    mkListResourceRecordSets,

    -- ** Request lenses
    lrrsStartRecordName,
    lrrsStartRecordType,
    lrrsStartRecordIdentifier,
    lrrsMaxItems,
    lrrsHostedZoneId,

    -- * Destructuring the response
    ListResourceRecordSetsResponse (..),
    mkListResourceRecordSetsResponse,

    -- ** Response lenses
    lrrsrsNextRecordType,
    lrrsrsNextRecordName,
    lrrsrsNextRecordIdentifier,
    lrrsrsResponseStatus,
    lrrsrsResourceRecordSets,
    lrrsrsIsTruncated,
    lrrsrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request for the resource record sets that are associated with a specified hosted zone.
--
-- /See:/ 'mkListResourceRecordSets' smart constructor.
data ListResourceRecordSets = ListResourceRecordSets'
  { startRecordName ::
      Lude.Maybe Lude.Text,
    startRecordType :: Lude.Maybe RecordType,
    startRecordIdentifier :: Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text,
    hostedZoneId :: ResourceId
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceRecordSets' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that contains the resource record sets that you want to list.
-- * 'maxItems' - (Optional) The maximum number of resource records sets to include in the response body for this request. If the response includes more than @maxitems@ resource record sets, the value of the @IsTruncated@ element in the response is @true@ , and the values of the @NextRecordName@ and @NextRecordType@ elements in the response identify the first resource record set in the next group of @maxitems@ resource record sets.
-- * 'startRecordIdentifier' - /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, specify the value of @NextRecordIdentifier@ from the previous response to get the next resource record set that has the current DNS name and type.
-- * 'startRecordName' - The first name in the lexicographic ordering of resource record sets that you want to list. If the specified record name doesn't exist, the results begin with the first resource record set that has a name greater than the value of @name@ .
-- * 'startRecordType' - The type of resource record set to begin the record listing from.
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
mkListResourceRecordSets ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListResourceRecordSets
mkListResourceRecordSets pHostedZoneId_ =
  ListResourceRecordSets'
    { startRecordName = Lude.Nothing,
      startRecordType = Lude.Nothing,
      startRecordIdentifier = Lude.Nothing,
      maxItems = Lude.Nothing,
      hostedZoneId = pHostedZoneId_
    }

-- | The first name in the lexicographic ordering of resource record sets that you want to list. If the specified record name doesn't exist, the results begin with the first resource record set that has a name greater than the value of @name@ .
--
-- /Note:/ Consider using 'startRecordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsStartRecordName :: Lens.Lens' ListResourceRecordSets (Lude.Maybe Lude.Text)
lrrsStartRecordName = Lens.lens (startRecordName :: ListResourceRecordSets -> Lude.Maybe Lude.Text) (\s a -> s {startRecordName = a} :: ListResourceRecordSets)
{-# DEPRECATED lrrsStartRecordName "Use generic-lens or generic-optics with 'startRecordName' instead." #-}

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
lrrsStartRecordType :: Lens.Lens' ListResourceRecordSets (Lude.Maybe RecordType)
lrrsStartRecordType = Lens.lens (startRecordType :: ListResourceRecordSets -> Lude.Maybe RecordType) (\s a -> s {startRecordType = a} :: ListResourceRecordSets)
{-# DEPRECATED lrrsStartRecordType "Use generic-lens or generic-optics with 'startRecordType' instead." #-}

-- | /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, specify the value of @NextRecordIdentifier@ from the previous response to get the next resource record set that has the current DNS name and type.
--
-- /Note:/ Consider using 'startRecordIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsStartRecordIdentifier :: Lens.Lens' ListResourceRecordSets (Lude.Maybe Lude.Text)
lrrsStartRecordIdentifier = Lens.lens (startRecordIdentifier :: ListResourceRecordSets -> Lude.Maybe Lude.Text) (\s a -> s {startRecordIdentifier = a} :: ListResourceRecordSets)
{-# DEPRECATED lrrsStartRecordIdentifier "Use generic-lens or generic-optics with 'startRecordIdentifier' instead." #-}

-- | (Optional) The maximum number of resource records sets to include in the response body for this request. If the response includes more than @maxitems@ resource record sets, the value of the @IsTruncated@ element in the response is @true@ , and the values of the @NextRecordName@ and @NextRecordType@ elements in the response identify the first resource record set in the next group of @maxitems@ resource record sets.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsMaxItems :: Lens.Lens' ListResourceRecordSets (Lude.Maybe Lude.Text)
lrrsMaxItems = Lens.lens (maxItems :: ListResourceRecordSets -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListResourceRecordSets)
{-# DEPRECATED lrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the hosted zone that contains the resource record sets that you want to list.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsHostedZoneId :: Lens.Lens' ListResourceRecordSets ResourceId
lrrsHostedZoneId = Lens.lens (hostedZoneId :: ListResourceRecordSets -> ResourceId) (\s a -> s {hostedZoneId = a} :: ListResourceRecordSets)
{-# DEPRECATED lrrsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

instance Page.AWSPager ListResourceRecordSets where
  page rq rs
    | Page.stop (rs Lens.^. lrrsrsIsTruncated) = Lude.Nothing
    | Lude.isNothing (rs Lens.^. lrrsrsNextRecordName)
        Lude.&& Lude.isNothing (rs Lens.^. lrrsrsNextRecordType)
        Lude.&& Lude.isNothing (rs Lens.^. lrrsrsNextRecordIdentifier) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrrsStartRecordName
          Lens..~ rs Lens.^. lrrsrsNextRecordName
          Lude.& lrrsStartRecordType
          Lens..~ rs Lens.^. lrrsrsNextRecordType
          Lude.& lrrsStartRecordIdentifier
          Lens..~ rs Lens.^. lrrsrsNextRecordIdentifier

instance Lude.AWSRequest ListResourceRecordSets where
  type Rs ListResourceRecordSets = ListResourceRecordSetsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListResourceRecordSetsResponse'
            Lude.<$> (x Lude..@? "NextRecordType")
            Lude.<*> (x Lude..@? "NextRecordName")
            Lude.<*> (x Lude..@? "NextRecordIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "ResourceRecordSets" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "ResourceRecordSet"
                     )
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListResourceRecordSets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListResourceRecordSets where
  toPath ListResourceRecordSets' {..} =
    Lude.mconcat
      ["/2013-04-01/hostedzone/", Lude.toBS hostedZoneId, "/rrset"]

instance Lude.ToQuery ListResourceRecordSets where
  toQuery ListResourceRecordSets' {..} =
    Lude.mconcat
      [ "name" Lude.=: startRecordName,
        "type" Lude.=: startRecordType,
        "identifier" Lude.=: startRecordIdentifier,
        "maxitems" Lude.=: maxItems
      ]

-- | A complex type that contains list information for the resource record set.
--
-- /See:/ 'mkListResourceRecordSetsResponse' smart constructor.
data ListResourceRecordSetsResponse = ListResourceRecordSetsResponse'
  { nextRecordType ::
      Lude.Maybe RecordType,
    nextRecordName ::
      Lude.Maybe Lude.Text,
    nextRecordIdentifier ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    resourceRecordSets ::
      [ResourceRecordSet],
    isTruncated :: Lude.Bool,
    maxItems :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListResourceRecordSetsResponse' with the minimum fields required to make a request.
--
-- * 'isTruncated' - A flag that indicates whether more resource record sets remain to be listed. If your results were truncated, you can make a follow-up pagination request by using the @NextRecordName@ element.
-- * 'maxItems' - The maximum number of records you requested.
-- * 'nextRecordIdentifier' - /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, the value of @SetIdentifier@ for the next resource record set that has the current DNS name and type.
--
-- For information about routing policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy> in the /Amazon Route 53 Developer Guide/ .
-- * 'nextRecordName' - If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
-- * 'nextRecordType' - If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
-- * 'resourceRecordSets' - Information about multiple resource record sets.
-- * 'responseStatus' - The response status code.
mkListResourceRecordSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListResourceRecordSetsResponse
mkListResourceRecordSetsResponse
  pResponseStatus_
  pIsTruncated_
  pMaxItems_ =
    ListResourceRecordSetsResponse'
      { nextRecordType = Lude.Nothing,
        nextRecordName = Lude.Nothing,
        nextRecordIdentifier = Lude.Nothing,
        responseStatus = pResponseStatus_,
        resourceRecordSets = Lude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If the results were truncated, the type of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
--
-- /Note:/ Consider using 'nextRecordType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsNextRecordType :: Lens.Lens' ListResourceRecordSetsResponse (Lude.Maybe RecordType)
lrrsrsNextRecordType = Lens.lens (nextRecordType :: ListResourceRecordSetsResponse -> Lude.Maybe RecordType) (\s a -> s {nextRecordType = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsNextRecordType "Use generic-lens or generic-optics with 'nextRecordType' instead." #-}

-- | If the results were truncated, the name of the next record in the list.
--
-- This element is present only if @IsTruncated@ is true.
--
-- /Note:/ Consider using 'nextRecordName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsNextRecordName :: Lens.Lens' ListResourceRecordSetsResponse (Lude.Maybe Lude.Text)
lrrsrsNextRecordName = Lens.lens (nextRecordName :: ListResourceRecordSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextRecordName = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsNextRecordName "Use generic-lens or generic-optics with 'nextRecordName' instead." #-}

-- | /Resource record sets that have a routing policy other than simple:/ If results were truncated for a given DNS name and type, the value of @SetIdentifier@ for the next resource record set that has the current DNS name and type.
--
-- For information about routing policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/routing-policy.html Choosing a Routing Policy> in the /Amazon Route 53 Developer Guide/ .
--
-- /Note:/ Consider using 'nextRecordIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsNextRecordIdentifier :: Lens.Lens' ListResourceRecordSetsResponse (Lude.Maybe Lude.Text)
lrrsrsNextRecordIdentifier = Lens.lens (nextRecordIdentifier :: ListResourceRecordSetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextRecordIdentifier = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsNextRecordIdentifier "Use generic-lens or generic-optics with 'nextRecordIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsResponseStatus :: Lens.Lens' ListResourceRecordSetsResponse Lude.Int
lrrsrsResponseStatus = Lens.lens (responseStatus :: ListResourceRecordSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Information about multiple resource record sets.
--
-- /Note:/ Consider using 'resourceRecordSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsResourceRecordSets :: Lens.Lens' ListResourceRecordSetsResponse [ResourceRecordSet]
lrrsrsResourceRecordSets = Lens.lens (resourceRecordSets :: ListResourceRecordSetsResponse -> [ResourceRecordSet]) (\s a -> s {resourceRecordSets = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsResourceRecordSets "Use generic-lens or generic-optics with 'resourceRecordSets' instead." #-}

-- | A flag that indicates whether more resource record sets remain to be listed. If your results were truncated, you can make a follow-up pagination request by using the @NextRecordName@ element.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsIsTruncated :: Lens.Lens' ListResourceRecordSetsResponse Lude.Bool
lrrsrsIsTruncated = Lens.lens (isTruncated :: ListResourceRecordSetsResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The maximum number of records you requested.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsrsMaxItems :: Lens.Lens' ListResourceRecordSetsResponse Lude.Text
lrrsrsMaxItems = Lens.lens (maxItems :: ListResourceRecordSetsResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListResourceRecordSetsResponse)
{-# DEPRECATED lrrsrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
