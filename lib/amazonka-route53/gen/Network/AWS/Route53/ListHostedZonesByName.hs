{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListHostedZonesByName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of your hosted zones in lexicographic order. The response includes a @HostedZones@ child element for each hosted zone created by the current AWS account.
--
-- @ListHostedZonesByName@ sorts hosted zones by name with the labels reversed. For example:
-- @com.example.www.@
-- Note the trailing dot, which can change the sort order in some circumstances.
-- If the domain name includes escape characters or Punycode, @ListHostedZonesByName@ alphabetizes the domain name using the escaped or Punycoded value, which is the format that Amazon Route 53 saves in its database. For example, to create a hosted zone for exämple.com, you specify ex\344mple.com for the domain name. @ListHostedZonesByName@ alphabetizes it as:
-- @com.ex\344mple.@
-- The labels are reversed and alphabetized using the escaped value. For more information about valid domain name formats, including internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guide/ .
-- Route 53 returns up to 100 items in each response. If you have a lot of hosted zones, use the @MaxItems@ parameter to list them in groups of up to 100. The response includes values that help navigate from one group of @MaxItems@ hosted zones to the next:
--
--     * The @DNSName@ and @HostedZoneId@ elements in the response contain the values, if any, specified for the @dnsname@ and @hostedzoneid@ parameters in the request that produced the current response.
--
--
--     * The @MaxItems@ element in the response contains the value, if any, that you specified for the @maxitems@ parameter in the request that produced the current response.
--
--
--     * If the value of @IsTruncated@ in the response is true, there are more hosted zones associated with the current AWS account.
-- If @IsTruncated@ is false, this response includes the last hosted zone that is associated with the current account. The @NextDNSName@ element and @NextHostedZoneId@ elements are omitted from the response.
--
--
--     * The @NextDNSName@ and @NextHostedZoneId@ elements in the response contain the domain name and the hosted zone ID of the next hosted zone that is associated with the current AWS account. If you want to list more hosted zones, make another call to @ListHostedZonesByName@ , and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
module Network.AWS.Route53.ListHostedZonesByName
  ( -- * Creating a request
    ListHostedZonesByName (..),
    mkListHostedZonesByName,

    -- ** Request lenses
    lhzbnDNSName,
    lhzbnHostedZoneId,
    lhzbnMaxItems,

    -- * Destructuring the response
    ListHostedZonesByNameResponse (..),
    mkListHostedZonesByNameResponse,

    -- ** Response lenses
    lhzbnrrsHostedZones,
    lhzbnrrsIsTruncated,
    lhzbnrrsMaxItems,
    lhzbnrrsDNSName,
    lhzbnrrsHostedZoneId,
    lhzbnrrsNextDNSName,
    lhzbnrrsNextHostedZoneId,
    lhzbnrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name.
--
-- /See:/ 'mkListHostedZonesByName' smart constructor.
data ListHostedZonesByName = ListHostedZonesByName'
  { -- | (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
    dNSName :: Core.Maybe Types.DNSName,
    -- | (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter.
    --
    -- If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
    hostedZoneId :: Core.Maybe Types.ResourceId,
    -- | The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
    maxItems :: Core.Maybe Types.MaxItems
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByName' value with any optional fields omitted.
mkListHostedZonesByName ::
  ListHostedZonesByName
mkListHostedZonesByName =
  ListHostedZonesByName'
    { dNSName = Core.Nothing,
      hostedZoneId = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
--
-- /Note:/ Consider using 'dNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnDNSName :: Lens.Lens' ListHostedZonesByName (Core.Maybe Types.DNSName)
lhzbnDNSName = Lens.field @"dNSName"
{-# DEPRECATED lhzbnDNSName "Use generic-lens or generic-optics with 'dNSName' instead." #-}

-- | (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter.
--
-- If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnHostedZoneId :: Lens.Lens' ListHostedZonesByName (Core.Maybe Types.ResourceId)
lhzbnHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED lhzbnHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnMaxItems :: Lens.Lens' ListHostedZonesByName (Core.Maybe Types.MaxItems)
lhzbnMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzbnMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListHostedZonesByName where
  type Rs ListHostedZonesByName = ListHostedZonesByNameResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/hostedzonesbyname",
        Core._rqQuery =
          Core.toQueryValue "dnsname" Core.<$> dNSName
            Core.<> (Core.toQueryValue "hostedzoneid" Core.<$> hostedZoneId)
            Core.<> (Core.toQueryValue "maxitems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListHostedZonesByNameResponse'
            Core.<$> ( x Core..@? "HostedZones" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "HostedZone"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "DNSName")
            Core.<*> (x Core..@? "HostedZoneId")
            Core.<*> (x Core..@? "NextDNSName")
            Core.<*> (x Core..@? "NextHostedZoneId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListHostedZonesByNameResponse' smart constructor.
data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse'
  { -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [Types.HostedZone],
    -- | A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
    maxItems :: Types.MaxItems,
    -- | For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
    dNSName :: Core.Maybe Types.DNSName,
    -- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
    hostedZoneId :: Core.Maybe Types.ResourceId,
    -- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextDNSName :: Core.Maybe Types.NextDNSName,
    -- | If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextHostedZoneId :: Core.Maybe Types.ResourceId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHostedZonesByNameResponse' value with any optional fields omitted.
mkListHostedZonesByNameResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListHostedZonesByNameResponse
mkListHostedZonesByNameResponse isTruncated maxItems responseStatus =
  ListHostedZonesByNameResponse'
    { hostedZones = Core.mempty,
      isTruncated,
      maxItems,
      dNSName = Core.Nothing,
      hostedZoneId = Core.Nothing,
      nextDNSName = Core.Nothing,
      nextHostedZoneId = Core.Nothing,
      responseStatus
    }

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsHostedZones :: Lens.Lens' ListHostedZonesByNameResponse [Types.HostedZone]
lhzbnrrsHostedZones = Lens.field @"hostedZones"
{-# DEPRECATED lhzbnrrsHostedZones "Use generic-lens or generic-optics with 'hostedZones' instead." #-}

-- | A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsIsTruncated :: Lens.Lens' ListHostedZonesByNameResponse Core.Bool
lhzbnrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lhzbnrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsMaxItems :: Lens.Lens' ListHostedZonesByNameResponse Types.MaxItems
lhzbnrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lhzbnrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'dNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsDNSName :: Lens.Lens' ListHostedZonesByNameResponse (Core.Maybe Types.DNSName)
lhzbnrrsDNSName = Lens.field @"dNSName"
{-# DEPRECATED lhzbnrrsDNSName "Use generic-lens or generic-optics with 'dNSName' instead." #-}

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsHostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Core.Maybe Types.ResourceId)
lhzbnrrsHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED lhzbnrrsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsNextDNSName :: Lens.Lens' ListHostedZonesByNameResponse (Core.Maybe Types.NextDNSName)
lhzbnrrsNextDNSName = Lens.field @"nextDNSName"
{-# DEPRECATED lhzbnrrsNextDNSName "Use generic-lens or generic-optics with 'nextDNSName' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsNextHostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Core.Maybe Types.ResourceId)
lhzbnrrsNextHostedZoneId = Lens.field @"nextHostedZoneId"
{-# DEPRECATED lhzbnrrsNextHostedZoneId "Use generic-lens or generic-optics with 'nextHostedZoneId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrrsResponseStatus :: Lens.Lens' ListHostedZonesByNameResponse Core.Int
lhzbnrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhzbnrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
