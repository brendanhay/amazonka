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
    lhzbnHostedZoneId,
    lhzbnMaxItems,
    lhzbnDNSName,

    -- * Destructuring the response
    ListHostedZonesByNameResponse (..),
    mkListHostedZonesByNameResponse,

    -- ** Response lenses
    lhzbnrsHostedZoneId,
    lhzbnrsNextHostedZoneId,
    lhzbnrsHostedZones,
    lhzbnrsMaxItems,
    lhzbnrsIsTruncated,
    lhzbnrsDNSName,
    lhzbnrsNextDNSName,
    lhzbnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name.
--
-- /See:/ 'mkListHostedZonesByName' smart constructor.
data ListHostedZonesByName = ListHostedZonesByName'
  { -- | (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter.
    --
    -- If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
    hostedZoneId :: Lude.Maybe ResourceId,
    -- | The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
    maxItems :: Lude.Maybe Lude.Text,
    -- | (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
    dnsName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHostedZonesByName' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter.
--
-- If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
-- * 'maxItems' - The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
-- * 'dnsName' - (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
mkListHostedZonesByName ::
  ListHostedZonesByName
mkListHostedZonesByName =
  ListHostedZonesByName'
    { hostedZoneId = Lude.Nothing,
      maxItems = Lude.Nothing,
      dnsName = Lude.Nothing
    }

-- | (Optional) For your first request to @ListHostedZonesByName@ , do not include the @hostedzoneid@ parameter.
--
-- If you have more hosted zones than the value of @maxitems@ , @ListHostedZonesByName@ returns only the first @maxitems@ hosted zones. To get the next group of @maxitems@ hosted zones, submit another request to @ListHostedZonesByName@ and include both @dnsname@ and @hostedzoneid@ parameters. For the value of @hostedzoneid@ , specify the value of the @NextHostedZoneId@ element from the previous response.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnHostedZoneId :: Lens.Lens' ListHostedZonesByName (Lude.Maybe ResourceId)
lhzbnHostedZoneId = Lens.lens (hostedZoneId :: ListHostedZonesByName -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneId = a} :: ListHostedZonesByName)
{-# DEPRECATED lhzbnHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The maximum number of hosted zones to be included in the response body for this request. If you have more than @maxitems@ hosted zones, then the value of the @IsTruncated@ element in the response is true, and the values of @NextDNSName@ and @NextHostedZoneId@ specify the first hosted zone in the next group of @maxitems@ hosted zones.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnMaxItems :: Lens.Lens' ListHostedZonesByName (Lude.Maybe Lude.Text)
lhzbnMaxItems = Lens.lens (maxItems :: ListHostedZonesByName -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZonesByName)
{-# DEPRECATED lhzbnMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | (Optional) For your first request to @ListHostedZonesByName@ , include the @dnsname@ parameter only if you want to specify the name of the first hosted zone in the response. If you don't include the @dnsname@ parameter, Amazon Route 53 returns all of the hosted zones that were created by the current AWS account, in ASCII order. For subsequent requests, include both @dnsname@ and @hostedzoneid@ parameters. For @dnsname@ , specify the value of @NextDNSName@ from the previous response.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnDNSName :: Lens.Lens' ListHostedZonesByName (Lude.Maybe Lude.Text)
lhzbnDNSName = Lens.lens (dnsName :: ListHostedZonesByName -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: ListHostedZonesByName)
{-# DEPRECATED lhzbnDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

instance Lude.AWSRequest ListHostedZonesByName where
  type Rs ListHostedZonesByName = ListHostedZonesByNameResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListHostedZonesByNameResponse'
            Lude.<$> (x Lude..@? "HostedZoneId")
            Lude.<*> (x Lude..@? "NextHostedZoneId")
            Lude.<*> ( x Lude..@? "HostedZones" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HostedZone"
                     )
            Lude.<*> (x Lude..@ "MaxItems")
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@? "DNSName")
            Lude.<*> (x Lude..@? "NextDNSName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListHostedZonesByName where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListHostedZonesByName where
  toPath = Lude.const "/2013-04-01/hostedzonesbyname"

instance Lude.ToQuery ListHostedZonesByName where
  toQuery ListHostedZonesByName' {..} =
    Lude.mconcat
      [ "hostedzoneid" Lude.=: hostedZoneId,
        "maxitems" Lude.=: maxItems,
        "dnsname" Lude.=: dnsName
      ]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListHostedZonesByNameResponse' smart constructor.
data ListHostedZonesByNameResponse = ListHostedZonesByNameResponse'
  { -- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
    hostedZoneId :: Lude.Maybe ResourceId,
    -- | If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextHostedZoneId :: Lude.Maybe ResourceId,
    -- | A complex type that contains general information about the hosted zone.
    hostedZones :: [HostedZone],
    -- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
    maxItems :: Lude.Text,
    -- | A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
    isTruncated :: Lude.Bool,
    -- | For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
    dnsName :: Lude.Maybe Lude.Text,
    -- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    nextDNSName :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHostedZonesByNameResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID that Amazon Route 53 assigned to the hosted zone when you created it.
-- * 'nextHostedZoneId' - If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
-- * 'hostedZones' - A complex type that contains general information about the hosted zone.
-- * 'maxItems' - The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
-- * 'isTruncated' - A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
-- * 'dnsName' - For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
-- * 'nextDNSName' - If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
-- * 'responseStatus' - The response status code.
mkListHostedZonesByNameResponse ::
  -- | 'maxItems'
  Lude.Text ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ListHostedZonesByNameResponse
mkListHostedZonesByNameResponse
  pMaxItems_
  pIsTruncated_
  pResponseStatus_ =
    ListHostedZonesByNameResponse'
      { hostedZoneId = Lude.Nothing,
        nextHostedZoneId = Lude.Nothing,
        hostedZones = Lude.mempty,
        maxItems = pMaxItems_,
        isTruncated = pIsTruncated_,
        dnsName = Lude.Nothing,
        nextDNSName = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsHostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Lude.Maybe ResourceId)
lhzbnrsHostedZoneId = Lens.lens (hostedZoneId :: ListHostedZonesByNameResponse -> Lude.Maybe ResourceId) (\s a -> s {hostedZoneId = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @NextHostedZoneId@ identifies the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsNextHostedZoneId :: Lens.Lens' ListHostedZonesByNameResponse (Lude.Maybe ResourceId)
lhzbnrsNextHostedZoneId = Lens.lens (nextHostedZoneId :: ListHostedZonesByNameResponse -> Lude.Maybe ResourceId) (\s a -> s {nextHostedZoneId = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsNextHostedZoneId "Use generic-lens or generic-optics with 'nextHostedZoneId' instead." #-}

-- | A complex type that contains general information about the hosted zone.
--
-- /Note:/ Consider using 'hostedZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsHostedZones :: Lens.Lens' ListHostedZonesByNameResponse [HostedZone]
lhzbnrsHostedZones = Lens.lens (hostedZones :: ListHostedZonesByNameResponse -> [HostedZone]) (\s a -> s {hostedZones = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsHostedZones "Use generic-lens or generic-optics with 'hostedZones' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the call to @ListHostedZonesByName@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsMaxItems :: Lens.Lens' ListHostedZonesByNameResponse Lude.Text
lhzbnrsMaxItems = Lens.lens (maxItems :: ListHostedZonesByNameResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | A flag that indicates whether there are more hosted zones to be listed. If the response was truncated, you can get the next group of @maxitems@ hosted zones by calling @ListHostedZonesByName@ again and specifying the values of @NextDNSName@ and @NextHostedZoneId@ elements in the @dnsname@ and @hostedzoneid@ parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsIsTruncated :: Lens.Lens' ListHostedZonesByNameResponse Lude.Bool
lhzbnrsIsTruncated = Lens.lens (isTruncated :: ListHostedZonesByNameResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | For the second and subsequent calls to @ListHostedZonesByName@ , @DNSName@ is the value that you specified for the @dnsname@ parameter in the request that produced the current response.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsDNSName :: Lens.Lens' ListHostedZonesByNameResponse (Lude.Maybe Lude.Text)
lhzbnrsDNSName = Lens.lens (dnsName :: ListHostedZonesByNameResponse -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

-- | If @IsTruncated@ is true, the value of @NextDNSName@ is the name of the first hosted zone in the next group of @maxitems@ hosted zones. Call @ListHostedZonesByName@ again and specify the value of @NextDNSName@ and @NextHostedZoneId@ in the @dnsname@ and @hostedzoneid@ parameters, respectively.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'nextDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsNextDNSName :: Lens.Lens' ListHostedZonesByNameResponse (Lude.Maybe Lude.Text)
lhzbnrsNextDNSName = Lens.lens (nextDNSName :: ListHostedZonesByNameResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextDNSName = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsNextDNSName "Use generic-lens or generic-optics with 'nextDNSName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbnrsResponseStatus :: Lens.Lens' ListHostedZonesByNameResponse Lude.Int
lhzbnrsResponseStatus = Lens.lens (responseStatus :: ListHostedZonesByNameResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHostedZonesByNameResponse)
{-# DEPRECATED lhzbnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
