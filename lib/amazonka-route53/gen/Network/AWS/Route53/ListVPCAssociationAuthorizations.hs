{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListVPCAssociationAuthorizations (..),
    mkListVPCAssociationAuthorizations,

    -- ** Request lenses
    lvaaHostedZoneId,
    lvaaNextToken,
    lvaaMaxResults,

    -- * Destructuring the response
    ListVPCAssociationAuthorizationsResponse (..),
    mkListVPCAssociationAuthorizationsResponse,

    -- ** Response lenses
    lvaarsHostedZoneId,
    lvaarsVPCs,
    lvaarsNextToken,
    lvaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about that can be associated with your hosted zone.
--
-- /See:/ 'mkListVPCAssociationAuthorizations' smart constructor.
data ListVPCAssociationAuthorizations = ListVPCAssociationAuthorizations'
  { -- | The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
    hostedZoneId :: ResourceId,
    -- | /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
    nextToken :: Lude.Maybe Lude.Text,
    -- | /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Route 53 returns up to 50 VPCs per page.
    maxResults :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVPCAssociationAuthorizations' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
-- * 'nextToken' - /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
-- * 'maxResults' - /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Route 53 returns up to 50 VPCs per page.
mkListVPCAssociationAuthorizations ::
  -- | 'hostedZoneId'
  ResourceId ->
  ListVPCAssociationAuthorizations
mkListVPCAssociationAuthorizations pHostedZoneId_ =
  ListVPCAssociationAuthorizations'
    { hostedZoneId = pHostedZoneId_,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ID of the hosted zone for which you want a list of VPCs that can be associated with the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaaHostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizations ResourceId
lvaaHostedZoneId = Lens.lens (hostedZoneId :: ListVPCAssociationAuthorizations -> ResourceId) (\s a -> s {hostedZoneId = a} :: ListVPCAssociationAuthorizations)
{-# DEPRECATED lvaaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | /Optional/ : If a response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of results, submit another request, and include the value of @NextToken@ from the response in the @nexttoken@ parameter in another @ListVPCAssociationAuthorizations@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaaNextToken :: Lens.Lens' ListVPCAssociationAuthorizations (Lude.Maybe Lude.Text)
lvaaNextToken = Lens.lens (nextToken :: ListVPCAssociationAuthorizations -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVPCAssociationAuthorizations)
{-# DEPRECATED lvaaNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | /Optional/ : An integer that specifies the maximum number of VPCs that you want Amazon Route 53 to return. If you don't specify a value for @MaxResults@ , Route 53 returns up to 50 VPCs per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaaMaxResults :: Lens.Lens' ListVPCAssociationAuthorizations (Lude.Maybe Lude.Text)
lvaaMaxResults = Lens.lens (maxResults :: ListVPCAssociationAuthorizations -> Lude.Maybe Lude.Text) (\s a -> s {maxResults = a} :: ListVPCAssociationAuthorizations)
{-# DEPRECATED lvaaMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListVPCAssociationAuthorizations where
  page rq rs
    | Page.stop (rs Lens.^. lvaarsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lvaarsVPCs) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lvaaNextToken Lens..~ rs Lens.^. lvaarsNextToken

instance Lude.AWSRequest ListVPCAssociationAuthorizations where
  type
    Rs ListVPCAssociationAuthorizations =
      ListVPCAssociationAuthorizationsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListVPCAssociationAuthorizationsResponse'
            Lude.<$> (x Lude..@ "HostedZoneId")
            Lude.<*> ( x Lude..@? "VPCs" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLNonEmpty "VPC"
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVPCAssociationAuthorizations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListVPCAssociationAuthorizations where
  toPath ListVPCAssociationAuthorizations' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzone/",
        Lude.toBS hostedZoneId,
        "/authorizevpcassociation"
      ]

instance Lude.ToQuery ListVPCAssociationAuthorizations where
  toQuery ListVPCAssociationAuthorizations' {..} =
    Lude.mconcat
      ["nexttoken" Lude.=: nextToken, "maxresults" Lude.=: maxResults]

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListVPCAssociationAuthorizationsResponse' smart constructor.
data ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse'
  { -- | The ID of the hosted zone that you can associate the listed VPCs with.
    hostedZoneId :: ResourceId,
    -- | The list of VPCs that are authorized to be associated with the specified hosted zone.
    vpcs :: Lude.NonEmpty VPC,
    -- | When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVPCAssociationAuthorizationsResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that you can associate the listed VPCs with.
-- * 'vpcs' - The list of VPCs that are authorized to be associated with the specified hosted zone.
-- * 'nextToken' - When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
-- * 'responseStatus' - The response status code.
mkListVPCAssociationAuthorizationsResponse ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpcs'
  Lude.NonEmpty VPC ->
  -- | 'responseStatus'
  Lude.Int ->
  ListVPCAssociationAuthorizationsResponse
mkListVPCAssociationAuthorizationsResponse
  pHostedZoneId_
  pVPCs_
  pResponseStatus_ =
    ListVPCAssociationAuthorizationsResponse'
      { hostedZoneId =
          pHostedZoneId_,
        vpcs = pVPCs_,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | The ID of the hosted zone that you can associate the listed VPCs with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaarsHostedZoneId :: Lens.Lens' ListVPCAssociationAuthorizationsResponse ResourceId
lvaarsHostedZoneId = Lens.lens (hostedZoneId :: ListVPCAssociationAuthorizationsResponse -> ResourceId) (\s a -> s {hostedZoneId = a} :: ListVPCAssociationAuthorizationsResponse)
{-# DEPRECATED lvaarsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The list of VPCs that are authorized to be associated with the specified hosted zone.
--
-- /Note:/ Consider using 'vpcs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaarsVPCs :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Lude.NonEmpty VPC)
lvaarsVPCs = Lens.lens (vpcs :: ListVPCAssociationAuthorizationsResponse -> Lude.NonEmpty VPC) (\s a -> s {vpcs = a} :: ListVPCAssociationAuthorizationsResponse)
{-# DEPRECATED lvaarsVPCs "Use generic-lens or generic-optics with 'vpcs' instead." #-}

-- | When the response includes a @NextToken@ element, there are more VPCs that can be associated with the specified hosted zone. To get the next page of VPCs, submit another @ListVPCAssociationAuthorizations@ request, and include the value of the @NextToken@ element from the response in the @nexttoken@ request parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaarsNextToken :: Lens.Lens' ListVPCAssociationAuthorizationsResponse (Lude.Maybe Lude.Text)
lvaarsNextToken = Lens.lens (nextToken :: ListVPCAssociationAuthorizationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListVPCAssociationAuthorizationsResponse)
{-# DEPRECATED lvaarsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvaarsResponseStatus :: Lens.Lens' ListVPCAssociationAuthorizationsResponse Lude.Int
lvaarsResponseStatus = Lens.lens (responseStatus :: ListVPCAssociationAuthorizationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVPCAssociationAuthorizationsResponse)
{-# DEPRECATED lvaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
