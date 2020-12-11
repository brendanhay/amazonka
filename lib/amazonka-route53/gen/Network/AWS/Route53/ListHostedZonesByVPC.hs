{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lhzbvNextToken,
    lhzbvMaxItems,
    lhzbvVPCId,
    lhzbvVPCRegion,

    -- * Destructuring the response
    ListHostedZonesByVPCResponse (..),
    mkListHostedZonesByVPCResponse,

    -- ** Response lenses
    lhzbvrsNextToken,
    lhzbvrsResponseStatus,
    lhzbvrsHostedZoneSummaries,
    lhzbvrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | Lists all the private hosted zones that a specified VPC is associated with, regardless of which AWS account created the hosted zones.
--
-- /See:/ 'mkListHostedZonesByVPC' smart constructor.
data ListHostedZonesByVPC = ListHostedZonesByVPC'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text,
    vpcId :: Lude.Text,
    vpcRegion :: VPCRegion
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListHostedZonesByVPC' with the minimum fields required to make a request.
--
-- * 'maxItems' - (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
-- * 'nextToken' - If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.
--
-- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
-- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
-- * 'vpcId' - The ID of the Amazon VPC that you want to list hosted zones for.
-- * 'vpcRegion' - For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
mkListHostedZonesByVPC ::
  -- | 'vpcId'
  Lude.Text ->
  -- | 'vpcRegion'
  VPCRegion ->
  ListHostedZonesByVPC
mkListHostedZonesByVPC pVPCId_ pVPCRegion_ =
  ListHostedZonesByVPC'
    { nextToken = Lude.Nothing,
      maxItems = Lude.Nothing,
      vpcId = pVPCId_,
      vpcRegion = pVPCRegion_
    }

-- | If the previous response included a @NextToken@ element, the specified VPC is associated with more hosted zones. To get more hosted zones, submit another @ListHostedZonesByVPC@ request.
--
-- For the value of @NextToken@ , specify the value of @NextToken@ from the previous response.
-- If the previous response didn't include a @NextToken@ element, there are no more hosted zones to get.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvNextToken :: Lens.Lens' ListHostedZonesByVPC (Lude.Maybe Lude.Text)
lhzbvNextToken = Lens.lens (nextToken :: ListHostedZonesByVPC -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHostedZonesByVPC)
{-# DEPRECATED lhzbvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | (Optional) The maximum number of hosted zones that you want Amazon Route 53 to return. If the specified VPC is associated with more than @MaxItems@ hosted zones, the response includes a @NextToken@ element. @NextToken@ contains an encrypted token that identifies the first hosted zone that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvMaxItems :: Lens.Lens' ListHostedZonesByVPC (Lude.Maybe Lude.Text)
lhzbvMaxItems = Lens.lens (maxItems :: ListHostedZonesByVPC -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZonesByVPC)
{-# DEPRECATED lhzbvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The ID of the Amazon VPC that you want to list hosted zones for.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvVPCId :: Lens.Lens' ListHostedZonesByVPC Lude.Text
lhzbvVPCId = Lens.lens (vpcId :: ListHostedZonesByVPC -> Lude.Text) (\s a -> s {vpcId = a} :: ListHostedZonesByVPC)
{-# DEPRECATED lhzbvVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | For the Amazon VPC that you specified for @VPCId@ , the AWS Region that you created the VPC in.
--
-- /Note:/ Consider using 'vpcRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvVPCRegion :: Lens.Lens' ListHostedZonesByVPC VPCRegion
lhzbvVPCRegion = Lens.lens (vpcRegion :: ListHostedZonesByVPC -> VPCRegion) (\s a -> s {vpcRegion = a} :: ListHostedZonesByVPC)
{-# DEPRECATED lhzbvVPCRegion "Use generic-lens or generic-optics with 'vpcRegion' instead." #-}

instance Lude.AWSRequest ListHostedZonesByVPC where
  type Rs ListHostedZonesByVPC = ListHostedZonesByVPCResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListHostedZonesByVPCResponse'
            Lude.<$> (x Lude..@? "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "HostedZoneSummaries" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "HostedZoneSummary"
                     )
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListHostedZonesByVPC where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListHostedZonesByVPC where
  toPath = Lude.const "/2013-04-01/hostedzonesbyvpc"

instance Lude.ToQuery ListHostedZonesByVPC where
  toQuery ListHostedZonesByVPC' {..} =
    Lude.mconcat
      [ "nexttoken" Lude.=: nextToken,
        "maxitems" Lude.=: maxItems,
        "vpcid" Lude.=: vpcId,
        "vpcregion" Lude.=: vpcRegion
      ]

-- | /See:/ 'mkListHostedZonesByVPCResponse' smart constructor.
data ListHostedZonesByVPCResponse = ListHostedZonesByVPCResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    hostedZoneSummaries ::
      [HostedZoneSummary],
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

-- | Creates a value of 'ListHostedZonesByVPCResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneSummaries' - A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
-- * 'maxItems' - The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
-- * 'nextToken' - The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
-- * 'responseStatus' - The response status code.
mkListHostedZonesByVPCResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'maxItems'
  Lude.Text ->
  ListHostedZonesByVPCResponse
mkListHostedZonesByVPCResponse pResponseStatus_ pMaxItems_ =
  ListHostedZonesByVPCResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      hostedZoneSummaries = Lude.mempty,
      maxItems = pMaxItems_
    }

-- | The value that you specified for @NextToken@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvrsNextToken :: Lens.Lens' ListHostedZonesByVPCResponse (Lude.Maybe Lude.Text)
lhzbvrsNextToken = Lens.lens (nextToken :: ListHostedZonesByVPCResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListHostedZonesByVPCResponse)
{-# DEPRECATED lhzbvrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvrsResponseStatus :: Lens.Lens' ListHostedZonesByVPCResponse Lude.Int
lhzbvrsResponseStatus = Lens.lens (responseStatus :: ListHostedZonesByVPCResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListHostedZonesByVPCResponse)
{-# DEPRECATED lhzbvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list that contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvrsHostedZoneSummaries :: Lens.Lens' ListHostedZonesByVPCResponse [HostedZoneSummary]
lhzbvrsHostedZoneSummaries = Lens.lens (hostedZoneSummaries :: ListHostedZonesByVPCResponse -> [HostedZoneSummary]) (\s a -> s {hostedZoneSummaries = a} :: ListHostedZonesByVPCResponse)
{-# DEPRECATED lhzbvrsHostedZoneSummaries "Use generic-lens or generic-optics with 'hostedZoneSummaries' instead." #-}

-- | The value that you specified for @MaxItems@ in the most recent @ListHostedZonesByVPC@ request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhzbvrsMaxItems :: Lens.Lens' ListHostedZonesByVPCResponse Lude.Text
lhzbvrsMaxItems = Lens.lens (maxItems :: ListHostedZonesByVPCResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListHostedZonesByVPCResponse)
{-# DEPRECATED lhzbvrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
