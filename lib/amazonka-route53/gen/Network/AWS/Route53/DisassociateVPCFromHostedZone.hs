{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DisassociateVPCFromHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon Virtual Private Cloud (Amazon VPC) from an Amazon Route 53 private hosted zone. Note the following:
--
--
--     * You can't disassociate the last Amazon VPC from a private hosted zone.
--
--
--     * You can't convert a private hosted zone into a public hosted zone.
--
--
--     * You can submit a @DisassociateVPCFromHostedZone@ request using either the account that created the hosted zone or the account that created the Amazon VPC.
--
--
--     * Some services, such as AWS Cloud Map and Amazon Elastic File System (Amazon EFS) automatically create hosted zones and associate VPCs with the hosted zones. A service can create a hosted zone using your account or using its own account. You can disassociate a VPC from a hosted zone only if the service created the hosted zone using your account.
-- When you run <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListHostedZonesByVPC.html DisassociateVPCFromHostedZone> , if the hosted zone has a value for @OwningAccount@ , you can use @DisassociateVPCFromHostedZone@ . If the hosted zone has a value for @OwningService@ , you can't use @DisassociateVPCFromHostedZone@ .
module Network.AWS.Route53.DisassociateVPCFromHostedZone
  ( -- * Creating a request
    DisassociateVPCFromHostedZone (..),
    mkDisassociateVPCFromHostedZone,

    -- ** Request lenses
    dvfhzHostedZoneId,
    dvfhzVPC,
    dvfhzComment,

    -- * Destructuring the response
    DisassociateVPCFromHostedZoneResponse (..),
    mkDisassociateVPCFromHostedZoneResponse,

    -- ** Response lenses
    dvfhzrsChangeInfo,
    dvfhzrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the VPC that you want to disassociate from a specified private hosted zone.
--
-- /See:/ 'mkDisassociateVPCFromHostedZone' smart constructor.
data DisassociateVPCFromHostedZone = DisassociateVPCFromHostedZone'
  { -- | The ID of the private hosted zone that you want to disassociate a VPC from.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains information about the VPC that you're disassociating from the specified hosted zone.
    vpc :: VPC,
    -- | /Optional:/ A comment about the disassociation request.
    comment :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateVPCFromHostedZone' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the private hosted zone that you want to disassociate a VPC from.
-- * 'vpc' - A complex type that contains information about the VPC that you're disassociating from the specified hosted zone.
-- * 'comment' - /Optional:/ A comment about the disassociation request.
mkDisassociateVPCFromHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  DisassociateVPCFromHostedZone
mkDisassociateVPCFromHostedZone pHostedZoneId_ pVPC_ =
  DisassociateVPCFromHostedZone'
    { hostedZoneId = pHostedZoneId_,
      vpc = pVPC_,
      comment = Lude.Nothing
    }

-- | The ID of the private hosted zone that you want to disassociate a VPC from.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfhzHostedZoneId :: Lens.Lens' DisassociateVPCFromHostedZone ResourceId
dvfhzHostedZoneId = Lens.lens (hostedZoneId :: DisassociateVPCFromHostedZone -> ResourceId) (\s a -> s {hostedZoneId = a} :: DisassociateVPCFromHostedZone)
{-# DEPRECATED dvfhzHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains information about the VPC that you're disassociating from the specified hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfhzVPC :: Lens.Lens' DisassociateVPCFromHostedZone VPC
dvfhzVPC = Lens.lens (vpc :: DisassociateVPCFromHostedZone -> VPC) (\s a -> s {vpc = a} :: DisassociateVPCFromHostedZone)
{-# DEPRECATED dvfhzVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | /Optional:/ A comment about the disassociation request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfhzComment :: Lens.Lens' DisassociateVPCFromHostedZone (Lude.Maybe Lude.Text)
dvfhzComment = Lens.lens (comment :: DisassociateVPCFromHostedZone -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: DisassociateVPCFromHostedZone)
{-# DEPRECATED dvfhzComment "Use generic-lens or generic-optics with 'comment' instead." #-}

instance Lude.AWSRequest DisassociateVPCFromHostedZone where
  type
    Rs DisassociateVPCFromHostedZone =
      DisassociateVPCFromHostedZoneResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          DisassociateVPCFromHostedZoneResponse'
            Lude.<$> (x Lude..@ "ChangeInfo") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement DisassociateVPCFromHostedZone where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DisassociateVPCFromHostedZoneRequest"

instance Lude.ToHeaders DisassociateVPCFromHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateVPCFromHostedZone where
  toPath DisassociateVPCFromHostedZone' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzone/",
        Lude.toBS hostedZoneId,
        "/disassociatevpc"
      ]

instance Lude.ToQuery DisassociateVPCFromHostedZone where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML DisassociateVPCFromHostedZone where
  toXML DisassociateVPCFromHostedZone' {..} =
    Lude.mconcat ["VPC" Lude.@= vpc, "Comment" Lude.@= comment]

-- | A complex type that contains the response information for the disassociate request.
--
-- /See:/ 'mkDisassociateVPCFromHostedZoneResponse' smart constructor.
data DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse'
  { -- | A complex type that describes the changes made to the specified private hosted zone.
    changeInfo :: ChangeInfo,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateVPCFromHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'changeInfo' - A complex type that describes the changes made to the specified private hosted zone.
-- * 'responseStatus' - The response status code.
mkDisassociateVPCFromHostedZoneResponse ::
  -- | 'changeInfo'
  ChangeInfo ->
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateVPCFromHostedZoneResponse
mkDisassociateVPCFromHostedZoneResponse
  pChangeInfo_
  pResponseStatus_ =
    DisassociateVPCFromHostedZoneResponse'
      { changeInfo = pChangeInfo_,
        responseStatus = pResponseStatus_
      }

-- | A complex type that describes the changes made to the specified private hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfhzrsChangeInfo :: Lens.Lens' DisassociateVPCFromHostedZoneResponse ChangeInfo
dvfhzrsChangeInfo = Lens.lens (changeInfo :: DisassociateVPCFromHostedZoneResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: DisassociateVPCFromHostedZoneResponse)
{-# DEPRECATED dvfhzrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvfhzrsResponseStatus :: Lens.Lens' DisassociateVPCFromHostedZoneResponse Lude.Int
dvfhzrsResponseStatus = Lens.lens (responseStatus :: DisassociateVPCFromHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateVPCFromHostedZoneResponse)
{-# DEPRECATED dvfhzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
