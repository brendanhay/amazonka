{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.AssociateVPCWithHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Amazon VPC with a private hosted zone.
--
-- /Important:/ To perform the association, the VPC and the private hosted zone must already exist. You can't convert a public hosted zone into a private hosted zone.
module Network.AWS.Route53.AssociateVPCWithHostedZone
  ( -- * Creating a request
    AssociateVPCWithHostedZone (..),
    mkAssociateVPCWithHostedZone,

    -- ** Request lenses
    avwhzComment,
    avwhzHostedZoneId,
    avwhzVPC,

    -- * Destructuring the response
    AssociateVPCWithHostedZoneResponse (..),
    mkAssociateVPCWithHostedZoneResponse,

    -- ** Response lenses
    avwhzrsResponseStatus,
    avwhzrsChangeInfo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to associate a VPC with a private hosted zone.
--
-- /See:/ 'mkAssociateVPCWithHostedZone' smart constructor.
data AssociateVPCWithHostedZone = AssociateVPCWithHostedZone'
  { comment ::
      Lude.Maybe Lude.Text,
    hostedZoneId :: ResourceId,
    vpc :: VPC
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateVPCWithHostedZone' with the minimum fields required to make a request.
--
-- * 'comment' - /Optional:/ A comment about the association request.
-- * 'hostedZoneId' - The ID of the private hosted zone that you want to associate an Amazon VPC with.
--
-- Note that you can't associate a VPC with a hosted zone that doesn't have an existing VPC association.
-- * 'vpc' - A complex type that contains information about the VPC that you want to associate with a private hosted zone.
mkAssociateVPCWithHostedZone ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  AssociateVPCWithHostedZone
mkAssociateVPCWithHostedZone pHostedZoneId_ pVPC_ =
  AssociateVPCWithHostedZone'
    { comment = Lude.Nothing,
      hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | /Optional:/ A comment about the association request.
--
-- /Note:/ Consider using 'comment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avwhzComment :: Lens.Lens' AssociateVPCWithHostedZone (Lude.Maybe Lude.Text)
avwhzComment = Lens.lens (comment :: AssociateVPCWithHostedZone -> Lude.Maybe Lude.Text) (\s a -> s {comment = a} :: AssociateVPCWithHostedZone)
{-# DEPRECATED avwhzComment "Use generic-lens or generic-optics with 'comment' instead." #-}

-- | The ID of the private hosted zone that you want to associate an Amazon VPC with.
--
-- Note that you can't associate a VPC with a hosted zone that doesn't have an existing VPC association.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avwhzHostedZoneId :: Lens.Lens' AssociateVPCWithHostedZone ResourceId
avwhzHostedZoneId = Lens.lens (hostedZoneId :: AssociateVPCWithHostedZone -> ResourceId) (\s a -> s {hostedZoneId = a} :: AssociateVPCWithHostedZone)
{-# DEPRECATED avwhzHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains information about the VPC that you want to associate with a private hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avwhzVPC :: Lens.Lens' AssociateVPCWithHostedZone VPC
avwhzVPC = Lens.lens (vpc :: AssociateVPCWithHostedZone -> VPC) (\s a -> s {vpc = a} :: AssociateVPCWithHostedZone)
{-# DEPRECATED avwhzVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Lude.AWSRequest AssociateVPCWithHostedZone where
  type
    Rs AssociateVPCWithHostedZone =
      AssociateVPCWithHostedZoneResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          AssociateVPCWithHostedZoneResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "ChangeInfo")
      )

instance Lude.ToElement AssociateVPCWithHostedZone where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}AssociateVPCWithHostedZoneRequest"

instance Lude.ToHeaders AssociateVPCWithHostedZone where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateVPCWithHostedZone where
  toPath AssociateVPCWithHostedZone' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzone/",
        Lude.toBS hostedZoneId,
        "/associatevpc"
      ]

instance Lude.ToQuery AssociateVPCWithHostedZone where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML AssociateVPCWithHostedZone where
  toXML AssociateVPCWithHostedZone' {..} =
    Lude.mconcat ["Comment" Lude.@= comment, "VPC" Lude.@= vpc]

-- | A complex type that contains the response information for the @AssociateVPCWithHostedZone@ request.
--
-- /See:/ 'mkAssociateVPCWithHostedZoneResponse' smart constructor.
data AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse'
  { responseStatus ::
      Lude.Int,
    changeInfo ::
      ChangeInfo
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateVPCWithHostedZoneResponse' with the minimum fields required to make a request.
--
-- * 'changeInfo' - A complex type that describes the changes made to your hosted zone.
-- * 'responseStatus' - The response status code.
mkAssociateVPCWithHostedZoneResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  AssociateVPCWithHostedZoneResponse
mkAssociateVPCWithHostedZoneResponse pResponseStatus_ pChangeInfo_ =
  AssociateVPCWithHostedZoneResponse'
    { responseStatus =
        pResponseStatus_,
      changeInfo = pChangeInfo_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avwhzrsResponseStatus :: Lens.Lens' AssociateVPCWithHostedZoneResponse Lude.Int
avwhzrsResponseStatus = Lens.lens (responseStatus :: AssociateVPCWithHostedZoneResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssociateVPCWithHostedZoneResponse)
{-# DEPRECATED avwhzrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that describes the changes made to your hosted zone.
--
-- /Note:/ Consider using 'changeInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avwhzrsChangeInfo :: Lens.Lens' AssociateVPCWithHostedZoneResponse ChangeInfo
avwhzrsChangeInfo = Lens.lens (changeInfo :: AssociateVPCWithHostedZoneResponse -> ChangeInfo) (\s a -> s {changeInfo = a} :: AssociateVPCWithHostedZoneResponse)
{-# DEPRECATED avwhzrsChangeInfo "Use generic-lens or generic-optics with 'changeInfo' instead." #-}
