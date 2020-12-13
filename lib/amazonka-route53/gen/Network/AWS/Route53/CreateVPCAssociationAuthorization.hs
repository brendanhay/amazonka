{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.CreateVPCAssociationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the AWS account that created a specified VPC to submit an @AssociateVPCWithHostedZone@ request to associate the VPC with a specified hosted zone that was created by a different account. To submit a @CreateVPCAssociationAuthorization@ request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an @AssociateVPCWithHostedZone@ request.
module Network.AWS.Route53.CreateVPCAssociationAuthorization
  ( -- * Creating a request
    CreateVPCAssociationAuthorization (..),
    mkCreateVPCAssociationAuthorization,

    -- ** Request lenses
    cvaaHostedZoneId,
    cvaaVPC,

    -- * Destructuring the response
    CreateVPCAssociationAuthorizationResponse (..),
    mkCreateVPCAssociationAuthorizationResponse,

    -- ** Response lenses
    cvaarsHostedZoneId,
    cvaarsVPC,
    cvaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.
--
-- /See:/ 'mkCreateVPCAssociationAuthorization' smart constructor.
data CreateVPCAssociationAuthorization = CreateVPCAssociationAuthorization'
  { -- | The ID of the private hosted zone that you want to authorize associating a VPC with.
    hostedZoneId :: ResourceId,
    -- | A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
    vpc :: VPC
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCAssociationAuthorization' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the private hosted zone that you want to authorize associating a VPC with.
-- * 'vpc' - A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
mkCreateVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  CreateVPCAssociationAuthorization
mkCreateVPCAssociationAuthorization pHostedZoneId_ pVPC_ =
  CreateVPCAssociationAuthorization'
    { hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | The ID of the private hosted zone that you want to authorize associating a VPC with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaaHostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorization ResourceId
cvaaHostedZoneId = Lens.lens (hostedZoneId :: CreateVPCAssociationAuthorization -> ResourceId) (\s a -> s {hostedZoneId = a} :: CreateVPCAssociationAuthorization)
{-# DEPRECATED cvaaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A complex type that contains the VPC ID and region for the VPC that you want to authorize associating with your hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaaVPC :: Lens.Lens' CreateVPCAssociationAuthorization VPC
cvaaVPC = Lens.lens (vpc :: CreateVPCAssociationAuthorization -> VPC) (\s a -> s {vpc = a} :: CreateVPCAssociationAuthorization)
{-# DEPRECATED cvaaVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Lude.AWSRequest CreateVPCAssociationAuthorization where
  type
    Rs CreateVPCAssociationAuthorization =
      CreateVPCAssociationAuthorizationResponse
  request = Req.postXML route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          CreateVPCAssociationAuthorizationResponse'
            Lude.<$> (x Lude..@ "HostedZoneId")
            Lude.<*> (x Lude..@ "VPC")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement CreateVPCAssociationAuthorization where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}CreateVPCAssociationAuthorizationRequest"

instance Lude.ToHeaders CreateVPCAssociationAuthorization where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVPCAssociationAuthorization where
  toPath CreateVPCAssociationAuthorization' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzone/",
        Lude.toBS hostedZoneId,
        "/authorizevpcassociation"
      ]

instance Lude.ToQuery CreateVPCAssociationAuthorization where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML CreateVPCAssociationAuthorization where
  toXML CreateVPCAssociationAuthorization' {..} =
    Lude.mconcat ["VPC" Lude.@= vpc]

-- | A complex type that contains the response information from a @CreateVPCAssociationAuthorization@ request.
--
-- /See:/ 'mkCreateVPCAssociationAuthorizationResponse' smart constructor.
data CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse'
  { -- | The ID of the hosted zone that you authorized associating a VPC with.
    hostedZoneId :: ResourceId,
    -- | The VPC that you authorized associating with a hosted zone.
    vpc :: VPC,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVPCAssociationAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID of the hosted zone that you authorized associating a VPC with.
-- * 'vpc' - The VPC that you authorized associating with a hosted zone.
-- * 'responseStatus' - The response status code.
mkCreateVPCAssociationAuthorizationResponse ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateVPCAssociationAuthorizationResponse
mkCreateVPCAssociationAuthorizationResponse
  pHostedZoneId_
  pVPC_
  pResponseStatus_ =
    CreateVPCAssociationAuthorizationResponse'
      { hostedZoneId =
          pHostedZoneId_,
        vpc = pVPC_,
        responseStatus = pResponseStatus_
      }

-- | The ID of the hosted zone that you authorized associating a VPC with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaarsHostedZoneId :: Lens.Lens' CreateVPCAssociationAuthorizationResponse ResourceId
cvaarsHostedZoneId = Lens.lens (hostedZoneId :: CreateVPCAssociationAuthorizationResponse -> ResourceId) (\s a -> s {hostedZoneId = a} :: CreateVPCAssociationAuthorizationResponse)
{-# DEPRECATED cvaarsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The VPC that you authorized associating with a hosted zone.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaarsVPC :: Lens.Lens' CreateVPCAssociationAuthorizationResponse VPC
cvaarsVPC = Lens.lens (vpc :: CreateVPCAssociationAuthorizationResponse -> VPC) (\s a -> s {vpc = a} :: CreateVPCAssociationAuthorizationResponse)
{-# DEPRECATED cvaarsVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvaarsResponseStatus :: Lens.Lens' CreateVPCAssociationAuthorizationResponse Lude.Int
cvaarsResponseStatus = Lens.lens (responseStatus :: CreateVPCAssociationAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateVPCAssociationAuthorizationResponse)
{-# DEPRECATED cvaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
