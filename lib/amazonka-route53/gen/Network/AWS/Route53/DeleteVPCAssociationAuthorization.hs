{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteVPCAssociationAuthorization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization to submit an @AssociateVPCWithHostedZone@ request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a @DeleteVPCAssociationAuthorization@ request.
--
-- /Important:/ Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, @DeleteVPCAssociationAuthorization@ won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use @DisassociateVPCFromHostedZone@ .
module Network.AWS.Route53.DeleteVPCAssociationAuthorization
  ( -- * Creating a request
    DeleteVPCAssociationAuthorization (..),
    mkDeleteVPCAssociationAuthorization,

    -- ** Request lenses
    dvaaHostedZoneId,
    dvaaVPC,

    -- * Destructuring the response
    DeleteVPCAssociationAuthorizationResponse (..),
    mkDeleteVPCAssociationAuthorizationResponse,

    -- ** Response lenses
    dvaarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account.
--
-- /See:/ 'mkDeleteVPCAssociationAuthorization' smart constructor.
data DeleteVPCAssociationAuthorization = DeleteVPCAssociationAuthorization'
  { hostedZoneId ::
      ResourceId,
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

-- | Creates a value of 'DeleteVPCAssociationAuthorization' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
-- * 'vpc' - When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
mkDeleteVPCAssociationAuthorization ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'vpc'
  VPC ->
  DeleteVPCAssociationAuthorization
mkDeleteVPCAssociationAuthorization pHostedZoneId_ pVPC_ =
  DeleteVPCAssociationAuthorization'
    { hostedZoneId = pHostedZoneId_,
      vpc = pVPC_
    }

-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, the ID of the hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaaHostedZoneId :: Lens.Lens' DeleteVPCAssociationAuthorization ResourceId
dvaaHostedZoneId = Lens.lens (hostedZoneId :: DeleteVPCAssociationAuthorization -> ResourceId) (\s a -> s {hostedZoneId = a} :: DeleteVPCAssociationAuthorization)
{-# DEPRECATED dvaaHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | When removing authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account, a complex type that includes the ID and region of the VPC.
--
-- /Note:/ Consider using 'vpc' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaaVPC :: Lens.Lens' DeleteVPCAssociationAuthorization VPC
dvaaVPC = Lens.lens (vpc :: DeleteVPCAssociationAuthorization -> VPC) (\s a -> s {vpc = a} :: DeleteVPCAssociationAuthorization)
{-# DEPRECATED dvaaVPC "Use generic-lens or generic-optics with 'vpc' instead." #-}

instance Lude.AWSRequest DeleteVPCAssociationAuthorization where
  type
    Rs DeleteVPCAssociationAuthorization =
      DeleteVPCAssociationAuthorizationResponse
  request = Req.postXML route53Service
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteVPCAssociationAuthorizationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToElement DeleteVPCAssociationAuthorization where
  toElement =
    Lude.mkElement
      "{https://route53.amazonaws.com/doc/2013-04-01/}DeleteVPCAssociationAuthorizationRequest"

instance Lude.ToHeaders DeleteVPCAssociationAuthorization where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPCAssociationAuthorization where
  toPath DeleteVPCAssociationAuthorization' {..} =
    Lude.mconcat
      [ "/2013-04-01/hostedzone/",
        Lude.toBS hostedZoneId,
        "/deauthorizevpcassociation"
      ]

instance Lude.ToQuery DeleteVPCAssociationAuthorization where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML DeleteVPCAssociationAuthorization where
  toXML DeleteVPCAssociationAuthorization' {..} =
    Lude.mconcat ["VPC" Lude.@= vpc]

-- | Empty response for the request.
--
-- /See:/ 'mkDeleteVPCAssociationAuthorizationResponse' smart constructor.
newtype DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCAssociationAuthorizationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteVPCAssociationAuthorizationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCAssociationAuthorizationResponse
mkDeleteVPCAssociationAuthorizationResponse pResponseStatus_ =
  DeleteVPCAssociationAuthorizationResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvaarsResponseStatus :: Lens.Lens' DeleteVPCAssociationAuthorizationResponse Lude.Int
dvaarsResponseStatus = Lens.lens (responseStatus :: DeleteVPCAssociationAuthorizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCAssociationAuthorizationResponse)
{-# DEPRECATED dvaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
