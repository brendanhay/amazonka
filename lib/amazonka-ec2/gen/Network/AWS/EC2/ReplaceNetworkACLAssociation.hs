{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReplaceNetworkACLAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes which network ACL a subnet is associated with. By default when you create a subnet, it's automatically associated with the default network ACL. For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs> in the /Amazon Virtual Private Cloud User Guide/ .
--
-- This is an idempotent operation.
module Network.AWS.EC2.ReplaceNetworkACLAssociation
  ( -- * Creating a request
    ReplaceNetworkACLAssociation (..),
    mkReplaceNetworkACLAssociation,

    -- ** Request lenses
    rnaaDryRun,
    rnaaAssociationId,
    rnaaNetworkACLId,

    -- * Destructuring the response
    ReplaceNetworkACLAssociationResponse (..),
    mkReplaceNetworkACLAssociationResponse,

    -- ** Response lenses
    rnaarsNewAssociationId,
    rnaarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkReplaceNetworkACLAssociation' smart constructor.
data ReplaceNetworkACLAssociation = ReplaceNetworkACLAssociation'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    associationId :: Lude.Text,
    networkACLId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceNetworkACLAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the current association between the original network ACL and the subnet.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkACLId' - The ID of the new network ACL to associate with the subnet.
mkReplaceNetworkACLAssociation ::
  -- | 'associationId'
  Lude.Text ->
  -- | 'networkACLId'
  Lude.Text ->
  ReplaceNetworkACLAssociation
mkReplaceNetworkACLAssociation pAssociationId_ pNetworkACLId_ =
  ReplaceNetworkACLAssociation'
    { dryRun = Lude.Nothing,
      associationId = pAssociationId_,
      networkACLId = pNetworkACLId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaDryRun :: Lens.Lens' ReplaceNetworkACLAssociation (Lude.Maybe Lude.Bool)
rnaaDryRun = Lens.lens (dryRun :: ReplaceNetworkACLAssociation -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ReplaceNetworkACLAssociation)
{-# DEPRECATED rnaaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the current association between the original network ACL and the subnet.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaAssociationId :: Lens.Lens' ReplaceNetworkACLAssociation Lude.Text
rnaaAssociationId = Lens.lens (associationId :: ReplaceNetworkACLAssociation -> Lude.Text) (\s a -> s {associationId = a} :: ReplaceNetworkACLAssociation)
{-# DEPRECATED rnaaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the new network ACL to associate with the subnet.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaaNetworkACLId :: Lens.Lens' ReplaceNetworkACLAssociation Lude.Text
rnaaNetworkACLId = Lens.lens (networkACLId :: ReplaceNetworkACLAssociation -> Lude.Text) (\s a -> s {networkACLId = a} :: ReplaceNetworkACLAssociation)
{-# DEPRECATED rnaaNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

instance Lude.AWSRequest ReplaceNetworkACLAssociation where
  type
    Rs ReplaceNetworkACLAssociation =
      ReplaceNetworkACLAssociationResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ReplaceNetworkACLAssociationResponse'
            Lude.<$> (x Lude..@? "newAssociationId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReplaceNetworkACLAssociation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ReplaceNetworkACLAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery ReplaceNetworkACLAssociation where
  toQuery ReplaceNetworkACLAssociation' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ReplaceNetworkAclAssociation" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "AssociationId" Lude.=: associationId,
        "NetworkAclId" Lude.=: networkACLId
      ]

-- | /See:/ 'mkReplaceNetworkACLAssociationResponse' smart constructor.
data ReplaceNetworkACLAssociationResponse = ReplaceNetworkACLAssociationResponse'
  { newAssociationId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceNetworkACLAssociationResponse' with the minimum fields required to make a request.
--
-- * 'newAssociationId' - The ID of the new association.
-- * 'responseStatus' - The response status code.
mkReplaceNetworkACLAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ReplaceNetworkACLAssociationResponse
mkReplaceNetworkACLAssociationResponse pResponseStatus_ =
  ReplaceNetworkACLAssociationResponse'
    { newAssociationId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the new association.
--
-- /Note:/ Consider using 'newAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaarsNewAssociationId :: Lens.Lens' ReplaceNetworkACLAssociationResponse (Lude.Maybe Lude.Text)
rnaarsNewAssociationId = Lens.lens (newAssociationId :: ReplaceNetworkACLAssociationResponse -> Lude.Maybe Lude.Text) (\s a -> s {newAssociationId = a} :: ReplaceNetworkACLAssociationResponse)
{-# DEPRECATED rnaarsNewAssociationId "Use generic-lens or generic-optics with 'newAssociationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rnaarsResponseStatus :: Lens.Lens' ReplaceNetworkACLAssociationResponse Lude.Int
rnaarsResponseStatus = Lens.lens (responseStatus :: ReplaceNetworkACLAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReplaceNetworkACLAssociationResponse)
{-# DEPRECATED rnaarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
