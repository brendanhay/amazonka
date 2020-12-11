{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network ACL. You can't delete the ACL if it's associated with any subnets. You can't delete the default network ACL.
module Network.AWS.EC2.DeleteNetworkACL
  ( -- * Creating a request
    DeleteNetworkACL (..),
    mkDeleteNetworkACL,

    -- ** Request lenses
    dnaDryRun,
    dnaNetworkACLId,

    -- * Destructuring the response
    DeleteNetworkACLResponse (..),
    mkDeleteNetworkACLResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNetworkACL' smart constructor.
data DeleteNetworkACL = DeleteNetworkACL'
  { dryRun ::
      Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DeleteNetworkACL' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkACLId' - The ID of the network ACL.
mkDeleteNetworkACL ::
  -- | 'networkACLId'
  Lude.Text ->
  DeleteNetworkACL
mkDeleteNetworkACL pNetworkACLId_ =
  DeleteNetworkACL'
    { dryRun = Lude.Nothing,
      networkACLId = pNetworkACLId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaDryRun :: Lens.Lens' DeleteNetworkACL (Lude.Maybe Lude.Bool)
dnaDryRun = Lens.lens (dryRun :: DeleteNetworkACL -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteNetworkACL)
{-# DEPRECATED dnaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnaNetworkACLId :: Lens.Lens' DeleteNetworkACL Lude.Text
dnaNetworkACLId = Lens.lens (networkACLId :: DeleteNetworkACL -> Lude.Text) (\s a -> s {networkACLId = a} :: DeleteNetworkACL)
{-# DEPRECATED dnaNetworkACLId "Use generic-lens or generic-optics with 'networkACLId' instead." #-}

instance Lude.AWSRequest DeleteNetworkACL where
  type Rs DeleteNetworkACL = DeleteNetworkACLResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteNetworkACLResponse'

instance Lude.ToHeaders DeleteNetworkACL where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNetworkACL where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNetworkACL where
  toQuery DeleteNetworkACL' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteNetworkAcl" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "NetworkAclId" Lude.=: networkACLId
      ]

-- | /See:/ 'mkDeleteNetworkACLResponse' smart constructor.
data DeleteNetworkACLResponse = DeleteNetworkACLResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkACLResponse' with the minimum fields required to make a request.
mkDeleteNetworkACLResponse ::
  DeleteNetworkACLResponse
mkDeleteNetworkACLResponse = DeleteNetworkACLResponse'
