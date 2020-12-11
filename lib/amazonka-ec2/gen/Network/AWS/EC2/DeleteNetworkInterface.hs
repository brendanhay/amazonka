{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified network interface. You must detach the network interface before you can delete it.
module Network.AWS.EC2.DeleteNetworkInterface
  ( -- * Creating a request
    DeleteNetworkInterface (..),
    mkDeleteNetworkInterface,

    -- ** Request lenses
    dninDryRun,
    dninNetworkInterfaceId,

    -- * Destructuring the response
    DeleteNetworkInterfaceResponse (..),
    mkDeleteNetworkInterfaceResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteNetworkInterface.
--
-- /See:/ 'mkDeleteNetworkInterface' smart constructor.
data DeleteNetworkInterface = DeleteNetworkInterface'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfaceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkInterface' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'networkInterfaceId' - The ID of the network interface.
mkDeleteNetworkInterface ::
  -- | 'networkInterfaceId'
  Lude.Text ->
  DeleteNetworkInterface
mkDeleteNetworkInterface pNetworkInterfaceId_ =
  DeleteNetworkInterface'
    { dryRun = Lude.Nothing,
      networkInterfaceId = pNetworkInterfaceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dninDryRun :: Lens.Lens' DeleteNetworkInterface (Lude.Maybe Lude.Bool)
dninDryRun = Lens.lens (dryRun :: DeleteNetworkInterface -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteNetworkInterface)
{-# DEPRECATED dninDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dninNetworkInterfaceId :: Lens.Lens' DeleteNetworkInterface Lude.Text
dninNetworkInterfaceId = Lens.lens (networkInterfaceId :: DeleteNetworkInterface -> Lude.Text) (\s a -> s {networkInterfaceId = a} :: DeleteNetworkInterface)
{-# DEPRECATED dninNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

instance Lude.AWSRequest DeleteNetworkInterface where
  type Rs DeleteNetworkInterface = DeleteNetworkInterfaceResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteNetworkInterfaceResponse'

instance Lude.ToHeaders DeleteNetworkInterface where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNetworkInterface where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNetworkInterface where
  toQuery DeleteNetworkInterface' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteNetworkInterface" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "NetworkInterfaceId" Lude.=: networkInterfaceId
      ]

-- | /See:/ 'mkDeleteNetworkInterfaceResponse' smart constructor.
data DeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkInterfaceResponse' with the minimum fields required to make a request.
mkDeleteNetworkInterfaceResponse ::
  DeleteNetworkInterfaceResponse
mkDeleteNetworkInterfaceResponse = DeleteNetworkInterfaceResponse'
