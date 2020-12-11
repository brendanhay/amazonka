{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a permission for a network interface. By default, you cannot delete the permission if the account for which you're removing the permission has attached the network interface to an instance. However, you can force delete the permission, regardless of any attachment.
module Network.AWS.EC2.DeleteNetworkInterfacePermission
  ( -- * Creating a request
    DeleteNetworkInterfacePermission (..),
    mkDeleteNetworkInterfacePermission,

    -- ** Request lenses
    dnipForce,
    dnipDryRun,
    dnipNetworkInterfacePermissionId,

    -- * Destructuring the response
    DeleteNetworkInterfacePermissionResponse (..),
    mkDeleteNetworkInterfacePermissionResponse,

    -- ** Response lenses
    dniprsReturn,
    dniprsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermission' smart constructor.
data DeleteNetworkInterfacePermission = DeleteNetworkInterfacePermission'
  { force ::
      Lude.Maybe Lude.Bool,
    dryRun ::
      Lude.Maybe Lude.Bool,
    networkInterfacePermissionId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNetworkInterfacePermission' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'force' - Specify @true@ to remove the permission even if the network interface is attached to an instance.
-- * 'networkInterfacePermissionId' - The ID of the network interface permission.
mkDeleteNetworkInterfacePermission ::
  -- | 'networkInterfacePermissionId'
  Lude.Text ->
  DeleteNetworkInterfacePermission
mkDeleteNetworkInterfacePermission pNetworkInterfacePermissionId_ =
  DeleteNetworkInterfacePermission'
    { force = Lude.Nothing,
      dryRun = Lude.Nothing,
      networkInterfacePermissionId = pNetworkInterfacePermissionId_
    }

-- | Specify @true@ to remove the permission even if the network interface is attached to an instance.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipForce :: Lens.Lens' DeleteNetworkInterfacePermission (Lude.Maybe Lude.Bool)
dnipForce = Lens.lens (force :: DeleteNetworkInterfacePermission -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: DeleteNetworkInterfacePermission)
{-# DEPRECATED dnipForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipDryRun :: Lens.Lens' DeleteNetworkInterfacePermission (Lude.Maybe Lude.Bool)
dnipDryRun = Lens.lens (dryRun :: DeleteNetworkInterfacePermission -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteNetworkInterfacePermission)
{-# DEPRECATED dnipDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnipNetworkInterfacePermissionId :: Lens.Lens' DeleteNetworkInterfacePermission Lude.Text
dnipNetworkInterfacePermissionId = Lens.lens (networkInterfacePermissionId :: DeleteNetworkInterfacePermission -> Lude.Text) (\s a -> s {networkInterfacePermissionId = a} :: DeleteNetworkInterfacePermission)
{-# DEPRECATED dnipNetworkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead." #-}

instance Lude.AWSRequest DeleteNetworkInterfacePermission where
  type
    Rs DeleteNetworkInterfacePermission =
      DeleteNetworkInterfacePermissionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteNetworkInterfacePermissionResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNetworkInterfacePermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNetworkInterfacePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNetworkInterfacePermission where
  toQuery DeleteNetworkInterfacePermission' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteNetworkInterfacePermission" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Force" Lude.=: force,
        "DryRun" Lude.=: dryRun,
        "NetworkInterfacePermissionId"
          Lude.=: networkInterfacePermissionId
      ]

-- | Contains the output for DeleteNetworkInterfacePermission.
--
-- /See:/ 'mkDeleteNetworkInterfacePermissionResponse' smart constructor.
data DeleteNetworkInterfacePermissionResponse = DeleteNetworkInterfacePermissionResponse'
  { return ::
      Lude.Maybe
        Lude.Bool,
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

-- | Creates a value of 'DeleteNetworkInterfacePermissionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds, otherwise returns an error.
mkDeleteNetworkInterfacePermissionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNetworkInterfacePermissionResponse
mkDeleteNetworkInterfacePermissionResponse pResponseStatus_ =
  DeleteNetworkInterfacePermissionResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds, otherwise returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprsReturn :: Lens.Lens' DeleteNetworkInterfacePermissionResponse (Lude.Maybe Lude.Bool)
dniprsReturn = Lens.lens (return :: DeleteNetworkInterfacePermissionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DeleteNetworkInterfacePermissionResponse)
{-# DEPRECATED dniprsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dniprsResponseStatus :: Lens.Lens' DeleteNetworkInterfacePermissionResponse Lude.Int
dniprsResponseStatus = Lens.lens (responseStatus :: DeleteNetworkInterfacePermissionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNetworkInterfacePermissionResponse)
{-# DEPRECATED dniprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
