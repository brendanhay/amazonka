{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterfacePermission
  ( NetworkInterfacePermission (..),

    -- * Smart constructor
    mkNetworkInterfacePermission,

    -- * Lenses
    nipPermissionState,
    nipNetworkInterfacePermissionId,
    nipNetworkInterfaceId,
    nipAWSAccountId,
    nipAWSService,
    nipPermission,
  )
where

import Network.AWS.EC2.Types.InterfacePermissionType
import Network.AWS.EC2.Types.NetworkInterfacePermissionState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a permission for a network interface.
--
-- /See:/ 'mkNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { permissionState ::
      Lude.Maybe
        NetworkInterfacePermissionState,
    networkInterfacePermissionId ::
      Lude.Maybe Lude.Text,
    networkInterfaceId ::
      Lude.Maybe Lude.Text,
    awsAccountId :: Lude.Maybe Lude.Text,
    awsService :: Lude.Maybe Lude.Text,
    permission ::
      Lude.Maybe InterfacePermissionType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NetworkInterfacePermission' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The AWS account ID.
-- * 'awsService' - The AWS service.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'networkInterfacePermissionId' - The ID of the network interface permission.
-- * 'permission' - The type of permission.
-- * 'permissionState' - Information about the state of the permission.
mkNetworkInterfacePermission ::
  NetworkInterfacePermission
mkNetworkInterfacePermission =
  NetworkInterfacePermission'
    { permissionState = Lude.Nothing,
      networkInterfacePermissionId = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      awsAccountId = Lude.Nothing,
      awsService = Lude.Nothing,
      permission = Lude.Nothing
    }

-- | Information about the state of the permission.
--
-- /Note:/ Consider using 'permissionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermissionState :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe NetworkInterfacePermissionState)
nipPermissionState = Lens.lens (permissionState :: NetworkInterfacePermission -> Lude.Maybe NetworkInterfacePermissionState) (\s a -> s {permissionState = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipPermissionState "Use generic-lens or generic-optics with 'permissionState' instead." #-}

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe Lude.Text)
nipNetworkInterfacePermissionId = Lens.lens (networkInterfacePermissionId :: NetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfacePermissionId = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipNetworkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe Lude.Text)
nipNetworkInterfaceId = Lens.lens (networkInterfaceId :: NetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAWSAccountId :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe Lude.Text)
nipAWSAccountId = Lens.lens (awsAccountId :: NetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {awsAccountId = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The AWS service.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAWSService :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe Lude.Text)
nipAWSService = Lens.lens (awsService :: NetworkInterfacePermission -> Lude.Maybe Lude.Text) (\s a -> s {awsService = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipAWSService "Use generic-lens or generic-optics with 'awsService' instead." #-}

-- | The type of permission.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermission :: Lens.Lens' NetworkInterfacePermission (Lude.Maybe InterfacePermissionType)
nipPermission = Lens.lens (permission :: NetworkInterfacePermission -> Lude.Maybe InterfacePermissionType) (\s a -> s {permission = a} :: NetworkInterfacePermission)
{-# DEPRECATED nipPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

instance Lude.FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      Lude.<$> (x Lude..@? "permissionState")
      Lude.<*> (x Lude..@? "networkInterfacePermissionId")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "awsAccountId")
      Lude.<*> (x Lude..@? "awsService")
      Lude.<*> (x Lude..@? "permission")
