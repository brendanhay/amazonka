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
    nipAwsAccountId,
    nipAwsService,
    nipNetworkInterfaceId,
    nipNetworkInterfacePermissionId,
    nipPermission,
    nipPermissionState,
  )
where

import qualified Network.AWS.EC2.Types.InterfacePermissionType as Types
import qualified Network.AWS.EC2.Types.NetworkInterfacePermissionState as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a permission for a network interface.
--
-- /See:/ 'mkNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { -- | The AWS account ID.
    awsAccountId :: Core.Maybe Types.String,
    -- | The AWS service.
    awsService :: Core.Maybe Types.String,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The ID of the network interface permission.
    networkInterfacePermissionId :: Core.Maybe Types.String,
    -- | The type of permission.
    permission :: Core.Maybe Types.InterfacePermissionType,
    -- | Information about the state of the permission.
    permissionState :: Core.Maybe Types.NetworkInterfacePermissionState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfacePermission' value with any optional fields omitted.
mkNetworkInterfacePermission ::
  NetworkInterfacePermission
mkNetworkInterfacePermission =
  NetworkInterfacePermission'
    { awsAccountId = Core.Nothing,
      awsService = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      networkInterfacePermissionId = Core.Nothing,
      permission = Core.Nothing,
      permissionState = Core.Nothing
    }

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAwsAccountId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.String)
nipAwsAccountId = Lens.field @"awsAccountId"
{-# DEPRECATED nipAwsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The AWS service.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAwsService :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.String)
nipAwsService = Lens.field @"awsService"
{-# DEPRECATED nipAwsService "Use generic-lens or generic-optics with 'awsService' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.String)
nipNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED nipNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.String)
nipNetworkInterfacePermissionId = Lens.field @"networkInterfacePermissionId"
{-# DEPRECATED nipNetworkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead." #-}

-- | The type of permission.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermission :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.InterfacePermissionType)
nipPermission = Lens.field @"permission"
{-# DEPRECATED nipPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | Information about the state of the permission.
--
-- /Note:/ Consider using 'permissionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermissionState :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.NetworkInterfacePermissionState)
nipPermissionState = Lens.field @"permissionState"
{-# DEPRECATED nipPermissionState "Use generic-lens or generic-optics with 'permissionState' instead." #-}

instance Core.FromXML NetworkInterfacePermission where
  parseXML x =
    NetworkInterfacePermission'
      Core.<$> (x Core..@? "awsAccountId")
      Core.<*> (x Core..@? "awsService")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "networkInterfacePermissionId")
      Core.<*> (x Core..@? "permission")
      Core.<*> (x Core..@? "permissionState")
