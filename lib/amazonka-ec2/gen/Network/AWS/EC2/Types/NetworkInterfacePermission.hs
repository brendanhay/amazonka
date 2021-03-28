{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfacePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfacePermission
  ( NetworkInterfacePermission (..)
  -- * Smart constructor
  , mkNetworkInterfacePermission
  -- * Lenses
  , nipAwsAccountId
  , nipAwsService
  , nipNetworkInterfaceId
  , nipNetworkInterfacePermissionId
  , nipPermission
  , nipPermissionState
  ) where

import qualified Network.AWS.EC2.Types.InterfacePermissionType as Types
import qualified Network.AWS.EC2.Types.NetworkInterfacePermissionState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a permission for a network interface.
--
-- /See:/ 'mkNetworkInterfacePermission' smart constructor.
data NetworkInterfacePermission = NetworkInterfacePermission'
  { awsAccountId :: Core.Maybe Core.Text
    -- ^ The AWS account ID.
  , awsService :: Core.Maybe Core.Text
    -- ^ The AWS service.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , networkInterfacePermissionId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface permission.
  , permission :: Core.Maybe Types.InterfacePermissionType
    -- ^ The type of permission.
  , permissionState :: Core.Maybe Types.NetworkInterfacePermissionState
    -- ^ Information about the state of the permission.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfacePermission' value with any optional fields omitted.
mkNetworkInterfacePermission
    :: NetworkInterfacePermission
mkNetworkInterfacePermission
  = NetworkInterfacePermission'{awsAccountId = Core.Nothing,
                                awsService = Core.Nothing, networkInterfaceId = Core.Nothing,
                                networkInterfacePermissionId = Core.Nothing,
                                permission = Core.Nothing, permissionState = Core.Nothing}

-- | The AWS account ID.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAwsAccountId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
nipAwsAccountId = Lens.field @"awsAccountId"
{-# INLINEABLE nipAwsAccountId #-}
{-# DEPRECATED awsAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead"  #-}

-- | The AWS service.
--
-- /Note:/ Consider using 'awsService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipAwsService :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
nipAwsService = Lens.field @"awsService"
{-# INLINEABLE nipAwsService #-}
{-# DEPRECATED awsService "Use generic-lens or generic-optics with 'awsService' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfaceId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
nipNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE nipNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the network interface permission.
--
-- /Note:/ Consider using 'networkInterfacePermissionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipNetworkInterfacePermissionId :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Core.Text)
nipNetworkInterfacePermissionId = Lens.field @"networkInterfacePermissionId"
{-# INLINEABLE nipNetworkInterfacePermissionId #-}
{-# DEPRECATED networkInterfacePermissionId "Use generic-lens or generic-optics with 'networkInterfacePermissionId' instead"  #-}

-- | The type of permission.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermission :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.InterfacePermissionType)
nipPermission = Lens.field @"permission"
{-# INLINEABLE nipPermission #-}
{-# DEPRECATED permission "Use generic-lens or generic-optics with 'permission' instead"  #-}

-- | Information about the state of the permission.
--
-- /Note:/ Consider using 'permissionState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nipPermissionState :: Lens.Lens' NetworkInterfacePermission (Core.Maybe Types.NetworkInterfacePermissionState)
nipPermissionState = Lens.field @"permissionState"
{-# INLINEABLE nipPermissionState #-}
{-# DEPRECATED permissionState "Use generic-lens or generic-optics with 'permissionState' instead"  #-}

instance Core.FromXML NetworkInterfacePermission where
        parseXML x
          = NetworkInterfacePermission' Core.<$>
              (x Core..@? "awsAccountId") Core.<*> x Core..@? "awsService"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "networkInterfacePermissionId"
                Core.<*> x Core..@? "permission"
                Core.<*> x Core..@? "permissionState"
