{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.StaleSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.StaleSecurityGroup
  ( StaleSecurityGroup (..),

    -- * Smart constructor
    mkStaleSecurityGroup,

    -- * Lenses
    ssgDescription,
    ssgGroupId,
    ssgGroupName,
    ssgStaleIpPermissions,
    ssgStaleIpPermissionsEgress,
    ssgVpcId,
  )
where

import qualified Network.AWS.EC2.Types.StaleIpPermission as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a stale security group (a security group that contains stale rules).
--
-- /See:/ 'mkStaleSecurityGroup' smart constructor.
data StaleSecurityGroup = StaleSecurityGroup'
  { -- | The description of the security group.
    description :: Core.Maybe Types.String,
    -- | The ID of the security group.
    groupId :: Core.Maybe Types.String,
    -- | The name of the security group.
    groupName :: Core.Maybe Types.String,
    -- | Information about the stale inbound rules in the security group.
    staleIpPermissions :: Core.Maybe [Types.StaleIpPermission],
    -- | Information about the stale outbound rules in the security group.
    staleIpPermissionsEgress :: Core.Maybe [Types.StaleIpPermission],
    -- | The ID of the VPC for the security group.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StaleSecurityGroup' value with any optional fields omitted.
mkStaleSecurityGroup ::
  StaleSecurityGroup
mkStaleSecurityGroup =
  StaleSecurityGroup'
    { description = Core.Nothing,
      groupId = Core.Nothing,
      groupName = Core.Nothing,
      staleIpPermissions = Core.Nothing,
      staleIpPermissionsEgress = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgDescription :: Lens.Lens' StaleSecurityGroup (Core.Maybe Types.String)
ssgDescription = Lens.field @"description"
{-# DEPRECATED ssgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupId :: Lens.Lens' StaleSecurityGroup (Core.Maybe Types.String)
ssgGroupId = Lens.field @"groupId"
{-# DEPRECATED ssgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupName :: Lens.Lens' StaleSecurityGroup (Core.Maybe Types.String)
ssgGroupName = Lens.field @"groupName"
{-# DEPRECATED ssgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Information about the stale inbound rules in the security group.
--
-- /Note:/ Consider using 'staleIpPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgStaleIpPermissions :: Lens.Lens' StaleSecurityGroup (Core.Maybe [Types.StaleIpPermission])
ssgStaleIpPermissions = Lens.field @"staleIpPermissions"
{-# DEPRECATED ssgStaleIpPermissions "Use generic-lens or generic-optics with 'staleIpPermissions' instead." #-}

-- | Information about the stale outbound rules in the security group.
--
-- /Note:/ Consider using 'staleIpPermissionsEgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgStaleIpPermissionsEgress :: Lens.Lens' StaleSecurityGroup (Core.Maybe [Types.StaleIpPermission])
ssgStaleIpPermissionsEgress = Lens.field @"staleIpPermissionsEgress"
{-# DEPRECATED ssgStaleIpPermissionsEgress "Use generic-lens or generic-optics with 'staleIpPermissionsEgress' instead." #-}

-- | The ID of the VPC for the security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgVpcId :: Lens.Lens' StaleSecurityGroup (Core.Maybe Types.String)
ssgVpcId = Lens.field @"vpcId"
{-# DEPRECATED ssgVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML StaleSecurityGroup where
  parseXML x =
    StaleSecurityGroup'
      Core.<$> (x Core..@? "description")
      Core.<*> (x Core..@? "groupId")
      Core.<*> (x Core..@? "groupName")
      Core.<*> ( x Core..@? "staleIpPermissions"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> ( x Core..@? "staleIpPermissionsEgress"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "vpcId")
