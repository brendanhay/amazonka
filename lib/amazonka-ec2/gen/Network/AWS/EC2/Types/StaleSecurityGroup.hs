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
    ssgVPCId,
    ssgGroupId,
    ssgGroupName,
    ssgStaleIPPermissionsEgress,
    ssgStaleIPPermissions,
    ssgDescription,
  )
where

import Network.AWS.EC2.Types.StaleIPPermission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a stale security group (a security group that contains stale rules).
--
-- /See:/ 'mkStaleSecurityGroup' smart constructor.
data StaleSecurityGroup = StaleSecurityGroup'
  { -- | The ID of the VPC for the security group.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The ID of the security group.
    groupId :: Lude.Maybe Lude.Text,
    -- | The name of the security group.
    groupName :: Lude.Maybe Lude.Text,
    -- | Information about the stale outbound rules in the security group.
    staleIPPermissionsEgress :: Lude.Maybe [StaleIPPermission],
    -- | Information about the stale inbound rules in the security group.
    staleIPPermissions :: Lude.Maybe [StaleIPPermission],
    -- | The description of the security group.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaleSecurityGroup' with the minimum fields required to make a request.
--
-- * 'vpcId' - The ID of the VPC for the security group.
-- * 'groupId' - The ID of the security group.
-- * 'groupName' - The name of the security group.
-- * 'staleIPPermissionsEgress' - Information about the stale outbound rules in the security group.
-- * 'staleIPPermissions' - Information about the stale inbound rules in the security group.
-- * 'description' - The description of the security group.
mkStaleSecurityGroup ::
  StaleSecurityGroup
mkStaleSecurityGroup =
  StaleSecurityGroup'
    { vpcId = Lude.Nothing,
      groupId = Lude.Nothing,
      groupName = Lude.Nothing,
      staleIPPermissionsEgress = Lude.Nothing,
      staleIPPermissions = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The ID of the VPC for the security group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgVPCId :: Lens.Lens' StaleSecurityGroup (Lude.Maybe Lude.Text)
ssgVPCId = Lens.lens (vpcId :: StaleSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupId :: Lens.Lens' StaleSecurityGroup (Lude.Maybe Lude.Text)
ssgGroupId = Lens.lens (groupId :: StaleSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupId = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

-- | The name of the security group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgGroupName :: Lens.Lens' StaleSecurityGroup (Lude.Maybe Lude.Text)
ssgGroupName = Lens.lens (groupName :: StaleSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Information about the stale outbound rules in the security group.
--
-- /Note:/ Consider using 'staleIPPermissionsEgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgStaleIPPermissionsEgress :: Lens.Lens' StaleSecurityGroup (Lude.Maybe [StaleIPPermission])
ssgStaleIPPermissionsEgress = Lens.lens (staleIPPermissionsEgress :: StaleSecurityGroup -> Lude.Maybe [StaleIPPermission]) (\s a -> s {staleIPPermissionsEgress = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgStaleIPPermissionsEgress "Use generic-lens or generic-optics with 'staleIPPermissionsEgress' instead." #-}

-- | Information about the stale inbound rules in the security group.
--
-- /Note:/ Consider using 'staleIPPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgStaleIPPermissions :: Lens.Lens' StaleSecurityGroup (Lude.Maybe [StaleIPPermission])
ssgStaleIPPermissions = Lens.lens (staleIPPermissions :: StaleSecurityGroup -> Lude.Maybe [StaleIPPermission]) (\s a -> s {staleIPPermissions = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgStaleIPPermissions "Use generic-lens or generic-optics with 'staleIPPermissions' instead." #-}

-- | The description of the security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssgDescription :: Lens.Lens' StaleSecurityGroup (Lude.Maybe Lude.Text)
ssgDescription = Lens.lens (description :: StaleSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StaleSecurityGroup)
{-# DEPRECATED ssgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML StaleSecurityGroup where
  parseXML x =
    StaleSecurityGroup'
      Lude.<$> (x Lude..@? "vpcId")
      Lude.<*> (x Lude..@? "groupId")
      Lude.<*> (x Lude..@? "groupName")
      Lude.<*> ( x Lude..@? "staleIpPermissionsEgress" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "staleIpPermissions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "description")
