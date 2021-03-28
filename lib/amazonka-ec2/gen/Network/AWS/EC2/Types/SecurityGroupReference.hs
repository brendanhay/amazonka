{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SecurityGroupReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SecurityGroupReference
  ( SecurityGroupReference (..)
  -- * Smart constructor
  , mkSecurityGroupReference
  -- * Lenses
  , sgrGroupId
  , sgrReferencingVpcId
  , sgrVpcPeeringConnectionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a VPC with a security group that references your security group.
--
-- /See:/ 'mkSecurityGroupReference' smart constructor.
data SecurityGroupReference = SecurityGroupReference'
  { groupId :: Core.Maybe Core.Text
    -- ^ The ID of your security group.
  , referencingVpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC with the referencing security group.
  , vpcPeeringConnectionId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC peering connection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SecurityGroupReference' value with any optional fields omitted.
mkSecurityGroupReference
    :: SecurityGroupReference
mkSecurityGroupReference
  = SecurityGroupReference'{groupId = Core.Nothing,
                            referencingVpcId = Core.Nothing,
                            vpcPeeringConnectionId = Core.Nothing}

-- | The ID of your security group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrGroupId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
sgrGroupId = Lens.field @"groupId"
{-# INLINEABLE sgrGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The ID of the VPC with the referencing security group.
--
-- /Note:/ Consider using 'referencingVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrReferencingVpcId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
sgrReferencingVpcId = Lens.field @"referencingVpcId"
{-# INLINEABLE sgrReferencingVpcId #-}
{-# DEPRECATED referencingVpcId "Use generic-lens or generic-optics with 'referencingVpcId' instead"  #-}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgrVpcPeeringConnectionId :: Lens.Lens' SecurityGroupReference (Core.Maybe Core.Text)
sgrVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE sgrVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.FromXML SecurityGroupReference where
        parseXML x
          = SecurityGroupReference' Core.<$>
              (x Core..@? "groupId") Core.<*> x Core..@? "referencingVpcId"
                Core.<*> x Core..@? "vpcPeeringConnectionId"
