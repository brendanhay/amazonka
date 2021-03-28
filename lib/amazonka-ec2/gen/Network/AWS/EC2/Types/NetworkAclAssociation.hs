{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkAclAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkAclAssociation
  ( NetworkAclAssociation (..)
  -- * Smart constructor
  , mkNetworkAclAssociation
  -- * Lenses
  , naaNetworkAclAssociationId
  , naaNetworkAclId
  , naaSubnetId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an association between a network ACL and a subnet.
--
-- /See:/ 'mkNetworkAclAssociation' smart constructor.
data NetworkAclAssociation = NetworkAclAssociation'
  { networkAclAssociationId :: Core.Maybe Core.Text
    -- ^ The ID of the association between a network ACL and a subnet.
  , networkAclId :: Core.Maybe Core.Text
    -- ^ The ID of the network ACL.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkAclAssociation' value with any optional fields omitted.
mkNetworkAclAssociation
    :: NetworkAclAssociation
mkNetworkAclAssociation
  = NetworkAclAssociation'{networkAclAssociationId = Core.Nothing,
                           networkAclId = Core.Nothing, subnetId = Core.Nothing}

-- | The ID of the association between a network ACL and a subnet.
--
-- /Note:/ Consider using 'networkAclAssociationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaNetworkAclAssociationId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
naaNetworkAclAssociationId = Lens.field @"networkAclAssociationId"
{-# INLINEABLE naaNetworkAclAssociationId #-}
{-# DEPRECATED networkAclAssociationId "Use generic-lens or generic-optics with 'networkAclAssociationId' instead"  #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaNetworkAclId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
naaNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE naaNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naaSubnetId :: Lens.Lens' NetworkAclAssociation (Core.Maybe Core.Text)
naaSubnetId = Lens.field @"subnetId"
{-# INLINEABLE naaSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.FromXML NetworkAclAssociation where
        parseXML x
          = NetworkAclAssociation' Core.<$>
              (x Core..@? "networkAclAssociationId") Core.<*>
                x Core..@? "networkAclId"
                Core.<*> x Core..@? "subnetId"
