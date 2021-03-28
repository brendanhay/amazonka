{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkAcl
  ( NetworkAcl (..)
  -- * Smart constructor
  , mkNetworkAcl
  -- * Lenses
  , naAssociations
  , naEntries
  , naIsDefault
  , naNetworkAclId
  , naOwnerId
  , naTags
  , naVpcId
  ) where

import qualified Network.AWS.EC2.Types.NetworkAclAssociation as Types
import qualified Network.AWS.EC2.Types.NetworkAclEntry as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network ACL.
--
-- /See:/ 'mkNetworkAcl' smart constructor.
data NetworkAcl = NetworkAcl'
  { associations :: Core.Maybe [Types.NetworkAclAssociation]
    -- ^ Any associations between the network ACL and one or more subnets
  , entries :: Core.Maybe [Types.NetworkAclEntry]
    -- ^ One or more entries (rules) in the network ACL.
  , isDefault :: Core.Maybe Core.Bool
    -- ^ Indicates whether this is the default network ACL for the VPC.
  , networkAclId :: Core.Maybe Core.Text
    -- ^ The ID of the network ACL.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the network ACL.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the network ACL.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC for the network ACL.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkAcl' value with any optional fields omitted.
mkNetworkAcl
    :: NetworkAcl
mkNetworkAcl
  = NetworkAcl'{associations = Core.Nothing, entries = Core.Nothing,
                isDefault = Core.Nothing, networkAclId = Core.Nothing,
                ownerId = Core.Nothing, tags = Core.Nothing, vpcId = Core.Nothing}

-- | Any associations between the network ACL and one or more subnets
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naAssociations :: Lens.Lens' NetworkAcl (Core.Maybe [Types.NetworkAclAssociation])
naAssociations = Lens.field @"associations"
{-# INLINEABLE naAssociations #-}
{-# DEPRECATED associations "Use generic-lens or generic-optics with 'associations' instead"  #-}

-- | One or more entries (rules) in the network ACL.
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naEntries :: Lens.Lens' NetworkAcl (Core.Maybe [Types.NetworkAclEntry])
naEntries = Lens.field @"entries"
{-# INLINEABLE naEntries #-}
{-# DEPRECATED entries "Use generic-lens or generic-optics with 'entries' instead"  #-}

-- | Indicates whether this is the default network ACL for the VPC.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naIsDefault :: Lens.Lens' NetworkAcl (Core.Maybe Core.Bool)
naIsDefault = Lens.field @"isDefault"
{-# INLINEABLE naIsDefault #-}
{-# DEPRECATED isDefault "Use generic-lens or generic-optics with 'isDefault' instead"  #-}

-- | The ID of the network ACL.
--
-- /Note:/ Consider using 'networkAclId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naNetworkAclId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
naNetworkAclId = Lens.field @"networkAclId"
{-# INLINEABLE naNetworkAclId #-}
{-# DEPRECATED networkAclId "Use generic-lens or generic-optics with 'networkAclId' instead"  #-}

-- | The ID of the AWS account that owns the network ACL.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naOwnerId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
naOwnerId = Lens.field @"ownerId"
{-# INLINEABLE naOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Any tags assigned to the network ACL.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naTags :: Lens.Lens' NetworkAcl (Core.Maybe [Types.Tag])
naTags = Lens.field @"tags"
{-# INLINEABLE naTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The ID of the VPC for the network ACL.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
naVpcId :: Lens.Lens' NetworkAcl (Core.Maybe Core.Text)
naVpcId = Lens.field @"vpcId"
{-# INLINEABLE naVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML NetworkAcl where
        parseXML x
          = NetworkAcl' Core.<$>
              (x Core..@? "associationSet" Core..<@> Core.parseXMLList "item")
                Core.<*> x Core..@? "entrySet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "default"
                Core.<*> x Core..@? "networkAclId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "vpcId"
