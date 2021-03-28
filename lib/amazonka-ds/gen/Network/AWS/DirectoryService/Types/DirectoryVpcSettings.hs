{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVpcSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryVpcSettings
  ( DirectoryVpcSettings (..)
  -- * Smart constructor
  , mkDirectoryVpcSettings
  -- * Lenses
  , dvsVpcId
  , dvsSubnetIds
  ) where

import qualified Network.AWS.DirectoryService.Types.SubnetId as Types
import qualified Network.AWS.DirectoryService.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
-- /See:/ 'mkDirectoryVpcSettings' smart constructor.
data DirectoryVpcSettings = DirectoryVpcSettings'
  { vpcId :: Types.VpcId
    -- ^ The identifier of the VPC in which to create the directory.
  , subnetIds :: [Types.SubnetId]
    -- ^ The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryVpcSettings' value with any optional fields omitted.
mkDirectoryVpcSettings
    :: Types.VpcId -- ^ 'vpcId'
    -> DirectoryVpcSettings
mkDirectoryVpcSettings vpcId
  = DirectoryVpcSettings'{vpcId, subnetIds = Core.mempty}

-- | The identifier of the VPC in which to create the directory.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsVpcId :: Lens.Lens' DirectoryVpcSettings Types.VpcId
dvsVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvsVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

-- | The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsSubnetIds :: Lens.Lens' DirectoryVpcSettings [Types.SubnetId]
dvsSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dvsSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

instance Core.FromJSON DirectoryVpcSettings where
        toJSON DirectoryVpcSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VpcId" Core..= vpcId),
                  Core.Just ("SubnetIds" Core..= subnetIds)])

instance Core.FromJSON DirectoryVpcSettings where
        parseJSON
          = Core.withObject "DirectoryVpcSettings" Core.$
              \ x ->
                DirectoryVpcSettings' Core.<$>
                  (x Core..: "VpcId") Core.<*>
                    x Core..:? "SubnetIds" Core..!= Core.mempty
