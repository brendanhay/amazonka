{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
  ( DirectoryVpcSettingsDescription (..)
  -- * Smart constructor
  , mkDirectoryVpcSettingsDescription
  -- * Lenses
  , dvsdAvailabilityZones
  , dvsdSecurityGroupId
  , dvsdSubnetIds
  , dvsdVpcId
  ) where

import qualified Network.AWS.DirectoryService.Types.AvailabilityZone as Types
import qualified Network.AWS.DirectoryService.Types.SecurityGroupId as Types
import qualified Network.AWS.DirectoryService.Types.SubnetId as Types
import qualified Network.AWS.DirectoryService.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the directory.
--
-- /See:/ 'mkDirectoryVpcSettingsDescription' smart constructor.
data DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription'
  { availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ The list of Availability Zones that the directory is in.
  , securityGroupId :: Core.Maybe Types.SecurityGroupId
    -- ^ The domain controller security group identifier for the directory.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ The identifiers of the subnets for the directory servers.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The identifier of the VPC that the directory is in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryVpcSettingsDescription' value with any optional fields omitted.
mkDirectoryVpcSettingsDescription
    :: DirectoryVpcSettingsDescription
mkDirectoryVpcSettingsDescription
  = DirectoryVpcSettingsDescription'{availabilityZones =
                                       Core.Nothing,
                                     securityGroupId = Core.Nothing, subnetIds = Core.Nothing,
                                     vpcId = Core.Nothing}

-- | The list of Availability Zones that the directory is in.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdAvailabilityZones :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe [Types.AvailabilityZone])
dvsdAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dvsdAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The domain controller security group identifier for the directory.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdSecurityGroupId :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe Types.SecurityGroupId)
dvsdSecurityGroupId = Lens.field @"securityGroupId"
{-# INLINEABLE dvsdSecurityGroupId #-}
{-# DEPRECATED securityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead"  #-}

-- | The identifiers of the subnets for the directory servers.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdSubnetIds :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe [Types.SubnetId])
dvsdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dvsdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The identifier of the VPC that the directory is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdVpcId :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe Types.VpcId)
dvsdVpcId = Lens.field @"vpcId"
{-# INLINEABLE dvsdVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON DirectoryVpcSettingsDescription where
        parseJSON
          = Core.withObject "DirectoryVpcSettingsDescription" Core.$
              \ x ->
                DirectoryVpcSettingsDescription' Core.<$>
                  (x Core..:? "AvailabilityZones") Core.<*>
                    x Core..:? "SecurityGroupId"
                    Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "VpcId"
