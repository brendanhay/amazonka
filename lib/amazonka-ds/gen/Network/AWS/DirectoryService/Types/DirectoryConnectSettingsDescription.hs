{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
  ( DirectoryConnectSettingsDescription (..)
  -- * Smart constructor
  , mkDirectoryConnectSettingsDescription
  -- * Lenses
  , dcsdAvailabilityZones
  , dcsdConnectIps
  , dcsdCustomerUserName
  , dcsdSecurityGroupId
  , dcsdSubnetIds
  , dcsdVpcId
  ) where

import qualified Network.AWS.DirectoryService.Types.AvailabilityZone as Types
import qualified Network.AWS.DirectoryService.Types.IpAddr as Types
import qualified Network.AWS.DirectoryService.Types.SecurityGroupId as Types
import qualified Network.AWS.DirectoryService.Types.SubnetId as Types
import qualified Network.AWS.DirectoryService.Types.UserName as Types
import qualified Network.AWS.DirectoryService.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'mkDirectoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ A list of the Availability Zones that the directory is in.
  , connectIps :: Core.Maybe [Types.IpAddr]
    -- ^ The IP addresses of the AD Connector servers.
  , customerUserName :: Core.Maybe Types.UserName
    -- ^ The user name of the service account in the on-premises directory.
  , securityGroupId :: Core.Maybe Types.SecurityGroupId
    -- ^ The security group identifier for the AD Connector directory.
  , subnetIds :: Core.Maybe [Types.SubnetId]
    -- ^ A list of subnet identifiers in the VPC that the AD Connector is in.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The identifier of the VPC that the AD Connector is in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DirectoryConnectSettingsDescription' value with any optional fields omitted.
mkDirectoryConnectSettingsDescription
    :: DirectoryConnectSettingsDescription
mkDirectoryConnectSettingsDescription
  = DirectoryConnectSettingsDescription'{availabilityZones =
                                           Core.Nothing,
                                         connectIps = Core.Nothing, customerUserName = Core.Nothing,
                                         securityGroupId = Core.Nothing, subnetIds = Core.Nothing,
                                         vpcId = Core.Nothing}

-- | A list of the Availability Zones that the directory is in.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdAvailabilityZones :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Types.AvailabilityZone])
dcsdAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE dcsdAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The IP addresses of the AD Connector servers.
--
-- /Note:/ Consider using 'connectIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdConnectIps :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Types.IpAddr])
dcsdConnectIps = Lens.field @"connectIps"
{-# INLINEABLE dcsdConnectIps #-}
{-# DEPRECATED connectIps "Use generic-lens or generic-optics with 'connectIps' instead"  #-}

-- | The user name of the service account in the on-premises directory.
--
-- /Note:/ Consider using 'customerUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdCustomerUserName :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Types.UserName)
dcsdCustomerUserName = Lens.field @"customerUserName"
{-# INLINEABLE dcsdCustomerUserName #-}
{-# DEPRECATED customerUserName "Use generic-lens or generic-optics with 'customerUserName' instead"  #-}

-- | The security group identifier for the AD Connector directory.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdSecurityGroupId :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Types.SecurityGroupId)
dcsdSecurityGroupId = Lens.field @"securityGroupId"
{-# INLINEABLE dcsdSecurityGroupId #-}
{-# DEPRECATED securityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead"  #-}

-- | A list of subnet identifiers in the VPC that the AD Connector is in.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdSubnetIds :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Types.SubnetId])
dcsdSubnetIds = Lens.field @"subnetIds"
{-# INLINEABLE dcsdSubnetIds #-}
{-# DEPRECATED subnetIds "Use generic-lens or generic-optics with 'subnetIds' instead"  #-}

-- | The identifier of the VPC that the AD Connector is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdVpcId :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Types.VpcId)
dcsdVpcId = Lens.field @"vpcId"
{-# INLINEABLE dcsdVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON DirectoryConnectSettingsDescription where
        parseJSON
          = Core.withObject "DirectoryConnectSettingsDescription" Core.$
              \ x ->
                DirectoryConnectSettingsDescription' Core.<$>
                  (x Core..:? "AvailabilityZones") Core.<*> x Core..:? "ConnectIps"
                    Core.<*> x Core..:? "CustomerUserName"
                    Core.<*> x Core..:? "SecurityGroupId"
                    Core.<*> x Core..:? "SubnetIds"
                    Core.<*> x Core..:? "VpcId"
