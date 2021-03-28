{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DirectoryService.Types.DomainController
  ( DomainController (..)
  -- * Smart constructor
  , mkDomainController
  -- * Lenses
  , dcAvailabilityZone
  , dcDirectoryId
  , dcDnsIpAddr
  , dcDomainControllerId
  , dcLaunchTime
  , dcStatus
  , dcStatusLastUpdatedDateTime
  , dcStatusReason
  , dcSubnetId
  , dcVpcId
  ) where

import qualified Network.AWS.DirectoryService.Types.AvailabilityZone as Types
import qualified Network.AWS.DirectoryService.Types.DirectoryId as Types
import qualified Network.AWS.DirectoryService.Types.DnsIpAddr as Types
import qualified Network.AWS.DirectoryService.Types.DomainControllerId as Types
import qualified Network.AWS.DirectoryService.Types.DomainControllerStatus as Types
import qualified Network.AWS.DirectoryService.Types.StatusReason as Types
import qualified Network.AWS.DirectoryService.Types.SubnetId as Types
import qualified Network.AWS.DirectoryService.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the domain controllers for a specified directory.
--
-- /See:/ 'mkDomainController' smart constructor.
data DomainController = DomainController'
  { availabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone where the domain controller is located.
  , directoryId :: Core.Maybe Types.DirectoryId
    -- ^ Identifier of the directory where the domain controller resides.
  , dnsIpAddr :: Core.Maybe Types.DnsIpAddr
    -- ^ The IP address of the domain controller.
  , domainControllerId :: Core.Maybe Types.DomainControllerId
    -- ^ Identifies a specific domain controller in the directory.
  , launchTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Specifies when the domain controller was created.
  , status :: Core.Maybe Types.DomainControllerStatus
    -- ^ The status of the domain controller.
  , statusLastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time that the status was last updated.
  , statusReason :: Core.Maybe Types.StatusReason
    -- ^ A description of the domain controller state.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ Identifier of the subnet in the VPC that contains the domain controller.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The identifier of the VPC that contains the domain controller.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DomainController' value with any optional fields omitted.
mkDomainController
    :: DomainController
mkDomainController
  = DomainController'{availabilityZone = Core.Nothing,
                      directoryId = Core.Nothing, dnsIpAddr = Core.Nothing,
                      domainControllerId = Core.Nothing, launchTime = Core.Nothing,
                      status = Core.Nothing, statusLastUpdatedDateTime = Core.Nothing,
                      statusReason = Core.Nothing, subnetId = Core.Nothing,
                      vpcId = Core.Nothing}

-- | The Availability Zone where the domain controller is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAvailabilityZone :: Lens.Lens' DomainController (Core.Maybe Types.AvailabilityZone)
dcAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE dcAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | Identifier of the directory where the domain controller resides.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDirectoryId :: Lens.Lens' DomainController (Core.Maybe Types.DirectoryId)
dcDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE dcDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The IP address of the domain controller.
--
-- /Note:/ Consider using 'dnsIpAddr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDnsIpAddr :: Lens.Lens' DomainController (Core.Maybe Types.DnsIpAddr)
dcDnsIpAddr = Lens.field @"dnsIpAddr"
{-# INLINEABLE dcDnsIpAddr #-}
{-# DEPRECATED dnsIpAddr "Use generic-lens or generic-optics with 'dnsIpAddr' instead"  #-}

-- | Identifies a specific domain controller in the directory.
--
-- /Note:/ Consider using 'domainControllerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDomainControllerId :: Lens.Lens' DomainController (Core.Maybe Types.DomainControllerId)
dcDomainControllerId = Lens.field @"domainControllerId"
{-# INLINEABLE dcDomainControllerId #-}
{-# DEPRECATED domainControllerId "Use generic-lens or generic-optics with 'domainControllerId' instead"  #-}

-- | Specifies when the domain controller was created.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLaunchTime :: Lens.Lens' DomainController (Core.Maybe Core.NominalDiffTime)
dcLaunchTime = Lens.field @"launchTime"
{-# INLINEABLE dcLaunchTime #-}
{-# DEPRECATED launchTime "Use generic-lens or generic-optics with 'launchTime' instead"  #-}

-- | The status of the domain controller.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatus :: Lens.Lens' DomainController (Core.Maybe Types.DomainControllerStatus)
dcStatus = Lens.field @"status"
{-# INLINEABLE dcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The date and time that the status was last updated.
--
-- /Note:/ Consider using 'statusLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusLastUpdatedDateTime :: Lens.Lens' DomainController (Core.Maybe Core.NominalDiffTime)
dcStatusLastUpdatedDateTime = Lens.field @"statusLastUpdatedDateTime"
{-# INLINEABLE dcStatusLastUpdatedDateTime #-}
{-# DEPRECATED statusLastUpdatedDateTime "Use generic-lens or generic-optics with 'statusLastUpdatedDateTime' instead"  #-}

-- | A description of the domain controller state.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusReason :: Lens.Lens' DomainController (Core.Maybe Types.StatusReason)
dcStatusReason = Lens.field @"statusReason"
{-# INLINEABLE dcStatusReason #-}
{-# DEPRECATED statusReason "Use generic-lens or generic-optics with 'statusReason' instead"  #-}

-- | Identifier of the subnet in the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSubnetId :: Lens.Lens' DomainController (Core.Maybe Types.SubnetId)
dcSubnetId = Lens.field @"subnetId"
{-# INLINEABLE dcSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The identifier of the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVpcId :: Lens.Lens' DomainController (Core.Maybe Types.VpcId)
dcVpcId = Lens.field @"vpcId"
{-# INLINEABLE dcVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromJSON DomainController where
        parseJSON
          = Core.withObject "DomainController" Core.$
              \ x ->
                DomainController' Core.<$>
                  (x Core..:? "AvailabilityZone") Core.<*> x Core..:? "DirectoryId"
                    Core.<*> x Core..:? "DnsIpAddr"
                    Core.<*> x Core..:? "DomainControllerId"
                    Core.<*> x Core..:? "LaunchTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "StatusLastUpdatedDateTime"
                    Core.<*> x Core..:? "StatusReason"
                    Core.<*> x Core..:? "SubnetId"
                    Core.<*> x Core..:? "VpcId"
