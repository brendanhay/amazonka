{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DomainController
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DomainController
  ( DomainController (..),

    -- * Smart constructor
    mkDomainController,

    -- * Lenses
    dcAvailabilityZone,
    dcDirectoryId,
    dcDnsIpAddr,
    dcDomainControllerId,
    dcLaunchTime,
    dcStatus,
    dcStatusLastUpdatedDateTime,
    dcStatusReason,
    dcSubnetId,
    dcVpcId,
  )
where

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
  { -- | The Availability Zone where the domain controller is located.
    availabilityZone :: Core.Maybe Types.AvailabilityZone,
    -- | Identifier of the directory where the domain controller resides.
    directoryId :: Core.Maybe Types.DirectoryId,
    -- | The IP address of the domain controller.
    dnsIpAddr :: Core.Maybe Types.DnsIpAddr,
    -- | Identifies a specific domain controller in the directory.
    domainControllerId :: Core.Maybe Types.DomainControllerId,
    -- | Specifies when the domain controller was created.
    launchTime :: Core.Maybe Core.NominalDiffTime,
    -- | The status of the domain controller.
    status :: Core.Maybe Types.DomainControllerStatus,
    -- | The date and time that the status was last updated.
    statusLastUpdatedDateTime :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the domain controller state.
    statusReason :: Core.Maybe Types.StatusReason,
    -- | Identifier of the subnet in the VPC that contains the domain controller.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The identifier of the VPC that contains the domain controller.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DomainController' value with any optional fields omitted.
mkDomainController ::
  DomainController
mkDomainController =
  DomainController'
    { availabilityZone = Core.Nothing,
      directoryId = Core.Nothing,
      dnsIpAddr = Core.Nothing,
      domainControllerId = Core.Nothing,
      launchTime = Core.Nothing,
      status = Core.Nothing,
      statusLastUpdatedDateTime = Core.Nothing,
      statusReason = Core.Nothing,
      subnetId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The Availability Zone where the domain controller is located.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAvailabilityZone :: Lens.Lens' DomainController (Core.Maybe Types.AvailabilityZone)
dcAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED dcAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | Identifier of the directory where the domain controller resides.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDirectoryId :: Lens.Lens' DomainController (Core.Maybe Types.DirectoryId)
dcDirectoryId = Lens.field @"directoryId"
{-# DEPRECATED dcDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | The IP address of the domain controller.
--
-- /Note:/ Consider using 'dnsIpAddr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDnsIpAddr :: Lens.Lens' DomainController (Core.Maybe Types.DnsIpAddr)
dcDnsIpAddr = Lens.field @"dnsIpAddr"
{-# DEPRECATED dcDnsIpAddr "Use generic-lens or generic-optics with 'dnsIpAddr' instead." #-}

-- | Identifies a specific domain controller in the directory.
--
-- /Note:/ Consider using 'domainControllerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDomainControllerId :: Lens.Lens' DomainController (Core.Maybe Types.DomainControllerId)
dcDomainControllerId = Lens.field @"domainControllerId"
{-# DEPRECATED dcDomainControllerId "Use generic-lens or generic-optics with 'domainControllerId' instead." #-}

-- | Specifies when the domain controller was created.
--
-- /Note:/ Consider using 'launchTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLaunchTime :: Lens.Lens' DomainController (Core.Maybe Core.NominalDiffTime)
dcLaunchTime = Lens.field @"launchTime"
{-# DEPRECATED dcLaunchTime "Use generic-lens or generic-optics with 'launchTime' instead." #-}

-- | The status of the domain controller.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatus :: Lens.Lens' DomainController (Core.Maybe Types.DomainControllerStatus)
dcStatus = Lens.field @"status"
{-# DEPRECATED dcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time that the status was last updated.
--
-- /Note:/ Consider using 'statusLastUpdatedDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusLastUpdatedDateTime :: Lens.Lens' DomainController (Core.Maybe Core.NominalDiffTime)
dcStatusLastUpdatedDateTime = Lens.field @"statusLastUpdatedDateTime"
{-# DEPRECATED dcStatusLastUpdatedDateTime "Use generic-lens or generic-optics with 'statusLastUpdatedDateTime' instead." #-}

-- | A description of the domain controller state.
--
-- /Note:/ Consider using 'statusReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatusReason :: Lens.Lens' DomainController (Core.Maybe Types.StatusReason)
dcStatusReason = Lens.field @"statusReason"
{-# DEPRECATED dcStatusReason "Use generic-lens or generic-optics with 'statusReason' instead." #-}

-- | Identifier of the subnet in the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcSubnetId :: Lens.Lens' DomainController (Core.Maybe Types.SubnetId)
dcSubnetId = Lens.field @"subnetId"
{-# DEPRECATED dcSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The identifier of the VPC that contains the domain controller.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVpcId :: Lens.Lens' DomainController (Core.Maybe Types.VpcId)
dcVpcId = Lens.field @"vpcId"
{-# DEPRECATED dcVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromJSON DomainController where
  parseJSON =
    Core.withObject "DomainController" Core.$
      \x ->
        DomainController'
          Core.<$> (x Core..:? "AvailabilityZone")
          Core.<*> (x Core..:? "DirectoryId")
          Core.<*> (x Core..:? "DnsIpAddr")
          Core.<*> (x Core..:? "DomainControllerId")
          Core.<*> (x Core..:? "LaunchTime")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusLastUpdatedDateTime")
          Core.<*> (x Core..:? "StatusReason")
          Core.<*> (x Core..:? "SubnetId")
          Core.<*> (x Core..:? "VpcId")
