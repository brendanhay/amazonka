{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
  ( ImportInstanceLaunchSpecification (..)
  -- * Smart constructor
  , mkImportInstanceLaunchSpecification
  -- * Lenses
  , iilsAdditionalInfo
  , iilsArchitecture
  , iilsGroupIds
  , iilsGroupNames
  , iilsInstanceInitiatedShutdownBehavior
  , iilsInstanceType
  , iilsMonitoring
  , iilsPlacement
  , iilsPrivateIpAddress
  , iilsSubnetId
  , iilsUserData
  ) where

import qualified Network.AWS.EC2.Types.ArchitectureValues as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.Placement as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.SecurityGroupName as Types
import qualified Network.AWS.EC2.Types.ShutdownBehavior as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.EC2.Types.UserData as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the launch specification for VM import.
--
-- /See:/ 'mkImportInstanceLaunchSpecification' smart constructor.
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'
  { additionalInfo :: Core.Maybe Core.Text
    -- ^ Reserved.
  , architecture :: Core.Maybe Types.ArchitectureValues
    -- ^ The architecture of the instance.
  , groupIds :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The security group IDs.
  , groupNames :: Core.Maybe [Types.SecurityGroupName]
    -- ^ The security group names.
  , instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior
    -- ^ Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
  , instanceType :: Core.Maybe Types.InstanceType
    -- ^ The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
  , monitoring :: Core.Maybe Core.Bool
    -- ^ Indicates whether monitoring is enabled.
  , placement :: Core.Maybe Types.Placement
    -- ^ The placement information for the instance.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ [EC2-VPC] An available IP address from the IP address range of the subnet.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ [EC2-VPC] The ID of the subnet in which to launch the instance.
  , userData :: Core.Maybe Types.UserData
    -- ^ The Base64-encoded user data to make available to the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstanceLaunchSpecification' value with any optional fields omitted.
mkImportInstanceLaunchSpecification
    :: ImportInstanceLaunchSpecification
mkImportInstanceLaunchSpecification
  = ImportInstanceLaunchSpecification'{additionalInfo = Core.Nothing,
                                       architecture = Core.Nothing, groupIds = Core.Nothing,
                                       groupNames = Core.Nothing,
                                       instanceInitiatedShutdownBehavior = Core.Nothing,
                                       instanceType = Core.Nothing, monitoring = Core.Nothing,
                                       placement = Core.Nothing, privateIpAddress = Core.Nothing,
                                       subnetId = Core.Nothing, userData = Core.Nothing}

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsAdditionalInfo :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Text)
iilsAdditionalInfo = Lens.field @"additionalInfo"
{-# INLINEABLE iilsAdditionalInfo #-}
{-# DEPRECATED additionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead"  #-}

-- | The architecture of the instance.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsArchitecture :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.ArchitectureValues)
iilsArchitecture = Lens.field @"architecture"
{-# INLINEABLE iilsArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | The security group IDs.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupIds :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Types.SecurityGroupId])
iilsGroupIds = Lens.field @"groupIds"
{-# INLINEABLE iilsGroupIds #-}
{-# DEPRECATED groupIds "Use generic-lens or generic-optics with 'groupIds' instead"  #-}

-- | The security group names.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupNames :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Types.SecurityGroupName])
iilsGroupNames = Lens.field @"groupNames"
{-# INLINEABLE iilsGroupNames #-}
{-# DEPRECATED groupNames "Use generic-lens or generic-optics with 'groupNames' instead"  #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceInitiatedShutdownBehavior :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# INLINEABLE iilsInstanceInitiatedShutdownBehavior #-}
{-# DEPRECATED instanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead"  #-}

-- | The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceType :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.InstanceType)
iilsInstanceType = Lens.field @"instanceType"
{-# INLINEABLE iilsInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Indicates whether monitoring is enabled.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsMonitoring :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Bool)
iilsMonitoring = Lens.field @"monitoring"
{-# INLINEABLE iilsMonitoring #-}
{-# DEPRECATED monitoring "Use generic-lens or generic-optics with 'monitoring' instead"  #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPlacement :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.Placement)
iilsPlacement = Lens.field @"placement"
{-# INLINEABLE iilsPlacement #-}
{-# DEPRECATED placement "Use generic-lens or generic-optics with 'placement' instead"  #-}

-- | [EC2-VPC] An available IP address from the IP address range of the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPrivateIpAddress :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Text)
iilsPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE iilsPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsSubnetId :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.SubnetId)
iilsSubnetId = Lens.field @"subnetId"
{-# INLINEABLE iilsSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The Base64-encoded user data to make available to the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsUserData :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.UserData)
iilsUserData = Lens.field @"userData"
{-# INLINEABLE iilsUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

instance Core.ToQuery ImportInstanceLaunchSpecification where
        toQuery ImportInstanceLaunchSpecification{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AdditionalInfo")
              additionalInfo
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Architecture")
                architecture
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GroupId") groupIds
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "GroupName") groupNames
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "InstanceInitiatedShutdownBehavior")
                instanceInitiatedShutdownBehavior
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Monitoring") monitoring
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Placement") placement
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData
