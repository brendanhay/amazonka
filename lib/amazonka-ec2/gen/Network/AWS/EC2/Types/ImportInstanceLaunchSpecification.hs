{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
  ( ImportInstanceLaunchSpecification (..),

    -- * Smart constructor
    mkImportInstanceLaunchSpecification,

    -- * Lenses
    iilsAdditionalInfo,
    iilsArchitecture,
    iilsGroupIds,
    iilsGroupNames,
    iilsInstanceInitiatedShutdownBehavior,
    iilsInstanceType,
    iilsMonitoring,
    iilsPlacement,
    iilsPrivateIpAddress,
    iilsSubnetId,
    iilsUserData,
  )
where

import qualified Network.AWS.EC2.Types.AdditionalInfo as Types
import qualified Network.AWS.EC2.Types.ArchitectureValues as Types
import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.Placement as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddress as Types
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
  { -- | Reserved.
    additionalInfo :: Core.Maybe Types.AdditionalInfo,
    -- | The architecture of the instance.
    architecture :: Core.Maybe Types.ArchitectureValues,
    -- | The security group IDs.
    groupIds :: Core.Maybe [Types.SecurityGroupId],
    -- | The security group names.
    groupNames :: Core.Maybe [Types.SecurityGroupName],
    -- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe Types.ShutdownBehavior,
    -- | The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | Indicates whether monitoring is enabled.
    monitoring :: Core.Maybe Core.Bool,
    -- | The placement information for the instance.
    placement :: Core.Maybe Types.Placement,
    -- | [EC2-VPC] An available IP address from the IP address range of the subnet.
    privateIpAddress :: Core.Maybe Types.PrivateIpAddress,
    -- | [EC2-VPC] The ID of the subnet in which to launch the instance.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | The Base64-encoded user data to make available to the instance.
    userData :: Core.Maybe Types.UserData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImportInstanceLaunchSpecification' value with any optional fields omitted.
mkImportInstanceLaunchSpecification ::
  ImportInstanceLaunchSpecification
mkImportInstanceLaunchSpecification =
  ImportInstanceLaunchSpecification'
    { additionalInfo = Core.Nothing,
      architecture = Core.Nothing,
      groupIds = Core.Nothing,
      groupNames = Core.Nothing,
      instanceInitiatedShutdownBehavior = Core.Nothing,
      instanceType = Core.Nothing,
      monitoring = Core.Nothing,
      placement = Core.Nothing,
      privateIpAddress = Core.Nothing,
      subnetId = Core.Nothing,
      userData = Core.Nothing
    }

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsAdditionalInfo :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.AdditionalInfo)
iilsAdditionalInfo = Lens.field @"additionalInfo"
{-# DEPRECATED iilsAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The architecture of the instance.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsArchitecture :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.ArchitectureValues)
iilsArchitecture = Lens.field @"architecture"
{-# DEPRECATED iilsArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The security group IDs.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupIds :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Types.SecurityGroupId])
iilsGroupIds = Lens.field @"groupIds"
{-# DEPRECATED iilsGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | The security group names.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupNames :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Types.SecurityGroupName])
iilsGroupNames = Lens.field @"groupNames"
{-# DEPRECATED iilsGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceInitiatedShutdownBehavior :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# DEPRECATED iilsInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceType :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.InstanceType)
iilsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED iilsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether monitoring is enabled.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsMonitoring :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Bool)
iilsMonitoring = Lens.field @"monitoring"
{-# DEPRECATED iilsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPlacement :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.Placement)
iilsPlacement = Lens.field @"placement"
{-# DEPRECATED iilsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

-- | [EC2-VPC] An available IP address from the IP address range of the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPrivateIpAddress :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.PrivateIpAddress)
iilsPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED iilsPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsSubnetId :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.SubnetId)
iilsSubnetId = Lens.field @"subnetId"
{-# DEPRECATED iilsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The Base64-encoded user data to make available to the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsUserData :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Types.UserData)
iilsUserData = Lens.field @"userData"
{-# DEPRECATED iilsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}
