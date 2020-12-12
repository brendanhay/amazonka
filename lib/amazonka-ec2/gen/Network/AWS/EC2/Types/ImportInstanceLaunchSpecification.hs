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
    iilsGroupNames,
    iilsSubnetId,
    iilsInstanceType,
    iilsGroupIds,
    iilsUserData,
    iilsMonitoring,
    iilsPrivateIPAddress,
    iilsInstanceInitiatedShutdownBehavior,
    iilsArchitecture,
    iilsPlacement,
  )
where

import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.EC2.Types.UserData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the launch specification for VM import.
--
-- /See:/ 'mkImportInstanceLaunchSpecification' smart constructor.
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'
  { additionalInfo ::
      Lude.Maybe Lude.Text,
    groupNames ::
      Lude.Maybe [Lude.Text],
    subnetId ::
      Lude.Maybe Lude.Text,
    instanceType ::
      Lude.Maybe InstanceType,
    groupIds ::
      Lude.Maybe [Lude.Text],
    userData ::
      Lude.Maybe UserData,
    monitoring ::
      Lude.Maybe Lude.Bool,
    privateIPAddress ::
      Lude.Maybe Lude.Text,
    instanceInitiatedShutdownBehavior ::
      Lude.Maybe
        ShutdownBehavior,
    architecture ::
      Lude.Maybe
        ArchitectureValues,
    placement ::
      Lude.Maybe Placement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImportInstanceLaunchSpecification' with the minimum fields required to make a request.
--
-- * 'additionalInfo' - Reserved.
-- * 'architecture' - The architecture of the instance.
-- * 'groupIds' - The security group IDs.
-- * 'groupNames' - The security group names.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
-- * 'instanceType' - The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
-- * 'monitoring' - Indicates whether monitoring is enabled.
-- * 'placement' - The placement information for the instance.
-- * 'privateIPAddress' - [EC2-VPC] An available IP address from the IP address range of the subnet.
-- * 'subnetId' - [EC2-VPC] The ID of the subnet in which to launch the instance.
-- * 'userData' - The Base64-encoded user data to make available to the instance.
mkImportInstanceLaunchSpecification ::
  ImportInstanceLaunchSpecification
mkImportInstanceLaunchSpecification =
  ImportInstanceLaunchSpecification'
    { additionalInfo = Lude.Nothing,
      groupNames = Lude.Nothing,
      subnetId = Lude.Nothing,
      instanceType = Lude.Nothing,
      groupIds = Lude.Nothing,
      userData = Lude.Nothing,
      monitoring = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      architecture = Lude.Nothing,
      placement = Lude.Nothing
    }

-- | Reserved.
--
-- /Note:/ Consider using 'additionalInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsAdditionalInfo :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe Lude.Text)
iilsAdditionalInfo = Lens.lens (additionalInfo :: ImportInstanceLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {additionalInfo = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsAdditionalInfo "Use generic-lens or generic-optics with 'additionalInfo' instead." #-}

-- | The security group names.
--
-- /Note:/ Consider using 'groupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupNames :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe [Lude.Text])
iilsGroupNames = Lens.lens (groupNames :: ImportInstanceLaunchSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {groupNames = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsGroupNames "Use generic-lens or generic-optics with 'groupNames' instead." #-}

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsSubnetId :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe Lude.Text)
iilsSubnetId = Lens.lens (subnetId :: ImportInstanceLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The instance type. For more information about the instance types that you can import, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types> in the VM Import/Export User Guide.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceType :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe InstanceType)
iilsInstanceType = Lens.lens (instanceType :: ImportInstanceLaunchSpecification -> Lude.Maybe InstanceType) (\s a -> s {instanceType = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The security group IDs.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsGroupIds :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe [Lude.Text])
iilsGroupIds = Lens.lens (groupIds :: ImportInstanceLaunchSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {groupIds = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsGroupIds "Use generic-lens or generic-optics with 'groupIds' instead." #-}

-- | The Base64-encoded user data to make available to the instance.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsUserData :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe UserData)
iilsUserData = Lens.lens (userData :: ImportInstanceLaunchSpecification -> Lude.Maybe UserData) (\s a -> s {userData = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Indicates whether monitoring is enabled.
--
-- /Note:/ Consider using 'monitoring' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsMonitoring :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe Lude.Bool)
iilsMonitoring = Lens.lens (monitoring :: ImportInstanceLaunchSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {monitoring = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsMonitoring "Use generic-lens or generic-optics with 'monitoring' instead." #-}

-- | [EC2-VPC] An available IP address from the IP address range of the subnet.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPrivateIPAddress :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe Lude.Text)
iilsPrivateIPAddress = Lens.lens (privateIPAddress :: ImportInstanceLaunchSpecification -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsInstanceInitiatedShutdownBehavior :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe ShutdownBehavior)
iilsInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: ImportInstanceLaunchSpecification -> Lude.Maybe ShutdownBehavior) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The architecture of the instance.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsArchitecture :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe ArchitectureValues)
iilsArchitecture = Lens.lens (architecture :: ImportInstanceLaunchSpecification -> Lude.Maybe ArchitectureValues) (\s a -> s {architecture = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The placement information for the instance.
--
-- /Note:/ Consider using 'placement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iilsPlacement :: Lens.Lens' ImportInstanceLaunchSpecification (Lude.Maybe Placement)
iilsPlacement = Lens.lens (placement :: ImportInstanceLaunchSpecification -> Lude.Maybe Placement) (\s a -> s {placement = a} :: ImportInstanceLaunchSpecification)
{-# DEPRECATED iilsPlacement "Use generic-lens or generic-optics with 'placement' instead." #-}

instance Lude.ToQuery ImportInstanceLaunchSpecification where
  toQuery ImportInstanceLaunchSpecification' {..} =
    Lude.mconcat
      [ "AdditionalInfo" Lude.=: additionalInfo,
        Lude.toQuery (Lude.toQueryList "GroupName" Lude.<$> groupNames),
        "SubnetId" Lude.=: subnetId,
        "InstanceType" Lude.=: instanceType,
        Lude.toQuery (Lude.toQueryList "GroupId" Lude.<$> groupIds),
        "UserData" Lude.=: userData,
        "Monitoring" Lude.=: monitoring,
        "PrivateIpAddress" Lude.=: privateIPAddress,
        "InstanceInitiatedShutdownBehavior"
          Lude.=: instanceInitiatedShutdownBehavior,
        "Architecture" Lude.=: architecture,
        "Placement" Lude.=: placement
      ]
