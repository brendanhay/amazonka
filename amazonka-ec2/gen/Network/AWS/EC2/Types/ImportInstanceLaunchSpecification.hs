{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ImportInstanceLaunchSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImportInstanceLaunchSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.EC2.Types.UserData
import qualified Network.AWS.Lens as Lens

-- | Describes the launch specification for VM import.
--
-- /See:/ 'newImportInstanceLaunchSpecification' smart constructor.
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'
  { -- | Reserved.
    additionalInfo :: Core.Maybe Core.Text,
    -- | The instance type. For more information about the instance types that
    -- you can import, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types>
    -- in the VM Import\/Export User Guide.
    instanceType :: Core.Maybe InstanceType,
    -- | The Base64-encoded user data to make available to the instance.
    userData :: Core.Maybe (Core.Sensitive UserData),
    -- | The placement information for the instance.
    placement :: Core.Maybe Placement,
    -- | The security group IDs.
    groupIds :: Core.Maybe [Core.Text],
    -- | The security group names.
    groupNames :: Core.Maybe [Core.Text],
    -- | The architecture of the instance.
    architecture :: Core.Maybe ArchitectureValues,
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe ShutdownBehavior,
    -- | Indicates whether monitoring is enabled.
    monitoring :: Core.Maybe Core.Bool,
    -- | [EC2-VPC] The ID of the subnet in which to launch the instance.
    subnetId :: Core.Maybe Core.Text,
    -- | [EC2-VPC] An available IP address from the IP address range of the
    -- subnet.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImportInstanceLaunchSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'importInstanceLaunchSpecification_additionalInfo' - Reserved.
--
-- 'instanceType', 'importInstanceLaunchSpecification_instanceType' - The instance type. For more information about the instance types that
-- you can import, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types>
-- in the VM Import\/Export User Guide.
--
-- 'userData', 'importInstanceLaunchSpecification_userData' - The Base64-encoded user data to make available to the instance.
--
-- 'placement', 'importInstanceLaunchSpecification_placement' - The placement information for the instance.
--
-- 'groupIds', 'importInstanceLaunchSpecification_groupIds' - The security group IDs.
--
-- 'groupNames', 'importInstanceLaunchSpecification_groupNames' - The security group names.
--
-- 'architecture', 'importInstanceLaunchSpecification_architecture' - The architecture of the instance.
--
-- 'instanceInitiatedShutdownBehavior', 'importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
--
-- 'monitoring', 'importInstanceLaunchSpecification_monitoring' - Indicates whether monitoring is enabled.
--
-- 'subnetId', 'importInstanceLaunchSpecification_subnetId' - [EC2-VPC] The ID of the subnet in which to launch the instance.
--
-- 'privateIpAddress', 'importInstanceLaunchSpecification_privateIpAddress' - [EC2-VPC] An available IP address from the IP address range of the
-- subnet.
newImportInstanceLaunchSpecification ::
  ImportInstanceLaunchSpecification
newImportInstanceLaunchSpecification =
  ImportInstanceLaunchSpecification'
    { additionalInfo =
        Core.Nothing,
      instanceType = Core.Nothing,
      userData = Core.Nothing,
      placement = Core.Nothing,
      groupIds = Core.Nothing,
      groupNames = Core.Nothing,
      architecture = Core.Nothing,
      instanceInitiatedShutdownBehavior =
        Core.Nothing,
      monitoring = Core.Nothing,
      subnetId = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | Reserved.
importInstanceLaunchSpecification_additionalInfo :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Text)
importInstanceLaunchSpecification_additionalInfo = Lens.lens (\ImportInstanceLaunchSpecification' {additionalInfo} -> additionalInfo) (\s@ImportInstanceLaunchSpecification' {} a -> s {additionalInfo = a} :: ImportInstanceLaunchSpecification)

-- | The instance type. For more information about the instance types that
-- you can import, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types>
-- in the VM Import\/Export User Guide.
importInstanceLaunchSpecification_instanceType :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe InstanceType)
importInstanceLaunchSpecification_instanceType = Lens.lens (\ImportInstanceLaunchSpecification' {instanceType} -> instanceType) (\s@ImportInstanceLaunchSpecification' {} a -> s {instanceType = a} :: ImportInstanceLaunchSpecification)

-- | The Base64-encoded user data to make available to the instance.
importInstanceLaunchSpecification_userData :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe UserData)
importInstanceLaunchSpecification_userData = Lens.lens (\ImportInstanceLaunchSpecification' {userData} -> userData) (\s@ImportInstanceLaunchSpecification' {} a -> s {userData = a} :: ImportInstanceLaunchSpecification) Core.. Lens.mapping Core._Sensitive

-- | The placement information for the instance.
importInstanceLaunchSpecification_placement :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Placement)
importInstanceLaunchSpecification_placement = Lens.lens (\ImportInstanceLaunchSpecification' {placement} -> placement) (\s@ImportInstanceLaunchSpecification' {} a -> s {placement = a} :: ImportInstanceLaunchSpecification)

-- | The security group IDs.
importInstanceLaunchSpecification_groupIds :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Core.Text])
importInstanceLaunchSpecification_groupIds = Lens.lens (\ImportInstanceLaunchSpecification' {groupIds} -> groupIds) (\s@ImportInstanceLaunchSpecification' {} a -> s {groupIds = a} :: ImportInstanceLaunchSpecification) Core.. Lens.mapping Lens._Coerce

-- | The security group names.
importInstanceLaunchSpecification_groupNames :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe [Core.Text])
importInstanceLaunchSpecification_groupNames = Lens.lens (\ImportInstanceLaunchSpecification' {groupNames} -> groupNames) (\s@ImportInstanceLaunchSpecification' {} a -> s {groupNames = a} :: ImportInstanceLaunchSpecification) Core.. Lens.mapping Lens._Coerce

-- | The architecture of the instance.
importInstanceLaunchSpecification_architecture :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe ArchitectureValues)
importInstanceLaunchSpecification_architecture = Lens.lens (\ImportInstanceLaunchSpecification' {architecture} -> architecture) (\s@ImportInstanceLaunchSpecification' {} a -> s {architecture = a} :: ImportInstanceLaunchSpecification)

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe ShutdownBehavior)
importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior = Lens.lens (\ImportInstanceLaunchSpecification' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ImportInstanceLaunchSpecification' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ImportInstanceLaunchSpecification)

-- | Indicates whether monitoring is enabled.
importInstanceLaunchSpecification_monitoring :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Bool)
importInstanceLaunchSpecification_monitoring = Lens.lens (\ImportInstanceLaunchSpecification' {monitoring} -> monitoring) (\s@ImportInstanceLaunchSpecification' {} a -> s {monitoring = a} :: ImportInstanceLaunchSpecification)

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
importInstanceLaunchSpecification_subnetId :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Text)
importInstanceLaunchSpecification_subnetId = Lens.lens (\ImportInstanceLaunchSpecification' {subnetId} -> subnetId) (\s@ImportInstanceLaunchSpecification' {} a -> s {subnetId = a} :: ImportInstanceLaunchSpecification)

-- | [EC2-VPC] An available IP address from the IP address range of the
-- subnet.
importInstanceLaunchSpecification_privateIpAddress :: Lens.Lens' ImportInstanceLaunchSpecification (Core.Maybe Core.Text)
importInstanceLaunchSpecification_privateIpAddress = Lens.lens (\ImportInstanceLaunchSpecification' {privateIpAddress} -> privateIpAddress) (\s@ImportInstanceLaunchSpecification' {} a -> s {privateIpAddress = a} :: ImportInstanceLaunchSpecification)

instance
  Core.Hashable
    ImportInstanceLaunchSpecification

instance
  Core.NFData
    ImportInstanceLaunchSpecification

instance
  Core.ToQuery
    ImportInstanceLaunchSpecification
  where
  toQuery ImportInstanceLaunchSpecification' {..} =
    Core.mconcat
      [ "AdditionalInfo" Core.=: additionalInfo,
        "InstanceType" Core.=: instanceType,
        "UserData" Core.=: userData,
        "Placement" Core.=: placement,
        Core.toQuery
          (Core.toQueryList "GroupId" Core.<$> groupIds),
        Core.toQuery
          (Core.toQueryList "GroupName" Core.<$> groupNames),
        "Architecture" Core.=: architecture,
        "InstanceInitiatedShutdownBehavior"
          Core.=: instanceInitiatedShutdownBehavior,
        "Monitoring" Core.=: monitoring,
        "SubnetId" Core.=: subnetId,
        "PrivateIpAddress" Core.=: privateIpAddress
      ]
