{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.Placement
import Network.AWS.EC2.Types.ShutdownBehavior
import Network.AWS.EC2.Types.UserData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the launch specification for VM import.
--
-- /See:/ 'newImportInstanceLaunchSpecification' smart constructor.
data ImportInstanceLaunchSpecification = ImportInstanceLaunchSpecification'
  { -- | Reserved.
    additionalInfo :: Prelude.Maybe Prelude.Text,
    -- | The instance type. For more information about the instance types that
    -- you can import, see
    -- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types>
    -- in the VM Import\/Export User Guide.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The Base64-encoded user data to make available to the instance.
    userData :: Prelude.Maybe (Prelude.Sensitive UserData),
    -- | The placement information for the instance.
    placement :: Prelude.Maybe Placement,
    -- | The security group IDs.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | The security group names.
    groupNames :: Prelude.Maybe [Prelude.Text],
    -- | The architecture of the instance.
    architecture :: Prelude.Maybe ArchitectureValues,
    -- | Indicates whether an instance stops or terminates when you initiate
    -- shutdown from the instance (using the operating system command for
    -- system shutdown).
    instanceInitiatedShutdownBehavior :: Prelude.Maybe ShutdownBehavior,
    -- | Indicates whether monitoring is enabled.
    monitoring :: Prelude.Maybe Prelude.Bool,
    -- | [EC2-VPC] The ID of the subnet in which to launch the instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | [EC2-VPC] An available IP address from the IP address range of the
    -- subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      userData = Prelude.Nothing,
      placement = Prelude.Nothing,
      groupIds = Prelude.Nothing,
      groupNames = Prelude.Nothing,
      architecture = Prelude.Nothing,
      instanceInitiatedShutdownBehavior =
        Prelude.Nothing,
      monitoring = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | Reserved.
importInstanceLaunchSpecification_additionalInfo :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe Prelude.Text)
importInstanceLaunchSpecification_additionalInfo = Lens.lens (\ImportInstanceLaunchSpecification' {additionalInfo} -> additionalInfo) (\s@ImportInstanceLaunchSpecification' {} a -> s {additionalInfo = a} :: ImportInstanceLaunchSpecification)

-- | The instance type. For more information about the instance types that
-- you can import, see
-- <https://docs.aws.amazon.com/vm-import/latest/userguide/vmie_prereqs.html#vmimport-instance-types Instance Types>
-- in the VM Import\/Export User Guide.
importInstanceLaunchSpecification_instanceType :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe InstanceType)
importInstanceLaunchSpecification_instanceType = Lens.lens (\ImportInstanceLaunchSpecification' {instanceType} -> instanceType) (\s@ImportInstanceLaunchSpecification' {} a -> s {instanceType = a} :: ImportInstanceLaunchSpecification)

-- | The Base64-encoded user data to make available to the instance.
importInstanceLaunchSpecification_userData :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe UserData)
importInstanceLaunchSpecification_userData = Lens.lens (\ImportInstanceLaunchSpecification' {userData} -> userData) (\s@ImportInstanceLaunchSpecification' {} a -> s {userData = a} :: ImportInstanceLaunchSpecification) Prelude.. Lens.mapping Prelude._Sensitive

-- | The placement information for the instance.
importInstanceLaunchSpecification_placement :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe Placement)
importInstanceLaunchSpecification_placement = Lens.lens (\ImportInstanceLaunchSpecification' {placement} -> placement) (\s@ImportInstanceLaunchSpecification' {} a -> s {placement = a} :: ImportInstanceLaunchSpecification)

-- | The security group IDs.
importInstanceLaunchSpecification_groupIds :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe [Prelude.Text])
importInstanceLaunchSpecification_groupIds = Lens.lens (\ImportInstanceLaunchSpecification' {groupIds} -> groupIds) (\s@ImportInstanceLaunchSpecification' {} a -> s {groupIds = a} :: ImportInstanceLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The security group names.
importInstanceLaunchSpecification_groupNames :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe [Prelude.Text])
importInstanceLaunchSpecification_groupNames = Lens.lens (\ImportInstanceLaunchSpecification' {groupNames} -> groupNames) (\s@ImportInstanceLaunchSpecification' {} a -> s {groupNames = a} :: ImportInstanceLaunchSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The architecture of the instance.
importInstanceLaunchSpecification_architecture :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe ArchitectureValues)
importInstanceLaunchSpecification_architecture = Lens.lens (\ImportInstanceLaunchSpecification' {architecture} -> architecture) (\s@ImportInstanceLaunchSpecification' {} a -> s {architecture = a} :: ImportInstanceLaunchSpecification)

-- | Indicates whether an instance stops or terminates when you initiate
-- shutdown from the instance (using the operating system command for
-- system shutdown).
importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe ShutdownBehavior)
importInstanceLaunchSpecification_instanceInitiatedShutdownBehavior = Lens.lens (\ImportInstanceLaunchSpecification' {instanceInitiatedShutdownBehavior} -> instanceInitiatedShutdownBehavior) (\s@ImportInstanceLaunchSpecification' {} a -> s {instanceInitiatedShutdownBehavior = a} :: ImportInstanceLaunchSpecification)

-- | Indicates whether monitoring is enabled.
importInstanceLaunchSpecification_monitoring :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe Prelude.Bool)
importInstanceLaunchSpecification_monitoring = Lens.lens (\ImportInstanceLaunchSpecification' {monitoring} -> monitoring) (\s@ImportInstanceLaunchSpecification' {} a -> s {monitoring = a} :: ImportInstanceLaunchSpecification)

-- | [EC2-VPC] The ID of the subnet in which to launch the instance.
importInstanceLaunchSpecification_subnetId :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe Prelude.Text)
importInstanceLaunchSpecification_subnetId = Lens.lens (\ImportInstanceLaunchSpecification' {subnetId} -> subnetId) (\s@ImportInstanceLaunchSpecification' {} a -> s {subnetId = a} :: ImportInstanceLaunchSpecification)

-- | [EC2-VPC] An available IP address from the IP address range of the
-- subnet.
importInstanceLaunchSpecification_privateIpAddress :: Lens.Lens' ImportInstanceLaunchSpecification (Prelude.Maybe Prelude.Text)
importInstanceLaunchSpecification_privateIpAddress = Lens.lens (\ImportInstanceLaunchSpecification' {privateIpAddress} -> privateIpAddress) (\s@ImportInstanceLaunchSpecification' {} a -> s {privateIpAddress = a} :: ImportInstanceLaunchSpecification)

instance
  Prelude.Hashable
    ImportInstanceLaunchSpecification

instance
  Prelude.NFData
    ImportInstanceLaunchSpecification

instance
  Prelude.ToQuery
    ImportInstanceLaunchSpecification
  where
  toQuery ImportInstanceLaunchSpecification' {..} =
    Prelude.mconcat
      [ "AdditionalInfo" Prelude.=: additionalInfo,
        "InstanceType" Prelude.=: instanceType,
        "UserData" Prelude.=: userData,
        "Placement" Prelude.=: placement,
        Prelude.toQuery
          (Prelude.toQueryList "GroupId" Prelude.<$> groupIds),
        Prelude.toQuery
          ( Prelude.toQueryList "GroupName"
              Prelude.<$> groupNames
          ),
        "Architecture" Prelude.=: architecture,
        "InstanceInitiatedShutdownBehavior"
          Prelude.=: instanceInitiatedShutdownBehavior,
        "Monitoring" Prelude.=: monitoring,
        "SubnetId" Prelude.=: subnetId,
        "PrivateIpAddress" Prelude.=: privateIpAddress
      ]
