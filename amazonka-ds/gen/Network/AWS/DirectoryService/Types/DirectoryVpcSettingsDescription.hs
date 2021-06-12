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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the directory.
--
-- /See:/ 'newDirectoryVpcSettingsDescription' smart constructor.
data DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription'
  { -- | The domain controller security group identifier for the directory.
    securityGroupId :: Core.Maybe Core.Text,
    -- | The list of Availability Zones that the directory is in.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The identifiers of the subnets for the directory servers.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The identifier of the VPC that the directory is in.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectoryVpcSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupId', 'directoryVpcSettingsDescription_securityGroupId' - The domain controller security group identifier for the directory.
--
-- 'availabilityZones', 'directoryVpcSettingsDescription_availabilityZones' - The list of Availability Zones that the directory is in.
--
-- 'subnetIds', 'directoryVpcSettingsDescription_subnetIds' - The identifiers of the subnets for the directory servers.
--
-- 'vpcId', 'directoryVpcSettingsDescription_vpcId' - The identifier of the VPC that the directory is in.
newDirectoryVpcSettingsDescription ::
  DirectoryVpcSettingsDescription
newDirectoryVpcSettingsDescription =
  DirectoryVpcSettingsDescription'
    { securityGroupId =
        Core.Nothing,
      availabilityZones = Core.Nothing,
      subnetIds = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The domain controller security group identifier for the directory.
directoryVpcSettingsDescription_securityGroupId :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe Core.Text)
directoryVpcSettingsDescription_securityGroupId = Lens.lens (\DirectoryVpcSettingsDescription' {securityGroupId} -> securityGroupId) (\s@DirectoryVpcSettingsDescription' {} a -> s {securityGroupId = a} :: DirectoryVpcSettingsDescription)

-- | The list of Availability Zones that the directory is in.
directoryVpcSettingsDescription_availabilityZones :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe [Core.Text])
directoryVpcSettingsDescription_availabilityZones = Lens.lens (\DirectoryVpcSettingsDescription' {availabilityZones} -> availabilityZones) (\s@DirectoryVpcSettingsDescription' {} a -> s {availabilityZones = a} :: DirectoryVpcSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The identifiers of the subnets for the directory servers.
directoryVpcSettingsDescription_subnetIds :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe [Core.Text])
directoryVpcSettingsDescription_subnetIds = Lens.lens (\DirectoryVpcSettingsDescription' {subnetIds} -> subnetIds) (\s@DirectoryVpcSettingsDescription' {} a -> s {subnetIds = a} :: DirectoryVpcSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the VPC that the directory is in.
directoryVpcSettingsDescription_vpcId :: Lens.Lens' DirectoryVpcSettingsDescription (Core.Maybe Core.Text)
directoryVpcSettingsDescription_vpcId = Lens.lens (\DirectoryVpcSettingsDescription' {vpcId} -> vpcId) (\s@DirectoryVpcSettingsDescription' {} a -> s {vpcId = a} :: DirectoryVpcSettingsDescription)

instance
  Core.FromJSON
    DirectoryVpcSettingsDescription
  where
  parseJSON =
    Core.withObject
      "DirectoryVpcSettingsDescription"
      ( \x ->
          DirectoryVpcSettingsDescription'
            Core.<$> (x Core..:? "SecurityGroupId")
            Core.<*> (x Core..:? "AvailabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VpcId")
      )

instance
  Core.Hashable
    DirectoryVpcSettingsDescription

instance Core.NFData DirectoryVpcSettingsDescription
