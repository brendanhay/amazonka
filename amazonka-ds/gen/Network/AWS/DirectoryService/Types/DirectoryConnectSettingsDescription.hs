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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'newDirectoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { -- | The security group identifier for the AD Connector directory.
    securityGroupId :: Core.Maybe Core.Text,
    -- | A list of the Availability Zones that the directory is in.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | A list of subnet identifiers in the VPC that the AD Connector is in.
    subnetIds :: Core.Maybe [Core.Text],
    -- | The user name of the service account in the on-premises directory.
    customerUserName :: Core.Maybe Core.Text,
    -- | The IP addresses of the AD Connector servers.
    connectIps :: Core.Maybe [Core.Text],
    -- | The identifier of the VPC that the AD Connector is in.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DirectoryConnectSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupId', 'directoryConnectSettingsDescription_securityGroupId' - The security group identifier for the AD Connector directory.
--
-- 'availabilityZones', 'directoryConnectSettingsDescription_availabilityZones' - A list of the Availability Zones that the directory is in.
--
-- 'subnetIds', 'directoryConnectSettingsDescription_subnetIds' - A list of subnet identifiers in the VPC that the AD Connector is in.
--
-- 'customerUserName', 'directoryConnectSettingsDescription_customerUserName' - The user name of the service account in the on-premises directory.
--
-- 'connectIps', 'directoryConnectSettingsDescription_connectIps' - The IP addresses of the AD Connector servers.
--
-- 'vpcId', 'directoryConnectSettingsDescription_vpcId' - The identifier of the VPC that the AD Connector is in.
newDirectoryConnectSettingsDescription ::
  DirectoryConnectSettingsDescription
newDirectoryConnectSettingsDescription =
  DirectoryConnectSettingsDescription'
    { securityGroupId =
        Core.Nothing,
      availabilityZones = Core.Nothing,
      subnetIds = Core.Nothing,
      customerUserName = Core.Nothing,
      connectIps = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The security group identifier for the AD Connector directory.
directoryConnectSettingsDescription_securityGroupId :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Core.Text)
directoryConnectSettingsDescription_securityGroupId = Lens.lens (\DirectoryConnectSettingsDescription' {securityGroupId} -> securityGroupId) (\s@DirectoryConnectSettingsDescription' {} a -> s {securityGroupId = a} :: DirectoryConnectSettingsDescription)

-- | A list of the Availability Zones that the directory is in.
directoryConnectSettingsDescription_availabilityZones :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Core.Text])
directoryConnectSettingsDescription_availabilityZones = Lens.lens (\DirectoryConnectSettingsDescription' {availabilityZones} -> availabilityZones) (\s@DirectoryConnectSettingsDescription' {} a -> s {availabilityZones = a} :: DirectoryConnectSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | A list of subnet identifiers in the VPC that the AD Connector is in.
directoryConnectSettingsDescription_subnetIds :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Core.Text])
directoryConnectSettingsDescription_subnetIds = Lens.lens (\DirectoryConnectSettingsDescription' {subnetIds} -> subnetIds) (\s@DirectoryConnectSettingsDescription' {} a -> s {subnetIds = a} :: DirectoryConnectSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The user name of the service account in the on-premises directory.
directoryConnectSettingsDescription_customerUserName :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Core.Text)
directoryConnectSettingsDescription_customerUserName = Lens.lens (\DirectoryConnectSettingsDescription' {customerUserName} -> customerUserName) (\s@DirectoryConnectSettingsDescription' {} a -> s {customerUserName = a} :: DirectoryConnectSettingsDescription)

-- | The IP addresses of the AD Connector servers.
directoryConnectSettingsDescription_connectIps :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe [Core.Text])
directoryConnectSettingsDescription_connectIps = Lens.lens (\DirectoryConnectSettingsDescription' {connectIps} -> connectIps) (\s@DirectoryConnectSettingsDescription' {} a -> s {connectIps = a} :: DirectoryConnectSettingsDescription) Core.. Lens.mapping Lens._Coerce

-- | The identifier of the VPC that the AD Connector is in.
directoryConnectSettingsDescription_vpcId :: Lens.Lens' DirectoryConnectSettingsDescription (Core.Maybe Core.Text)
directoryConnectSettingsDescription_vpcId = Lens.lens (\DirectoryConnectSettingsDescription' {vpcId} -> vpcId) (\s@DirectoryConnectSettingsDescription' {} a -> s {vpcId = a} :: DirectoryConnectSettingsDescription)

instance
  Core.FromJSON
    DirectoryConnectSettingsDescription
  where
  parseJSON =
    Core.withObject
      "DirectoryConnectSettingsDescription"
      ( \x ->
          DirectoryConnectSettingsDescription'
            Core.<$> (x Core..:? "SecurityGroupId")
            Core.<*> (x Core..:? "AvailabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "CustomerUserName")
            Core.<*> (x Core..:? "ConnectIps" Core..!= Core.mempty)
            Core.<*> (x Core..:? "VpcId")
      )

instance
  Core.Hashable
    DirectoryConnectSettingsDescription

instance
  Core.NFData
    DirectoryConnectSettingsDescription
