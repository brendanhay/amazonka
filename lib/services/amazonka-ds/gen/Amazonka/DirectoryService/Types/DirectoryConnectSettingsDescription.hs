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
-- Module      : Amazonka.DirectoryService.Types.DirectoryConnectSettingsDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryConnectSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'newDirectoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { -- | A list of the Availability Zones that the directory is in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The IP addresses of the AD Connector servers.
    connectIps :: Prelude.Maybe [Prelude.Text],
    -- | The user name of the service account in your self-managed directory.
    customerUserName :: Prelude.Maybe Prelude.Text,
    -- | The security group identifier for the AD Connector directory.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | A list of subnet identifiers in the VPC that the AD Connector is in.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the VPC that the AD Connector is in.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryConnectSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'directoryConnectSettingsDescription_availabilityZones' - A list of the Availability Zones that the directory is in.
--
-- 'connectIps', 'directoryConnectSettingsDescription_connectIps' - The IP addresses of the AD Connector servers.
--
-- 'customerUserName', 'directoryConnectSettingsDescription_customerUserName' - The user name of the service account in your self-managed directory.
--
-- 'securityGroupId', 'directoryConnectSettingsDescription_securityGroupId' - The security group identifier for the AD Connector directory.
--
-- 'subnetIds', 'directoryConnectSettingsDescription_subnetIds' - A list of subnet identifiers in the VPC that the AD Connector is in.
--
-- 'vpcId', 'directoryConnectSettingsDescription_vpcId' - The identifier of the VPC that the AD Connector is in.
newDirectoryConnectSettingsDescription ::
  DirectoryConnectSettingsDescription
newDirectoryConnectSettingsDescription =
  DirectoryConnectSettingsDescription'
    { availabilityZones =
        Prelude.Nothing,
      connectIps = Prelude.Nothing,
      customerUserName = Prelude.Nothing,
      securityGroupId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | A list of the Availability Zones that the directory is in.
directoryConnectSettingsDescription_availabilityZones :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe [Prelude.Text])
directoryConnectSettingsDescription_availabilityZones = Lens.lens (\DirectoryConnectSettingsDescription' {availabilityZones} -> availabilityZones) (\s@DirectoryConnectSettingsDescription' {} a -> s {availabilityZones = a} :: DirectoryConnectSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The IP addresses of the AD Connector servers.
directoryConnectSettingsDescription_connectIps :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe [Prelude.Text])
directoryConnectSettingsDescription_connectIps = Lens.lens (\DirectoryConnectSettingsDescription' {connectIps} -> connectIps) (\s@DirectoryConnectSettingsDescription' {} a -> s {connectIps = a} :: DirectoryConnectSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The user name of the service account in your self-managed directory.
directoryConnectSettingsDescription_customerUserName :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe Prelude.Text)
directoryConnectSettingsDescription_customerUserName = Lens.lens (\DirectoryConnectSettingsDescription' {customerUserName} -> customerUserName) (\s@DirectoryConnectSettingsDescription' {} a -> s {customerUserName = a} :: DirectoryConnectSettingsDescription)

-- | The security group identifier for the AD Connector directory.
directoryConnectSettingsDescription_securityGroupId :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe Prelude.Text)
directoryConnectSettingsDescription_securityGroupId = Lens.lens (\DirectoryConnectSettingsDescription' {securityGroupId} -> securityGroupId) (\s@DirectoryConnectSettingsDescription' {} a -> s {securityGroupId = a} :: DirectoryConnectSettingsDescription)

-- | A list of subnet identifiers in the VPC that the AD Connector is in.
directoryConnectSettingsDescription_subnetIds :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe [Prelude.Text])
directoryConnectSettingsDescription_subnetIds = Lens.lens (\DirectoryConnectSettingsDescription' {subnetIds} -> subnetIds) (\s@DirectoryConnectSettingsDescription' {} a -> s {subnetIds = a} :: DirectoryConnectSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC that the AD Connector is in.
directoryConnectSettingsDescription_vpcId :: Lens.Lens' DirectoryConnectSettingsDescription (Prelude.Maybe Prelude.Text)
directoryConnectSettingsDescription_vpcId = Lens.lens (\DirectoryConnectSettingsDescription' {vpcId} -> vpcId) (\s@DirectoryConnectSettingsDescription' {} a -> s {vpcId = a} :: DirectoryConnectSettingsDescription)

instance
  Data.FromJSON
    DirectoryConnectSettingsDescription
  where
  parseJSON =
    Data.withObject
      "DirectoryConnectSettingsDescription"
      ( \x ->
          DirectoryConnectSettingsDescription'
            Prelude.<$> ( x
                            Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ConnectIps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CustomerUserName")
            Prelude.<*> (x Data..:? "SecurityGroupId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    DirectoryConnectSettingsDescription
  where
  hashWithSalt
    _salt
    DirectoryConnectSettingsDescription' {..} =
      _salt
        `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` connectIps
        `Prelude.hashWithSalt` customerUserName
        `Prelude.hashWithSalt` securityGroupId
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    DirectoryConnectSettingsDescription
  where
  rnf DirectoryConnectSettingsDescription' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf connectIps
      `Prelude.seq` Prelude.rnf customerUserName
      `Prelude.seq` Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
