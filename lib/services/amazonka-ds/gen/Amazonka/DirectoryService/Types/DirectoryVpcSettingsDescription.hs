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
-- Module      : Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the directory.
--
-- /See:/ 'newDirectoryVpcSettingsDescription' smart constructor.
data DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription'
  { -- | The list of Availability Zones that the directory is in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The domain controller security group identifier for the directory.
    securityGroupId :: Prelude.Maybe Prelude.Text,
    -- | The identifiers of the subnets for the directory servers.
    subnetIds :: Prelude.Maybe [Prelude.Text],
    -- | The identifier of the VPC that the directory is in.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryVpcSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'directoryVpcSettingsDescription_availabilityZones' - The list of Availability Zones that the directory is in.
--
-- 'securityGroupId', 'directoryVpcSettingsDescription_securityGroupId' - The domain controller security group identifier for the directory.
--
-- 'subnetIds', 'directoryVpcSettingsDescription_subnetIds' - The identifiers of the subnets for the directory servers.
--
-- 'vpcId', 'directoryVpcSettingsDescription_vpcId' - The identifier of the VPC that the directory is in.
newDirectoryVpcSettingsDescription ::
  DirectoryVpcSettingsDescription
newDirectoryVpcSettingsDescription =
  DirectoryVpcSettingsDescription'
    { availabilityZones =
        Prelude.Nothing,
      securityGroupId = Prelude.Nothing,
      subnetIds = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The list of Availability Zones that the directory is in.
directoryVpcSettingsDescription_availabilityZones :: Lens.Lens' DirectoryVpcSettingsDescription (Prelude.Maybe [Prelude.Text])
directoryVpcSettingsDescription_availabilityZones = Lens.lens (\DirectoryVpcSettingsDescription' {availabilityZones} -> availabilityZones) (\s@DirectoryVpcSettingsDescription' {} a -> s {availabilityZones = a} :: DirectoryVpcSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The domain controller security group identifier for the directory.
directoryVpcSettingsDescription_securityGroupId :: Lens.Lens' DirectoryVpcSettingsDescription (Prelude.Maybe Prelude.Text)
directoryVpcSettingsDescription_securityGroupId = Lens.lens (\DirectoryVpcSettingsDescription' {securityGroupId} -> securityGroupId) (\s@DirectoryVpcSettingsDescription' {} a -> s {securityGroupId = a} :: DirectoryVpcSettingsDescription)

-- | The identifiers of the subnets for the directory servers.
directoryVpcSettingsDescription_subnetIds :: Lens.Lens' DirectoryVpcSettingsDescription (Prelude.Maybe [Prelude.Text])
directoryVpcSettingsDescription_subnetIds = Lens.lens (\DirectoryVpcSettingsDescription' {subnetIds} -> subnetIds) (\s@DirectoryVpcSettingsDescription' {} a -> s {subnetIds = a} :: DirectoryVpcSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC that the directory is in.
directoryVpcSettingsDescription_vpcId :: Lens.Lens' DirectoryVpcSettingsDescription (Prelude.Maybe Prelude.Text)
directoryVpcSettingsDescription_vpcId = Lens.lens (\DirectoryVpcSettingsDescription' {vpcId} -> vpcId) (\s@DirectoryVpcSettingsDescription' {} a -> s {vpcId = a} :: DirectoryVpcSettingsDescription)

instance
  Data.FromJSON
    DirectoryVpcSettingsDescription
  where
  parseJSON =
    Data.withObject
      "DirectoryVpcSettingsDescription"
      ( \x ->
          DirectoryVpcSettingsDescription'
            Prelude.<$> ( x Data..:? "AvailabilityZones"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SecurityGroupId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    DirectoryVpcSettingsDescription
  where
  hashWithSalt
    _salt
    DirectoryVpcSettingsDescription' {..} =
      _salt `Prelude.hashWithSalt` availabilityZones
        `Prelude.hashWithSalt` securityGroupId
        `Prelude.hashWithSalt` subnetIds
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    DirectoryVpcSettingsDescription
  where
  rnf DirectoryVpcSettingsDescription' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf securityGroupId
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf vpcId
