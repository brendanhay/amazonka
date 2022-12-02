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
-- Module      : Amazonka.DirectoryService.Types.DirectoryVpcSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.DirectoryVpcSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
--
-- /See:/ 'newDirectoryVpcSettings' smart constructor.
data DirectoryVpcSettings = DirectoryVpcSettings'
  { -- | The identifier of the VPC in which to create the directory.
    vpcId :: Prelude.Text,
    -- | The identifiers of the subnets for the directory servers. The two
    -- subnets must be in different Availability Zones. Directory Service
    -- creates a directory server and a DNS server in each of these subnets.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DirectoryVpcSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'directoryVpcSettings_vpcId' - The identifier of the VPC in which to create the directory.
--
-- 'subnetIds', 'directoryVpcSettings_subnetIds' - The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. Directory Service
-- creates a directory server and a DNS server in each of these subnets.
newDirectoryVpcSettings ::
  -- | 'vpcId'
  Prelude.Text ->
  DirectoryVpcSettings
newDirectoryVpcSettings pVpcId_ =
  DirectoryVpcSettings'
    { vpcId = pVpcId_,
      subnetIds = Prelude.mempty
    }

-- | The identifier of the VPC in which to create the directory.
directoryVpcSettings_vpcId :: Lens.Lens' DirectoryVpcSettings Prelude.Text
directoryVpcSettings_vpcId = Lens.lens (\DirectoryVpcSettings' {vpcId} -> vpcId) (\s@DirectoryVpcSettings' {} a -> s {vpcId = a} :: DirectoryVpcSettings)

-- | The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. Directory Service
-- creates a directory server and a DNS server in each of these subnets.
directoryVpcSettings_subnetIds :: Lens.Lens' DirectoryVpcSettings [Prelude.Text]
directoryVpcSettings_subnetIds = Lens.lens (\DirectoryVpcSettings' {subnetIds} -> subnetIds) (\s@DirectoryVpcSettings' {} a -> s {subnetIds = a} :: DirectoryVpcSettings) Prelude.. Lens.coerced

instance Data.FromJSON DirectoryVpcSettings where
  parseJSON =
    Data.withObject
      "DirectoryVpcSettings"
      ( \x ->
          DirectoryVpcSettings'
            Prelude.<$> (x Data..: "VpcId")
            Prelude.<*> (x Data..:? "SubnetIds" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DirectoryVpcSettings where
  hashWithSalt _salt DirectoryVpcSettings' {..} =
    _salt `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData DirectoryVpcSettings where
  rnf DirectoryVpcSettings' {..} =
    Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToJSON DirectoryVpcSettings where
  toJSON DirectoryVpcSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VpcId" Data..= vpcId),
            Prelude.Just ("SubnetIds" Data..= subnetIds)
          ]
      )
