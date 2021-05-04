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
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVpcSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVpcSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
--
-- /See:/ 'newDirectoryVpcSettings' smart constructor.
data DirectoryVpcSettings = DirectoryVpcSettings'
  { -- | The identifier of the VPC in which to create the directory.
    vpcId :: Prelude.Text,
    -- | The identifiers of the subnets for the directory servers. The two
    -- subnets must be in different Availability Zones. AWS Directory Service
    -- creates a directory server and a DNS server in each of these subnets.
    subnetIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- subnets must be in different Availability Zones. AWS Directory Service
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
-- subnets must be in different Availability Zones. AWS Directory Service
-- creates a directory server and a DNS server in each of these subnets.
directoryVpcSettings_subnetIds :: Lens.Lens' DirectoryVpcSettings [Prelude.Text]
directoryVpcSettings_subnetIds = Lens.lens (\DirectoryVpcSettings' {subnetIds} -> subnetIds) (\s@DirectoryVpcSettings' {} a -> s {subnetIds = a} :: DirectoryVpcSettings) Prelude.. Prelude._Coerce

instance Prelude.FromJSON DirectoryVpcSettings where
  parseJSON =
    Prelude.withObject
      "DirectoryVpcSettings"
      ( \x ->
          DirectoryVpcSettings'
            Prelude.<$> (x Prelude..: "VpcId")
            Prelude.<*> ( x Prelude..:? "SubnetIds"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DirectoryVpcSettings

instance Prelude.NFData DirectoryVpcSettings

instance Prelude.ToJSON DirectoryVpcSettings where
  toJSON DirectoryVpcSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("VpcId" Prelude..= vpcId),
            Prelude.Just ("SubnetIds" Prelude..= subnetIds)
          ]
      )
