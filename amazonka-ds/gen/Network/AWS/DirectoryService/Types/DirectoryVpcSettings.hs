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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains VPC information for the CreateDirectory or CreateMicrosoftAD
-- operation.
--
-- /See:/ 'newDirectoryVpcSettings' smart constructor.
data DirectoryVpcSettings = DirectoryVpcSettings'
  { -- | The identifier of the VPC in which to create the directory.
    vpcId :: Core.Text,
    -- | The identifiers of the subnets for the directory servers. The two
    -- subnets must be in different Availability Zones. AWS Directory Service
    -- creates a directory server and a DNS server in each of these subnets.
    subnetIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DirectoryVpcSettings
newDirectoryVpcSettings pVpcId_ =
  DirectoryVpcSettings'
    { vpcId = pVpcId_,
      subnetIds = Core.mempty
    }

-- | The identifier of the VPC in which to create the directory.
directoryVpcSettings_vpcId :: Lens.Lens' DirectoryVpcSettings Core.Text
directoryVpcSettings_vpcId = Lens.lens (\DirectoryVpcSettings' {vpcId} -> vpcId) (\s@DirectoryVpcSettings' {} a -> s {vpcId = a} :: DirectoryVpcSettings)

-- | The identifiers of the subnets for the directory servers. The two
-- subnets must be in different Availability Zones. AWS Directory Service
-- creates a directory server and a DNS server in each of these subnets.
directoryVpcSettings_subnetIds :: Lens.Lens' DirectoryVpcSettings [Core.Text]
directoryVpcSettings_subnetIds = Lens.lens (\DirectoryVpcSettings' {subnetIds} -> subnetIds) (\s@DirectoryVpcSettings' {} a -> s {subnetIds = a} :: DirectoryVpcSettings) Core.. Lens._Coerce

instance Core.FromJSON DirectoryVpcSettings where
  parseJSON =
    Core.withObject
      "DirectoryVpcSettings"
      ( \x ->
          DirectoryVpcSettings'
            Core.<$> (x Core..: "VpcId")
            Core.<*> (x Core..:? "SubnetIds" Core..!= Core.mempty)
      )

instance Core.Hashable DirectoryVpcSettings

instance Core.NFData DirectoryVpcSettings

instance Core.ToJSON DirectoryVpcSettings where
  toJSON DirectoryVpcSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VpcId" Core..= vpcId),
            Core.Just ("SubnetIds" Core..= subnetIds)
          ]
      )
