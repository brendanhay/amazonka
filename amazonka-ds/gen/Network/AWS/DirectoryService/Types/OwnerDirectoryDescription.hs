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
-- Module      : Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.OwnerDirectoryDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types.DirectoryVpcSettingsDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import qualified Network.AWS.Lens as Lens

-- | Describes the directory owner account details that have been shared to
-- the directory consumer account.
--
-- /See:/ 'newOwnerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { -- | Information about the status of the RADIUS server.
    radiusStatus :: Core.Maybe RadiusStatus,
    -- | Identifier of the directory owner account.
    accountId :: Core.Maybe Core.Text,
    -- | Information about the VPC settings for the directory.
    vpcSettings :: Core.Maybe DirectoryVpcSettingsDescription,
    -- | Identifier of the AWS Managed Microsoft AD directory in the directory
    -- owner account.
    directoryId :: Core.Maybe Core.Text,
    -- | IP address of the directory’s domain controllers.
    dnsIpAddrs :: Core.Maybe [Core.Text],
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server.
    radiusSettings :: Core.Maybe RadiusSettings
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'OwnerDirectoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'radiusStatus', 'ownerDirectoryDescription_radiusStatus' - Information about the status of the RADIUS server.
--
-- 'accountId', 'ownerDirectoryDescription_accountId' - Identifier of the directory owner account.
--
-- 'vpcSettings', 'ownerDirectoryDescription_vpcSettings' - Information about the VPC settings for the directory.
--
-- 'directoryId', 'ownerDirectoryDescription_directoryId' - Identifier of the AWS Managed Microsoft AD directory in the directory
-- owner account.
--
-- 'dnsIpAddrs', 'ownerDirectoryDescription_dnsIpAddrs' - IP address of the directory’s domain controllers.
--
-- 'radiusSettings', 'ownerDirectoryDescription_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server.
newOwnerDirectoryDescription ::
  OwnerDirectoryDescription
newOwnerDirectoryDescription =
  OwnerDirectoryDescription'
    { radiusStatus =
        Core.Nothing,
      accountId = Core.Nothing,
      vpcSettings = Core.Nothing,
      directoryId = Core.Nothing,
      dnsIpAddrs = Core.Nothing,
      radiusSettings = Core.Nothing
    }

-- | Information about the status of the RADIUS server.
ownerDirectoryDescription_radiusStatus :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe RadiusStatus)
ownerDirectoryDescription_radiusStatus = Lens.lens (\OwnerDirectoryDescription' {radiusStatus} -> radiusStatus) (\s@OwnerDirectoryDescription' {} a -> s {radiusStatus = a} :: OwnerDirectoryDescription)

-- | Identifier of the directory owner account.
ownerDirectoryDescription_accountId :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Core.Text)
ownerDirectoryDescription_accountId = Lens.lens (\OwnerDirectoryDescription' {accountId} -> accountId) (\s@OwnerDirectoryDescription' {} a -> s {accountId = a} :: OwnerDirectoryDescription)

-- | Information about the VPC settings for the directory.
ownerDirectoryDescription_vpcSettings :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe DirectoryVpcSettingsDescription)
ownerDirectoryDescription_vpcSettings = Lens.lens (\OwnerDirectoryDescription' {vpcSettings} -> vpcSettings) (\s@OwnerDirectoryDescription' {} a -> s {vpcSettings = a} :: OwnerDirectoryDescription)

-- | Identifier of the AWS Managed Microsoft AD directory in the directory
-- owner account.
ownerDirectoryDescription_directoryId :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe Core.Text)
ownerDirectoryDescription_directoryId = Lens.lens (\OwnerDirectoryDescription' {directoryId} -> directoryId) (\s@OwnerDirectoryDescription' {} a -> s {directoryId = a} :: OwnerDirectoryDescription)

-- | IP address of the directory’s domain controllers.
ownerDirectoryDescription_dnsIpAddrs :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe [Core.Text])
ownerDirectoryDescription_dnsIpAddrs = Lens.lens (\OwnerDirectoryDescription' {dnsIpAddrs} -> dnsIpAddrs) (\s@OwnerDirectoryDescription' {} a -> s {dnsIpAddrs = a} :: OwnerDirectoryDescription) Core.. Lens.mapping Lens._Coerce

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
ownerDirectoryDescription_radiusSettings :: Lens.Lens' OwnerDirectoryDescription (Core.Maybe RadiusSettings)
ownerDirectoryDescription_radiusSettings = Lens.lens (\OwnerDirectoryDescription' {radiusSettings} -> radiusSettings) (\s@OwnerDirectoryDescription' {} a -> s {radiusSettings = a} :: OwnerDirectoryDescription)

instance Core.FromJSON OwnerDirectoryDescription where
  parseJSON =
    Core.withObject
      "OwnerDirectoryDescription"
      ( \x ->
          OwnerDirectoryDescription'
            Core.<$> (x Core..:? "RadiusStatus")
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "VpcSettings")
            Core.<*> (x Core..:? "DirectoryId")
            Core.<*> (x Core..:? "DnsIpAddrs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "RadiusSettings")
      )

instance Core.Hashable OwnerDirectoryDescription

instance Core.NFData OwnerDirectoryDescription
