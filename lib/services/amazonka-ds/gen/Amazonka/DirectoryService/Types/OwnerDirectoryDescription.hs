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
-- Module      : Amazonka.DirectoryService.Types.OwnerDirectoryDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.OwnerDirectoryDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.DirectoryVpcSettingsDescription
import Amazonka.DirectoryService.Types.RadiusSettings
import Amazonka.DirectoryService.Types.RadiusStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the directory owner account details that have been shared to
-- the directory consumer account.
--
-- /See:/ 'newOwnerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { -- | Identifier of the directory owner account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier of the Managed Microsoft AD directory in the directory owner
    -- account.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | IP address of the directory’s domain controllers.
    dnsIpAddrs :: Prelude.Maybe [Prelude.Text],
    -- | A RadiusSettings object that contains information about the RADIUS
    -- server.
    radiusSettings :: Prelude.Maybe RadiusSettings,
    -- | Information about the status of the RADIUS server.
    radiusStatus :: Prelude.Maybe RadiusStatus,
    -- | Information about the VPC settings for the directory.
    vpcSettings :: Prelude.Maybe DirectoryVpcSettingsDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OwnerDirectoryDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'ownerDirectoryDescription_accountId' - Identifier of the directory owner account.
--
-- 'directoryId', 'ownerDirectoryDescription_directoryId' - Identifier of the Managed Microsoft AD directory in the directory owner
-- account.
--
-- 'dnsIpAddrs', 'ownerDirectoryDescription_dnsIpAddrs' - IP address of the directory’s domain controllers.
--
-- 'radiusSettings', 'ownerDirectoryDescription_radiusSettings' - A RadiusSettings object that contains information about the RADIUS
-- server.
--
-- 'radiusStatus', 'ownerDirectoryDescription_radiusStatus' - Information about the status of the RADIUS server.
--
-- 'vpcSettings', 'ownerDirectoryDescription_vpcSettings' - Information about the VPC settings for the directory.
newOwnerDirectoryDescription ::
  OwnerDirectoryDescription
newOwnerDirectoryDescription =
  OwnerDirectoryDescription'
    { accountId =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      dnsIpAddrs = Prelude.Nothing,
      radiusSettings = Prelude.Nothing,
      radiusStatus = Prelude.Nothing,
      vpcSettings = Prelude.Nothing
    }

-- | Identifier of the directory owner account.
ownerDirectoryDescription_accountId :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe Prelude.Text)
ownerDirectoryDescription_accountId = Lens.lens (\OwnerDirectoryDescription' {accountId} -> accountId) (\s@OwnerDirectoryDescription' {} a -> s {accountId = a} :: OwnerDirectoryDescription)

-- | Identifier of the Managed Microsoft AD directory in the directory owner
-- account.
ownerDirectoryDescription_directoryId :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe Prelude.Text)
ownerDirectoryDescription_directoryId = Lens.lens (\OwnerDirectoryDescription' {directoryId} -> directoryId) (\s@OwnerDirectoryDescription' {} a -> s {directoryId = a} :: OwnerDirectoryDescription)

-- | IP address of the directory’s domain controllers.
ownerDirectoryDescription_dnsIpAddrs :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe [Prelude.Text])
ownerDirectoryDescription_dnsIpAddrs = Lens.lens (\OwnerDirectoryDescription' {dnsIpAddrs} -> dnsIpAddrs) (\s@OwnerDirectoryDescription' {} a -> s {dnsIpAddrs = a} :: OwnerDirectoryDescription) Prelude.. Lens.mapping Lens.coerced

-- | A RadiusSettings object that contains information about the RADIUS
-- server.
ownerDirectoryDescription_radiusSettings :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe RadiusSettings)
ownerDirectoryDescription_radiusSettings = Lens.lens (\OwnerDirectoryDescription' {radiusSettings} -> radiusSettings) (\s@OwnerDirectoryDescription' {} a -> s {radiusSettings = a} :: OwnerDirectoryDescription)

-- | Information about the status of the RADIUS server.
ownerDirectoryDescription_radiusStatus :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe RadiusStatus)
ownerDirectoryDescription_radiusStatus = Lens.lens (\OwnerDirectoryDescription' {radiusStatus} -> radiusStatus) (\s@OwnerDirectoryDescription' {} a -> s {radiusStatus = a} :: OwnerDirectoryDescription)

-- | Information about the VPC settings for the directory.
ownerDirectoryDescription_vpcSettings :: Lens.Lens' OwnerDirectoryDescription (Prelude.Maybe DirectoryVpcSettingsDescription)
ownerDirectoryDescription_vpcSettings = Lens.lens (\OwnerDirectoryDescription' {vpcSettings} -> vpcSettings) (\s@OwnerDirectoryDescription' {} a -> s {vpcSettings = a} :: OwnerDirectoryDescription)

instance Data.FromJSON OwnerDirectoryDescription where
  parseJSON =
    Data.withObject
      "OwnerDirectoryDescription"
      ( \x ->
          OwnerDirectoryDescription'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "DnsIpAddrs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RadiusSettings")
            Prelude.<*> (x Data..:? "RadiusStatus")
            Prelude.<*> (x Data..:? "VpcSettings")
      )

instance Prelude.Hashable OwnerDirectoryDescription where
  hashWithSalt _salt OwnerDirectoryDescription' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` dnsIpAddrs
      `Prelude.hashWithSalt` radiusSettings
      `Prelude.hashWithSalt` radiusStatus
      `Prelude.hashWithSalt` vpcSettings

instance Prelude.NFData OwnerDirectoryDescription where
  rnf OwnerDirectoryDescription' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf dnsIpAddrs
      `Prelude.seq` Prelude.rnf radiusSettings
      `Prelude.seq` Prelude.rnf radiusStatus
      `Prelude.seq` Prelude.rnf vpcSettings
