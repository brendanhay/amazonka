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
-- Module      : Amazonka.FSx.Types.NFSDataRepositoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.NFSDataRepositoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AutoExportPolicy
import Amazonka.FSx.Types.NfsVersion
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a data repository association that links an Amazon
-- File Cache resource to an NFS data repository.
--
-- /See:/ 'newNFSDataRepositoryConfiguration' smart constructor.
data NFSDataRepositoryConfiguration = NFSDataRepositoryConfiguration'
  { -- | This parameter is not supported for Amazon File Cache.
    autoExportPolicy :: Prelude.Maybe AutoExportPolicy,
    -- | A list of up to 2 IP addresses of DNS servers used to resolve the NFS
    -- file system domain name. The provided IP addresses can either be the IP
    -- addresses of a DNS forwarder or resolver that the customer manages and
    -- runs inside the customer VPC, or the IP addresses of the on-premises DNS
    -- servers.
    dnsIps :: Prelude.Maybe [Prelude.Text],
    -- | The version of the NFS (Network File System) protocol of the NFS data
    -- repository. Currently, the only supported value is @NFS3@, which
    -- indicates that the data repository must support the NFSv3 protocol.
    version :: NfsVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NFSDataRepositoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoExportPolicy', 'nFSDataRepositoryConfiguration_autoExportPolicy' - This parameter is not supported for Amazon File Cache.
--
-- 'dnsIps', 'nFSDataRepositoryConfiguration_dnsIps' - A list of up to 2 IP addresses of DNS servers used to resolve the NFS
-- file system domain name. The provided IP addresses can either be the IP
-- addresses of a DNS forwarder or resolver that the customer manages and
-- runs inside the customer VPC, or the IP addresses of the on-premises DNS
-- servers.
--
-- 'version', 'nFSDataRepositoryConfiguration_version' - The version of the NFS (Network File System) protocol of the NFS data
-- repository. Currently, the only supported value is @NFS3@, which
-- indicates that the data repository must support the NFSv3 protocol.
newNFSDataRepositoryConfiguration ::
  -- | 'version'
  NfsVersion ->
  NFSDataRepositoryConfiguration
newNFSDataRepositoryConfiguration pVersion_ =
  NFSDataRepositoryConfiguration'
    { autoExportPolicy =
        Prelude.Nothing,
      dnsIps = Prelude.Nothing,
      version = pVersion_
    }

-- | This parameter is not supported for Amazon File Cache.
nFSDataRepositoryConfiguration_autoExportPolicy :: Lens.Lens' NFSDataRepositoryConfiguration (Prelude.Maybe AutoExportPolicy)
nFSDataRepositoryConfiguration_autoExportPolicy = Lens.lens (\NFSDataRepositoryConfiguration' {autoExportPolicy} -> autoExportPolicy) (\s@NFSDataRepositoryConfiguration' {} a -> s {autoExportPolicy = a} :: NFSDataRepositoryConfiguration)

-- | A list of up to 2 IP addresses of DNS servers used to resolve the NFS
-- file system domain name. The provided IP addresses can either be the IP
-- addresses of a DNS forwarder or resolver that the customer manages and
-- runs inside the customer VPC, or the IP addresses of the on-premises DNS
-- servers.
nFSDataRepositoryConfiguration_dnsIps :: Lens.Lens' NFSDataRepositoryConfiguration (Prelude.Maybe [Prelude.Text])
nFSDataRepositoryConfiguration_dnsIps = Lens.lens (\NFSDataRepositoryConfiguration' {dnsIps} -> dnsIps) (\s@NFSDataRepositoryConfiguration' {} a -> s {dnsIps = a} :: NFSDataRepositoryConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The version of the NFS (Network File System) protocol of the NFS data
-- repository. Currently, the only supported value is @NFS3@, which
-- indicates that the data repository must support the NFSv3 protocol.
nFSDataRepositoryConfiguration_version :: Lens.Lens' NFSDataRepositoryConfiguration NfsVersion
nFSDataRepositoryConfiguration_version = Lens.lens (\NFSDataRepositoryConfiguration' {version} -> version) (\s@NFSDataRepositoryConfiguration' {} a -> s {version = a} :: NFSDataRepositoryConfiguration)

instance Data.FromJSON NFSDataRepositoryConfiguration where
  parseJSON =
    Data.withObject
      "NFSDataRepositoryConfiguration"
      ( \x ->
          NFSDataRepositoryConfiguration'
            Prelude.<$> (x Data..:? "AutoExportPolicy")
            Prelude.<*> (x Data..:? "DnsIps" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Version")
      )

instance
  Prelude.Hashable
    NFSDataRepositoryConfiguration
  where
  hashWithSalt
    _salt
    NFSDataRepositoryConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoExportPolicy
        `Prelude.hashWithSalt` dnsIps
        `Prelude.hashWithSalt` version

instance
  Prelude.NFData
    NFSDataRepositoryConfiguration
  where
  rnf NFSDataRepositoryConfiguration' {..} =
    Prelude.rnf autoExportPolicy
      `Prelude.seq` Prelude.rnf dnsIps
      `Prelude.seq` Prelude.rnf version
