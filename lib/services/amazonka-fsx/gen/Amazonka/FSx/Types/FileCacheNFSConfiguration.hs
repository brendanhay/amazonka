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
-- Module      : Amazonka.FSx.Types.FileCacheNFSConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheNFSConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.NfsVersion
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an NFS data repository association (DRA) created
-- during the creation of the Amazon File Cache resource.
--
-- /See:/ 'newFileCacheNFSConfiguration' smart constructor.
data FileCacheNFSConfiguration = FileCacheNFSConfiguration'
  { -- | A list of up to 2 IP addresses of DNS servers used to resolve the NFS
    -- file system domain name. The provided IP addresses can either be the IP
    -- addresses of a DNS forwarder or resolver that the customer manages and
    -- runs inside the customer VPC, or the IP addresses of the on-premises DNS
    -- servers.
    dnsIps :: Prelude.Maybe [Prelude.Text],
    -- | The version of the NFS (Network File System) protocol of the NFS data
    -- repository. The only supported value is @NFS3@, which indicates that the
    -- data repository must support the NFSv3 protocol.
    version :: NfsVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheNFSConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsIps', 'fileCacheNFSConfiguration_dnsIps' - A list of up to 2 IP addresses of DNS servers used to resolve the NFS
-- file system domain name. The provided IP addresses can either be the IP
-- addresses of a DNS forwarder or resolver that the customer manages and
-- runs inside the customer VPC, or the IP addresses of the on-premises DNS
-- servers.
--
-- 'version', 'fileCacheNFSConfiguration_version' - The version of the NFS (Network File System) protocol of the NFS data
-- repository. The only supported value is @NFS3@, which indicates that the
-- data repository must support the NFSv3 protocol.
newFileCacheNFSConfiguration ::
  -- | 'version'
  NfsVersion ->
  FileCacheNFSConfiguration
newFileCacheNFSConfiguration pVersion_ =
  FileCacheNFSConfiguration'
    { dnsIps =
        Prelude.Nothing,
      version = pVersion_
    }

-- | A list of up to 2 IP addresses of DNS servers used to resolve the NFS
-- file system domain name. The provided IP addresses can either be the IP
-- addresses of a DNS forwarder or resolver that the customer manages and
-- runs inside the customer VPC, or the IP addresses of the on-premises DNS
-- servers.
fileCacheNFSConfiguration_dnsIps :: Lens.Lens' FileCacheNFSConfiguration (Prelude.Maybe [Prelude.Text])
fileCacheNFSConfiguration_dnsIps = Lens.lens (\FileCacheNFSConfiguration' {dnsIps} -> dnsIps) (\s@FileCacheNFSConfiguration' {} a -> s {dnsIps = a} :: FileCacheNFSConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The version of the NFS (Network File System) protocol of the NFS data
-- repository. The only supported value is @NFS3@, which indicates that the
-- data repository must support the NFSv3 protocol.
fileCacheNFSConfiguration_version :: Lens.Lens' FileCacheNFSConfiguration NfsVersion
fileCacheNFSConfiguration_version = Lens.lens (\FileCacheNFSConfiguration' {version} -> version) (\s@FileCacheNFSConfiguration' {} a -> s {version = a} :: FileCacheNFSConfiguration)

instance Prelude.Hashable FileCacheNFSConfiguration where
  hashWithSalt _salt FileCacheNFSConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dnsIps
      `Prelude.hashWithSalt` version

instance Prelude.NFData FileCacheNFSConfiguration where
  rnf FileCacheNFSConfiguration' {..} =
    Prelude.rnf dnsIps
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON FileCacheNFSConfiguration where
  toJSON FileCacheNFSConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DnsIps" Data..=) Prelude.<$> dnsIps,
            Prelude.Just ("Version" Data..= version)
          ]
      )
