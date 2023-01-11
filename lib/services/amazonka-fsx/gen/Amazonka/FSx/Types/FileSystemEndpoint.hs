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
-- Module      : Amazonka.FSx.Types.FileSystemEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystemEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An Amazon FSx for NetApp ONTAP file system has two endpoints that are
-- used to access data or to manage the file system using the NetApp ONTAP
-- CLI, REST API, or NetApp SnapMirror. They are the @Management@ and
-- @Intercluster@ endpoints.
--
-- /See:/ 'newFileSystemEndpoint' smart constructor.
data FileSystemEndpoint = FileSystemEndpoint'
  { dNSName :: Prelude.Maybe Prelude.Text,
    -- | IP addresses of the file system endpoint.
    ipAddresses :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dNSName', 'fileSystemEndpoint_dNSName' - Undocumented member.
--
-- 'ipAddresses', 'fileSystemEndpoint_ipAddresses' - IP addresses of the file system endpoint.
newFileSystemEndpoint ::
  FileSystemEndpoint
newFileSystemEndpoint =
  FileSystemEndpoint'
    { dNSName = Prelude.Nothing,
      ipAddresses = Prelude.Nothing
    }

-- | Undocumented member.
fileSystemEndpoint_dNSName :: Lens.Lens' FileSystemEndpoint (Prelude.Maybe Prelude.Text)
fileSystemEndpoint_dNSName = Lens.lens (\FileSystemEndpoint' {dNSName} -> dNSName) (\s@FileSystemEndpoint' {} a -> s {dNSName = a} :: FileSystemEndpoint)

-- | IP addresses of the file system endpoint.
fileSystemEndpoint_ipAddresses :: Lens.Lens' FileSystemEndpoint (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
fileSystemEndpoint_ipAddresses = Lens.lens (\FileSystemEndpoint' {ipAddresses} -> ipAddresses) (\s@FileSystemEndpoint' {} a -> s {ipAddresses = a} :: FileSystemEndpoint) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FileSystemEndpoint where
  parseJSON =
    Data.withObject
      "FileSystemEndpoint"
      ( \x ->
          FileSystemEndpoint'
            Prelude.<$> (x Data..:? "DNSName")
            Prelude.<*> (x Data..:? "IpAddresses")
      )

instance Prelude.Hashable FileSystemEndpoint where
  hashWithSalt _salt FileSystemEndpoint' {..} =
    _salt `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` ipAddresses

instance Prelude.NFData FileSystemEndpoint where
  rnf FileSystemEndpoint' {..} =
    Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf ipAddresses
