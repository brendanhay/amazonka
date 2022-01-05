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
-- Module      : Amazonka.DataSync.Types.NfsMountOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NfsMountOptions where

import qualified Amazonka.Core as Core
import Amazonka.DataSync.Types.NfsVersion
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the mount options that are available for DataSync to access
-- an NFS location.
--
-- /See:/ 'newNfsMountOptions' smart constructor.
data NfsMountOptions = NfsMountOptions'
  { -- | The specific NFS version that you want DataSync to use to mount your NFS
    -- share. If the server refuses to use the version specified, the sync will
    -- fail. If you don\'t specify a version, DataSync defaults to @AUTOMATIC@.
    -- That is, DataSync automatically selects a version based on negotiation
    -- with the NFS server.
    --
    -- You can specify the following NFS versions:
    --
    -- -   __<https://tools.ietf.org/html/rfc1813 NFSv3>__ - stateless protocol
    --     version that allows for asynchronous writes on the server.
    --
    -- -   __<https://tools.ietf.org/html/rfc3530 NFSv4.0>__ - stateful,
    --     firewall-friendly protocol version that supports delegations and
    --     pseudo filesystems.
    --
    -- -   __<https://tools.ietf.org/html/rfc5661 NFSv4.1>__ - stateful
    --     protocol version that supports sessions, directory delegations, and
    --     parallel data processing. Version 4.1 also includes all features
    --     available in version 4.0.
    version :: Prelude.Maybe NfsVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NfsMountOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'version', 'nfsMountOptions_version' - The specific NFS version that you want DataSync to use to mount your NFS
-- share. If the server refuses to use the version specified, the sync will
-- fail. If you don\'t specify a version, DataSync defaults to @AUTOMATIC@.
-- That is, DataSync automatically selects a version based on negotiation
-- with the NFS server.
--
-- You can specify the following NFS versions:
--
-- -   __<https://tools.ietf.org/html/rfc1813 NFSv3>__ - stateless protocol
--     version that allows for asynchronous writes on the server.
--
-- -   __<https://tools.ietf.org/html/rfc3530 NFSv4.0>__ - stateful,
--     firewall-friendly protocol version that supports delegations and
--     pseudo filesystems.
--
-- -   __<https://tools.ietf.org/html/rfc5661 NFSv4.1>__ - stateful
--     protocol version that supports sessions, directory delegations, and
--     parallel data processing. Version 4.1 also includes all features
--     available in version 4.0.
newNfsMountOptions ::
  NfsMountOptions
newNfsMountOptions =
  NfsMountOptions' {version = Prelude.Nothing}

-- | The specific NFS version that you want DataSync to use to mount your NFS
-- share. If the server refuses to use the version specified, the sync will
-- fail. If you don\'t specify a version, DataSync defaults to @AUTOMATIC@.
-- That is, DataSync automatically selects a version based on negotiation
-- with the NFS server.
--
-- You can specify the following NFS versions:
--
-- -   __<https://tools.ietf.org/html/rfc1813 NFSv3>__ - stateless protocol
--     version that allows for asynchronous writes on the server.
--
-- -   __<https://tools.ietf.org/html/rfc3530 NFSv4.0>__ - stateful,
--     firewall-friendly protocol version that supports delegations and
--     pseudo filesystems.
--
-- -   __<https://tools.ietf.org/html/rfc5661 NFSv4.1>__ - stateful
--     protocol version that supports sessions, directory delegations, and
--     parallel data processing. Version 4.1 also includes all features
--     available in version 4.0.
nfsMountOptions_version :: Lens.Lens' NfsMountOptions (Prelude.Maybe NfsVersion)
nfsMountOptions_version = Lens.lens (\NfsMountOptions' {version} -> version) (\s@NfsMountOptions' {} a -> s {version = a} :: NfsMountOptions)

instance Core.FromJSON NfsMountOptions where
  parseJSON =
    Core.withObject
      "NfsMountOptions"
      ( \x ->
          NfsMountOptions' Prelude.<$> (x Core..:? "Version")
      )

instance Prelude.Hashable NfsMountOptions where
  hashWithSalt _salt NfsMountOptions' {..} =
    _salt `Prelude.hashWithSalt` version

instance Prelude.NFData NfsMountOptions where
  rnf NfsMountOptions' {..} = Prelude.rnf version

instance Core.ToJSON NfsMountOptions where
  toJSON NfsMountOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Version" Core..=) Prelude.<$> version]
      )
