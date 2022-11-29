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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.NfsMountOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types.NfsVersion
import qualified Amazonka.Prelude as Prelude

-- | Specifies how DataSync can access a location using the NFS protocol.
--
-- /See:/ 'newNfsMountOptions' smart constructor.
data NfsMountOptions = NfsMountOptions'
  { -- | Specifies the NFS version that you want DataSync to use when mounting
    -- your NFS share. If the server refuses to use the version specified, the
    -- task fails.
    --
    -- You can specify the following options:
    --
    -- -   @AUTOMATIC@ (default): DataSync chooses NFS version 4.1.
    --
    -- -   @NFS3@: Stateless protocol version that allows for asynchronous
    --     writes on the server.
    --
    -- -   @NFSv4_0@: Stateful, firewall-friendly protocol version that
    --     supports delegations and pseudo file systems.
    --
    -- -   @NFSv4_1@: Stateful protocol version that supports sessions,
    --     directory delegations, and parallel data processing. NFS version 4.1
    --     also includes all features available in version 4.0.
    --
    -- DataSync currently only supports NFS version 3 with Amazon FSx for
    -- NetApp ONTAP locations.
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
-- 'version', 'nfsMountOptions_version' - Specifies the NFS version that you want DataSync to use when mounting
-- your NFS share. If the server refuses to use the version specified, the
-- task fails.
--
-- You can specify the following options:
--
-- -   @AUTOMATIC@ (default): DataSync chooses NFS version 4.1.
--
-- -   @NFS3@: Stateless protocol version that allows for asynchronous
--     writes on the server.
--
-- -   @NFSv4_0@: Stateful, firewall-friendly protocol version that
--     supports delegations and pseudo file systems.
--
-- -   @NFSv4_1@: Stateful protocol version that supports sessions,
--     directory delegations, and parallel data processing. NFS version 4.1
--     also includes all features available in version 4.0.
--
-- DataSync currently only supports NFS version 3 with Amazon FSx for
-- NetApp ONTAP locations.
newNfsMountOptions ::
  NfsMountOptions
newNfsMountOptions =
  NfsMountOptions' {version = Prelude.Nothing}

-- | Specifies the NFS version that you want DataSync to use when mounting
-- your NFS share. If the server refuses to use the version specified, the
-- task fails.
--
-- You can specify the following options:
--
-- -   @AUTOMATIC@ (default): DataSync chooses NFS version 4.1.
--
-- -   @NFS3@: Stateless protocol version that allows for asynchronous
--     writes on the server.
--
-- -   @NFSv4_0@: Stateful, firewall-friendly protocol version that
--     supports delegations and pseudo file systems.
--
-- -   @NFSv4_1@: Stateful protocol version that supports sessions,
--     directory delegations, and parallel data processing. NFS version 4.1
--     also includes all features available in version 4.0.
--
-- DataSync currently only supports NFS version 3 with Amazon FSx for
-- NetApp ONTAP locations.
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
