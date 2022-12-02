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
-- Module      : Amazonka.FSx.Types.OpenZFSClientConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.OpenZFSClientConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies who can mount an OpenZFS file system and the options available
-- while mounting the file system.
--
-- /See:/ 'newOpenZFSClientConfiguration' smart constructor.
data OpenZFSClientConfiguration = OpenZFSClientConfiguration'
  { -- | A value that specifies who can mount the file system. You can provide a
    -- wildcard character (@*@), an IP address (@0.0.0.0@), or a CIDR address
    -- (@192.0.2.0\/24@). By default, Amazon FSx uses the wildcard character
    -- when specifying the client.
    clients :: Prelude.Text,
    -- | The options to use when mounting the file system. For a list of options
    -- that you can use with Network File System (NFS), see the
    -- <https://linux.die.net/man/5/exports exports(5) - Linux man page>. When
    -- choosing your options, consider the following:
    --
    -- -   @crossmnt@ is used by default. If you don\'t specify @crossmnt@ when
    --     changing the client configuration, you won\'t be able to see or
    --     access snapshots in your file system\'s snapshot directory.
    --
    -- -   @sync@ is used by default. If you instead specify @async@, the
    --     system acknowledges writes before writing to disk. If the system
    --     crashes before the writes are finished, you lose the unwritten data.
    options :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenZFSClientConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clients', 'openZFSClientConfiguration_clients' - A value that specifies who can mount the file system. You can provide a
-- wildcard character (@*@), an IP address (@0.0.0.0@), or a CIDR address
-- (@192.0.2.0\/24@). By default, Amazon FSx uses the wildcard character
-- when specifying the client.
--
-- 'options', 'openZFSClientConfiguration_options' - The options to use when mounting the file system. For a list of options
-- that you can use with Network File System (NFS), see the
-- <https://linux.die.net/man/5/exports exports(5) - Linux man page>. When
-- choosing your options, consider the following:
--
-- -   @crossmnt@ is used by default. If you don\'t specify @crossmnt@ when
--     changing the client configuration, you won\'t be able to see or
--     access snapshots in your file system\'s snapshot directory.
--
-- -   @sync@ is used by default. If you instead specify @async@, the
--     system acknowledges writes before writing to disk. If the system
--     crashes before the writes are finished, you lose the unwritten data.
newOpenZFSClientConfiguration ::
  -- | 'clients'
  Prelude.Text ->
  -- | 'options'
  Prelude.NonEmpty Prelude.Text ->
  OpenZFSClientConfiguration
newOpenZFSClientConfiguration pClients_ pOptions_ =
  OpenZFSClientConfiguration'
    { clients = pClients_,
      options = Lens.coerced Lens.# pOptions_
    }

-- | A value that specifies who can mount the file system. You can provide a
-- wildcard character (@*@), an IP address (@0.0.0.0@), or a CIDR address
-- (@192.0.2.0\/24@). By default, Amazon FSx uses the wildcard character
-- when specifying the client.
openZFSClientConfiguration_clients :: Lens.Lens' OpenZFSClientConfiguration Prelude.Text
openZFSClientConfiguration_clients = Lens.lens (\OpenZFSClientConfiguration' {clients} -> clients) (\s@OpenZFSClientConfiguration' {} a -> s {clients = a} :: OpenZFSClientConfiguration)

-- | The options to use when mounting the file system. For a list of options
-- that you can use with Network File System (NFS), see the
-- <https://linux.die.net/man/5/exports exports(5) - Linux man page>. When
-- choosing your options, consider the following:
--
-- -   @crossmnt@ is used by default. If you don\'t specify @crossmnt@ when
--     changing the client configuration, you won\'t be able to see or
--     access snapshots in your file system\'s snapshot directory.
--
-- -   @sync@ is used by default. If you instead specify @async@, the
--     system acknowledges writes before writing to disk. If the system
--     crashes before the writes are finished, you lose the unwritten data.
openZFSClientConfiguration_options :: Lens.Lens' OpenZFSClientConfiguration (Prelude.NonEmpty Prelude.Text)
openZFSClientConfiguration_options = Lens.lens (\OpenZFSClientConfiguration' {options} -> options) (\s@OpenZFSClientConfiguration' {} a -> s {options = a} :: OpenZFSClientConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON OpenZFSClientConfiguration where
  parseJSON =
    Data.withObject
      "OpenZFSClientConfiguration"
      ( \x ->
          OpenZFSClientConfiguration'
            Prelude.<$> (x Data..: "Clients")
            Prelude.<*> (x Data..: "Options")
      )

instance Prelude.Hashable OpenZFSClientConfiguration where
  hashWithSalt _salt OpenZFSClientConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clients
      `Prelude.hashWithSalt` options

instance Prelude.NFData OpenZFSClientConfiguration where
  rnf OpenZFSClientConfiguration' {..} =
    Prelude.rnf clients
      `Prelude.seq` Prelude.rnf options

instance Data.ToJSON OpenZFSClientConfiguration where
  toJSON OpenZFSClientConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Clients" Data..= clients),
            Prelude.Just ("Options" Data..= options)
          ]
      )
