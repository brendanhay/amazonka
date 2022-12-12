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
-- Module      : Amazonka.FSx.Types.Alias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.Alias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AliasLifecycle
import qualified Amazonka.Prelude as Prelude

-- | A DNS alias that is associated with the file system. You can use a DNS
-- alias to access a file system using user-defined DNS names, in addition
-- to the default DNS name that Amazon FSx assigns to the file system. For
-- more information, see
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/managing-dns-aliases.html DNS aliases>
-- in the /FSx for Windows File Server User Guide/.
--
-- /See:/ 'newAlias' smart constructor.
data Alias = Alias'
  { -- | Describes the state of the DNS alias.
    --
    -- -   AVAILABLE - The DNS alias is associated with an Amazon FSx file
    --     system.
    --
    -- -   CREATING - Amazon FSx is creating the DNS alias and associating it
    --     with the file system.
    --
    -- -   CREATE_FAILED - Amazon FSx was unable to associate the DNS alias
    --     with the file system.
    --
    -- -   DELETING - Amazon FSx is disassociating the DNS alias from the file
    --     system and deleting it.
    --
    -- -   DELETE_FAILED - Amazon FSx was unable to disassociate the DNS alias
    --     from the file system.
    lifecycle :: Prelude.Maybe AliasLifecycle,
    -- | The name of the DNS alias. The alias name has to meet the following
    -- requirements:
    --
    -- -   Formatted as a fully-qualified domain name (FQDN),
    --     @hostname.domain@, for example, @accounting.example.com@.
    --
    -- -   Can contain alphanumeric characters, the underscore (_), and the
    --     hyphen (-).
    --
    -- -   Cannot start or end with a hyphen.
    --
    -- -   Can start with a numeric.
    --
    -- For DNS names, Amazon FSx stores alphabetic characters as lowercase
    -- letters (a-z), regardless of how you specify them: as uppercase letters,
    -- lowercase letters, or the corresponding letters in escape codes.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Alias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'alias_lifecycle' - Describes the state of the DNS alias.
--
-- -   AVAILABLE - The DNS alias is associated with an Amazon FSx file
--     system.
--
-- -   CREATING - Amazon FSx is creating the DNS alias and associating it
--     with the file system.
--
-- -   CREATE_FAILED - Amazon FSx was unable to associate the DNS alias
--     with the file system.
--
-- -   DELETING - Amazon FSx is disassociating the DNS alias from the file
--     system and deleting it.
--
-- -   DELETE_FAILED - Amazon FSx was unable to disassociate the DNS alias
--     from the file system.
--
-- 'name', 'alias_name' - The name of the DNS alias. The alias name has to meet the following
-- requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     @hostname.domain@, for example, @accounting.example.com@.
--
-- -   Can contain alphanumeric characters, the underscore (_), and the
--     hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS names, Amazon FSx stores alphabetic characters as lowercase
-- letters (a-z), regardless of how you specify them: as uppercase letters,
-- lowercase letters, or the corresponding letters in escape codes.
newAlias ::
  Alias
newAlias =
  Alias'
    { lifecycle = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Describes the state of the DNS alias.
--
-- -   AVAILABLE - The DNS alias is associated with an Amazon FSx file
--     system.
--
-- -   CREATING - Amazon FSx is creating the DNS alias and associating it
--     with the file system.
--
-- -   CREATE_FAILED - Amazon FSx was unable to associate the DNS alias
--     with the file system.
--
-- -   DELETING - Amazon FSx is disassociating the DNS alias from the file
--     system and deleting it.
--
-- -   DELETE_FAILED - Amazon FSx was unable to disassociate the DNS alias
--     from the file system.
alias_lifecycle :: Lens.Lens' Alias (Prelude.Maybe AliasLifecycle)
alias_lifecycle = Lens.lens (\Alias' {lifecycle} -> lifecycle) (\s@Alias' {} a -> s {lifecycle = a} :: Alias)

-- | The name of the DNS alias. The alias name has to meet the following
-- requirements:
--
-- -   Formatted as a fully-qualified domain name (FQDN),
--     @hostname.domain@, for example, @accounting.example.com@.
--
-- -   Can contain alphanumeric characters, the underscore (_), and the
--     hyphen (-).
--
-- -   Cannot start or end with a hyphen.
--
-- -   Can start with a numeric.
--
-- For DNS names, Amazon FSx stores alphabetic characters as lowercase
-- letters (a-z), regardless of how you specify them: as uppercase letters,
-- lowercase letters, or the corresponding letters in escape codes.
alias_name :: Lens.Lens' Alias (Prelude.Maybe Prelude.Text)
alias_name = Lens.lens (\Alias' {name} -> name) (\s@Alias' {} a -> s {name = a} :: Alias)

instance Data.FromJSON Alias where
  parseJSON =
    Data.withObject
      "Alias"
      ( \x ->
          Alias'
            Prelude.<$> (x Data..:? "Lifecycle")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable Alias where
  hashWithSalt _salt Alias' {..} =
    _salt `Prelude.hashWithSalt` lifecycle
      `Prelude.hashWithSalt` name

instance Prelude.NFData Alias where
  rnf Alias' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf name
