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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceRegistryLogin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the login information for the container image registry of an
-- Amazon Lightsail account.
--
-- /See:/ 'newContainerServiceRegistryLogin' smart constructor.
data ContainerServiceRegistryLogin = ContainerServiceRegistryLogin'
  { -- | The timestamp of when the container image registry username and password
    -- expire.
    --
    -- The log in credentials expire 12 hours after they are created, at which
    -- point you will need to create a new set of log in credentials using the
    -- @CreateContainerServiceRegistryLogin@ action.
    expiresAt :: Prelude.Maybe Data.POSIX,
    -- | The container service registry password to use to push container images
    -- to the container image registry of a Lightsail account
    password :: Prelude.Maybe Prelude.Text,
    -- | The address to use to push container images to the container image
    -- registry of a Lightsail account.
    registry :: Prelude.Maybe Prelude.Text,
    -- | The container service registry username to use to push container images
    -- to the container image registry of a Lightsail account.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceRegistryLogin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresAt', 'containerServiceRegistryLogin_expiresAt' - The timestamp of when the container image registry username and password
-- expire.
--
-- The log in credentials expire 12 hours after they are created, at which
-- point you will need to create a new set of log in credentials using the
-- @CreateContainerServiceRegistryLogin@ action.
--
-- 'password', 'containerServiceRegistryLogin_password' - The container service registry password to use to push container images
-- to the container image registry of a Lightsail account
--
-- 'registry', 'containerServiceRegistryLogin_registry' - The address to use to push container images to the container image
-- registry of a Lightsail account.
--
-- 'username', 'containerServiceRegistryLogin_username' - The container service registry username to use to push container images
-- to the container image registry of a Lightsail account.
newContainerServiceRegistryLogin ::
  ContainerServiceRegistryLogin
newContainerServiceRegistryLogin =
  ContainerServiceRegistryLogin'
    { expiresAt =
        Prelude.Nothing,
      password = Prelude.Nothing,
      registry = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The timestamp of when the container image registry username and password
-- expire.
--
-- The log in credentials expire 12 hours after they are created, at which
-- point you will need to create a new set of log in credentials using the
-- @CreateContainerServiceRegistryLogin@ action.
containerServiceRegistryLogin_expiresAt :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.UTCTime)
containerServiceRegistryLogin_expiresAt = Lens.lens (\ContainerServiceRegistryLogin' {expiresAt} -> expiresAt) (\s@ContainerServiceRegistryLogin' {} a -> s {expiresAt = a} :: ContainerServiceRegistryLogin) Prelude.. Lens.mapping Data._Time

-- | The container service registry password to use to push container images
-- to the container image registry of a Lightsail account
containerServiceRegistryLogin_password :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_password = Lens.lens (\ContainerServiceRegistryLogin' {password} -> password) (\s@ContainerServiceRegistryLogin' {} a -> s {password = a} :: ContainerServiceRegistryLogin)

-- | The address to use to push container images to the container image
-- registry of a Lightsail account.
containerServiceRegistryLogin_registry :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_registry = Lens.lens (\ContainerServiceRegistryLogin' {registry} -> registry) (\s@ContainerServiceRegistryLogin' {} a -> s {registry = a} :: ContainerServiceRegistryLogin)

-- | The container service registry username to use to push container images
-- to the container image registry of a Lightsail account.
containerServiceRegistryLogin_username :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_username = Lens.lens (\ContainerServiceRegistryLogin' {username} -> username) (\s@ContainerServiceRegistryLogin' {} a -> s {username = a} :: ContainerServiceRegistryLogin)

instance Data.FromJSON ContainerServiceRegistryLogin where
  parseJSON =
    Data.withObject
      "ContainerServiceRegistryLogin"
      ( \x ->
          ContainerServiceRegistryLogin'
            Prelude.<$> (x Data..:? "expiresAt")
            Prelude.<*> (x Data..:? "password")
            Prelude.<*> (x Data..:? "registry")
            Prelude.<*> (x Data..:? "username")
      )

instance
  Prelude.Hashable
    ContainerServiceRegistryLogin
  where
  hashWithSalt _salt ContainerServiceRegistryLogin' {..} =
    _salt
      `Prelude.hashWithSalt` expiresAt
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` registry
      `Prelude.hashWithSalt` username

instance Prelude.NFData ContainerServiceRegistryLogin where
  rnf ContainerServiceRegistryLogin' {..} =
    Prelude.rnf expiresAt `Prelude.seq`
      Prelude.rnf password `Prelude.seq`
        Prelude.rnf registry `Prelude.seq`
          Prelude.rnf username
