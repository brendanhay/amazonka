{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    expiresAt :: Prelude.Maybe Prelude.POSIX,
    -- | The address to use to push container images to the container image
    -- registry of a Lightsail account.
    registry :: Prelude.Maybe Prelude.Text,
    -- | The container service registry password to use to push container images
    -- to the container image registry of a Lightsail account
    password :: Prelude.Maybe Prelude.Text,
    -- | The container service registry username to use to push container images
    -- to the container image registry of a Lightsail account.
    username :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'registry', 'containerServiceRegistryLogin_registry' - The address to use to push container images to the container image
-- registry of a Lightsail account.
--
-- 'password', 'containerServiceRegistryLogin_password' - The container service registry password to use to push container images
-- to the container image registry of a Lightsail account
--
-- 'username', 'containerServiceRegistryLogin_username' - The container service registry username to use to push container images
-- to the container image registry of a Lightsail account.
newContainerServiceRegistryLogin ::
  ContainerServiceRegistryLogin
newContainerServiceRegistryLogin =
  ContainerServiceRegistryLogin'
    { expiresAt =
        Prelude.Nothing,
      registry = Prelude.Nothing,
      password = Prelude.Nothing,
      username = Prelude.Nothing
    }

-- | The timestamp of when the container image registry username and password
-- expire.
--
-- The log in credentials expire 12 hours after they are created, at which
-- point you will need to create a new set of log in credentials using the
-- @CreateContainerServiceRegistryLogin@ action.
containerServiceRegistryLogin_expiresAt :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.UTCTime)
containerServiceRegistryLogin_expiresAt = Lens.lens (\ContainerServiceRegistryLogin' {expiresAt} -> expiresAt) (\s@ContainerServiceRegistryLogin' {} a -> s {expiresAt = a} :: ContainerServiceRegistryLogin) Prelude.. Lens.mapping Prelude._Time

-- | The address to use to push container images to the container image
-- registry of a Lightsail account.
containerServiceRegistryLogin_registry :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_registry = Lens.lens (\ContainerServiceRegistryLogin' {registry} -> registry) (\s@ContainerServiceRegistryLogin' {} a -> s {registry = a} :: ContainerServiceRegistryLogin)

-- | The container service registry password to use to push container images
-- to the container image registry of a Lightsail account
containerServiceRegistryLogin_password :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_password = Lens.lens (\ContainerServiceRegistryLogin' {password} -> password) (\s@ContainerServiceRegistryLogin' {} a -> s {password = a} :: ContainerServiceRegistryLogin)

-- | The container service registry username to use to push container images
-- to the container image registry of a Lightsail account.
containerServiceRegistryLogin_username :: Lens.Lens' ContainerServiceRegistryLogin (Prelude.Maybe Prelude.Text)
containerServiceRegistryLogin_username = Lens.lens (\ContainerServiceRegistryLogin' {username} -> username) (\s@ContainerServiceRegistryLogin' {} a -> s {username = a} :: ContainerServiceRegistryLogin)

instance
  Prelude.FromJSON
    ContainerServiceRegistryLogin
  where
  parseJSON =
    Prelude.withObject
      "ContainerServiceRegistryLogin"
      ( \x ->
          ContainerServiceRegistryLogin'
            Prelude.<$> (x Prelude..:? "expiresAt")
            Prelude.<*> (x Prelude..:? "registry")
            Prelude.<*> (x Prelude..:? "password")
            Prelude.<*> (x Prelude..:? "username")
      )

instance
  Prelude.Hashable
    ContainerServiceRegistryLogin

instance Prelude.NFData ContainerServiceRegistryLogin
