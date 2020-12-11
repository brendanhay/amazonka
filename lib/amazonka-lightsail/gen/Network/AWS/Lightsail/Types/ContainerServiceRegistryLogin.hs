-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
  ( ContainerServiceRegistryLogin (..),

    -- * Smart constructor
    mkContainerServiceRegistryLogin,

    -- * Lenses
    csrlExpiresAt,
    csrlUsername,
    csrlPassword,
    csrlRegistry,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the login information for the container image registry of an Amazon Lightsail account.
--
-- /See:/ 'mkContainerServiceRegistryLogin' smart constructor.
data ContainerServiceRegistryLogin = ContainerServiceRegistryLogin'
  { expiresAt ::
      Lude.Maybe Lude.Timestamp,
    username ::
      Lude.Maybe Lude.Text,
    password ::
      Lude.Maybe Lude.Text,
    registry ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerServiceRegistryLogin' with the minimum fields required to make a request.
--
-- * 'expiresAt' - The timestamp of when the container image registry username and password expire.
--
-- The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials using the @CreateContainerServiceRegistryLogin@ action.
-- * 'password' - The container service registry password to use to push container images to the container image registry of a Lightsail account
-- * 'registry' - The address to use to push container images to the container image registry of a Lightsail account.
-- * 'username' - The container service registry username to use to push container images to the container image registry of a Lightsail account.
mkContainerServiceRegistryLogin ::
  ContainerServiceRegistryLogin
mkContainerServiceRegistryLogin =
  ContainerServiceRegistryLogin'
    { expiresAt = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      registry = Lude.Nothing
    }

-- | The timestamp of when the container image registry username and password expire.
--
-- The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials using the @CreateContainerServiceRegistryLogin@ action.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrlExpiresAt :: Lens.Lens' ContainerServiceRegistryLogin (Lude.Maybe Lude.Timestamp)
csrlExpiresAt = Lens.lens (expiresAt :: ContainerServiceRegistryLogin -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiresAt = a} :: ContainerServiceRegistryLogin)
{-# DEPRECATED csrlExpiresAt "Use generic-lens or generic-optics with 'expiresAt' instead." #-}

-- | The container service registry username to use to push container images to the container image registry of a Lightsail account.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrlUsername :: Lens.Lens' ContainerServiceRegistryLogin (Lude.Maybe Lude.Text)
csrlUsername = Lens.lens (username :: ContainerServiceRegistryLogin -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: ContainerServiceRegistryLogin)
{-# DEPRECATED csrlUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The container service registry password to use to push container images to the container image registry of a Lightsail account
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrlPassword :: Lens.Lens' ContainerServiceRegistryLogin (Lude.Maybe Lude.Text)
csrlPassword = Lens.lens (password :: ContainerServiceRegistryLogin -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: ContainerServiceRegistryLogin)
{-# DEPRECATED csrlPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The address to use to push container images to the container image registry of a Lightsail account.
--
-- /Note:/ Consider using 'registry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrlRegistry :: Lens.Lens' ContainerServiceRegistryLogin (Lude.Maybe Lude.Text)
csrlRegistry = Lens.lens (registry :: ContainerServiceRegistryLogin -> Lude.Maybe Lude.Text) (\s a -> s {registry = a} :: ContainerServiceRegistryLogin)
{-# DEPRECATED csrlRegistry "Use generic-lens or generic-optics with 'registry' instead." #-}

instance Lude.FromJSON ContainerServiceRegistryLogin where
  parseJSON =
    Lude.withObject
      "ContainerServiceRegistryLogin"
      ( \x ->
          ContainerServiceRegistryLogin'
            Lude.<$> (x Lude..:? "expiresAt")
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "password")
            Lude.<*> (x Lude..:? "registry")
      )
