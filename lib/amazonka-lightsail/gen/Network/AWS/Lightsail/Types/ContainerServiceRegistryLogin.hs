{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the login information for the container image registry of an Amazon Lightsail account.
--
--
--
-- /See:/ 'containerServiceRegistryLogin' smart constructor.
data ContainerServiceRegistryLogin = ContainerServiceRegistryLogin'
  { _csrlExpiresAt ::
      !(Maybe POSIX),
    _csrlUsername :: !(Maybe Text),
    _csrlPassword :: !(Maybe Text),
    _csrlRegistry :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerServiceRegistryLogin' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrlExpiresAt' - The timestamp of when the container image registry username and password expire. The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials using the @CreateContainerServiceRegistryLogin@ action.
--
-- * 'csrlUsername' - The container service registry username to use to push container images to the container image registry of a Lightsail account.
--
-- * 'csrlPassword' - The container service registry password to use to push container images to the container image registry of a Lightsail account
--
-- * 'csrlRegistry' - The address to use to push container images to the container image registry of a Lightsail account.
containerServiceRegistryLogin ::
  ContainerServiceRegistryLogin
containerServiceRegistryLogin =
  ContainerServiceRegistryLogin'
    { _csrlExpiresAt = Nothing,
      _csrlUsername = Nothing,
      _csrlPassword = Nothing,
      _csrlRegistry = Nothing
    }

-- | The timestamp of when the container image registry username and password expire. The log in credentials expire 12 hours after they are created, at which point you will need to create a new set of log in credentials using the @CreateContainerServiceRegistryLogin@ action.
csrlExpiresAt :: Lens' ContainerServiceRegistryLogin (Maybe UTCTime)
csrlExpiresAt = lens _csrlExpiresAt (\s a -> s {_csrlExpiresAt = a}) . mapping _Time

-- | The container service registry username to use to push container images to the container image registry of a Lightsail account.
csrlUsername :: Lens' ContainerServiceRegistryLogin (Maybe Text)
csrlUsername = lens _csrlUsername (\s a -> s {_csrlUsername = a})

-- | The container service registry password to use to push container images to the container image registry of a Lightsail account
csrlPassword :: Lens' ContainerServiceRegistryLogin (Maybe Text)
csrlPassword = lens _csrlPassword (\s a -> s {_csrlPassword = a})

-- | The address to use to push container images to the container image registry of a Lightsail account.
csrlRegistry :: Lens' ContainerServiceRegistryLogin (Maybe Text)
csrlRegistry = lens _csrlRegistry (\s a -> s {_csrlRegistry = a})

instance FromJSON ContainerServiceRegistryLogin where
  parseJSON =
    withObject
      "ContainerServiceRegistryLogin"
      ( \x ->
          ContainerServiceRegistryLogin'
            <$> (x .:? "expiresAt")
            <*> (x .:? "username")
            <*> (x .:? "password")
            <*> (x .:? "registry")
      )

instance Hashable ContainerServiceRegistryLogin

instance NFData ContainerServiceRegistryLogin
