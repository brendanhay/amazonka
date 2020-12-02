{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceAccessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceAccessDetails where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Prelude

-- | The parameters for gaining temporary access to one of your Amazon Lightsail instances.
--
--
--
-- /See:/ 'instanceAccessDetails' smart constructor.
data InstanceAccessDetails = InstanceAccessDetails'
  { _iadHostKeys ::
      !(Maybe [HostKeyAttributes]),
    _iadCertKey :: !(Maybe Text),
    _iadIpAddress :: !(Maybe Text),
    _iadPrivateKey :: !(Maybe Text),
    _iadExpiresAt :: !(Maybe POSIX),
    _iadUsername :: !(Maybe Text),
    _iadProtocol :: !(Maybe InstanceAccessProtocol),
    _iadPasswordData :: !(Maybe PasswordData),
    _iadPassword :: !(Maybe Text),
    _iadInstanceName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAccessDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iadHostKeys' - Describes the public SSH host keys or the RDP certificate.
--
-- * 'iadCertKey' - For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
--
-- * 'iadIpAddress' - The public IP address of the Amazon Lightsail instance.
--
-- * 'iadPrivateKey' - For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
--
-- * 'iadExpiresAt' - For SSH access, the date on which the temporary keys expire.
--
-- * 'iadUsername' - The user name to use when logging in to the Amazon Lightsail instance.
--
-- * 'iadProtocol' - The protocol for these Amazon Lightsail instance access details.
--
-- * 'iadPasswordData' - For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- * 'iadPassword' - For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- * 'iadInstanceName' - The name of this Amazon Lightsail instance.
instanceAccessDetails ::
  InstanceAccessDetails
instanceAccessDetails =
  InstanceAccessDetails'
    { _iadHostKeys = Nothing,
      _iadCertKey = Nothing,
      _iadIpAddress = Nothing,
      _iadPrivateKey = Nothing,
      _iadExpiresAt = Nothing,
      _iadUsername = Nothing,
      _iadProtocol = Nothing,
      _iadPasswordData = Nothing,
      _iadPassword = Nothing,
      _iadInstanceName = Nothing
    }

-- | Describes the public SSH host keys or the RDP certificate.
iadHostKeys :: Lens' InstanceAccessDetails [HostKeyAttributes]
iadHostKeys = lens _iadHostKeys (\s a -> s {_iadHostKeys = a}) . _Default . _Coerce

-- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
iadCertKey :: Lens' InstanceAccessDetails (Maybe Text)
iadCertKey = lens _iadCertKey (\s a -> s {_iadCertKey = a})

-- | The public IP address of the Amazon Lightsail instance.
iadIpAddress :: Lens' InstanceAccessDetails (Maybe Text)
iadIpAddress = lens _iadIpAddress (\s a -> s {_iadIpAddress = a})

-- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
iadPrivateKey :: Lens' InstanceAccessDetails (Maybe Text)
iadPrivateKey = lens _iadPrivateKey (\s a -> s {_iadPrivateKey = a})

-- | For SSH access, the date on which the temporary keys expire.
iadExpiresAt :: Lens' InstanceAccessDetails (Maybe UTCTime)
iadExpiresAt = lens _iadExpiresAt (\s a -> s {_iadExpiresAt = a}) . mapping _Time

-- | The user name to use when logging in to the Amazon Lightsail instance.
iadUsername :: Lens' InstanceAccessDetails (Maybe Text)
iadUsername = lens _iadUsername (\s a -> s {_iadUsername = a})

-- | The protocol for these Amazon Lightsail instance access details.
iadProtocol :: Lens' InstanceAccessDetails (Maybe InstanceAccessProtocol)
iadProtocol = lens _iadProtocol (\s a -> s {_iadProtocol = a})

-- | For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
iadPasswordData :: Lens' InstanceAccessDetails (Maybe PasswordData)
iadPasswordData = lens _iadPasswordData (\s a -> s {_iadPasswordData = a})

-- | For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
iadPassword :: Lens' InstanceAccessDetails (Maybe Text)
iadPassword = lens _iadPassword (\s a -> s {_iadPassword = a})

-- | The name of this Amazon Lightsail instance.
iadInstanceName :: Lens' InstanceAccessDetails (Maybe Text)
iadInstanceName = lens _iadInstanceName (\s a -> s {_iadInstanceName = a})

instance FromJSON InstanceAccessDetails where
  parseJSON =
    withObject
      "InstanceAccessDetails"
      ( \x ->
          InstanceAccessDetails'
            <$> (x .:? "hostKeys" .!= mempty)
            <*> (x .:? "certKey")
            <*> (x .:? "ipAddress")
            <*> (x .:? "privateKey")
            <*> (x .:? "expiresAt")
            <*> (x .:? "username")
            <*> (x .:? "protocol")
            <*> (x .:? "passwordData")
            <*> (x .:? "password")
            <*> (x .:? "instanceName")
      )

instance Hashable InstanceAccessDetails

instance NFData InstanceAccessDetails
