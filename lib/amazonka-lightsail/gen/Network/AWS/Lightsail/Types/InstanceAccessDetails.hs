{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceAccessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceAccessDetails
  ( InstanceAccessDetails (..),

    -- * Smart constructor
    mkInstanceAccessDetails,

    -- * Lenses
    iadHostKeys,
    iadCertKey,
    iadIpAddress,
    iadPrivateKey,
    iadExpiresAt,
    iadUsername,
    iadProtocol,
    iadPasswordData,
    iadPassword,
    iadInstanceName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
import Network.AWS.Lightsail.Types.PasswordData
import qualified Network.AWS.Prelude as Lude

-- | The parameters for gaining temporary access to one of your Amazon Lightsail instances.
--
-- /See:/ 'mkInstanceAccessDetails' smart constructor.
data InstanceAccessDetails = InstanceAccessDetails'
  { -- | Describes the public SSH host keys or the RDP certificate.
    hostKeys :: Lude.Maybe [HostKeyAttributes],
    -- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
    certKey :: Lude.Maybe Lude.Text,
    -- | The public IP address of the Amazon Lightsail instance.
    ipAddress :: Lude.Maybe Lude.Text,
    -- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
    privateKey :: Lude.Maybe Lude.Text,
    -- | For SSH access, the date on which the temporary keys expire.
    expiresAt :: Lude.Maybe Lude.Timestamp,
    -- | The user name to use when logging in to the Amazon Lightsail instance.
    username :: Lude.Maybe Lude.Text,
    -- | The protocol for these Amazon Lightsail instance access details.
    protocol :: Lude.Maybe InstanceAccessProtocol,
    -- | For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
    passwordData :: Lude.Maybe PasswordData,
    -- | For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
    password :: Lude.Maybe Lude.Text,
    -- | The name of this Amazon Lightsail instance.
    instanceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAccessDetails' with the minimum fields required to make a request.
--
-- * 'hostKeys' - Describes the public SSH host keys or the RDP certificate.
-- * 'certKey' - For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
-- * 'ipAddress' - The public IP address of the Amazon Lightsail instance.
-- * 'privateKey' - For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
-- * 'expiresAt' - For SSH access, the date on which the temporary keys expire.
-- * 'username' - The user name to use when logging in to the Amazon Lightsail instance.
-- * 'protocol' - The protocol for these Amazon Lightsail instance access details.
-- * 'passwordData' - For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
-- * 'password' - For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
-- * 'instanceName' - The name of this Amazon Lightsail instance.
mkInstanceAccessDetails ::
  InstanceAccessDetails
mkInstanceAccessDetails =
  InstanceAccessDetails'
    { hostKeys = Lude.Nothing,
      certKey = Lude.Nothing,
      ipAddress = Lude.Nothing,
      privateKey = Lude.Nothing,
      expiresAt = Lude.Nothing,
      username = Lude.Nothing,
      protocol = Lude.Nothing,
      passwordData = Lude.Nothing,
      password = Lude.Nothing,
      instanceName = Lude.Nothing
    }

-- | Describes the public SSH host keys or the RDP certificate.
--
-- /Note:/ Consider using 'hostKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadHostKeys :: Lens.Lens' InstanceAccessDetails (Lude.Maybe [HostKeyAttributes])
iadHostKeys = Lens.lens (hostKeys :: InstanceAccessDetails -> Lude.Maybe [HostKeyAttributes]) (\s a -> s {hostKeys = a} :: InstanceAccessDetails)
{-# DEPRECATED iadHostKeys "Use generic-lens or generic-optics with 'hostKeys' instead." #-}

-- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
--
-- /Note:/ Consider using 'certKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadCertKey :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadCertKey = Lens.lens (certKey :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {certKey = a} :: InstanceAccessDetails)
{-# DEPRECATED iadCertKey "Use generic-lens or generic-optics with 'certKey' instead." #-}

-- | The public IP address of the Amazon Lightsail instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadIpAddress :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadIpAddress = Lens.lens (ipAddress :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {ipAddress = a} :: InstanceAccessDetails)
{-# DEPRECATED iadIpAddress "Use generic-lens or generic-optics with 'ipAddress' instead." #-}

-- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPrivateKey :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadPrivateKey = Lens.lens (privateKey :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {privateKey = a} :: InstanceAccessDetails)
{-# DEPRECATED iadPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | For SSH access, the date on which the temporary keys expire.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadExpiresAt :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Timestamp)
iadExpiresAt = Lens.lens (expiresAt :: InstanceAccessDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiresAt = a} :: InstanceAccessDetails)
{-# DEPRECATED iadExpiresAt "Use generic-lens or generic-optics with 'expiresAt' instead." #-}

-- | The user name to use when logging in to the Amazon Lightsail instance.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadUsername :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadUsername = Lens.lens (username :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: InstanceAccessDetails)
{-# DEPRECATED iadUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The protocol for these Amazon Lightsail instance access details.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadProtocol :: Lens.Lens' InstanceAccessDetails (Lude.Maybe InstanceAccessProtocol)
iadProtocol = Lens.lens (protocol :: InstanceAccessDetails -> Lude.Maybe InstanceAccessProtocol) (\s a -> s {protocol = a} :: InstanceAccessDetails)
{-# DEPRECATED iadProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'passwordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPasswordData :: Lens.Lens' InstanceAccessDetails (Lude.Maybe PasswordData)
iadPasswordData = Lens.lens (passwordData :: InstanceAccessDetails -> Lude.Maybe PasswordData) (\s a -> s {passwordData = a} :: InstanceAccessDetails)
{-# DEPRECATED iadPasswordData "Use generic-lens or generic-optics with 'passwordData' instead." #-}

-- | For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPassword :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadPassword = Lens.lens (password :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: InstanceAccessDetails)
{-# DEPRECATED iadPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The name of this Amazon Lightsail instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadInstanceName :: Lens.Lens' InstanceAccessDetails (Lude.Maybe Lude.Text)
iadInstanceName = Lens.lens (instanceName :: InstanceAccessDetails -> Lude.Maybe Lude.Text) (\s a -> s {instanceName = a} :: InstanceAccessDetails)
{-# DEPRECATED iadInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.FromJSON InstanceAccessDetails where
  parseJSON =
    Lude.withObject
      "InstanceAccessDetails"
      ( \x ->
          InstanceAccessDetails'
            Lude.<$> (x Lude..:? "hostKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "certKey")
            Lude.<*> (x Lude..:? "ipAddress")
            Lude.<*> (x Lude..:? "privateKey")
            Lude.<*> (x Lude..:? "expiresAt")
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "protocol")
            Lude.<*> (x Lude..:? "passwordData")
            Lude.<*> (x Lude..:? "password")
            Lude.<*> (x Lude..:? "instanceName")
      )
