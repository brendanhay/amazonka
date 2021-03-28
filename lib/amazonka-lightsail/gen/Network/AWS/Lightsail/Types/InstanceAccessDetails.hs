{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceAccessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceAccessDetails
  ( InstanceAccessDetails (..)
  -- * Smart constructor
  , mkInstanceAccessDetails
  -- * Lenses
  , iadCertKey
  , iadExpiresAt
  , iadHostKeys
  , iadInstanceName
  , iadIpAddress
  , iadPassword
  , iadPasswordData
  , iadPrivateKey
  , iadProtocol
  , iadUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.HostKeyAttributes as Types
import qualified Network.AWS.Lightsail.Types.InstanceAccessProtocol as Types
import qualified Network.AWS.Lightsail.Types.IpAddress as Types
import qualified Network.AWS.Lightsail.Types.PasswordData as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | The parameters for gaining temporary access to one of your Amazon Lightsail instances.
--
-- /See:/ 'mkInstanceAccessDetails' smart constructor.
data InstanceAccessDetails = InstanceAccessDetails'
  { certKey :: Core.Maybe Core.Text
    -- ^ For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
  , expiresAt :: Core.Maybe Core.NominalDiffTime
    -- ^ For SSH access, the date on which the temporary keys expire.
  , hostKeys :: Core.Maybe [Types.HostKeyAttributes]
    -- ^ Describes the public SSH host keys or the RDP certificate.
  , instanceName :: Core.Maybe Types.ResourceName
    -- ^ The name of this Amazon Lightsail instance.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The public IP address of the Amazon Lightsail instance.
  , password :: Core.Maybe Core.Text
    -- ^ For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
  , passwordData :: Core.Maybe Types.PasswordData
    -- ^ For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
  , privateKey :: Core.Maybe Core.Text
    -- ^ For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
  , protocol :: Core.Maybe Types.InstanceAccessProtocol
    -- ^ The protocol for these Amazon Lightsail instance access details.
  , username :: Core.Maybe Core.Text
    -- ^ The user name to use when logging in to the Amazon Lightsail instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceAccessDetails' value with any optional fields omitted.
mkInstanceAccessDetails
    :: InstanceAccessDetails
mkInstanceAccessDetails
  = InstanceAccessDetails'{certKey = Core.Nothing,
                           expiresAt = Core.Nothing, hostKeys = Core.Nothing,
                           instanceName = Core.Nothing, ipAddress = Core.Nothing,
                           password = Core.Nothing, passwordData = Core.Nothing,
                           privateKey = Core.Nothing, protocol = Core.Nothing,
                           username = Core.Nothing}

-- | For SSH access, the public key to use when accessing your instance For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey-cert.pub@ .
--
-- /Note:/ Consider using 'certKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadCertKey :: Lens.Lens' InstanceAccessDetails (Core.Maybe Core.Text)
iadCertKey = Lens.field @"certKey"
{-# INLINEABLE iadCertKey #-}
{-# DEPRECATED certKey "Use generic-lens or generic-optics with 'certKey' instead"  #-}

-- | For SSH access, the date on which the temporary keys expire.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadExpiresAt :: Lens.Lens' InstanceAccessDetails (Core.Maybe Core.NominalDiffTime)
iadExpiresAt = Lens.field @"expiresAt"
{-# INLINEABLE iadExpiresAt #-}
{-# DEPRECATED expiresAt "Use generic-lens or generic-optics with 'expiresAt' instead"  #-}

-- | Describes the public SSH host keys or the RDP certificate.
--
-- /Note:/ Consider using 'hostKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadHostKeys :: Lens.Lens' InstanceAccessDetails (Core.Maybe [Types.HostKeyAttributes])
iadHostKeys = Lens.field @"hostKeys"
{-# INLINEABLE iadHostKeys #-}
{-# DEPRECATED hostKeys "Use generic-lens or generic-optics with 'hostKeys' instead"  #-}

-- | The name of this Amazon Lightsail instance.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadInstanceName :: Lens.Lens' InstanceAccessDetails (Core.Maybe Types.ResourceName)
iadInstanceName = Lens.field @"instanceName"
{-# INLINEABLE iadInstanceName #-}
{-# DEPRECATED instanceName "Use generic-lens or generic-optics with 'instanceName' instead"  #-}

-- | The public IP address of the Amazon Lightsail instance.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadIpAddress :: Lens.Lens' InstanceAccessDetails (Core.Maybe Types.IpAddress)
iadIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE iadIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | For RDP access, the password for your Amazon Lightsail instance. Password will be an empty string if the password for your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPassword :: Lens.Lens' InstanceAccessDetails (Core.Maybe Core.Text)
iadPassword = Lens.field @"password"
{-# INLINEABLE iadPassword #-}
{-# DEPRECATED password "Use generic-lens or generic-optics with 'password' instead"  #-}

-- | For a Windows Server-based instance, an object with the data you can use to retrieve your password. This is only needed if @password@ is empty and the instance is not new (and therefore the password is not ready yet). When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'passwordData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPasswordData :: Lens.Lens' InstanceAccessDetails (Core.Maybe Types.PasswordData)
iadPasswordData = Lens.field @"passwordData"
{-# INLINEABLE iadPasswordData #-}
{-# DEPRECATED passwordData "Use generic-lens or generic-optics with 'passwordData' instead"  #-}

-- | For SSH access, the temporary private key. For OpenSSH clients (e.g., command line SSH), you should save this value to @tempkey@ ).
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadPrivateKey :: Lens.Lens' InstanceAccessDetails (Core.Maybe Core.Text)
iadPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE iadPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The protocol for these Amazon Lightsail instance access details.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadProtocol :: Lens.Lens' InstanceAccessDetails (Core.Maybe Types.InstanceAccessProtocol)
iadProtocol = Lens.field @"protocol"
{-# INLINEABLE iadProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The user name to use when logging in to the Amazon Lightsail instance.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iadUsername :: Lens.Lens' InstanceAccessDetails (Core.Maybe Core.Text)
iadUsername = Lens.field @"username"
{-# INLINEABLE iadUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON InstanceAccessDetails where
        parseJSON
          = Core.withObject "InstanceAccessDetails" Core.$
              \ x ->
                InstanceAccessDetails' Core.<$>
                  (x Core..:? "certKey") Core.<*> x Core..:? "expiresAt" Core.<*>
                    x Core..:? "hostKeys"
                    Core.<*> x Core..:? "instanceName"
                    Core.<*> x Core..:? "ipAddress"
                    Core.<*> x Core..:? "password"
                    Core.<*> x Core..:? "passwordData"
                    Core.<*> x Core..:? "privateKey"
                    Core.<*> x Core..:? "protocol"
                    Core.<*> x Core..:? "username"
