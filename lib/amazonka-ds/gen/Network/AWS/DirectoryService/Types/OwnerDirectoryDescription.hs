{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.OwnerDirectoryDescription
  ( OwnerDirectoryDescription (..),

    -- * Smart constructor
    mkOwnerDirectoryDescription,

    -- * Lenses
    oddRadiusStatus,
    oddDirectoryId,
    oddRadiusSettings,
    oddAccountId,
    oddDNSIPAddrs,
    oddVPCSettings,
  )
where

import Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
import Network.AWS.DirectoryService.Types.RadiusSettings
import Network.AWS.DirectoryService.Types.RadiusStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the directory owner account details that have been shared to the directory consumer account.
--
-- /See:/ 'mkOwnerDirectoryDescription' smart constructor.
data OwnerDirectoryDescription = OwnerDirectoryDescription'
  { radiusStatus ::
      Lude.Maybe RadiusStatus,
    directoryId :: Lude.Maybe Lude.Text,
    radiusSettings ::
      Lude.Maybe RadiusSettings,
    accountId :: Lude.Maybe Lude.Text,
    dnsIPAddrs :: Lude.Maybe [Lude.Text],
    vpcSettings ::
      Lude.Maybe
        DirectoryVPCSettingsDescription
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OwnerDirectoryDescription' with the minimum fields required to make a request.
--
-- * 'accountId' - Identifier of the directory owner account.
-- * 'directoryId' - Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
-- * 'dnsIPAddrs' - IP address of the directory’s domain controllers.
-- * 'radiusSettings' - A 'RadiusSettings' object that contains information about the RADIUS server.
-- * 'radiusStatus' - Information about the status of the RADIUS server.
-- * 'vpcSettings' - Information about the VPC settings for the directory.
mkOwnerDirectoryDescription ::
  OwnerDirectoryDescription
mkOwnerDirectoryDescription =
  OwnerDirectoryDescription'
    { radiusStatus = Lude.Nothing,
      directoryId = Lude.Nothing,
      radiusSettings = Lude.Nothing,
      accountId = Lude.Nothing,
      dnsIPAddrs = Lude.Nothing,
      vpcSettings = Lude.Nothing
    }

-- | Information about the status of the RADIUS server.
--
-- /Note:/ Consider using 'radiusStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddRadiusStatus :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe RadiusStatus)
oddRadiusStatus = Lens.lens (radiusStatus :: OwnerDirectoryDescription -> Lude.Maybe RadiusStatus) (\s a -> s {radiusStatus = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddRadiusStatus "Use generic-lens or generic-optics with 'radiusStatus' instead." #-}

-- | Identifier of the AWS Managed Microsoft AD directory in the directory owner account.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddDirectoryId :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe Lude.Text)
oddDirectoryId = Lens.lens (directoryId :: OwnerDirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {directoryId = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddDirectoryId "Use generic-lens or generic-optics with 'directoryId' instead." #-}

-- | A 'RadiusSettings' object that contains information about the RADIUS server.
--
-- /Note:/ Consider using 'radiusSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddRadiusSettings :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe RadiusSettings)
oddRadiusSettings = Lens.lens (radiusSettings :: OwnerDirectoryDescription -> Lude.Maybe RadiusSettings) (\s a -> s {radiusSettings = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddRadiusSettings "Use generic-lens or generic-optics with 'radiusSettings' instead." #-}

-- | Identifier of the directory owner account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddAccountId :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe Lude.Text)
oddAccountId = Lens.lens (accountId :: OwnerDirectoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | IP address of the directory’s domain controllers.
--
-- /Note:/ Consider using 'dnsIPAddrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddDNSIPAddrs :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe [Lude.Text])
oddDNSIPAddrs = Lens.lens (dnsIPAddrs :: OwnerDirectoryDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {dnsIPAddrs = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddDNSIPAddrs "Use generic-lens or generic-optics with 'dnsIPAddrs' instead." #-}

-- | Information about the VPC settings for the directory.
--
-- /Note:/ Consider using 'vpcSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oddVPCSettings :: Lens.Lens' OwnerDirectoryDescription (Lude.Maybe DirectoryVPCSettingsDescription)
oddVPCSettings = Lens.lens (vpcSettings :: OwnerDirectoryDescription -> Lude.Maybe DirectoryVPCSettingsDescription) (\s a -> s {vpcSettings = a} :: OwnerDirectoryDescription)
{-# DEPRECATED oddVPCSettings "Use generic-lens or generic-optics with 'vpcSettings' instead." #-}

instance Lude.FromJSON OwnerDirectoryDescription where
  parseJSON =
    Lude.withObject
      "OwnerDirectoryDescription"
      ( \x ->
          OwnerDirectoryDescription'
            Lude.<$> (x Lude..:? "RadiusStatus")
            Lude.<*> (x Lude..:? "DirectoryId")
            Lude.<*> (x Lude..:? "RadiusSettings")
            Lude.<*> (x Lude..:? "AccountId")
            Lude.<*> (x Lude..:? "DnsIpAddrs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcSettings")
      )
