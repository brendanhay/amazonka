{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryConnectSettingsDescription
  ( DirectoryConnectSettingsDescription (..),

    -- * Smart constructor
    mkDirectoryConnectSettingsDescription,

    -- * Lenses
    dcsdCustomerUserName,
    dcsdSubnetIds,
    dcsdVPCId,
    dcsdSecurityGroupId,
    dcsdConnectIPs,
    dcsdAvailabilityZones,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an AD Connector directory.
--
-- /See:/ 'mkDirectoryConnectSettingsDescription' smart constructor.
data DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription'
  { customerUserName ::
      Lude.Maybe
        Lude.Text,
    subnetIds ::
      Lude.Maybe
        [Lude.Text],
    vpcId ::
      Lude.Maybe
        Lude.Text,
    securityGroupId ::
      Lude.Maybe
        Lude.Text,
    connectIPs ::
      Lude.Maybe
        [Lude.Text],
    availabilityZones ::
      Lude.Maybe
        [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryConnectSettingsDescription' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - A list of the Availability Zones that the directory is in.
-- * 'connectIPs' - The IP addresses of the AD Connector servers.
-- * 'customerUserName' - The user name of the service account in the on-premises directory.
-- * 'securityGroupId' - The security group identifier for the AD Connector directory.
-- * 'subnetIds' - A list of subnet identifiers in the VPC that the AD Connector is in.
-- * 'vpcId' - The identifier of the VPC that the AD Connector is in.
mkDirectoryConnectSettingsDescription ::
  DirectoryConnectSettingsDescription
mkDirectoryConnectSettingsDescription =
  DirectoryConnectSettingsDescription'
    { customerUserName =
        Lude.Nothing,
      subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      securityGroupId = Lude.Nothing,
      connectIPs = Lude.Nothing,
      availabilityZones = Lude.Nothing
    }

-- | The user name of the service account in the on-premises directory.
--
-- /Note:/ Consider using 'customerUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdCustomerUserName :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe Lude.Text)
dcsdCustomerUserName = Lens.lens (customerUserName :: DirectoryConnectSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {customerUserName = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdCustomerUserName "Use generic-lens or generic-optics with 'customerUserName' instead." #-}

-- | A list of subnet identifiers in the VPC that the AD Connector is in.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdSubnetIds :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe [Lude.Text])
dcsdSubnetIds = Lens.lens (subnetIds :: DirectoryConnectSettingsDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The identifier of the VPC that the AD Connector is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdVPCId :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe Lude.Text)
dcsdVPCId = Lens.lens (vpcId :: DirectoryConnectSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The security group identifier for the AD Connector directory.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdSecurityGroupId :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe Lude.Text)
dcsdSecurityGroupId = Lens.lens (securityGroupId :: DirectoryConnectSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {securityGroupId = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdSecurityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead." #-}

-- | The IP addresses of the AD Connector servers.
--
-- /Note:/ Consider using 'connectIPs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdConnectIPs :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe [Lude.Text])
dcsdConnectIPs = Lens.lens (connectIPs :: DirectoryConnectSettingsDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {connectIPs = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdConnectIPs "Use generic-lens or generic-optics with 'connectIPs' instead." #-}

-- | A list of the Availability Zones that the directory is in.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdAvailabilityZones :: Lens.Lens' DirectoryConnectSettingsDescription (Lude.Maybe [Lude.Text])
dcsdAvailabilityZones = Lens.lens (availabilityZones :: DirectoryConnectSettingsDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DirectoryConnectSettingsDescription)
{-# DEPRECATED dcsdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Lude.FromJSON DirectoryConnectSettingsDescription where
  parseJSON =
    Lude.withObject
      "DirectoryConnectSettingsDescription"
      ( \x ->
          DirectoryConnectSettingsDescription'
            Lude.<$> (x Lude..:? "CustomerUserName")
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "SecurityGroupId")
            Lude.<*> (x Lude..:? "ConnectIps" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AvailabilityZones" Lude..!= Lude.mempty)
      )
