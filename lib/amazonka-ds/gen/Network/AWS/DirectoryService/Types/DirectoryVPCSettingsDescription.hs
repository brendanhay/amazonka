{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVPCSettingsDescription
  ( DirectoryVPCSettingsDescription (..),

    -- * Smart constructor
    mkDirectoryVPCSettingsDescription,

    -- * Lenses
    dvsdSubnetIds,
    dvsdVPCId,
    dvsdSecurityGroupId,
    dvsdAvailabilityZones,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the directory.
--
-- /See:/ 'mkDirectoryVPCSettingsDescription' smart constructor.
data DirectoryVPCSettingsDescription = DirectoryVPCSettingsDescription'
  { -- | The identifiers of the subnets for the directory servers.
    subnetIds :: Lude.Maybe [Lude.Text],
    -- | The identifier of the VPC that the directory is in.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The domain controller security group identifier for the directory.
    securityGroupId :: Lude.Maybe Lude.Text,
    -- | The list of Availability Zones that the directory is in.
    availabilityZones :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryVPCSettingsDescription' with the minimum fields required to make a request.
--
-- * 'subnetIds' - The identifiers of the subnets for the directory servers.
-- * 'vpcId' - The identifier of the VPC that the directory is in.
-- * 'securityGroupId' - The domain controller security group identifier for the directory.
-- * 'availabilityZones' - The list of Availability Zones that the directory is in.
mkDirectoryVPCSettingsDescription ::
  DirectoryVPCSettingsDescription
mkDirectoryVPCSettingsDescription =
  DirectoryVPCSettingsDescription'
    { subnetIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      securityGroupId = Lude.Nothing,
      availabilityZones = Lude.Nothing
    }

-- | The identifiers of the subnets for the directory servers.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdSubnetIds :: Lens.Lens' DirectoryVPCSettingsDescription (Lude.Maybe [Lude.Text])
dvsdSubnetIds = Lens.lens (subnetIds :: DirectoryVPCSettingsDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: DirectoryVPCSettingsDescription)
{-# DEPRECATED dvsdSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

-- | The identifier of the VPC that the directory is in.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdVPCId :: Lens.Lens' DirectoryVPCSettingsDescription (Lude.Maybe Lude.Text)
dvsdVPCId = Lens.lens (vpcId :: DirectoryVPCSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: DirectoryVPCSettingsDescription)
{-# DEPRECATED dvsdVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The domain controller security group identifier for the directory.
--
-- /Note:/ Consider using 'securityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdSecurityGroupId :: Lens.Lens' DirectoryVPCSettingsDescription (Lude.Maybe Lude.Text)
dvsdSecurityGroupId = Lens.lens (securityGroupId :: DirectoryVPCSettingsDescription -> Lude.Maybe Lude.Text) (\s a -> s {securityGroupId = a} :: DirectoryVPCSettingsDescription)
{-# DEPRECATED dvsdSecurityGroupId "Use generic-lens or generic-optics with 'securityGroupId' instead." #-}

-- | The list of Availability Zones that the directory is in.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsdAvailabilityZones :: Lens.Lens' DirectoryVPCSettingsDescription (Lude.Maybe [Lude.Text])
dvsdAvailabilityZones = Lens.lens (availabilityZones :: DirectoryVPCSettingsDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DirectoryVPCSettingsDescription)
{-# DEPRECATED dvsdAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Lude.FromJSON DirectoryVPCSettingsDescription where
  parseJSON =
    Lude.withObject
      "DirectoryVPCSettingsDescription"
      ( \x ->
          DirectoryVPCSettingsDescription'
            Lude.<$> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "VpcId")
            Lude.<*> (x Lude..:? "SecurityGroupId")
            Lude.<*> (x Lude..:? "AvailabilityZones" Lude..!= Lude.mempty)
      )
