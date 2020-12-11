-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.DirectoryVPCSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.DirectoryVPCSettings
  ( DirectoryVPCSettings (..),

    -- * Smart constructor
    mkDirectoryVPCSettings,

    -- * Lenses
    dvsVPCId,
    dvsSubnetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains VPC information for the 'CreateDirectory' or 'CreateMicrosoftAD' operation.
--
-- /See:/ 'mkDirectoryVPCSettings' smart constructor.
data DirectoryVPCSettings = DirectoryVPCSettings'
  { vpcId ::
      Lude.Text,
    subnetIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DirectoryVPCSettings' with the minimum fields required to make a request.
--
-- * 'subnetIds' - The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
-- * 'vpcId' - The identifier of the VPC in which to create the directory.
mkDirectoryVPCSettings ::
  -- | 'vpcId'
  Lude.Text ->
  DirectoryVPCSettings
mkDirectoryVPCSettings pVPCId_ =
  DirectoryVPCSettings' {vpcId = pVPCId_, subnetIds = Lude.mempty}

-- | The identifier of the VPC in which to create the directory.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsVPCId :: Lens.Lens' DirectoryVPCSettings Lude.Text
dvsVPCId = Lens.lens (vpcId :: DirectoryVPCSettings -> Lude.Text) (\s a -> s {vpcId = a} :: DirectoryVPCSettings)
{-# DEPRECATED dvsVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The identifiers of the subnets for the directory servers. The two subnets must be in different Availability Zones. AWS Directory Service creates a directory server and a DNS server in each of these subnets.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvsSubnetIds :: Lens.Lens' DirectoryVPCSettings [Lude.Text]
dvsSubnetIds = Lens.lens (subnetIds :: DirectoryVPCSettings -> [Lude.Text]) (\s a -> s {subnetIds = a} :: DirectoryVPCSettings)
{-# DEPRECATED dvsSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.FromJSON DirectoryVPCSettings where
  parseJSON =
    Lude.withObject
      "DirectoryVPCSettings"
      ( \x ->
          DirectoryVPCSettings'
            Lude.<$> (x Lude..: "VpcId")
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON DirectoryVPCSettings where
  toJSON DirectoryVPCSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("VpcId" Lude..= vpcId),
            Lude.Just ("SubnetIds" Lude..= subnetIds)
          ]
      )
