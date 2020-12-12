{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.VPCConfig
  ( VPCConfig (..),

    -- * Smart constructor
    mkVPCConfig,

    -- * Lenses
    vcSecurityGroupIds,
    vcSubnetIds,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes VPC configuration information for fleets and image builders.
--
-- /See:/ 'mkVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    subnetIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - The identifiers of the security groups for the fleet or image builder.
-- * 'subnetIds' - The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
mkVPCConfig ::
  VPCConfig
mkVPCConfig =
  VPCConfig'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.Nothing
    }

-- | The identifiers of the security groups for the fleet or image builder.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCConfig)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The identifiers of the subnets to which a network interface is attached from the fleet instance or image builder instance. Fleet instances use one or more subnets. Image builder instances use one subnet.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnetIds :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSubnetIds = Lens.lens (subnetIds :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCConfig)
{-# DEPRECATED vcSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.FromJSON VPCConfig where
  parseJSON =
    Lude.withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            Lude.<$> (x Lude..:? "SecurityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SubnetIds" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("SubnetIds" Lude..=) Lude.<$> subnetIds
          ]
      )
