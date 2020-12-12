{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.VPCConfig
  ( VPCConfig (..),

    -- * Smart constructor
    mkVPCConfig,

    -- * Lenses
    vcSecurityGroupIds,
    vcVpcId,
    vcSubnets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the VPC configuration that AWS CodeBuild accesses.
--
-- /See:/ 'mkVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { securityGroupIds ::
      Lude.Maybe [Lude.Text],
    vpcId :: Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Lude.Text]
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
-- * 'securityGroupIds' - A list of one or more security groups IDs in your Amazon VPC.
-- * 'subnets' - A list of one or more subnet IDs in your Amazon VPC.
-- * 'vpcId' - The ID of the Amazon VPC.
mkVPCConfig ::
  VPCConfig
mkVPCConfig =
  VPCConfig'
    { securityGroupIds = Lude.Nothing,
      vpcId = Lude.Nothing,
      subnets = Lude.Nothing
    }

-- | A list of one or more security groups IDs in your Amazon VPC.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCConfig)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The ID of the Amazon VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVpcId :: Lens.Lens' VPCConfig (Lude.Maybe Lude.Text)
vcVpcId = Lens.lens (vpcId :: VPCConfig -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: VPCConfig)
{-# DEPRECATED vcVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of one or more subnet IDs in your Amazon VPC.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnets :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSubnets = Lens.lens (subnets :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: VPCConfig)
{-# DEPRECATED vcSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Lude.FromJSON VPCConfig where
  parseJSON =
    Lude.withObject
      "VPCConfig"
      ( \x ->
          VPCConfig'
            Lude.<$> (x Lude..:? "securityGroupIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "vpcId")
            Lude.<*> (x Lude..:? "subnets" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("securityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("vpcId" Lude..=) Lude.<$> vpcId,
            ("subnets" Lude..=) Lude.<$> subnets
          ]
      )
