{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.VPCConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.VPCConfig
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

-- | The VPC security groups and subnets that are attached to a Lambda function. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/configuration-vpc.html VPC Settings> .
--
-- /See:/ 'mkVPCConfig' smart constructor.
data VPCConfig = VPCConfig'
  { -- | A list of VPC security groups IDs.
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A list of VPC subnet IDs.
    subnetIds :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- * 'securityGroupIds' - A list of VPC security groups IDs.
-- * 'subnetIds' - A list of VPC subnet IDs.
mkVPCConfig ::
  VPCConfig
mkVPCConfig =
  VPCConfig'
    { securityGroupIds = Lude.Nothing,
      subnetIds = Lude.Nothing
    }

-- | A list of VPC security groups IDs.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSecurityGroupIds :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSecurityGroupIds = Lens.lens (securityGroupIds :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: VPCConfig)
{-# DEPRECATED vcSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | A list of VPC subnet IDs.
--
-- /Note:/ Consider using 'subnetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcSubnetIds :: Lens.Lens' VPCConfig (Lude.Maybe [Lude.Text])
vcSubnetIds = Lens.lens (subnetIds :: VPCConfig -> Lude.Maybe [Lude.Text]) (\s a -> s {subnetIds = a} :: VPCConfig)
{-# DEPRECATED vcSubnetIds "Use generic-lens or generic-optics with 'subnetIds' instead." #-}

instance Lude.ToJSON VPCConfig where
  toJSON VPCConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("SubnetIds" Lude..=) Lude.<$> subnetIds
          ]
      )
