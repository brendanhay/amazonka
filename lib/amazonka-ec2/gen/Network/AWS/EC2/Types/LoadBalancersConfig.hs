{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LoadBalancersConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LoadBalancersConfig
  ( LoadBalancersConfig (..),

    -- * Smart constructor
    mkLoadBalancersConfig,

    -- * Lenses
    lbcClassicLoadBalancersConfig,
    lbcTargetGroupsConfig,
  )
where

import Network.AWS.EC2.Types.ClassicLoadBalancersConfig
import Network.AWS.EC2.Types.TargetGroupsConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Classic Load Balancers and target groups to attach to a Spot Fleet request.
--
-- /See:/ 'mkLoadBalancersConfig' smart constructor.
data LoadBalancersConfig = LoadBalancersConfig'
  { classicLoadBalancersConfig ::
      Lude.Maybe ClassicLoadBalancersConfig,
    targetGroupsConfig :: Lude.Maybe TargetGroupsConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancersConfig' with the minimum fields required to make a request.
--
-- * 'classicLoadBalancersConfig' - The Classic Load Balancers.
-- * 'targetGroupsConfig' - The target groups.
mkLoadBalancersConfig ::
  LoadBalancersConfig
mkLoadBalancersConfig =
  LoadBalancersConfig'
    { classicLoadBalancersConfig = Lude.Nothing,
      targetGroupsConfig = Lude.Nothing
    }

-- | The Classic Load Balancers.
--
-- /Note:/ Consider using 'classicLoadBalancersConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcClassicLoadBalancersConfig :: Lens.Lens' LoadBalancersConfig (Lude.Maybe ClassicLoadBalancersConfig)
lbcClassicLoadBalancersConfig = Lens.lens (classicLoadBalancersConfig :: LoadBalancersConfig -> Lude.Maybe ClassicLoadBalancersConfig) (\s a -> s {classicLoadBalancersConfig = a} :: LoadBalancersConfig)
{-# DEPRECATED lbcClassicLoadBalancersConfig "Use generic-lens or generic-optics with 'classicLoadBalancersConfig' instead." #-}

-- | The target groups.
--
-- /Note:/ Consider using 'targetGroupsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbcTargetGroupsConfig :: Lens.Lens' LoadBalancersConfig (Lude.Maybe TargetGroupsConfig)
lbcTargetGroupsConfig = Lens.lens (targetGroupsConfig :: LoadBalancersConfig -> Lude.Maybe TargetGroupsConfig) (\s a -> s {targetGroupsConfig = a} :: LoadBalancersConfig)
{-# DEPRECATED lbcTargetGroupsConfig "Use generic-lens or generic-optics with 'targetGroupsConfig' instead." #-}

instance Lude.FromXML LoadBalancersConfig where
  parseXML x =
    LoadBalancersConfig'
      Lude.<$> (x Lude..@? "classicLoadBalancersConfig")
      Lude.<*> (x Lude..@? "targetGroupsConfig")

instance Lude.ToQuery LoadBalancersConfig where
  toQuery LoadBalancersConfig' {..} =
    Lude.mconcat
      [ "ClassicLoadBalancersConfig" Lude.=: classicLoadBalancersConfig,
        "TargetGroupsConfig" Lude.=: targetGroupsConfig
      ]
