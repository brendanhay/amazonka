-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LoadBalancerInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LoadBalancerInfo
  ( LoadBalancerInfo (..),

    -- * Smart constructor
    mkLoadBalancerInfo,

    -- * Lenses
    lbiElbInfoList,
    lbiTargetGroupInfoList,
    lbiTargetGroupPairInfoList,
  )
where

import Network.AWS.CodeDeploy.Types.ELBInfo
import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the Elastic Load Balancing load balancer or target group used in a deployment.
--
-- /See:/ 'mkLoadBalancerInfo' smart constructor.
data LoadBalancerInfo = LoadBalancerInfo'
  { elbInfoList ::
      Lude.Maybe [ELBInfo],
    targetGroupInfoList :: Lude.Maybe [TargetGroupInfo],
    targetGroupPairInfoList ::
      Lude.Maybe [TargetGroupPairInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerInfo' with the minimum fields required to make a request.
--
-- * 'elbInfoList' - An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
-- * 'targetGroupInfoList' - An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
-- * 'targetGroupPairInfoList' - The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one.
mkLoadBalancerInfo ::
  LoadBalancerInfo
mkLoadBalancerInfo =
  LoadBalancerInfo'
    { elbInfoList = Lude.Nothing,
      targetGroupInfoList = Lude.Nothing,
      targetGroupPairInfoList = Lude.Nothing
    }

-- | An array that contains information about the load balancer to use for load balancing in a deployment. In Elastic Load Balancing, load balancers are used with Classic Load Balancers.
--
-- /Note:/ Consider using 'elbInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiElbInfoList :: Lens.Lens' LoadBalancerInfo (Lude.Maybe [ELBInfo])
lbiElbInfoList = Lens.lens (elbInfoList :: LoadBalancerInfo -> Lude.Maybe [ELBInfo]) (\s a -> s {elbInfoList = a} :: LoadBalancerInfo)
{-# DEPRECATED lbiElbInfoList "Use generic-lens or generic-optics with 'elbInfoList' instead." #-}

-- | An array that contains information about the target group to use for load balancing in a deployment. In Elastic Load Balancing, target groups are used with Application Load Balancers.
--
-- /Note:/ Consider using 'targetGroupInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiTargetGroupInfoList :: Lens.Lens' LoadBalancerInfo (Lude.Maybe [TargetGroupInfo])
lbiTargetGroupInfoList = Lens.lens (targetGroupInfoList :: LoadBalancerInfo -> Lude.Maybe [TargetGroupInfo]) (\s a -> s {targetGroupInfoList = a} :: LoadBalancerInfo)
{-# DEPRECATED lbiTargetGroupInfoList "Use generic-lens or generic-optics with 'targetGroupInfoList' instead." #-}

-- | The target group pair information. This is an array of @TargeGroupPairInfo@ objects with a maximum size of one.
--
-- /Note:/ Consider using 'targetGroupPairInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbiTargetGroupPairInfoList :: Lens.Lens' LoadBalancerInfo (Lude.Maybe [TargetGroupPairInfo])
lbiTargetGroupPairInfoList = Lens.lens (targetGroupPairInfoList :: LoadBalancerInfo -> Lude.Maybe [TargetGroupPairInfo]) (\s a -> s {targetGroupPairInfoList = a} :: LoadBalancerInfo)
{-# DEPRECATED lbiTargetGroupPairInfoList "Use generic-lens or generic-optics with 'targetGroupPairInfoList' instead." #-}

instance Lude.FromJSON LoadBalancerInfo where
  parseJSON =
    Lude.withObject
      "LoadBalancerInfo"
      ( \x ->
          LoadBalancerInfo'
            Lude.<$> (x Lude..:? "elbInfoList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "targetGroupInfoList" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "targetGroupPairInfoList" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON LoadBalancerInfo where
  toJSON LoadBalancerInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("elbInfoList" Lude..=) Lude.<$> elbInfoList,
            ("targetGroupInfoList" Lude..=) Lude.<$> targetGroupInfoList,
            ("targetGroupPairInfoList" Lude..=)
              Lude.<$> targetGroupPairInfoList
          ]
      )
