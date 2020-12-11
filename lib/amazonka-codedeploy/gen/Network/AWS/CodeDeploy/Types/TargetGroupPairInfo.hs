-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TargetGroupPairInfo
  ( TargetGroupPairInfo (..),

    -- * Smart constructor
    mkTargetGroupPairInfo,

    -- * Lenses
    tgpiProdTrafficRoute,
    tgpiTestTrafficRoute,
    tgpiTargetGroups,
  )
where

import Network.AWS.CodeDeploy.Types.TargetGroupInfo
import Network.AWS.CodeDeploy.Types.TrafficRoute
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about two target groups and how traffic is routed during an Amazon ECS deployment. An optional test traffic route can be specified.
--
-- /See:/ 'mkTargetGroupPairInfo' smart constructor.
data TargetGroupPairInfo = TargetGroupPairInfo'
  { prodTrafficRoute ::
      Lude.Maybe TrafficRoute,
    testTrafficRoute :: Lude.Maybe TrafficRoute,
    targetGroups :: Lude.Maybe [TargetGroupInfo]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGroupPairInfo' with the minimum fields required to make a request.
--
-- * 'prodTrafficRoute' - The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete.
-- * 'targetGroups' - One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete.
-- * 'testTrafficRoute' - An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment.
mkTargetGroupPairInfo ::
  TargetGroupPairInfo
mkTargetGroupPairInfo =
  TargetGroupPairInfo'
    { prodTrafficRoute = Lude.Nothing,
      testTrafficRoute = Lude.Nothing,
      targetGroups = Lude.Nothing
    }

-- | The path used by a load balancer to route production traffic when an Amazon ECS deployment is complete.
--
-- /Note:/ Consider using 'prodTrafficRoute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiProdTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Lude.Maybe TrafficRoute)
tgpiProdTrafficRoute = Lens.lens (prodTrafficRoute :: TargetGroupPairInfo -> Lude.Maybe TrafficRoute) (\s a -> s {prodTrafficRoute = a} :: TargetGroupPairInfo)
{-# DEPRECATED tgpiProdTrafficRoute "Use generic-lens or generic-optics with 'prodTrafficRoute' instead." #-}

-- | An optional path used by a load balancer to route test traffic after an Amazon ECS deployment. Validation can occur while test traffic is served during a deployment.
--
-- /Note:/ Consider using 'testTrafficRoute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiTestTrafficRoute :: Lens.Lens' TargetGroupPairInfo (Lude.Maybe TrafficRoute)
tgpiTestTrafficRoute = Lens.lens (testTrafficRoute :: TargetGroupPairInfo -> Lude.Maybe TrafficRoute) (\s a -> s {testTrafficRoute = a} :: TargetGroupPairInfo)
{-# DEPRECATED tgpiTestTrafficRoute "Use generic-lens or generic-optics with 'testTrafficRoute' instead." #-}

-- | One pair of target groups. One is associated with the original task set. The second is associated with the task set that serves traffic after the deployment is complete.
--
-- /Note:/ Consider using 'targetGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpiTargetGroups :: Lens.Lens' TargetGroupPairInfo (Lude.Maybe [TargetGroupInfo])
tgpiTargetGroups = Lens.lens (targetGroups :: TargetGroupPairInfo -> Lude.Maybe [TargetGroupInfo]) (\s a -> s {targetGroups = a} :: TargetGroupPairInfo)
{-# DEPRECATED tgpiTargetGroups "Use generic-lens or generic-optics with 'targetGroups' instead." #-}

instance Lude.FromJSON TargetGroupPairInfo where
  parseJSON =
    Lude.withObject
      "TargetGroupPairInfo"
      ( \x ->
          TargetGroupPairInfo'
            Lude.<$> (x Lude..:? "prodTrafficRoute")
            Lude.<*> (x Lude..:? "testTrafficRoute")
            Lude.<*> (x Lude..:? "targetGroups" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON TargetGroupPairInfo where
  toJSON TargetGroupPairInfo' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("prodTrafficRoute" Lude..=) Lude.<$> prodTrafficRoute,
            ("testTrafficRoute" Lude..=) Lude.<$> testTrafficRoute,
            ("targetGroups" Lude..=) Lude.<$> targetGroups
          ]
      )
