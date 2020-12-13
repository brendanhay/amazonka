{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EcsCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EcsCluster
  ( EcsCluster (..),

    -- * Smart constructor
    mkEcsCluster,

    -- * Lenses
    ecEcsClusterARN,
    ecEcsClusterName,
    ecRegisteredAt,
    ecStackId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a registered Amazon ECS cluster.
--
-- /See:/ 'mkEcsCluster' smart constructor.
data EcsCluster = EcsCluster'
  { -- | The cluster's ARN.
    ecsClusterARN :: Lude.Maybe Lude.Text,
    -- | The cluster name.
    ecsClusterName :: Lude.Maybe Lude.Text,
    -- | The time and date that the cluster was registered with the stack.
    registeredAt :: Lude.Maybe Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EcsCluster' with the minimum fields required to make a request.
--
-- * 'ecsClusterARN' - The cluster's ARN.
-- * 'ecsClusterName' - The cluster name.
-- * 'registeredAt' - The time and date that the cluster was registered with the stack.
-- * 'stackId' - The stack ID.
mkEcsCluster ::
  EcsCluster
mkEcsCluster =
  EcsCluster'
    { ecsClusterARN = Lude.Nothing,
      ecsClusterName = Lude.Nothing,
      registeredAt = Lude.Nothing,
      stackId = Lude.Nothing
    }

-- | The cluster's ARN.
--
-- /Note:/ Consider using 'ecsClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEcsClusterARN :: Lens.Lens' EcsCluster (Lude.Maybe Lude.Text)
ecEcsClusterARN = Lens.lens (ecsClusterARN :: EcsCluster -> Lude.Maybe Lude.Text) (\s a -> s {ecsClusterARN = a} :: EcsCluster)
{-# DEPRECATED ecEcsClusterARN "Use generic-lens or generic-optics with 'ecsClusterARN' instead." #-}

-- | The cluster name.
--
-- /Note:/ Consider using 'ecsClusterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEcsClusterName :: Lens.Lens' EcsCluster (Lude.Maybe Lude.Text)
ecEcsClusterName = Lens.lens (ecsClusterName :: EcsCluster -> Lude.Maybe Lude.Text) (\s a -> s {ecsClusterName = a} :: EcsCluster)
{-# DEPRECATED ecEcsClusterName "Use generic-lens or generic-optics with 'ecsClusterName' instead." #-}

-- | The time and date that the cluster was registered with the stack.
--
-- /Note:/ Consider using 'registeredAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecRegisteredAt :: Lens.Lens' EcsCluster (Lude.Maybe Lude.Text)
ecRegisteredAt = Lens.lens (registeredAt :: EcsCluster -> Lude.Maybe Lude.Text) (\s a -> s {registeredAt = a} :: EcsCluster)
{-# DEPRECATED ecRegisteredAt "Use generic-lens or generic-optics with 'registeredAt' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecStackId :: Lens.Lens' EcsCluster (Lude.Maybe Lude.Text)
ecStackId = Lens.lens (stackId :: EcsCluster -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: EcsCluster)
{-# DEPRECATED ecStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

instance Lude.FromJSON EcsCluster where
  parseJSON =
    Lude.withObject
      "EcsCluster"
      ( \x ->
          EcsCluster'
            Lude.<$> (x Lude..:? "EcsClusterArn")
            Lude.<*> (x Lude..:? "EcsClusterName")
            Lude.<*> (x Lude..:? "RegisteredAt")
            Lude.<*> (x Lude..:? "StackId")
      )
