{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackSummary
  ( StackSummary (..),

    -- * Smart constructor
    mkStackSummary,

    -- * Lenses
    sARN,
    sAppsCount,
    sName,
    sStackId,
    sLayersCount,
    sInstancesCount,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.InstancesCount
import qualified Network.AWS.Prelude as Lude

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'mkStackSummary' smart constructor.
data StackSummary = StackSummary'
  { -- | The stack's ARN.
    arn :: Lude.Maybe Lude.Text,
    -- | The number of apps.
    appsCount :: Lude.Maybe Lude.Int,
    -- | The stack name.
    name :: Lude.Maybe Lude.Text,
    -- | The stack ID.
    stackId :: Lude.Maybe Lude.Text,
    -- | The number of layers.
    layersCount :: Lude.Maybe Lude.Int,
    -- | An @InstancesCount@ object with the number of instances in each status.
    instancesCount :: Lude.Maybe InstancesCount
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The stack's ARN.
-- * 'appsCount' - The number of apps.
-- * 'name' - The stack name.
-- * 'stackId' - The stack ID.
-- * 'layersCount' - The number of layers.
-- * 'instancesCount' - An @InstancesCount@ object with the number of instances in each status.
mkStackSummary ::
  StackSummary
mkStackSummary =
  StackSummary'
    { arn = Lude.Nothing,
      appsCount = Lude.Nothing,
      name = Lude.Nothing,
      stackId = Lude.Nothing,
      layersCount = Lude.Nothing,
      instancesCount = Lude.Nothing
    }

-- | The stack's ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sARN :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
sARN = Lens.lens (arn :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StackSummary)
{-# DEPRECATED sARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of apps.
--
-- /Note:/ Consider using 'appsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAppsCount :: Lens.Lens' StackSummary (Lude.Maybe Lude.Int)
sAppsCount = Lens.lens (appsCount :: StackSummary -> Lude.Maybe Lude.Int) (\s a -> s {appsCount = a} :: StackSummary)
{-# DEPRECATED sAppsCount "Use generic-lens or generic-optics with 'appsCount' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sName :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
sName = Lens.lens (name :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StackSummary)
{-# DEPRECATED sName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStackId :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
sStackId = Lens.lens (stackId :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackSummary)
{-# DEPRECATED sStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The number of layers.
--
-- /Note:/ Consider using 'layersCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLayersCount :: Lens.Lens' StackSummary (Lude.Maybe Lude.Int)
sLayersCount = Lens.lens (layersCount :: StackSummary -> Lude.Maybe Lude.Int) (\s a -> s {layersCount = a} :: StackSummary)
{-# DEPRECATED sLayersCount "Use generic-lens or generic-optics with 'layersCount' instead." #-}

-- | An @InstancesCount@ object with the number of instances in each status.
--
-- /Note:/ Consider using 'instancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstancesCount :: Lens.Lens' StackSummary (Lude.Maybe InstancesCount)
sInstancesCount = Lens.lens (instancesCount :: StackSummary -> Lude.Maybe InstancesCount) (\s a -> s {instancesCount = a} :: StackSummary)
{-# DEPRECATED sInstancesCount "Use generic-lens or generic-optics with 'instancesCount' instead." #-}

instance Lude.FromJSON StackSummary where
  parseJSON =
    Lude.withObject
      "StackSummary"
      ( \x ->
          StackSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "AppsCount")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "StackId")
            Lude.<*> (x Lude..:? "LayersCount")
            Lude.<*> (x Lude..:? "InstancesCount")
      )
