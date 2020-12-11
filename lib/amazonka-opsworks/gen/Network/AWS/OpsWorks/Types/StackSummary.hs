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
    ssARN,
    ssAppsCount,
    ssName,
    ssStackId,
    ssLayersCount,
    ssInstancesCount,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.InstancesCount
import qualified Network.AWS.Prelude as Lude

-- | Summarizes the number of layers, instances, and apps in a stack.
--
-- /See:/ 'mkStackSummary' smart constructor.
data StackSummary = StackSummary'
  { arn :: Lude.Maybe Lude.Text,
    appsCount :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text,
    stackId :: Lude.Maybe Lude.Text,
    layersCount :: Lude.Maybe Lude.Int,
    instancesCount :: Lude.Maybe InstancesCount
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackSummary' with the minimum fields required to make a request.
--
-- * 'appsCount' - The number of apps.
-- * 'arn' - The stack's ARN.
-- * 'instancesCount' - An @InstancesCount@ object with the number of instances in each status.
-- * 'layersCount' - The number of layers.
-- * 'name' - The stack name.
-- * 'stackId' - The stack ID.
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
ssARN :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssARN = Lens.lens (arn :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: StackSummary)
{-# DEPRECATED ssARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The number of apps.
--
-- /Note:/ Consider using 'appsCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssAppsCount :: Lens.Lens' StackSummary (Lude.Maybe Lude.Int)
ssAppsCount = Lens.lens (appsCount :: StackSummary -> Lude.Maybe Lude.Int) (\s a -> s {appsCount = a} :: StackSummary)
{-# DEPRECATED ssAppsCount "Use generic-lens or generic-optics with 'appsCount' instead." #-}

-- | The stack name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssName = Lens.lens (name :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StackSummary)
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssStackId :: Lens.Lens' StackSummary (Lude.Maybe Lude.Text)
ssStackId = Lens.lens (stackId :: StackSummary -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: StackSummary)
{-# DEPRECATED ssStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The number of layers.
--
-- /Note:/ Consider using 'layersCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLayersCount :: Lens.Lens' StackSummary (Lude.Maybe Lude.Int)
ssLayersCount = Lens.lens (layersCount :: StackSummary -> Lude.Maybe Lude.Int) (\s a -> s {layersCount = a} :: StackSummary)
{-# DEPRECATED ssLayersCount "Use generic-lens or generic-optics with 'layersCount' instead." #-}

-- | An @InstancesCount@ object with the number of instances in each status.
--
-- /Note:/ Consider using 'instancesCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInstancesCount :: Lens.Lens' StackSummary (Lude.Maybe InstancesCount)
ssInstancesCount = Lens.lens (instancesCount :: StackSummary -> Lude.Maybe InstancesCount) (\s a -> s {instancesCount = a} :: StackSummary)
{-# DEPRECATED ssInstancesCount "Use generic-lens or generic-optics with 'instancesCount' instead." #-}

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
