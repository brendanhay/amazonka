{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gMetrics,
    gKeys,
  )
where

import Network.AWS.CostExplorer.Types.MetricValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One level of grouped data in the results.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { metrics ::
      Lude.Maybe (Lude.HashMap Lude.Text (MetricValue)),
    keys :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- * 'keys' - The keys that are included in this group.
-- * 'metrics' - The metrics that are included in this group.
mkGroup ::
  Group
mkGroup = Group' {metrics = Lude.Nothing, keys = Lude.Nothing}

-- | The metrics that are included in this group.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gMetrics :: Lens.Lens' Group (Lude.Maybe (Lude.HashMap Lude.Text (MetricValue)))
gMetrics = Lens.lens (metrics :: Group -> Lude.Maybe (Lude.HashMap Lude.Text (MetricValue))) (\s a -> s {metrics = a} :: Group)
{-# DEPRECATED gMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The keys that are included in this group.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gKeys :: Lens.Lens' Group (Lude.Maybe [Lude.Text])
gKeys = Lens.lens (keys :: Group -> Lude.Maybe [Lude.Text]) (\s a -> s {keys = a} :: Group)
{-# DEPRECATED gKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

instance Lude.FromJSON Group where
  parseJSON =
    Lude.withObject
      "Group"
      ( \x ->
          Group'
            Lude.<$> (x Lude..:? "Metrics" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Keys" Lude..!= Lude.mempty)
      )
