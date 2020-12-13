{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentBehaviors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentBehaviors
  ( SegmentBehaviors (..),

    -- * Smart constructor
    mkSegmentBehaviors,

    -- * Lenses
    sbRecency,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RecencyDimension
import qualified Network.AWS.Prelude as Lude

-- | Specifies dimension settings for including or excluding endpoints from a segment based on how recently an endpoint was active.
--
-- /See:/ 'mkSegmentBehaviors' smart constructor.
newtype SegmentBehaviors = SegmentBehaviors'
  { -- | The dimension settings that are based on how recently an endpoint was active.
    recency :: Lude.Maybe RecencyDimension
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentBehaviors' with the minimum fields required to make a request.
--
-- * 'recency' - The dimension settings that are based on how recently an endpoint was active.
mkSegmentBehaviors ::
  SegmentBehaviors
mkSegmentBehaviors = SegmentBehaviors' {recency = Lude.Nothing}

-- | The dimension settings that are based on how recently an endpoint was active.
--
-- /Note:/ Consider using 'recency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbRecency :: Lens.Lens' SegmentBehaviors (Lude.Maybe RecencyDimension)
sbRecency = Lens.lens (recency :: SegmentBehaviors -> Lude.Maybe RecencyDimension) (\s a -> s {recency = a} :: SegmentBehaviors)
{-# DEPRECATED sbRecency "Use generic-lens or generic-optics with 'recency' instead." #-}

instance Lude.FromJSON SegmentBehaviors where
  parseJSON =
    Lude.withObject
      "SegmentBehaviors"
      (\x -> SegmentBehaviors' Lude.<$> (x Lude..:? "Recency"))

instance Lude.ToJSON SegmentBehaviors where
  toJSON SegmentBehaviors' {..} =
    Lude.object
      (Lude.catMaybes [("Recency" Lude..=) Lude.<$> recency])
