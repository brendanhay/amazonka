{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputParallelismUpdate
  ( InputParallelismUpdate (..)
  -- * Smart constructor
  , mkInputParallelismUpdate
  -- * Lenses
  , ipuCountUpdate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides updates to the parallelism count.
--
-- /See:/ 'mkInputParallelismUpdate' smart constructor.
newtype InputParallelismUpdate = InputParallelismUpdate'
  { countUpdate :: Core.Maybe Core.Natural
    -- ^ Number of in-application streams to create for the specified streaming source.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InputParallelismUpdate' value with any optional fields omitted.
mkInputParallelismUpdate
    :: InputParallelismUpdate
mkInputParallelismUpdate
  = InputParallelismUpdate'{countUpdate = Core.Nothing}

-- | Number of in-application streams to create for the specified streaming source.
--
-- /Note:/ Consider using 'countUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipuCountUpdate :: Lens.Lens' InputParallelismUpdate (Core.Maybe Core.Natural)
ipuCountUpdate = Lens.field @"countUpdate"
{-# INLINEABLE ipuCountUpdate #-}
{-# DEPRECATED countUpdate "Use generic-lens or generic-optics with 'countUpdate' instead"  #-}

instance Core.FromJSON InputParallelismUpdate where
        toJSON InputParallelismUpdate{..}
          = Core.object
              (Core.catMaybes [("CountUpdate" Core..=) Core.<$> countUpdate])
