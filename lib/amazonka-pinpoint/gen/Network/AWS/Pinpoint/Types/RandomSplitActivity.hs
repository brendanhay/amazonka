{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RandomSplitActivity
  ( RandomSplitActivity (..)
  -- * Smart constructor
  , mkRandomSplitActivity
  -- * Lenses
  , rsaBranches
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.RandomSplitEntry as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a random split activity in a journey. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
--
-- /See:/ 'mkRandomSplitActivity' smart constructor.
newtype RandomSplitActivity = RandomSplitActivity'
  { branches :: Core.Maybe [Types.RandomSplitEntry]
    -- ^ The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RandomSplitActivity' value with any optional fields omitted.
mkRandomSplitActivity
    :: RandomSplitActivity
mkRandomSplitActivity
  = RandomSplitActivity'{branches = Core.Nothing}

-- | The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaBranches :: Lens.Lens' RandomSplitActivity (Core.Maybe [Types.RandomSplitEntry])
rsaBranches = Lens.field @"branches"
{-# INLINEABLE rsaBranches #-}
{-# DEPRECATED branches "Use generic-lens or generic-optics with 'branches' instead"  #-}

instance Core.FromJSON RandomSplitActivity where
        toJSON RandomSplitActivity{..}
          = Core.object
              (Core.catMaybes [("Branches" Core..=) Core.<$> branches])

instance Core.FromJSON RandomSplitActivity where
        parseJSON
          = Core.withObject "RandomSplitActivity" Core.$
              \ x -> RandomSplitActivity' Core.<$> (x Core..:? "Branches")
