{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitActivity
  ( RandomSplitActivity (..),

    -- * Smart constructor
    mkRandomSplitActivity,

    -- * Lenses
    rsaBranches,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RandomSplitEntry
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a random split activity in a journey. This type of activity randomly sends specified percentages of participants down one of as many as five paths in a journey, based on conditions that you specify.
--
-- /See:/ 'mkRandomSplitActivity' smart constructor.
newtype RandomSplitActivity = RandomSplitActivity'
  { -- | The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
    branches :: Lude.Maybe [RandomSplitEntry]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RandomSplitActivity' with the minimum fields required to make a request.
--
-- * 'branches' - The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
mkRandomSplitActivity ::
  RandomSplitActivity
mkRandomSplitActivity =
  RandomSplitActivity' {branches = Lude.Nothing}

-- | The paths for the activity, including the percentage of participants to enter each path and the activity to perform for each path.
--
-- /Note:/ Consider using 'branches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaBranches :: Lens.Lens' RandomSplitActivity (Lude.Maybe [RandomSplitEntry])
rsaBranches = Lens.lens (branches :: RandomSplitActivity -> Lude.Maybe [RandomSplitEntry]) (\s a -> s {branches = a} :: RandomSplitActivity)
{-# DEPRECATED rsaBranches "Use generic-lens or generic-optics with 'branches' instead." #-}

instance Lude.FromJSON RandomSplitActivity where
  parseJSON =
    Lude.withObject
      "RandomSplitActivity"
      ( \x ->
          RandomSplitActivity'
            Lude.<$> (x Lude..:? "Branches" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON RandomSplitActivity where
  toJSON RandomSplitActivity' {..} =
    Lude.object
      (Lude.catMaybes [("Branches" Lude..=) Lude.<$> branches])
