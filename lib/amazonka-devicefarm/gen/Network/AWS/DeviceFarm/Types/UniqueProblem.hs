{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.UniqueProblem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.UniqueProblem
  ( UniqueProblem (..),

    -- * Smart constructor
    mkUniqueProblem,

    -- * Lenses
    upProblems,
    upMessage,
  )
where

import Network.AWS.DeviceFarm.Types.Problem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A collection of one or more problems, grouped by their result.
--
-- /See:/ 'mkUniqueProblem' smart constructor.
data UniqueProblem = UniqueProblem'
  { problems ::
      Lude.Maybe [Problem],
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UniqueProblem' with the minimum fields required to make a request.
--
-- * 'message' - A message about the unique problems' result.
-- * 'problems' - Information about the problems.
mkUniqueProblem ::
  UniqueProblem
mkUniqueProblem =
  UniqueProblem' {problems = Lude.Nothing, message = Lude.Nothing}

-- | Information about the problems.
--
-- /Note:/ Consider using 'problems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upProblems :: Lens.Lens' UniqueProblem (Lude.Maybe [Problem])
upProblems = Lens.lens (problems :: UniqueProblem -> Lude.Maybe [Problem]) (\s a -> s {problems = a} :: UniqueProblem)
{-# DEPRECATED upProblems "Use generic-lens or generic-optics with 'problems' instead." #-}

-- | A message about the unique problems' result.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upMessage :: Lens.Lens' UniqueProblem (Lude.Maybe Lude.Text)
upMessage = Lens.lens (message :: UniqueProblem -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UniqueProblem)
{-# DEPRECATED upMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON UniqueProblem where
  parseJSON =
    Lude.withObject
      "UniqueProblem"
      ( \x ->
          UniqueProblem'
            Lude.<$> (x Lude..:? "problems" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "message")
      )
