-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingCriteria
  ( FindingCriteria (..),

    -- * Smart constructor
    mkFindingCriteria,

    -- * Lenses
    fcCriterion,
  )
where

import Network.AWS.GuardDuty.Types.Condition
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the criteria used for querying findings.
--
-- /See:/ 'mkFindingCriteria' smart constructor.
newtype FindingCriteria = FindingCriteria'
  { criterion ::
      Lude.Maybe (Lude.HashMap Lude.Text (Condition))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FindingCriteria' with the minimum fields required to make a request.
--
-- * 'criterion' - Represents a map of finding properties that match specified conditions and values when querying findings.
mkFindingCriteria ::
  FindingCriteria
mkFindingCriteria = FindingCriteria' {criterion = Lude.Nothing}

-- | Represents a map of finding properties that match specified conditions and values when querying findings.
--
-- /Note:/ Consider using 'criterion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcCriterion :: Lens.Lens' FindingCriteria (Lude.Maybe (Lude.HashMap Lude.Text (Condition)))
fcCriterion = Lens.lens (criterion :: FindingCriteria -> Lude.Maybe (Lude.HashMap Lude.Text (Condition))) (\s a -> s {criterion = a} :: FindingCriteria)
{-# DEPRECATED fcCriterion "Use generic-lens or generic-optics with 'criterion' instead." #-}

instance Lude.FromJSON FindingCriteria where
  parseJSON =
    Lude.withObject
      "FindingCriteria"
      ( \x ->
          FindingCriteria'
            Lude.<$> (x Lude..:? "criterion" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON FindingCriteria where
  toJSON FindingCriteria' {..} =
    Lude.object
      (Lude.catMaybes [("criterion" Lude..=) Lude.<$> criterion])
