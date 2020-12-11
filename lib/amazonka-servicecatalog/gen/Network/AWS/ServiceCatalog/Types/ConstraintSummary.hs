-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ConstraintSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ConstraintSummary
  ( ConstraintSummary (..),

    -- * Smart constructor
    mkConstraintSummary,

    -- * Lenses
    csType,
    csDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about a constraint.
--
-- /See:/ 'mkConstraintSummary' smart constructor.
data ConstraintSummary = ConstraintSummary'
  { type' ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConstraintSummary' with the minimum fields required to make a request.
--
-- * 'description' - The description of the constraint.
-- * 'type'' - The type of constraint.
--
--
--     * @LAUNCH@
--
--
--     * @NOTIFICATION@
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@
mkConstraintSummary ::
  ConstraintSummary
mkConstraintSummary =
  ConstraintSummary'
    { type' = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The type of constraint.
--
--
--     * @LAUNCH@
--
--
--     * @NOTIFICATION@
--
--
--     * STACKSET
--
--
--     * @TEMPLATE@
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csType :: Lens.Lens' ConstraintSummary (Lude.Maybe Lude.Text)
csType = Lens.lens (type' :: ConstraintSummary -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: ConstraintSummary)
{-# DEPRECATED csType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The description of the constraint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csDescription :: Lens.Lens' ConstraintSummary (Lude.Maybe Lude.Text)
csDescription = Lens.lens (description :: ConstraintSummary -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ConstraintSummary)
{-# DEPRECATED csDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ConstraintSummary where
  parseJSON =
    Lude.withObject
      "ConstraintSummary"
      ( \x ->
          ConstraintSummary'
            Lude.<$> (x Lude..:? "Type") Lude.<*> (x Lude..:? "Description")
      )
