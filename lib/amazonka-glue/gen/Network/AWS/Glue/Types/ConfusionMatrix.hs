{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConfusionMatrix
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ConfusionMatrix
  ( ConfusionMatrix (..),

    -- * Smart constructor
    mkConfusionMatrix,

    -- * Lenses
    cmNumFalseNegatives,
    cmNumFalsePositives,
    cmNumTrueNegatives,
    cmNumTruePositives,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
--
-- /See:/ 'mkConfusionMatrix' smart constructor.
data ConfusionMatrix = ConfusionMatrix'
  { -- | The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
    numFalseNegatives :: Core.Maybe Core.Integer,
    -- | The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
    numFalsePositives :: Core.Maybe Core.Integer,
    -- | The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
    numTrueNegatives :: Core.Maybe Core.Integer,
    -- | The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
    numTruePositives :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConfusionMatrix' value with any optional fields omitted.
mkConfusionMatrix ::
  ConfusionMatrix
mkConfusionMatrix =
  ConfusionMatrix'
    { numFalseNegatives = Core.Nothing,
      numFalsePositives = Core.Nothing,
      numTrueNegatives = Core.Nothing,
      numTruePositives = Core.Nothing
    }

-- | The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numFalseNegatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumFalseNegatives :: Lens.Lens' ConfusionMatrix (Core.Maybe Core.Integer)
cmNumFalseNegatives = Lens.field @"numFalseNegatives"
{-# DEPRECATED cmNumFalseNegatives "Use generic-lens or generic-optics with 'numFalseNegatives' instead." #-}

-- | The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numFalsePositives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumFalsePositives :: Lens.Lens' ConfusionMatrix (Core.Maybe Core.Integer)
cmNumFalsePositives = Lens.field @"numFalsePositives"
{-# DEPRECATED cmNumFalsePositives "Use generic-lens or generic-optics with 'numFalsePositives' instead." #-}

-- | The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numTrueNegatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumTrueNegatives :: Lens.Lens' ConfusionMatrix (Core.Maybe Core.Integer)
cmNumTrueNegatives = Lens.field @"numTrueNegatives"
{-# DEPRECATED cmNumTrueNegatives "Use generic-lens or generic-optics with 'numTrueNegatives' instead." #-}

-- | The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numTruePositives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumTruePositives :: Lens.Lens' ConfusionMatrix (Core.Maybe Core.Integer)
cmNumTruePositives = Lens.field @"numTruePositives"
{-# DEPRECATED cmNumTruePositives "Use generic-lens or generic-optics with 'numTruePositives' instead." #-}

instance Core.FromJSON ConfusionMatrix where
  parseJSON =
    Core.withObject "ConfusionMatrix" Core.$
      \x ->
        ConfusionMatrix'
          Core.<$> (x Core..:? "NumFalseNegatives")
          Core.<*> (x Core..:? "NumFalsePositives")
          Core.<*> (x Core..:? "NumTrueNegatives")
          Core.<*> (x Core..:? "NumTruePositives")
