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
    cmNumTrueNegatives,
    cmNumFalseNegatives,
    cmNumTruePositives,
    cmNumFalsePositives,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
--
-- /See:/ 'mkConfusionMatrix' smart constructor.
data ConfusionMatrix = ConfusionMatrix'
  { numTrueNegatives ::
      Lude.Maybe Lude.Integer,
    numFalseNegatives :: Lude.Maybe Lude.Integer,
    numTruePositives :: Lude.Maybe Lude.Integer,
    numFalsePositives :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfusionMatrix' with the minimum fields required to make a request.
--
-- * 'numFalseNegatives' - The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
-- * 'numFalsePositives' - The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
-- * 'numTrueNegatives' - The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
-- * 'numTruePositives' - The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
mkConfusionMatrix ::
  ConfusionMatrix
mkConfusionMatrix =
  ConfusionMatrix'
    { numTrueNegatives = Lude.Nothing,
      numFalseNegatives = Lude.Nothing,
      numTruePositives = Lude.Nothing,
      numFalsePositives = Lude.Nothing
    }

-- | The number of nonmatches in the data that the transform correctly rejected, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numTrueNegatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumTrueNegatives :: Lens.Lens' ConfusionMatrix (Lude.Maybe Lude.Integer)
cmNumTrueNegatives = Lens.lens (numTrueNegatives :: ConfusionMatrix -> Lude.Maybe Lude.Integer) (\s a -> s {numTrueNegatives = a} :: ConfusionMatrix)
{-# DEPRECATED cmNumTrueNegatives "Use generic-lens or generic-optics with 'numTrueNegatives' instead." #-}

-- | The number of matches in the data that the transform didn't find, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numFalseNegatives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumFalseNegatives :: Lens.Lens' ConfusionMatrix (Lude.Maybe Lude.Integer)
cmNumFalseNegatives = Lens.lens (numFalseNegatives :: ConfusionMatrix -> Lude.Maybe Lude.Integer) (\s a -> s {numFalseNegatives = a} :: ConfusionMatrix)
{-# DEPRECATED cmNumFalseNegatives "Use generic-lens or generic-optics with 'numFalseNegatives' instead." #-}

-- | The number of matches in the data that the transform correctly found, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numTruePositives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumTruePositives :: Lens.Lens' ConfusionMatrix (Lude.Maybe Lude.Integer)
cmNumTruePositives = Lens.lens (numTruePositives :: ConfusionMatrix -> Lude.Maybe Lude.Integer) (\s a -> s {numTruePositives = a} :: ConfusionMatrix)
{-# DEPRECATED cmNumTruePositives "Use generic-lens or generic-optics with 'numTruePositives' instead." #-}

-- | The number of nonmatches in the data that the transform incorrectly classified as a match, in the confusion matrix for your transform.
--
-- /Note:/ Consider using 'numFalsePositives' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumFalsePositives :: Lens.Lens' ConfusionMatrix (Lude.Maybe Lude.Integer)
cmNumFalsePositives = Lens.lens (numFalsePositives :: ConfusionMatrix -> Lude.Maybe Lude.Integer) (\s a -> s {numFalsePositives = a} :: ConfusionMatrix)
{-# DEPRECATED cmNumFalsePositives "Use generic-lens or generic-optics with 'numFalsePositives' instead." #-}

instance Lude.FromJSON ConfusionMatrix where
  parseJSON =
    Lude.withObject
      "ConfusionMatrix"
      ( \x ->
          ConfusionMatrix'
            Lude.<$> (x Lude..:? "NumTrueNegatives")
            Lude.<*> (x Lude..:? "NumFalseNegatives")
            Lude.<*> (x Lude..:? "NumTruePositives")
            Lude.<*> (x Lude..:? "NumFalsePositives")
      )
