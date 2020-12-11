-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentScore
  ( SentimentScore (..),

    -- * Smart constructor
    mkSentimentScore,

    -- * Lenses
    ssMixed,
    ssNegative,
    ssNeutral,
    ssPositive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.
--
-- /See:/ 'mkSentimentScore' smart constructor.
data SentimentScore = SentimentScore'
  { mixed ::
      Lude.Maybe Lude.Double,
    negative :: Lude.Maybe Lude.Double,
    neutral :: Lude.Maybe Lude.Double,
    positive :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SentimentScore' with the minimum fields required to make a request.
--
-- * 'mixed' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
-- * 'negative' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
-- * 'neutral' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
-- * 'positive' - The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
mkSentimentScore ::
  SentimentScore
mkSentimentScore =
  SentimentScore'
    { mixed = Lude.Nothing,
      negative = Lude.Nothing,
      neutral = Lude.Nothing,
      positive = Lude.Nothing
    }

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
--
-- /Note:/ Consider using 'mixed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMixed :: Lens.Lens' SentimentScore (Lude.Maybe Lude.Double)
ssMixed = Lens.lens (mixed :: SentimentScore -> Lude.Maybe Lude.Double) (\s a -> s {mixed = a} :: SentimentScore)
{-# DEPRECATED ssMixed "Use generic-lens or generic-optics with 'mixed' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
--
-- /Note:/ Consider using 'negative' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNegative :: Lens.Lens' SentimentScore (Lude.Maybe Lude.Double)
ssNegative = Lens.lens (negative :: SentimentScore -> Lude.Maybe Lude.Double) (\s a -> s {negative = a} :: SentimentScore)
{-# DEPRECATED ssNegative "Use generic-lens or generic-optics with 'negative' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
--
-- /Note:/ Consider using 'neutral' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNeutral :: Lens.Lens' SentimentScore (Lude.Maybe Lude.Double)
ssNeutral = Lens.lens (neutral :: SentimentScore -> Lude.Maybe Lude.Double) (\s a -> s {neutral = a} :: SentimentScore)
{-# DEPRECATED ssNeutral "Use generic-lens or generic-optics with 'neutral' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
--
-- /Note:/ Consider using 'positive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPositive :: Lens.Lens' SentimentScore (Lude.Maybe Lude.Double)
ssPositive = Lens.lens (positive :: SentimentScore -> Lude.Maybe Lude.Double) (\s a -> s {positive = a} :: SentimentScore)
{-# DEPRECATED ssPositive "Use generic-lens or generic-optics with 'positive' instead." #-}

instance Lude.FromJSON SentimentScore where
  parseJSON =
    Lude.withObject
      "SentimentScore"
      ( \x ->
          SentimentScore'
            Lude.<$> (x Lude..:? "Mixed")
            Lude.<*> (x Lude..:? "Negative")
            Lude.<*> (x Lude..:? "Neutral")
            Lude.<*> (x Lude..:? "Positive")
      )
