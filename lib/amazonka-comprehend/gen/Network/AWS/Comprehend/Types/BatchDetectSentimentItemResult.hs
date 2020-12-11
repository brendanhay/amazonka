-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
  ( BatchDetectSentimentItemResult (..),

    -- * Smart constructor
    mkBatchDetectSentimentItemResult,

    -- * Lenses
    bSentiment,
    bSentimentScore,
    bIndex,
  )
where

import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SentimentType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { sentiment ::
      Lude.Maybe SentimentType,
    sentimentScore ::
      Lude.Maybe SentimentScore,
    index :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDetectSentimentItemResult' with the minimum fields required to make a request.
--
-- * 'index' - The zero-based index of the document in the input list.
-- * 'sentiment' - The sentiment detected in the document.
-- * 'sentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
mkBatchDetectSentimentItemResult ::
  BatchDetectSentimentItemResult
mkBatchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { sentiment = Lude.Nothing,
      sentimentScore = Lude.Nothing,
      index = Lude.Nothing
    }

-- | The sentiment detected in the document.
--
-- /Note:/ Consider using 'sentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSentiment :: Lens.Lens' BatchDetectSentimentItemResult (Lude.Maybe SentimentType)
bSentiment = Lens.lens (sentiment :: BatchDetectSentimentItemResult -> Lude.Maybe SentimentType) (\s a -> s {sentiment = a} :: BatchDetectSentimentItemResult)
{-# DEPRECATED bSentiment "Use generic-lens or generic-optics with 'sentiment' instead." #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSentimentScore :: Lens.Lens' BatchDetectSentimentItemResult (Lude.Maybe SentimentScore)
bSentimentScore = Lens.lens (sentimentScore :: BatchDetectSentimentItemResult -> Lude.Maybe SentimentScore) (\s a -> s {sentimentScore = a} :: BatchDetectSentimentItemResult)
{-# DEPRECATED bSentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead." #-}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bIndex :: Lens.Lens' BatchDetectSentimentItemResult (Lude.Maybe Lude.Int)
bIndex = Lens.lens (index :: BatchDetectSentimentItemResult -> Lude.Maybe Lude.Int) (\s a -> s {index = a} :: BatchDetectSentimentItemResult)
{-# DEPRECATED bIndex "Use generic-lens or generic-optics with 'index' instead." #-}

instance Lude.FromJSON BatchDetectSentimentItemResult where
  parseJSON =
    Lude.withObject
      "BatchDetectSentimentItemResult"
      ( \x ->
          BatchDetectSentimentItemResult'
            Lude.<$> (x Lude..:? "Sentiment")
            Lude.<*> (x Lude..:? "SentimentScore")
            Lude.<*> (x Lude..:? "Index")
      )
