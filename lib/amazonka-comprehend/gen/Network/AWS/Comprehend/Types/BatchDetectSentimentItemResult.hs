{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
  ( BatchDetectSentimentItemResult (..)
  -- * Smart constructor
  , mkBatchDetectSentimentItemResult
  -- * Lenses
  , bIndex
  , bSentiment
  , bSentimentScore
  ) where

import qualified Network.AWS.Comprehend.Types.SentimentScore as Types
import qualified Network.AWS.Comprehend.Types.SentimentType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.
--
-- /See:/ 'mkBatchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { index :: Core.Maybe Core.Int
    -- ^ The zero-based index of the document in the input list.
  , sentiment :: Core.Maybe Types.SentimentType
    -- ^ The sentiment detected in the document.
  , sentimentScore :: Core.Maybe Types.SentimentScore
    -- ^ The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchDetectSentimentItemResult' value with any optional fields omitted.
mkBatchDetectSentimentItemResult
    :: BatchDetectSentimentItemResult
mkBatchDetectSentimentItemResult
  = BatchDetectSentimentItemResult'{index = Core.Nothing,
                                    sentiment = Core.Nothing, sentimentScore = Core.Nothing}

-- | The zero-based index of the document in the input list.
--
-- /Note:/ Consider using 'index' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bIndex :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe Core.Int)
bIndex = Lens.field @"index"
{-# INLINEABLE bIndex #-}
{-# DEPRECATED index "Use generic-lens or generic-optics with 'index' instead"  #-}

-- | The sentiment detected in the document.
--
-- /Note:/ Consider using 'sentiment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSentiment :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe Types.SentimentType)
bSentiment = Lens.field @"sentiment"
{-# INLINEABLE bSentiment #-}
{-# DEPRECATED sentiment "Use generic-lens or generic-optics with 'sentiment' instead"  #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its sentiment detection.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bSentimentScore :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe Types.SentimentScore)
bSentimentScore = Lens.field @"sentimentScore"
{-# INLINEABLE bSentimentScore #-}
{-# DEPRECATED sentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead"  #-}

instance Core.FromJSON BatchDetectSentimentItemResult where
        parseJSON
          = Core.withObject "BatchDetectSentimentItemResult" Core.$
              \ x ->
                BatchDetectSentimentItemResult' Core.<$>
                  (x Core..:? "Index") Core.<*> x Core..:? "Sentiment" Core.<*>
                    x Core..:? "SentimentScore"
