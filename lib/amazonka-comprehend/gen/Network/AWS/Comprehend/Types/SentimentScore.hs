{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.SentimentScore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.SentimentScore
  ( SentimentScore (..)
  -- * Smart constructor
  , mkSentimentScore
  -- * Lenses
  , ssMixed
  , ssNegative
  , ssNeutral
  , ssPositive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.
--
-- /See:/ 'mkSentimentScore' smart constructor.
data SentimentScore = SentimentScore'
  { mixed :: Core.Maybe Core.Double
    -- ^ The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
  , negative :: Core.Maybe Core.Double
    -- ^ The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
  , neutral :: Core.Maybe Core.Double
    -- ^ The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
  , positive :: Core.Maybe Core.Double
    -- ^ The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SentimentScore' value with any optional fields omitted.
mkSentimentScore
    :: SentimentScore
mkSentimentScore
  = SentimentScore'{mixed = Core.Nothing, negative = Core.Nothing,
                    neutral = Core.Nothing, positive = Core.Nothing}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @MIXED@ sentiment.
--
-- /Note:/ Consider using 'mixed' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMixed :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
ssMixed = Lens.field @"mixed"
{-# INLINEABLE ssMixed #-}
{-# DEPRECATED mixed "Use generic-lens or generic-optics with 'mixed' instead"  #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEGATIVE@ sentiment.
--
-- /Note:/ Consider using 'negative' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNegative :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
ssNegative = Lens.field @"negative"
{-# INLINEABLE ssNegative #-}
{-# DEPRECATED negative "Use generic-lens or generic-optics with 'negative' instead"  #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @NEUTRAL@ sentiment.
--
-- /Note:/ Consider using 'neutral' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssNeutral :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
ssNeutral = Lens.field @"neutral"
{-# INLINEABLE ssNeutral #-}
{-# DEPRECATED neutral "Use generic-lens or generic-optics with 'neutral' instead"  #-}

-- | The level of confidence that Amazon Comprehend has in the accuracy of its detection of the @POSITIVE@ sentiment.
--
-- /Note:/ Consider using 'positive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssPositive :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
ssPositive = Lens.field @"positive"
{-# INLINEABLE ssPositive #-}
{-# DEPRECATED positive "Use generic-lens or generic-optics with 'positive' instead"  #-}

instance Core.FromJSON SentimentScore where
        parseJSON
          = Core.withObject "SentimentScore" Core.$
              \ x ->
                SentimentScore' Core.<$>
                  (x Core..:? "Mixed") Core.<*> x Core..:? "Negative" Core.<*>
                    x Core..:? "Neutral"
                    Core.<*> x Core..:? "Positive"
