{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.SentimentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexRuntime.Types.SentimentResponse
  ( SentimentResponse (..)
  -- * Smart constructor
  , mkSentimentResponse
  -- * Lenses
  , srSentimentLabel
  , srSentimentScore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexRuntime.Types.SentimentLabel as Types
import qualified Network.AWS.LexRuntime.Types.SentimentScore as Types
import qualified Network.AWS.Prelude as Core

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field structure contains the result of the analysis.
--
-- /See:/ 'mkSentimentResponse' smart constructor.
data SentimentResponse = SentimentResponse'
  { sentimentLabel :: Core.Maybe Types.SentimentLabel
    -- ^ The inferred sentiment that Amazon Comprehend has the highest confidence in.
  , sentimentScore :: Core.Maybe Types.SentimentScore
    -- ^ The likelihood that the sentiment was correctly inferred.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SentimentResponse' value with any optional fields omitted.
mkSentimentResponse
    :: SentimentResponse
mkSentimentResponse
  = SentimentResponse'{sentimentLabel = Core.Nothing,
                       sentimentScore = Core.Nothing}

-- | The inferred sentiment that Amazon Comprehend has the highest confidence in.
--
-- /Note:/ Consider using 'sentimentLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSentimentLabel :: Lens.Lens' SentimentResponse (Core.Maybe Types.SentimentLabel)
srSentimentLabel = Lens.field @"sentimentLabel"
{-# INLINEABLE srSentimentLabel #-}
{-# DEPRECATED sentimentLabel "Use generic-lens or generic-optics with 'sentimentLabel' instead"  #-}

-- | The likelihood that the sentiment was correctly inferred.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srSentimentScore :: Lens.Lens' SentimentResponse (Core.Maybe Types.SentimentScore)
srSentimentScore = Lens.field @"sentimentScore"
{-# INLINEABLE srSentimentScore #-}
{-# DEPRECATED sentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead"  #-}

instance Core.FromJSON SentimentResponse where
        parseJSON
          = Core.withObject "SentimentResponse" Core.$
              \ x ->
                SentimentResponse' Core.<$>
                  (x Core..:? "sentimentLabel") Core.<*> x Core..:? "sentimentScore"
