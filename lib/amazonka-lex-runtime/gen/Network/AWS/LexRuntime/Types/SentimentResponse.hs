{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.SentimentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.SentimentResponse
  ( SentimentResponse (..),

    -- * Smart constructor
    mkSentimentResponse,

    -- * Lenses
    sSentimentScore,
    sSentimentLabel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for sentiment analysis, this field structure contains the result of the analysis.
--
-- /See:/ 'mkSentimentResponse' smart constructor.
data SentimentResponse = SentimentResponse'
  { -- | The likelihood that the sentiment was correctly inferred.
    sentimentScore :: Lude.Maybe Lude.Text,
    -- | The inferred sentiment that Amazon Comprehend has the highest confidence in.
    sentimentLabel :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SentimentResponse' with the minimum fields required to make a request.
--
-- * 'sentimentScore' - The likelihood that the sentiment was correctly inferred.
-- * 'sentimentLabel' - The inferred sentiment that Amazon Comprehend has the highest confidence in.
mkSentimentResponse ::
  SentimentResponse
mkSentimentResponse =
  SentimentResponse'
    { sentimentScore = Lude.Nothing,
      sentimentLabel = Lude.Nothing
    }

-- | The likelihood that the sentiment was correctly inferred.
--
-- /Note:/ Consider using 'sentimentScore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSentimentScore :: Lens.Lens' SentimentResponse (Lude.Maybe Lude.Text)
sSentimentScore = Lens.lens (sentimentScore :: SentimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {sentimentScore = a} :: SentimentResponse)
{-# DEPRECATED sSentimentScore "Use generic-lens or generic-optics with 'sentimentScore' instead." #-}

-- | The inferred sentiment that Amazon Comprehend has the highest confidence in.
--
-- /Note:/ Consider using 'sentimentLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSentimentLabel :: Lens.Lens' SentimentResponse (Lude.Maybe Lude.Text)
sSentimentLabel = Lens.lens (sentimentLabel :: SentimentResponse -> Lude.Maybe Lude.Text) (\s a -> s {sentimentLabel = a} :: SentimentResponse)
{-# DEPRECATED sSentimentLabel "Use generic-lens or generic-optics with 'sentimentLabel' instead." #-}

instance Lude.FromJSON SentimentResponse where
  parseJSON =
    Lude.withObject
      "SentimentResponse"
      ( \x ->
          SentimentResponse'
            Lude.<$> (x Lude..:? "sentimentScore")
            Lude.<*> (x Lude..:? "sentimentLabel")
      )
