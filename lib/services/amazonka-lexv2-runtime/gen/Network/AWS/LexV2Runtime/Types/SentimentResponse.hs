{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexV2Runtime.Types.SentimentResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Runtime.Types.SentimentResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Runtime.Types.SentimentScore
import Network.AWS.LexV2Runtime.Types.SentimentType
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the sentiment expressed in a user\'s response
-- in a conversation. Sentiments are determined using Amazon Comprehend.
-- Sentiments are only returned if they are enabled for the bot.
--
-- For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-sentiment.html Determine Sentiment>
-- in the /Amazon Comprehend developer guide/.
--
-- /See:/ 'newSentimentResponse' smart constructor.
data SentimentResponse = SentimentResponse'
  { -- | The overall sentiment expressed in the user\'s response. This is the
    -- sentiment most likely expressed by the user based on the analysis by
    -- Amazon Comprehend.
    sentiment :: Prelude.Maybe SentimentType,
    sentimentScore :: Prelude.Maybe SentimentScore
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentiment', 'sentimentResponse_sentiment' - The overall sentiment expressed in the user\'s response. This is the
-- sentiment most likely expressed by the user based on the analysis by
-- Amazon Comprehend.
--
-- 'sentimentScore', 'sentimentResponse_sentimentScore' - Undocumented member.
newSentimentResponse ::
  SentimentResponse
newSentimentResponse =
  SentimentResponse'
    { sentiment = Prelude.Nothing,
      sentimentScore = Prelude.Nothing
    }

-- | The overall sentiment expressed in the user\'s response. This is the
-- sentiment most likely expressed by the user based on the analysis by
-- Amazon Comprehend.
sentimentResponse_sentiment :: Lens.Lens' SentimentResponse (Prelude.Maybe SentimentType)
sentimentResponse_sentiment = Lens.lens (\SentimentResponse' {sentiment} -> sentiment) (\s@SentimentResponse' {} a -> s {sentiment = a} :: SentimentResponse)

-- | Undocumented member.
sentimentResponse_sentimentScore :: Lens.Lens' SentimentResponse (Prelude.Maybe SentimentScore)
sentimentResponse_sentimentScore = Lens.lens (\SentimentResponse' {sentimentScore} -> sentimentScore) (\s@SentimentResponse' {} a -> s {sentimentScore = a} :: SentimentResponse)

instance Core.FromJSON SentimentResponse where
  parseJSON =
    Core.withObject
      "SentimentResponse"
      ( \x ->
          SentimentResponse'
            Prelude.<$> (x Core..:? "sentiment")
            Prelude.<*> (x Core..:? "sentimentScore")
      )

instance Prelude.Hashable SentimentResponse

instance Prelude.NFData SentimentResponse
