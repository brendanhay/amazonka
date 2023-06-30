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
-- Module      : Amazonka.Comprehend.Types.MentionSentiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.MentionSentiment where

import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SentimentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the sentiment and sentiment score for one mention of an entity.
--
-- For more information about targeted sentiment, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-targeted-sentiment.html Targeted sentiment>.
--
-- /See:/ 'newMentionSentiment' smart constructor.
data MentionSentiment = MentionSentiment'
  { -- | The sentiment of the mention.
    sentiment :: Prelude.Maybe SentimentType,
    sentimentScore :: Prelude.Maybe SentimentScore
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MentionSentiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentiment', 'mentionSentiment_sentiment' - The sentiment of the mention.
--
-- 'sentimentScore', 'mentionSentiment_sentimentScore' - Undocumented member.
newMentionSentiment ::
  MentionSentiment
newMentionSentiment =
  MentionSentiment'
    { sentiment = Prelude.Nothing,
      sentimentScore = Prelude.Nothing
    }

-- | The sentiment of the mention.
mentionSentiment_sentiment :: Lens.Lens' MentionSentiment (Prelude.Maybe SentimentType)
mentionSentiment_sentiment = Lens.lens (\MentionSentiment' {sentiment} -> sentiment) (\s@MentionSentiment' {} a -> s {sentiment = a} :: MentionSentiment)

-- | Undocumented member.
mentionSentiment_sentimentScore :: Lens.Lens' MentionSentiment (Prelude.Maybe SentimentScore)
mentionSentiment_sentimentScore = Lens.lens (\MentionSentiment' {sentimentScore} -> sentimentScore) (\s@MentionSentiment' {} a -> s {sentimentScore = a} :: MentionSentiment)

instance Data.FromJSON MentionSentiment where
  parseJSON =
    Data.withObject
      "MentionSentiment"
      ( \x ->
          MentionSentiment'
            Prelude.<$> (x Data..:? "Sentiment")
            Prelude.<*> (x Data..:? "SentimentScore")
      )

instance Prelude.Hashable MentionSentiment where
  hashWithSalt _salt MentionSentiment' {..} =
    _salt
      `Prelude.hashWithSalt` sentiment
      `Prelude.hashWithSalt` sentimentScore

instance Prelude.NFData MentionSentiment where
  rnf MentionSentiment' {..} =
    Prelude.rnf sentiment
      `Prelude.seq` Prelude.rnf sentimentScore
