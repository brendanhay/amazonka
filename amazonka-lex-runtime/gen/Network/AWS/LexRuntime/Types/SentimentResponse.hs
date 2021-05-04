{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.LexRuntime.Types.SentimentResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.SentimentResponse where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The sentiment expressed in an utterance.
--
-- When the bot is configured to send utterances to Amazon Comprehend for
-- sentiment analysis, this field structure contains the result of the
-- analysis.
--
-- /See:/ 'newSentimentResponse' smart constructor.
data SentimentResponse = SentimentResponse'
  { -- | The likelihood that the sentiment was correctly inferred.
    sentimentScore :: Prelude.Maybe Prelude.Text,
    -- | The inferred sentiment that Amazon Comprehend has the highest confidence
    -- in.
    sentimentLabel :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SentimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentScore', 'sentimentResponse_sentimentScore' - The likelihood that the sentiment was correctly inferred.
--
-- 'sentimentLabel', 'sentimentResponse_sentimentLabel' - The inferred sentiment that Amazon Comprehend has the highest confidence
-- in.
newSentimentResponse ::
  SentimentResponse
newSentimentResponse =
  SentimentResponse'
    { sentimentScore =
        Prelude.Nothing,
      sentimentLabel = Prelude.Nothing
    }

-- | The likelihood that the sentiment was correctly inferred.
sentimentResponse_sentimentScore :: Lens.Lens' SentimentResponse (Prelude.Maybe Prelude.Text)
sentimentResponse_sentimentScore = Lens.lens (\SentimentResponse' {sentimentScore} -> sentimentScore) (\s@SentimentResponse' {} a -> s {sentimentScore = a} :: SentimentResponse)

-- | The inferred sentiment that Amazon Comprehend has the highest confidence
-- in.
sentimentResponse_sentimentLabel :: Lens.Lens' SentimentResponse (Prelude.Maybe Prelude.Text)
sentimentResponse_sentimentLabel = Lens.lens (\SentimentResponse' {sentimentLabel} -> sentimentLabel) (\s@SentimentResponse' {} a -> s {sentimentLabel = a} :: SentimentResponse)

instance Prelude.FromJSON SentimentResponse where
  parseJSON =
    Prelude.withObject
      "SentimentResponse"
      ( \x ->
          SentimentResponse'
            Prelude.<$> (x Prelude..:? "sentimentScore")
            Prelude.<*> (x Prelude..:? "sentimentLabel")
      )

instance Prelude.Hashable SentimentResponse

instance Prelude.NFData SentimentResponse
