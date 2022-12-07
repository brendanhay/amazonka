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
-- Module      : Amazonka.Comprehend.Types.BatchDetectSentimentItemResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectSentimentItemResult where

import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SentimentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its sentiment detection.
    sentimentScore :: Prelude.Maybe SentimentScore,
    -- | The sentiment detected in the document.
    sentiment :: Prelude.Maybe SentimentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDetectSentimentItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'batchDetectSentimentItemResult_index' - The zero-based index of the document in the input list.
--
-- 'sentimentScore', 'batchDetectSentimentItemResult_sentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
--
-- 'sentiment', 'batchDetectSentimentItemResult_sentiment' - The sentiment detected in the document.
newBatchDetectSentimentItemResult ::
  BatchDetectSentimentItemResult
newBatchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { index =
        Prelude.Nothing,
      sentimentScore = Prelude.Nothing,
      sentiment = Prelude.Nothing
    }

-- | The zero-based index of the document in the input list.
batchDetectSentimentItemResult_index :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe Prelude.Int)
batchDetectSentimentItemResult_index = Lens.lens (\BatchDetectSentimentItemResult' {index} -> index) (\s@BatchDetectSentimentItemResult' {} a -> s {index = a} :: BatchDetectSentimentItemResult)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
batchDetectSentimentItemResult_sentimentScore :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe SentimentScore)
batchDetectSentimentItemResult_sentimentScore = Lens.lens (\BatchDetectSentimentItemResult' {sentimentScore} -> sentimentScore) (\s@BatchDetectSentimentItemResult' {} a -> s {sentimentScore = a} :: BatchDetectSentimentItemResult)

-- | The sentiment detected in the document.
batchDetectSentimentItemResult_sentiment :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe SentimentType)
batchDetectSentimentItemResult_sentiment = Lens.lens (\BatchDetectSentimentItemResult' {sentiment} -> sentiment) (\s@BatchDetectSentimentItemResult' {} a -> s {sentiment = a} :: BatchDetectSentimentItemResult)

instance Data.FromJSON BatchDetectSentimentItemResult where
  parseJSON =
    Data.withObject
      "BatchDetectSentimentItemResult"
      ( \x ->
          BatchDetectSentimentItemResult'
            Prelude.<$> (x Data..:? "Index")
            Prelude.<*> (x Data..:? "SentimentScore")
            Prelude.<*> (x Data..:? "Sentiment")
      )

instance
  Prelude.Hashable
    BatchDetectSentimentItemResult
  where
  hashWithSalt
    _salt
    BatchDetectSentimentItemResult' {..} =
      _salt `Prelude.hashWithSalt` index
        `Prelude.hashWithSalt` sentimentScore
        `Prelude.hashWithSalt` sentiment

instance
  Prelude.NFData
    BatchDetectSentimentItemResult
  where
  rnf BatchDetectSentimentItemResult' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf sentimentScore
      `Prelude.seq` Prelude.rnf sentiment
