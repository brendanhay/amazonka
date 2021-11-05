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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BatchDetectSentimentItemResult where

import Amazonka.Comprehend.Types.SentimentScore
import Amazonka.Comprehend.Types.SentimentType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { -- | The sentiment detected in the document.
    sentiment :: Prelude.Maybe SentimentType,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its sentiment detection.
    sentimentScore :: Prelude.Maybe SentimentScore,
    -- | The zero-based index of the document in the input list.
    index :: Prelude.Maybe Prelude.Int
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
-- 'sentiment', 'batchDetectSentimentItemResult_sentiment' - The sentiment detected in the document.
--
-- 'sentimentScore', 'batchDetectSentimentItemResult_sentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
--
-- 'index', 'batchDetectSentimentItemResult_index' - The zero-based index of the document in the input list.
newBatchDetectSentimentItemResult ::
  BatchDetectSentimentItemResult
newBatchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { sentiment =
        Prelude.Nothing,
      sentimentScore = Prelude.Nothing,
      index = Prelude.Nothing
    }

-- | The sentiment detected in the document.
batchDetectSentimentItemResult_sentiment :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe SentimentType)
batchDetectSentimentItemResult_sentiment = Lens.lens (\BatchDetectSentimentItemResult' {sentiment} -> sentiment) (\s@BatchDetectSentimentItemResult' {} a -> s {sentiment = a} :: BatchDetectSentimentItemResult)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
batchDetectSentimentItemResult_sentimentScore :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe SentimentScore)
batchDetectSentimentItemResult_sentimentScore = Lens.lens (\BatchDetectSentimentItemResult' {sentimentScore} -> sentimentScore) (\s@BatchDetectSentimentItemResult' {} a -> s {sentimentScore = a} :: BatchDetectSentimentItemResult)

-- | The zero-based index of the document in the input list.
batchDetectSentimentItemResult_index :: Lens.Lens' BatchDetectSentimentItemResult (Prelude.Maybe Prelude.Int)
batchDetectSentimentItemResult_index = Lens.lens (\BatchDetectSentimentItemResult' {index} -> index) (\s@BatchDetectSentimentItemResult' {} a -> s {index = a} :: BatchDetectSentimentItemResult)

instance Core.FromJSON BatchDetectSentimentItemResult where
  parseJSON =
    Core.withObject
      "BatchDetectSentimentItemResult"
      ( \x ->
          BatchDetectSentimentItemResult'
            Prelude.<$> (x Core..:? "Sentiment")
            Prelude.<*> (x Core..:? "SentimentScore")
            Prelude.<*> (x Core..:? "Index")
      )

instance
  Prelude.Hashable
    BatchDetectSentimentItemResult

instance
  Prelude.NFData
    BatchDetectSentimentItemResult
