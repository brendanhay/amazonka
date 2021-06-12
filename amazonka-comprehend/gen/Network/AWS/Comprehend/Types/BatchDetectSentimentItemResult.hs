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
-- Module      : Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.BatchDetectSentimentItemResult where

import Network.AWS.Comprehend.Types.SentimentScore
import Network.AWS.Comprehend.Types.SentimentType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The result of calling the operation. The operation returns one object
-- for each document that is successfully processed by the operation.
--
-- /See:/ 'newBatchDetectSentimentItemResult' smart constructor.
data BatchDetectSentimentItemResult = BatchDetectSentimentItemResult'
  { -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its sentiment detection.
    sentimentScore :: Core.Maybe SentimentScore,
    -- | The sentiment detected in the document.
    sentiment :: Core.Maybe SentimentType,
    -- | The zero-based index of the document in the input list.
    index :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDetectSentimentItemResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentScore', 'batchDetectSentimentItemResult_sentimentScore' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
--
-- 'sentiment', 'batchDetectSentimentItemResult_sentiment' - The sentiment detected in the document.
--
-- 'index', 'batchDetectSentimentItemResult_index' - The zero-based index of the document in the input list.
newBatchDetectSentimentItemResult ::
  BatchDetectSentimentItemResult
newBatchDetectSentimentItemResult =
  BatchDetectSentimentItemResult'
    { sentimentScore =
        Core.Nothing,
      sentiment = Core.Nothing,
      index = Core.Nothing
    }

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its sentiment detection.
batchDetectSentimentItemResult_sentimentScore :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe SentimentScore)
batchDetectSentimentItemResult_sentimentScore = Lens.lens (\BatchDetectSentimentItemResult' {sentimentScore} -> sentimentScore) (\s@BatchDetectSentimentItemResult' {} a -> s {sentimentScore = a} :: BatchDetectSentimentItemResult)

-- | The sentiment detected in the document.
batchDetectSentimentItemResult_sentiment :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe SentimentType)
batchDetectSentimentItemResult_sentiment = Lens.lens (\BatchDetectSentimentItemResult' {sentiment} -> sentiment) (\s@BatchDetectSentimentItemResult' {} a -> s {sentiment = a} :: BatchDetectSentimentItemResult)

-- | The zero-based index of the document in the input list.
batchDetectSentimentItemResult_index :: Lens.Lens' BatchDetectSentimentItemResult (Core.Maybe Core.Int)
batchDetectSentimentItemResult_index = Lens.lens (\BatchDetectSentimentItemResult' {index} -> index) (\s@BatchDetectSentimentItemResult' {} a -> s {index = a} :: BatchDetectSentimentItemResult)

instance Core.FromJSON BatchDetectSentimentItemResult where
  parseJSON =
    Core.withObject
      "BatchDetectSentimentItemResult"
      ( \x ->
          BatchDetectSentimentItemResult'
            Core.<$> (x Core..:? "SentimentScore")
            Core.<*> (x Core..:? "Sentiment")
            Core.<*> (x Core..:? "Index")
      )

instance Core.Hashable BatchDetectSentimentItemResult

instance Core.NFData BatchDetectSentimentItemResult
