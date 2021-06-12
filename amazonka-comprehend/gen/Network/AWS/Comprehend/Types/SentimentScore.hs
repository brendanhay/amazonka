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
-- Module      : Network.AWS.Comprehend.Types.SentimentScore
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.SentimentScore where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the level of confidence that Amazon Comprehend has in the
-- accuracy of its detection of sentiments.
--
-- /See:/ 'newSentimentScore' smart constructor.
data SentimentScore = SentimentScore'
  { -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its detection of the @NEGATIVE@ sentiment.
    negative :: Core.Maybe Core.Double,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its detection of the @MIXED@ sentiment.
    mixed :: Core.Maybe Core.Double,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its detection of the @POSITIVE@ sentiment.
    positive :: Core.Maybe Core.Double,
    -- | The level of confidence that Amazon Comprehend has in the accuracy of
    -- its detection of the @NEUTRAL@ sentiment.
    neutral :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SentimentScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negative', 'sentimentScore_negative' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @NEGATIVE@ sentiment.
--
-- 'mixed', 'sentimentScore_mixed' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @MIXED@ sentiment.
--
-- 'positive', 'sentimentScore_positive' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @POSITIVE@ sentiment.
--
-- 'neutral', 'sentimentScore_neutral' - The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @NEUTRAL@ sentiment.
newSentimentScore ::
  SentimentScore
newSentimentScore =
  SentimentScore'
    { negative = Core.Nothing,
      mixed = Core.Nothing,
      positive = Core.Nothing,
      neutral = Core.Nothing
    }

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @NEGATIVE@ sentiment.
sentimentScore_negative :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
sentimentScore_negative = Lens.lens (\SentimentScore' {negative} -> negative) (\s@SentimentScore' {} a -> s {negative = a} :: SentimentScore)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @MIXED@ sentiment.
sentimentScore_mixed :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
sentimentScore_mixed = Lens.lens (\SentimentScore' {mixed} -> mixed) (\s@SentimentScore' {} a -> s {mixed = a} :: SentimentScore)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @POSITIVE@ sentiment.
sentimentScore_positive :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
sentimentScore_positive = Lens.lens (\SentimentScore' {positive} -> positive) (\s@SentimentScore' {} a -> s {positive = a} :: SentimentScore)

-- | The level of confidence that Amazon Comprehend has in the accuracy of
-- its detection of the @NEUTRAL@ sentiment.
sentimentScore_neutral :: Lens.Lens' SentimentScore (Core.Maybe Core.Double)
sentimentScore_neutral = Lens.lens (\SentimentScore' {neutral} -> neutral) (\s@SentimentScore' {} a -> s {neutral = a} :: SentimentScore)

instance Core.FromJSON SentimentScore where
  parseJSON =
    Core.withObject
      "SentimentScore"
      ( \x ->
          SentimentScore'
            Core.<$> (x Core..:? "Negative")
            Core.<*> (x Core..:? "Mixed")
            Core.<*> (x Core..:? "Positive")
            Core.<*> (x Core..:? "Neutral")
      )

instance Core.Hashable SentimentScore

instance Core.NFData SentimentScore
