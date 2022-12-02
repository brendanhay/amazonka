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
-- Module      : Amazonka.Wisdom.Types.RecommendationData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Wisdom.Types.RecommendationData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Wisdom.Types.Document
import Amazonka.Wisdom.Types.RecommendationType
import Amazonka.Wisdom.Types.RelevanceLevel

-- | Information about the recommendation.
--
-- /See:/ 'newRecommendationData' smart constructor.
data RecommendationData = RecommendationData'
  { -- | The type of recommendation.
    type' :: Prelude.Maybe RecommendationType,
    -- | The relevance level of the recommendation.
    relevanceLevel :: Prelude.Maybe RelevanceLevel,
    -- | The relevance score of the recommendation.
    relevanceScore :: Prelude.Maybe Prelude.Double,
    -- | The recommended document.
    document :: Document,
    -- | The identifier of the recommendation.
    recommendationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'recommendationData_type' - The type of recommendation.
--
-- 'relevanceLevel', 'recommendationData_relevanceLevel' - The relevance level of the recommendation.
--
-- 'relevanceScore', 'recommendationData_relevanceScore' - The relevance score of the recommendation.
--
-- 'document', 'recommendationData_document' - The recommended document.
--
-- 'recommendationId', 'recommendationData_recommendationId' - The identifier of the recommendation.
newRecommendationData ::
  -- | 'document'
  Document ->
  -- | 'recommendationId'
  Prelude.Text ->
  RecommendationData
newRecommendationData pDocument_ pRecommendationId_ =
  RecommendationData'
    { type' = Prelude.Nothing,
      relevanceLevel = Prelude.Nothing,
      relevanceScore = Prelude.Nothing,
      document = pDocument_,
      recommendationId = pRecommendationId_
    }

-- | The type of recommendation.
recommendationData_type :: Lens.Lens' RecommendationData (Prelude.Maybe RecommendationType)
recommendationData_type = Lens.lens (\RecommendationData' {type'} -> type') (\s@RecommendationData' {} a -> s {type' = a} :: RecommendationData)

-- | The relevance level of the recommendation.
recommendationData_relevanceLevel :: Lens.Lens' RecommendationData (Prelude.Maybe RelevanceLevel)
recommendationData_relevanceLevel = Lens.lens (\RecommendationData' {relevanceLevel} -> relevanceLevel) (\s@RecommendationData' {} a -> s {relevanceLevel = a} :: RecommendationData)

-- | The relevance score of the recommendation.
recommendationData_relevanceScore :: Lens.Lens' RecommendationData (Prelude.Maybe Prelude.Double)
recommendationData_relevanceScore = Lens.lens (\RecommendationData' {relevanceScore} -> relevanceScore) (\s@RecommendationData' {} a -> s {relevanceScore = a} :: RecommendationData)

-- | The recommended document.
recommendationData_document :: Lens.Lens' RecommendationData Document
recommendationData_document = Lens.lens (\RecommendationData' {document} -> document) (\s@RecommendationData' {} a -> s {document = a} :: RecommendationData)

-- | The identifier of the recommendation.
recommendationData_recommendationId :: Lens.Lens' RecommendationData Prelude.Text
recommendationData_recommendationId = Lens.lens (\RecommendationData' {recommendationId} -> recommendationId) (\s@RecommendationData' {} a -> s {recommendationId = a} :: RecommendationData)

instance Data.FromJSON RecommendationData where
  parseJSON =
    Data.withObject
      "RecommendationData"
      ( \x ->
          RecommendationData'
            Prelude.<$> (x Data..:? "type")
            Prelude.<*> (x Data..:? "relevanceLevel")
            Prelude.<*> (x Data..:? "relevanceScore")
            Prelude.<*> (x Data..: "document")
            Prelude.<*> (x Data..: "recommendationId")
      )

instance Prelude.Hashable RecommendationData where
  hashWithSalt _salt RecommendationData' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` relevanceLevel
      `Prelude.hashWithSalt` relevanceScore
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` recommendationId

instance Prelude.NFData RecommendationData where
  rnf RecommendationData' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf relevanceLevel
      `Prelude.seq` Prelude.rnf relevanceScore
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf recommendationId
