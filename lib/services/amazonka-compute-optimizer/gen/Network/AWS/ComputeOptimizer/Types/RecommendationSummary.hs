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
-- Module      : Network.AWS.ComputeOptimizer.Types.RecommendationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ComputeOptimizer.Types.RecommendationSummary where

import Network.AWS.ComputeOptimizer.Types.RecommendationSourceType
import Network.AWS.ComputeOptimizer.Types.Summary
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of a recommendation.
--
-- /See:/ 'newRecommendationSummary' smart constructor.
data RecommendationSummary = RecommendationSummary'
  { -- | The Amazon Web Services account ID of the recommendation summary.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe a recommendation summary.
    summaries :: Prelude.Maybe [Summary],
    -- | The resource type of the recommendation.
    recommendationResourceType :: Prelude.Maybe RecommendationSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'recommendationSummary_accountId' - The Amazon Web Services account ID of the recommendation summary.
--
-- 'summaries', 'recommendationSummary_summaries' - An array of objects that describe a recommendation summary.
--
-- 'recommendationResourceType', 'recommendationSummary_recommendationResourceType' - The resource type of the recommendation.
newRecommendationSummary ::
  RecommendationSummary
newRecommendationSummary =
  RecommendationSummary'
    { accountId = Prelude.Nothing,
      summaries = Prelude.Nothing,
      recommendationResourceType = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the recommendation summary.
recommendationSummary_accountId :: Lens.Lens' RecommendationSummary (Prelude.Maybe Prelude.Text)
recommendationSummary_accountId = Lens.lens (\RecommendationSummary' {accountId} -> accountId) (\s@RecommendationSummary' {} a -> s {accountId = a} :: RecommendationSummary)

-- | An array of objects that describe a recommendation summary.
recommendationSummary_summaries :: Lens.Lens' RecommendationSummary (Prelude.Maybe [Summary])
recommendationSummary_summaries = Lens.lens (\RecommendationSummary' {summaries} -> summaries) (\s@RecommendationSummary' {} a -> s {summaries = a} :: RecommendationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The resource type of the recommendation.
recommendationSummary_recommendationResourceType :: Lens.Lens' RecommendationSummary (Prelude.Maybe RecommendationSourceType)
recommendationSummary_recommendationResourceType = Lens.lens (\RecommendationSummary' {recommendationResourceType} -> recommendationResourceType) (\s@RecommendationSummary' {} a -> s {recommendationResourceType = a} :: RecommendationSummary)

instance Core.FromJSON RecommendationSummary where
  parseJSON =
    Core.withObject
      "RecommendationSummary"
      ( \x ->
          RecommendationSummary'
            Prelude.<$> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "summaries" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "recommendationResourceType")
      )

instance Prelude.Hashable RecommendationSummary

instance Prelude.NFData RecommendationSummary
