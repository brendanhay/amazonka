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
-- Module      : Amazonka.ComputeOptimizer.Types.RecommendationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.RecommendationSummary where

import Amazonka.ComputeOptimizer.Types.CurrentPerformanceRiskRatings
import Amazonka.ComputeOptimizer.Types.InferredWorkloadSaving
import Amazonka.ComputeOptimizer.Types.RecommendationSourceType
import Amazonka.ComputeOptimizer.Types.SavingsOpportunity
import Amazonka.ComputeOptimizer.Types.Summary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of a recommendation.
--
-- /See:/ 'newRecommendationSummary' smart constructor.
data RecommendationSummary = RecommendationSummary'
  { -- | The Amazon Web Services account ID of the recommendation summary.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the performance risk ratings for a given
    -- resource type.
    currentPerformanceRiskRatings :: Prelude.Maybe CurrentPerformanceRiskRatings,
    -- | An array of objects that describes the estimated monthly saving amounts
    -- for the instances running on the specified @inferredWorkloadTypes@. The
    -- array contains the top three savings opportunites for the instances
    -- running inferred workload types.
    inferredWorkloadSavings :: Prelude.Maybe [InferredWorkloadSaving],
    -- | The resource type that the recommendation summary applies to.
    recommendationResourceType :: Prelude.Maybe RecommendationSourceType,
    -- | An object that describes the savings opportunity for a given resource
    -- type. Savings opportunity includes the estimated monthly savings amount
    -- and percentage.
    savingsOpportunity :: Prelude.Maybe SavingsOpportunity,
    -- | An array of objects that describe a recommendation summary.
    summaries :: Prelude.Maybe [Summary]
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
-- 'currentPerformanceRiskRatings', 'recommendationSummary_currentPerformanceRiskRatings' - An object that describes the performance risk ratings for a given
-- resource type.
--
-- 'inferredWorkloadSavings', 'recommendationSummary_inferredWorkloadSavings' - An array of objects that describes the estimated monthly saving amounts
-- for the instances running on the specified @inferredWorkloadTypes@. The
-- array contains the top three savings opportunites for the instances
-- running inferred workload types.
--
-- 'recommendationResourceType', 'recommendationSummary_recommendationResourceType' - The resource type that the recommendation summary applies to.
--
-- 'savingsOpportunity', 'recommendationSummary_savingsOpportunity' - An object that describes the savings opportunity for a given resource
-- type. Savings opportunity includes the estimated monthly savings amount
-- and percentage.
--
-- 'summaries', 'recommendationSummary_summaries' - An array of objects that describe a recommendation summary.
newRecommendationSummary ::
  RecommendationSummary
newRecommendationSummary =
  RecommendationSummary'
    { accountId = Prelude.Nothing,
      currentPerformanceRiskRatings = Prelude.Nothing,
      inferredWorkloadSavings = Prelude.Nothing,
      recommendationResourceType = Prelude.Nothing,
      savingsOpportunity = Prelude.Nothing,
      summaries = Prelude.Nothing
    }

-- | The Amazon Web Services account ID of the recommendation summary.
recommendationSummary_accountId :: Lens.Lens' RecommendationSummary (Prelude.Maybe Prelude.Text)
recommendationSummary_accountId = Lens.lens (\RecommendationSummary' {accountId} -> accountId) (\s@RecommendationSummary' {} a -> s {accountId = a} :: RecommendationSummary)

-- | An object that describes the performance risk ratings for a given
-- resource type.
recommendationSummary_currentPerformanceRiskRatings :: Lens.Lens' RecommendationSummary (Prelude.Maybe CurrentPerformanceRiskRatings)
recommendationSummary_currentPerformanceRiskRatings = Lens.lens (\RecommendationSummary' {currentPerformanceRiskRatings} -> currentPerformanceRiskRatings) (\s@RecommendationSummary' {} a -> s {currentPerformanceRiskRatings = a} :: RecommendationSummary)

-- | An array of objects that describes the estimated monthly saving amounts
-- for the instances running on the specified @inferredWorkloadTypes@. The
-- array contains the top three savings opportunites for the instances
-- running inferred workload types.
recommendationSummary_inferredWorkloadSavings :: Lens.Lens' RecommendationSummary (Prelude.Maybe [InferredWorkloadSaving])
recommendationSummary_inferredWorkloadSavings = Lens.lens (\RecommendationSummary' {inferredWorkloadSavings} -> inferredWorkloadSavings) (\s@RecommendationSummary' {} a -> s {inferredWorkloadSavings = a} :: RecommendationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The resource type that the recommendation summary applies to.
recommendationSummary_recommendationResourceType :: Lens.Lens' RecommendationSummary (Prelude.Maybe RecommendationSourceType)
recommendationSummary_recommendationResourceType = Lens.lens (\RecommendationSummary' {recommendationResourceType} -> recommendationResourceType) (\s@RecommendationSummary' {} a -> s {recommendationResourceType = a} :: RecommendationSummary)

-- | An object that describes the savings opportunity for a given resource
-- type. Savings opportunity includes the estimated monthly savings amount
-- and percentage.
recommendationSummary_savingsOpportunity :: Lens.Lens' RecommendationSummary (Prelude.Maybe SavingsOpportunity)
recommendationSummary_savingsOpportunity = Lens.lens (\RecommendationSummary' {savingsOpportunity} -> savingsOpportunity) (\s@RecommendationSummary' {} a -> s {savingsOpportunity = a} :: RecommendationSummary)

-- | An array of objects that describe a recommendation summary.
recommendationSummary_summaries :: Lens.Lens' RecommendationSummary (Prelude.Maybe [Summary])
recommendationSummary_summaries = Lens.lens (\RecommendationSummary' {summaries} -> summaries) (\s@RecommendationSummary' {} a -> s {summaries = a} :: RecommendationSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON RecommendationSummary where
  parseJSON =
    Data.withObject
      "RecommendationSummary"
      ( \x ->
          RecommendationSummary'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "currentPerformanceRiskRatings")
            Prelude.<*> ( x
                            Data..:? "inferredWorkloadSavings"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "recommendationResourceType")
            Prelude.<*> (x Data..:? "savingsOpportunity")
            Prelude.<*> (x Data..:? "summaries" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable RecommendationSummary where
  hashWithSalt _salt RecommendationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentPerformanceRiskRatings
      `Prelude.hashWithSalt` inferredWorkloadSavings
      `Prelude.hashWithSalt` recommendationResourceType
      `Prelude.hashWithSalt` savingsOpportunity
      `Prelude.hashWithSalt` summaries

instance Prelude.NFData RecommendationSummary where
  rnf RecommendationSummary' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentPerformanceRiskRatings
      `Prelude.seq` Prelude.rnf inferredWorkloadSavings
      `Prelude.seq` Prelude.rnf recommendationResourceType
      `Prelude.seq` Prelude.rnf savingsOpportunity
      `Prelude.seq` Prelude.rnf summaries
