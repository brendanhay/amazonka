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
-- Module      : Amazonka.CostExplorer.Types.GenerationSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.GenerationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.GenerationStatus
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary of the Savings Plans recommendation generation.
--
-- /See:/ 'newGenerationSummary' smart constructor.
data GenerationSummary = GenerationSummary'
  { -- | Indicates the estimated time for when the recommendation generation will
    -- complete.
    estimatedCompletionTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates the completion time of the recommendation generation.
    generationCompletionTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start time of the recommendation generation.
    generationStartedTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the recommendation generation succeeded, is
    -- processing, or failed.
    generationStatus :: Prelude.Maybe GenerationStatus,
    -- | Indicates the ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedCompletionTime', 'generationSummary_estimatedCompletionTime' - Indicates the estimated time for when the recommendation generation will
-- complete.
--
-- 'generationCompletionTime', 'generationSummary_generationCompletionTime' - Indicates the completion time of the recommendation generation.
--
-- 'generationStartedTime', 'generationSummary_generationStartedTime' - Indicates the start time of the recommendation generation.
--
-- 'generationStatus', 'generationSummary_generationStatus' - Indicates whether the recommendation generation succeeded, is
-- processing, or failed.
--
-- 'recommendationId', 'generationSummary_recommendationId' - Indicates the ID for this specific recommendation.
newGenerationSummary ::
  GenerationSummary
newGenerationSummary =
  GenerationSummary'
    { estimatedCompletionTime =
        Prelude.Nothing,
      generationCompletionTime = Prelude.Nothing,
      generationStartedTime = Prelude.Nothing,
      generationStatus = Prelude.Nothing,
      recommendationId = Prelude.Nothing
    }

-- | Indicates the estimated time for when the recommendation generation will
-- complete.
generationSummary_estimatedCompletionTime :: Lens.Lens' GenerationSummary (Prelude.Maybe Prelude.Text)
generationSummary_estimatedCompletionTime = Lens.lens (\GenerationSummary' {estimatedCompletionTime} -> estimatedCompletionTime) (\s@GenerationSummary' {} a -> s {estimatedCompletionTime = a} :: GenerationSummary)

-- | Indicates the completion time of the recommendation generation.
generationSummary_generationCompletionTime :: Lens.Lens' GenerationSummary (Prelude.Maybe Prelude.Text)
generationSummary_generationCompletionTime = Lens.lens (\GenerationSummary' {generationCompletionTime} -> generationCompletionTime) (\s@GenerationSummary' {} a -> s {generationCompletionTime = a} :: GenerationSummary)

-- | Indicates the start time of the recommendation generation.
generationSummary_generationStartedTime :: Lens.Lens' GenerationSummary (Prelude.Maybe Prelude.Text)
generationSummary_generationStartedTime = Lens.lens (\GenerationSummary' {generationStartedTime} -> generationStartedTime) (\s@GenerationSummary' {} a -> s {generationStartedTime = a} :: GenerationSummary)

-- | Indicates whether the recommendation generation succeeded, is
-- processing, or failed.
generationSummary_generationStatus :: Lens.Lens' GenerationSummary (Prelude.Maybe GenerationStatus)
generationSummary_generationStatus = Lens.lens (\GenerationSummary' {generationStatus} -> generationStatus) (\s@GenerationSummary' {} a -> s {generationStatus = a} :: GenerationSummary)

-- | Indicates the ID for this specific recommendation.
generationSummary_recommendationId :: Lens.Lens' GenerationSummary (Prelude.Maybe Prelude.Text)
generationSummary_recommendationId = Lens.lens (\GenerationSummary' {recommendationId} -> recommendationId) (\s@GenerationSummary' {} a -> s {recommendationId = a} :: GenerationSummary)

instance Data.FromJSON GenerationSummary where
  parseJSON =
    Data.withObject
      "GenerationSummary"
      ( \x ->
          GenerationSummary'
            Prelude.<$> (x Data..:? "EstimatedCompletionTime")
            Prelude.<*> (x Data..:? "GenerationCompletionTime")
            Prelude.<*> (x Data..:? "GenerationStartedTime")
            Prelude.<*> (x Data..:? "GenerationStatus")
            Prelude.<*> (x Data..:? "RecommendationId")
      )

instance Prelude.Hashable GenerationSummary where
  hashWithSalt _salt GenerationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedCompletionTime
      `Prelude.hashWithSalt` generationCompletionTime
      `Prelude.hashWithSalt` generationStartedTime
      `Prelude.hashWithSalt` generationStatus
      `Prelude.hashWithSalt` recommendationId

instance Prelude.NFData GenerationSummary where
  rnf GenerationSummary' {..} =
    Prelude.rnf estimatedCompletionTime
      `Prelude.seq` Prelude.rnf generationCompletionTime
      `Prelude.seq` Prelude.rnf generationStartedTime
      `Prelude.seq` Prelude.rnf generationStatus
      `Prelude.seq` Prelude.rnf recommendationId
