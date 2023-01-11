{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CostExplorer.StartSavingsPlansPurchaseRecommendationGeneration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a Savings Plans recommendation generation. This enables you to
-- calculate a fresh set of Savings Plans recommendations that takes your
-- latest usage data and current Savings Plans inventory into account. You
-- can refresh Savings Plans recommendations up to three times daily for a
-- consolidated billing family.
--
-- @StartSavingsPlansPurchaseRecommendationGeneration@ has no request
-- syntax because no input parameters are needed to support this operation.
module Amazonka.CostExplorer.StartSavingsPlansPurchaseRecommendationGeneration
  ( -- * Creating a Request
    StartSavingsPlansPurchaseRecommendationGeneration (..),
    newStartSavingsPlansPurchaseRecommendationGeneration,

    -- * Destructuring the Response
    StartSavingsPlansPurchaseRecommendationGenerationResponse (..),
    newStartSavingsPlansPurchaseRecommendationGenerationResponse,

    -- * Response Lenses
    startSavingsPlansPurchaseRecommendationGenerationResponse_estimatedCompletionTime,
    startSavingsPlansPurchaseRecommendationGenerationResponse_generationStartedTime,
    startSavingsPlansPurchaseRecommendationGenerationResponse_recommendationId,
    startSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSavingsPlansPurchaseRecommendationGeneration' smart constructor.
data StartSavingsPlansPurchaseRecommendationGeneration = StartSavingsPlansPurchaseRecommendationGeneration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSavingsPlansPurchaseRecommendationGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartSavingsPlansPurchaseRecommendationGeneration ::
  StartSavingsPlansPurchaseRecommendationGeneration
newStartSavingsPlansPurchaseRecommendationGeneration =
  StartSavingsPlansPurchaseRecommendationGeneration'

instance
  Core.AWSRequest
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  type
    AWSResponse
      StartSavingsPlansPurchaseRecommendationGeneration =
      StartSavingsPlansPurchaseRecommendationGenerationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartSavingsPlansPurchaseRecommendationGenerationResponse'
            Prelude.<$> (x Data..?> "EstimatedCompletionTime")
              Prelude.<*> (x Data..?> "GenerationStartedTime")
              Prelude.<*> (x Data..?> "RecommendationId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance
  Prelude.NFData
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  rnf _ = ()

instance
  Data.ToHeaders
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSInsightsIndexService.StartSavingsPlansPurchaseRecommendationGeneration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance
  Data.ToPath
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartSavingsPlansPurchaseRecommendationGeneration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSavingsPlansPurchaseRecommendationGenerationResponse' smart constructor.
data StartSavingsPlansPurchaseRecommendationGenerationResponse = StartSavingsPlansPurchaseRecommendationGenerationResponse'
  { -- | The estimated time for when the recommendation generation will complete.
    estimatedCompletionTime :: Prelude.Maybe Prelude.Text,
    -- | The start time of the recommendation generation.
    generationStartedTime :: Prelude.Maybe Prelude.Text,
    -- | The ID for this specific recommendation.
    recommendationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSavingsPlansPurchaseRecommendationGenerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedCompletionTime', 'startSavingsPlansPurchaseRecommendationGenerationResponse_estimatedCompletionTime' - The estimated time for when the recommendation generation will complete.
--
-- 'generationStartedTime', 'startSavingsPlansPurchaseRecommendationGenerationResponse_generationStartedTime' - The start time of the recommendation generation.
--
-- 'recommendationId', 'startSavingsPlansPurchaseRecommendationGenerationResponse_recommendationId' - The ID for this specific recommendation.
--
-- 'httpStatus', 'startSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus' - The response's http status code.
newStartSavingsPlansPurchaseRecommendationGenerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartSavingsPlansPurchaseRecommendationGenerationResponse
newStartSavingsPlansPurchaseRecommendationGenerationResponse
  pHttpStatus_ =
    StartSavingsPlansPurchaseRecommendationGenerationResponse'
      { estimatedCompletionTime =
          Prelude.Nothing,
        generationStartedTime =
          Prelude.Nothing,
        recommendationId =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The estimated time for when the recommendation generation will complete.
startSavingsPlansPurchaseRecommendationGenerationResponse_estimatedCompletionTime :: Lens.Lens' StartSavingsPlansPurchaseRecommendationGenerationResponse (Prelude.Maybe Prelude.Text)
startSavingsPlansPurchaseRecommendationGenerationResponse_estimatedCompletionTime = Lens.lens (\StartSavingsPlansPurchaseRecommendationGenerationResponse' {estimatedCompletionTime} -> estimatedCompletionTime) (\s@StartSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {estimatedCompletionTime = a} :: StartSavingsPlansPurchaseRecommendationGenerationResponse)

-- | The start time of the recommendation generation.
startSavingsPlansPurchaseRecommendationGenerationResponse_generationStartedTime :: Lens.Lens' StartSavingsPlansPurchaseRecommendationGenerationResponse (Prelude.Maybe Prelude.Text)
startSavingsPlansPurchaseRecommendationGenerationResponse_generationStartedTime = Lens.lens (\StartSavingsPlansPurchaseRecommendationGenerationResponse' {generationStartedTime} -> generationStartedTime) (\s@StartSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {generationStartedTime = a} :: StartSavingsPlansPurchaseRecommendationGenerationResponse)

-- | The ID for this specific recommendation.
startSavingsPlansPurchaseRecommendationGenerationResponse_recommendationId :: Lens.Lens' StartSavingsPlansPurchaseRecommendationGenerationResponse (Prelude.Maybe Prelude.Text)
startSavingsPlansPurchaseRecommendationGenerationResponse_recommendationId = Lens.lens (\StartSavingsPlansPurchaseRecommendationGenerationResponse' {recommendationId} -> recommendationId) (\s@StartSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {recommendationId = a} :: StartSavingsPlansPurchaseRecommendationGenerationResponse)

-- | The response's http status code.
startSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus :: Lens.Lens' StartSavingsPlansPurchaseRecommendationGenerationResponse Prelude.Int
startSavingsPlansPurchaseRecommendationGenerationResponse_httpStatus = Lens.lens (\StartSavingsPlansPurchaseRecommendationGenerationResponse' {httpStatus} -> httpStatus) (\s@StartSavingsPlansPurchaseRecommendationGenerationResponse' {} a -> s {httpStatus = a} :: StartSavingsPlansPurchaseRecommendationGenerationResponse)

instance
  Prelude.NFData
    StartSavingsPlansPurchaseRecommendationGenerationResponse
  where
  rnf
    StartSavingsPlansPurchaseRecommendationGenerationResponse' {..} =
      Prelude.rnf estimatedCompletionTime
        `Prelude.seq` Prelude.rnf generationStartedTime
        `Prelude.seq` Prelude.rnf recommendationId
        `Prelude.seq` Prelude.rnf httpStatus
