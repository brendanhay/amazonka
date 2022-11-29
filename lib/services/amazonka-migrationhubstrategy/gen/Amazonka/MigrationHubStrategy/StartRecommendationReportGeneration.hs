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
-- Module      : Amazonka.MigrationHubStrategy.StartRecommendationReportGeneration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts generating a recommendation report.
module Amazonka.MigrationHubStrategy.StartRecommendationReportGeneration
  ( -- * Creating a Request
    StartRecommendationReportGeneration (..),
    newStartRecommendationReportGeneration,

    -- * Request Lenses
    startRecommendationReportGeneration_groupIdFilter,
    startRecommendationReportGeneration_outputFormat,

    -- * Destructuring the Response
    StartRecommendationReportGenerationResponse (..),
    newStartRecommendationReportGenerationResponse,

    -- * Response Lenses
    startRecommendationReportGenerationResponse_id,
    startRecommendationReportGenerationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartRecommendationReportGeneration' smart constructor.
data StartRecommendationReportGeneration = StartRecommendationReportGeneration'
  { -- | Groups the resources in the recommendation report with a unique name.
    groupIdFilter :: Prelude.Maybe [Group],
    -- | The output format for the recommendation report file. The default format
    -- is Microsoft Excel.
    outputFormat :: Prelude.Maybe OutputFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecommendationReportGeneration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupIdFilter', 'startRecommendationReportGeneration_groupIdFilter' - Groups the resources in the recommendation report with a unique name.
--
-- 'outputFormat', 'startRecommendationReportGeneration_outputFormat' - The output format for the recommendation report file. The default format
-- is Microsoft Excel.
newStartRecommendationReportGeneration ::
  StartRecommendationReportGeneration
newStartRecommendationReportGeneration =
  StartRecommendationReportGeneration'
    { groupIdFilter =
        Prelude.Nothing,
      outputFormat = Prelude.Nothing
    }

-- | Groups the resources in the recommendation report with a unique name.
startRecommendationReportGeneration_groupIdFilter :: Lens.Lens' StartRecommendationReportGeneration (Prelude.Maybe [Group])
startRecommendationReportGeneration_groupIdFilter = Lens.lens (\StartRecommendationReportGeneration' {groupIdFilter} -> groupIdFilter) (\s@StartRecommendationReportGeneration' {} a -> s {groupIdFilter = a} :: StartRecommendationReportGeneration) Prelude.. Lens.mapping Lens.coerced

-- | The output format for the recommendation report file. The default format
-- is Microsoft Excel.
startRecommendationReportGeneration_outputFormat :: Lens.Lens' StartRecommendationReportGeneration (Prelude.Maybe OutputFormat)
startRecommendationReportGeneration_outputFormat = Lens.lens (\StartRecommendationReportGeneration' {outputFormat} -> outputFormat) (\s@StartRecommendationReportGeneration' {} a -> s {outputFormat = a} :: StartRecommendationReportGeneration)

instance
  Core.AWSRequest
    StartRecommendationReportGeneration
  where
  type
    AWSResponse StartRecommendationReportGeneration =
      StartRecommendationReportGenerationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartRecommendationReportGenerationResponse'
            Prelude.<$> (x Core..?> "id")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartRecommendationReportGeneration
  where
  hashWithSalt
    _salt
    StartRecommendationReportGeneration' {..} =
      _salt `Prelude.hashWithSalt` groupIdFilter
        `Prelude.hashWithSalt` outputFormat

instance
  Prelude.NFData
    StartRecommendationReportGeneration
  where
  rnf StartRecommendationReportGeneration' {..} =
    Prelude.rnf groupIdFilter
      `Prelude.seq` Prelude.rnf outputFormat

instance
  Core.ToHeaders
    StartRecommendationReportGeneration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    StartRecommendationReportGeneration
  where
  toJSON StartRecommendationReportGeneration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("groupIdFilter" Core..=) Prelude.<$> groupIdFilter,
            ("outputFormat" Core..=) Prelude.<$> outputFormat
          ]
      )

instance
  Core.ToPath
    StartRecommendationReportGeneration
  where
  toPath =
    Prelude.const
      "/start-recommendation-report-generation"

instance
  Core.ToQuery
    StartRecommendationReportGeneration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartRecommendationReportGenerationResponse' smart constructor.
data StartRecommendationReportGenerationResponse = StartRecommendationReportGenerationResponse'
  { -- | The ID of the recommendation report generation task.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRecommendationReportGenerationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'startRecommendationReportGenerationResponse_id' - The ID of the recommendation report generation task.
--
-- 'httpStatus', 'startRecommendationReportGenerationResponse_httpStatus' - The response's http status code.
newStartRecommendationReportGenerationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartRecommendationReportGenerationResponse
newStartRecommendationReportGenerationResponse
  pHttpStatus_ =
    StartRecommendationReportGenerationResponse'
      { id =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the recommendation report generation task.
startRecommendationReportGenerationResponse_id :: Lens.Lens' StartRecommendationReportGenerationResponse (Prelude.Maybe Prelude.Text)
startRecommendationReportGenerationResponse_id = Lens.lens (\StartRecommendationReportGenerationResponse' {id} -> id) (\s@StartRecommendationReportGenerationResponse' {} a -> s {id = a} :: StartRecommendationReportGenerationResponse)

-- | The response's http status code.
startRecommendationReportGenerationResponse_httpStatus :: Lens.Lens' StartRecommendationReportGenerationResponse Prelude.Int
startRecommendationReportGenerationResponse_httpStatus = Lens.lens (\StartRecommendationReportGenerationResponse' {httpStatus} -> httpStatus) (\s@StartRecommendationReportGenerationResponse' {} a -> s {httpStatus = a} :: StartRecommendationReportGenerationResponse)

instance
  Prelude.NFData
    StartRecommendationReportGenerationResponse
  where
  rnf StartRecommendationReportGenerationResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
