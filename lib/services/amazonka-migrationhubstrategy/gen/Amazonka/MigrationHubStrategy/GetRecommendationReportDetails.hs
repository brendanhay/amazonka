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
-- Module      : Amazonka.MigrationHubStrategy.GetRecommendationReportDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about the specified recommendation
-- report.
module Amazonka.MigrationHubStrategy.GetRecommendationReportDetails
  ( -- * Creating a Request
    GetRecommendationReportDetails (..),
    newGetRecommendationReportDetails,

    -- * Request Lenses
    getRecommendationReportDetails_id,

    -- * Destructuring the Response
    GetRecommendationReportDetailsResponse (..),
    newGetRecommendationReportDetailsResponse,

    -- * Response Lenses
    getRecommendationReportDetailsResponse_id,
    getRecommendationReportDetailsResponse_recommendationReportDetails,
    getRecommendationReportDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRecommendationReportDetails' smart constructor.
data GetRecommendationReportDetails = GetRecommendationReportDetails'
  { -- | The recommendation report generation task @id@ returned by
    -- StartRecommendationReportGeneration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationReportDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getRecommendationReportDetails_id' - The recommendation report generation task @id@ returned by
-- StartRecommendationReportGeneration.
newGetRecommendationReportDetails ::
  -- | 'id'
  Prelude.Text ->
  GetRecommendationReportDetails
newGetRecommendationReportDetails pId_ =
  GetRecommendationReportDetails' {id = pId_}

-- | The recommendation report generation task @id@ returned by
-- StartRecommendationReportGeneration.
getRecommendationReportDetails_id :: Lens.Lens' GetRecommendationReportDetails Prelude.Text
getRecommendationReportDetails_id = Lens.lens (\GetRecommendationReportDetails' {id} -> id) (\s@GetRecommendationReportDetails' {} a -> s {id = a} :: GetRecommendationReportDetails)

instance
  Core.AWSRequest
    GetRecommendationReportDetails
  where
  type
    AWSResponse GetRecommendationReportDetails =
      GetRecommendationReportDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommendationReportDetailsResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "recommendationReportDetails")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRecommendationReportDetails
  where
  hashWithSalt
    _salt
    GetRecommendationReportDetails' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetRecommendationReportDetails
  where
  rnf GetRecommendationReportDetails' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetRecommendationReportDetails
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRecommendationReportDetails where
  toPath GetRecommendationReportDetails' {..} =
    Prelude.mconcat
      ["/get-recommendation-report-details/", Data.toBS id]

instance Data.ToQuery GetRecommendationReportDetails where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRecommendationReportDetailsResponse' smart constructor.
data GetRecommendationReportDetailsResponse = GetRecommendationReportDetailsResponse'
  { -- | The ID of the recommendation report generation task. See the response of
    -- StartRecommendationReportGeneration.
    id :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the recommendation report.
    recommendationReportDetails :: Prelude.Maybe RecommendationReportDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRecommendationReportDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getRecommendationReportDetailsResponse_id' - The ID of the recommendation report generation task. See the response of
-- StartRecommendationReportGeneration.
--
-- 'recommendationReportDetails', 'getRecommendationReportDetailsResponse_recommendationReportDetails' - Detailed information about the recommendation report.
--
-- 'httpStatus', 'getRecommendationReportDetailsResponse_httpStatus' - The response's http status code.
newGetRecommendationReportDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRecommendationReportDetailsResponse
newGetRecommendationReportDetailsResponse
  pHttpStatus_ =
    GetRecommendationReportDetailsResponse'
      { id =
          Prelude.Nothing,
        recommendationReportDetails =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the recommendation report generation task. See the response of
-- StartRecommendationReportGeneration.
getRecommendationReportDetailsResponse_id :: Lens.Lens' GetRecommendationReportDetailsResponse (Prelude.Maybe Prelude.Text)
getRecommendationReportDetailsResponse_id = Lens.lens (\GetRecommendationReportDetailsResponse' {id} -> id) (\s@GetRecommendationReportDetailsResponse' {} a -> s {id = a} :: GetRecommendationReportDetailsResponse)

-- | Detailed information about the recommendation report.
getRecommendationReportDetailsResponse_recommendationReportDetails :: Lens.Lens' GetRecommendationReportDetailsResponse (Prelude.Maybe RecommendationReportDetails)
getRecommendationReportDetailsResponse_recommendationReportDetails = Lens.lens (\GetRecommendationReportDetailsResponse' {recommendationReportDetails} -> recommendationReportDetails) (\s@GetRecommendationReportDetailsResponse' {} a -> s {recommendationReportDetails = a} :: GetRecommendationReportDetailsResponse)

-- | The response's http status code.
getRecommendationReportDetailsResponse_httpStatus :: Lens.Lens' GetRecommendationReportDetailsResponse Prelude.Int
getRecommendationReportDetailsResponse_httpStatus = Lens.lens (\GetRecommendationReportDetailsResponse' {httpStatus} -> httpStatus) (\s@GetRecommendationReportDetailsResponse' {} a -> s {httpStatus = a} :: GetRecommendationReportDetailsResponse)

instance
  Prelude.NFData
    GetRecommendationReportDetailsResponse
  where
  rnf GetRecommendationReportDetailsResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf recommendationReportDetails
      `Prelude.seq` Prelude.rnf httpStatus
