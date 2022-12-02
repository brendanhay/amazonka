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
-- Module      : Amazonka.LookoutEquipment.ListModels
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a list of all models in the account, including model name and
-- ARN, dataset, and status.
module Amazonka.LookoutEquipment.ListModels
  ( -- * Creating a Request
    ListModels (..),
    newListModels,

    -- * Request Lenses
    listModels_nextToken,
    listModels_status,
    listModels_maxResults,
    listModels_modelNameBeginsWith,
    listModels_datasetNameBeginsWith,

    -- * Destructuring the Response
    ListModelsResponse (..),
    newListModelsResponse,

    -- * Response Lenses
    listModelsResponse_nextToken,
    listModelsResponse_modelSummaries,
    listModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListModels' smart constructor.
data ListModels = ListModels'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- ML models.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the ML model.
    status :: Prelude.Maybe ModelStatus,
    -- | Specifies the maximum number of ML models to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The beginning of the name of the ML models being listed.
    modelNameBeginsWith :: Prelude.Maybe Prelude.Text,
    -- | The beginning of the name of the dataset of the ML models to be listed.
    datasetNameBeginsWith :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModels_nextToken' - An opaque pagination token indicating where to continue the listing of
-- ML models.
--
-- 'status', 'listModels_status' - The status of the ML model.
--
-- 'maxResults', 'listModels_maxResults' - Specifies the maximum number of ML models to list.
--
-- 'modelNameBeginsWith', 'listModels_modelNameBeginsWith' - The beginning of the name of the ML models being listed.
--
-- 'datasetNameBeginsWith', 'listModels_datasetNameBeginsWith' - The beginning of the name of the dataset of the ML models to be listed.
newListModels ::
  ListModels
newListModels =
  ListModels'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelNameBeginsWith = Prelude.Nothing,
      datasetNameBeginsWith = Prelude.Nothing
    }

-- | An opaque pagination token indicating where to continue the listing of
-- ML models.
listModels_nextToken :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_nextToken = Lens.lens (\ListModels' {nextToken} -> nextToken) (\s@ListModels' {} a -> s {nextToken = a} :: ListModels)

-- | The status of the ML model.
listModels_status :: Lens.Lens' ListModels (Prelude.Maybe ModelStatus)
listModels_status = Lens.lens (\ListModels' {status} -> status) (\s@ListModels' {} a -> s {status = a} :: ListModels)

-- | Specifies the maximum number of ML models to list.
listModels_maxResults :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Natural)
listModels_maxResults = Lens.lens (\ListModels' {maxResults} -> maxResults) (\s@ListModels' {} a -> s {maxResults = a} :: ListModels)

-- | The beginning of the name of the ML models being listed.
listModels_modelNameBeginsWith :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_modelNameBeginsWith = Lens.lens (\ListModels' {modelNameBeginsWith} -> modelNameBeginsWith) (\s@ListModels' {} a -> s {modelNameBeginsWith = a} :: ListModels)

-- | The beginning of the name of the dataset of the ML models to be listed.
listModels_datasetNameBeginsWith :: Lens.Lens' ListModels (Prelude.Maybe Prelude.Text)
listModels_datasetNameBeginsWith = Lens.lens (\ListModels' {datasetNameBeginsWith} -> datasetNameBeginsWith) (\s@ListModels' {} a -> s {datasetNameBeginsWith = a} :: ListModels)

instance Core.AWSRequest ListModels where
  type AWSResponse ListModels = ListModelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ModelSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListModels where
  hashWithSalt _salt ListModels' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelNameBeginsWith
      `Prelude.hashWithSalt` datasetNameBeginsWith

instance Prelude.NFData ListModels where
  rnf ListModels' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelNameBeginsWith
      `Prelude.seq` Prelude.rnf datasetNameBeginsWith

instance Data.ToHeaders ListModels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListModels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModels where
  toJSON ListModels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Status" Data..=) Prelude.<$> status,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelNameBeginsWith" Data..=)
              Prelude.<$> modelNameBeginsWith,
            ("DatasetNameBeginsWith" Data..=)
              Prelude.<$> datasetNameBeginsWith
          ]
      )

instance Data.ToPath ListModels where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModels where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | An opaque pagination token indicating where to continue the listing of
    -- ML models.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Provides information on the specified model, including created time,
    -- model and dataset ARNs, and status.
    modelSummaries :: Prelude.Maybe [ModelSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- ML models.
--
-- 'modelSummaries', 'listModelsResponse_modelSummaries' - Provides information on the specified model, including created time,
-- model and dataset ARNs, and status.
--
-- 'httpStatus', 'listModelsResponse_httpStatus' - The response's http status code.
newListModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelsResponse
newListModelsResponse pHttpStatus_ =
  ListModelsResponse'
    { nextToken = Prelude.Nothing,
      modelSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token indicating where to continue the listing of
-- ML models.
listModelsResponse_nextToken :: Lens.Lens' ListModelsResponse (Prelude.Maybe Prelude.Text)
listModelsResponse_nextToken = Lens.lens (\ListModelsResponse' {nextToken} -> nextToken) (\s@ListModelsResponse' {} a -> s {nextToken = a} :: ListModelsResponse)

-- | Provides information on the specified model, including created time,
-- model and dataset ARNs, and status.
listModelsResponse_modelSummaries :: Lens.Lens' ListModelsResponse (Prelude.Maybe [ModelSummary])
listModelsResponse_modelSummaries = Lens.lens (\ListModelsResponse' {modelSummaries} -> modelSummaries) (\s@ListModelsResponse' {} a -> s {modelSummaries = a} :: ListModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listModelsResponse_httpStatus :: Lens.Lens' ListModelsResponse Prelude.Int
listModelsResponse_httpStatus = Lens.lens (\ListModelsResponse' {httpStatus} -> httpStatus) (\s@ListModelsResponse' {} a -> s {httpStatus = a} :: ListModelsResponse)

instance Prelude.NFData ListModelsResponse where
  rnf ListModelsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf modelSummaries
      `Prelude.seq` Prelude.rnf httpStatus
