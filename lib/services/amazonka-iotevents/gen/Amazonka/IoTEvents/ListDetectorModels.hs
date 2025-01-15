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
-- Module      : Amazonka.IoTEvents.ListDetectorModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the detector models you have created. Only the metadata associated
-- with each detector model is returned.
module Amazonka.IoTEvents.ListDetectorModels
  ( -- * Creating a Request
    ListDetectorModels (..),
    newListDetectorModels,

    -- * Request Lenses
    listDetectorModels_maxResults,
    listDetectorModels_nextToken,

    -- * Destructuring the Response
    ListDetectorModelsResponse (..),
    newListDetectorModelsResponse,

    -- * Response Lenses
    listDetectorModelsResponse_detectorModelSummaries,
    listDetectorModelsResponse_nextToken,
    listDetectorModelsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectorModels' smart constructor.
data ListDetectorModels = ListDetectorModels'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectorModels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDetectorModels_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDetectorModels_nextToken' - The token that you can use to return the next set of results.
newListDetectorModels ::
  ListDetectorModels
newListDetectorModels =
  ListDetectorModels'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to be returned per request.
listDetectorModels_maxResults :: Lens.Lens' ListDetectorModels (Prelude.Maybe Prelude.Natural)
listDetectorModels_maxResults = Lens.lens (\ListDetectorModels' {maxResults} -> maxResults) (\s@ListDetectorModels' {} a -> s {maxResults = a} :: ListDetectorModels)

-- | The token that you can use to return the next set of results.
listDetectorModels_nextToken :: Lens.Lens' ListDetectorModels (Prelude.Maybe Prelude.Text)
listDetectorModels_nextToken = Lens.lens (\ListDetectorModels' {nextToken} -> nextToken) (\s@ListDetectorModels' {} a -> s {nextToken = a} :: ListDetectorModels)

instance Core.AWSRequest ListDetectorModels where
  type
    AWSResponse ListDetectorModels =
      ListDetectorModelsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorModelsResponse'
            Prelude.<$> ( x
                            Data..?> "detectorModelSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDetectorModels where
  hashWithSalt _salt ListDetectorModels' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDetectorModels where
  rnf ListDetectorModels' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListDetectorModels where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDetectorModels where
  toPath = Prelude.const "/detector-models"

instance Data.ToQuery ListDetectorModels where
  toQuery ListDetectorModels' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDetectorModelsResponse' smart constructor.
data ListDetectorModelsResponse = ListDetectorModelsResponse'
  { -- | Summary information about the detector models.
    detectorModelSummaries :: Prelude.Maybe [DetectorModelSummary],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectorModelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorModelSummaries', 'listDetectorModelsResponse_detectorModelSummaries' - Summary information about the detector models.
--
-- 'nextToken', 'listDetectorModelsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'listDetectorModelsResponse_httpStatus' - The response's http status code.
newListDetectorModelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectorModelsResponse
newListDetectorModelsResponse pHttpStatus_ =
  ListDetectorModelsResponse'
    { detectorModelSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the detector models.
listDetectorModelsResponse_detectorModelSummaries :: Lens.Lens' ListDetectorModelsResponse (Prelude.Maybe [DetectorModelSummary])
listDetectorModelsResponse_detectorModelSummaries = Lens.lens (\ListDetectorModelsResponse' {detectorModelSummaries} -> detectorModelSummaries) (\s@ListDetectorModelsResponse' {} a -> s {detectorModelSummaries = a} :: ListDetectorModelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listDetectorModelsResponse_nextToken :: Lens.Lens' ListDetectorModelsResponse (Prelude.Maybe Prelude.Text)
listDetectorModelsResponse_nextToken = Lens.lens (\ListDetectorModelsResponse' {nextToken} -> nextToken) (\s@ListDetectorModelsResponse' {} a -> s {nextToken = a} :: ListDetectorModelsResponse)

-- | The response's http status code.
listDetectorModelsResponse_httpStatus :: Lens.Lens' ListDetectorModelsResponse Prelude.Int
listDetectorModelsResponse_httpStatus = Lens.lens (\ListDetectorModelsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorModelsResponse' {} a -> s {httpStatus = a} :: ListDetectorModelsResponse)

instance Prelude.NFData ListDetectorModelsResponse where
  rnf ListDetectorModelsResponse' {..} =
    Prelude.rnf detectorModelSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
