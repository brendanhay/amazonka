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
-- Module      : Amazonka.IoTEvents.ListDetectorModelVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of a detector model. Only the metadata associated
-- with each detector model version is returned.
module Amazonka.IoTEvents.ListDetectorModelVersions
  ( -- * Creating a Request
    ListDetectorModelVersions (..),
    newListDetectorModelVersions,

    -- * Request Lenses
    listDetectorModelVersions_maxResults,
    listDetectorModelVersions_nextToken,
    listDetectorModelVersions_detectorModelName,

    -- * Destructuring the Response
    ListDetectorModelVersionsResponse (..),
    newListDetectorModelVersionsResponse,

    -- * Response Lenses
    listDetectorModelVersionsResponse_detectorModelVersionSummaries,
    listDetectorModelVersionsResponse_nextToken,
    listDetectorModelVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectorModelVersions' smart constructor.
data ListDetectorModelVersions = ListDetectorModelVersions'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the detector model whose versions are returned.
    detectorModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectorModelVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listDetectorModelVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDetectorModelVersions_nextToken' - The token that you can use to return the next set of results.
--
-- 'detectorModelName', 'listDetectorModelVersions_detectorModelName' - The name of the detector model whose versions are returned.
newListDetectorModelVersions ::
  -- | 'detectorModelName'
  Prelude.Text ->
  ListDetectorModelVersions
newListDetectorModelVersions pDetectorModelName_ =
  ListDetectorModelVersions'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | The maximum number of results to be returned per request.
listDetectorModelVersions_maxResults :: Lens.Lens' ListDetectorModelVersions (Prelude.Maybe Prelude.Natural)
listDetectorModelVersions_maxResults = Lens.lens (\ListDetectorModelVersions' {maxResults} -> maxResults) (\s@ListDetectorModelVersions' {} a -> s {maxResults = a} :: ListDetectorModelVersions)

-- | The token that you can use to return the next set of results.
listDetectorModelVersions_nextToken :: Lens.Lens' ListDetectorModelVersions (Prelude.Maybe Prelude.Text)
listDetectorModelVersions_nextToken = Lens.lens (\ListDetectorModelVersions' {nextToken} -> nextToken) (\s@ListDetectorModelVersions' {} a -> s {nextToken = a} :: ListDetectorModelVersions)

-- | The name of the detector model whose versions are returned.
listDetectorModelVersions_detectorModelName :: Lens.Lens' ListDetectorModelVersions Prelude.Text
listDetectorModelVersions_detectorModelName = Lens.lens (\ListDetectorModelVersions' {detectorModelName} -> detectorModelName) (\s@ListDetectorModelVersions' {} a -> s {detectorModelName = a} :: ListDetectorModelVersions)

instance Core.AWSRequest ListDetectorModelVersions where
  type
    AWSResponse ListDetectorModelVersions =
      ListDetectorModelVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorModelVersionsResponse'
            Prelude.<$> ( x Data..?> "detectorModelVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDetectorModelVersions where
  hashWithSalt _salt ListDetectorModelVersions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData ListDetectorModelVersions where
  rnf ListDetectorModelVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf detectorModelName

instance Data.ToHeaders ListDetectorModelVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDetectorModelVersions where
  toPath ListDetectorModelVersions' {..} =
    Prelude.mconcat
      [ "/detector-models/",
        Data.toBS detectorModelName,
        "/versions"
      ]

instance Data.ToQuery ListDetectorModelVersions where
  toQuery ListDetectorModelVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDetectorModelVersionsResponse' smart constructor.
data ListDetectorModelVersionsResponse = ListDetectorModelVersionsResponse'
  { -- | Summary information about the detector model versions.
    detectorModelVersionSummaries :: Prelude.Maybe [DetectorModelVersionSummary],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectorModelVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorModelVersionSummaries', 'listDetectorModelVersionsResponse_detectorModelVersionSummaries' - Summary information about the detector model versions.
--
-- 'nextToken', 'listDetectorModelVersionsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'listDetectorModelVersionsResponse_httpStatus' - The response's http status code.
newListDetectorModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectorModelVersionsResponse
newListDetectorModelVersionsResponse pHttpStatus_ =
  ListDetectorModelVersionsResponse'
    { detectorModelVersionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about the detector model versions.
listDetectorModelVersionsResponse_detectorModelVersionSummaries :: Lens.Lens' ListDetectorModelVersionsResponse (Prelude.Maybe [DetectorModelVersionSummary])
listDetectorModelVersionsResponse_detectorModelVersionSummaries = Lens.lens (\ListDetectorModelVersionsResponse' {detectorModelVersionSummaries} -> detectorModelVersionSummaries) (\s@ListDetectorModelVersionsResponse' {} a -> s {detectorModelVersionSummaries = a} :: ListDetectorModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listDetectorModelVersionsResponse_nextToken :: Lens.Lens' ListDetectorModelVersionsResponse (Prelude.Maybe Prelude.Text)
listDetectorModelVersionsResponse_nextToken = Lens.lens (\ListDetectorModelVersionsResponse' {nextToken} -> nextToken) (\s@ListDetectorModelVersionsResponse' {} a -> s {nextToken = a} :: ListDetectorModelVersionsResponse)

-- | The response's http status code.
listDetectorModelVersionsResponse_httpStatus :: Lens.Lens' ListDetectorModelVersionsResponse Prelude.Int
listDetectorModelVersionsResponse_httpStatus = Lens.lens (\ListDetectorModelVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorModelVersionsResponse' {} a -> s {httpStatus = a} :: ListDetectorModelVersionsResponse)

instance
  Prelude.NFData
    ListDetectorModelVersionsResponse
  where
  rnf ListDetectorModelVersionsResponse' {..} =
    Prelude.rnf detectorModelVersionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
