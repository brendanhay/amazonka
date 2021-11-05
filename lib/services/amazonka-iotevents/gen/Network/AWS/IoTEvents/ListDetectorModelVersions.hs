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
-- Module      : Network.AWS.IoTEvents.ListDetectorModelVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the versions of a detector model. Only the metadata associated
-- with each detector model version is returned.
module Network.AWS.IoTEvents.ListDetectorModelVersions
  ( -- * Creating a Request
    ListDetectorModelVersions (..),
    newListDetectorModelVersions,

    -- * Request Lenses
    listDetectorModelVersions_nextToken,
    listDetectorModelVersions_maxResults,
    listDetectorModelVersions_detectorModelName,

    -- * Destructuring the Response
    ListDetectorModelVersionsResponse (..),
    newListDetectorModelVersionsResponse,

    -- * Response Lenses
    listDetectorModelVersionsResponse_nextToken,
    listDetectorModelVersionsResponse_detectorModelVersionSummaries,
    listDetectorModelVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDetectorModelVersions' smart constructor.
data ListDetectorModelVersions = ListDetectorModelVersions'
  { -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listDetectorModelVersions_nextToken' - The token that you can use to return the next set of results.
--
-- 'maxResults', 'listDetectorModelVersions_maxResults' - The maximum number of results to be returned per request.
--
-- 'detectorModelName', 'listDetectorModelVersions_detectorModelName' - The name of the detector model whose versions are returned.
newListDetectorModelVersions ::
  -- | 'detectorModelName'
  Prelude.Text ->
  ListDetectorModelVersions
newListDetectorModelVersions pDetectorModelName_ =
  ListDetectorModelVersions'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | The token that you can use to return the next set of results.
listDetectorModelVersions_nextToken :: Lens.Lens' ListDetectorModelVersions (Prelude.Maybe Prelude.Text)
listDetectorModelVersions_nextToken = Lens.lens (\ListDetectorModelVersions' {nextToken} -> nextToken) (\s@ListDetectorModelVersions' {} a -> s {nextToken = a} :: ListDetectorModelVersions)

-- | The maximum number of results to be returned per request.
listDetectorModelVersions_maxResults :: Lens.Lens' ListDetectorModelVersions (Prelude.Maybe Prelude.Natural)
listDetectorModelVersions_maxResults = Lens.lens (\ListDetectorModelVersions' {maxResults} -> maxResults) (\s@ListDetectorModelVersions' {} a -> s {maxResults = a} :: ListDetectorModelVersions)

-- | The name of the detector model whose versions are returned.
listDetectorModelVersions_detectorModelName :: Lens.Lens' ListDetectorModelVersions Prelude.Text
listDetectorModelVersions_detectorModelName = Lens.lens (\ListDetectorModelVersions' {detectorModelName} -> detectorModelName) (\s@ListDetectorModelVersions' {} a -> s {detectorModelName = a} :: ListDetectorModelVersions)

instance Core.AWSRequest ListDetectorModelVersions where
  type
    AWSResponse ListDetectorModelVersions =
      ListDetectorModelVersionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorModelVersionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "detectorModelVersionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDetectorModelVersions

instance Prelude.NFData ListDetectorModelVersions

instance Core.ToHeaders ListDetectorModelVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDetectorModelVersions where
  toPath ListDetectorModelVersions' {..} =
    Prelude.mconcat
      [ "/detector-models/",
        Core.toBS detectorModelName,
        "/versions"
      ]

instance Core.ToQuery ListDetectorModelVersions where
  toQuery ListDetectorModelVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDetectorModelVersionsResponse' smart constructor.
data ListDetectorModelVersionsResponse = ListDetectorModelVersionsResponse'
  { -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information about the detector model versions.
    detectorModelVersionSummaries :: Prelude.Maybe [DetectorModelVersionSummary],
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
-- 'nextToken', 'listDetectorModelVersionsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'detectorModelVersionSummaries', 'listDetectorModelVersionsResponse_detectorModelVersionSummaries' - Summary information about the detector model versions.
--
-- 'httpStatus', 'listDetectorModelVersionsResponse_httpStatus' - The response's http status code.
newListDetectorModelVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectorModelVersionsResponse
newListDetectorModelVersionsResponse pHttpStatus_ =
  ListDetectorModelVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      detectorModelVersionSummaries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listDetectorModelVersionsResponse_nextToken :: Lens.Lens' ListDetectorModelVersionsResponse (Prelude.Maybe Prelude.Text)
listDetectorModelVersionsResponse_nextToken = Lens.lens (\ListDetectorModelVersionsResponse' {nextToken} -> nextToken) (\s@ListDetectorModelVersionsResponse' {} a -> s {nextToken = a} :: ListDetectorModelVersionsResponse)

-- | Summary information about the detector model versions.
listDetectorModelVersionsResponse_detectorModelVersionSummaries :: Lens.Lens' ListDetectorModelVersionsResponse (Prelude.Maybe [DetectorModelVersionSummary])
listDetectorModelVersionsResponse_detectorModelVersionSummaries = Lens.lens (\ListDetectorModelVersionsResponse' {detectorModelVersionSummaries} -> detectorModelVersionSummaries) (\s@ListDetectorModelVersionsResponse' {} a -> s {detectorModelVersionSummaries = a} :: ListDetectorModelVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDetectorModelVersionsResponse_httpStatus :: Lens.Lens' ListDetectorModelVersionsResponse Prelude.Int
listDetectorModelVersionsResponse_httpStatus = Lens.lens (\ListDetectorModelVersionsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorModelVersionsResponse' {} a -> s {httpStatus = a} :: ListDetectorModelVersionsResponse)

instance
  Prelude.NFData
    ListDetectorModelVersionsResponse
