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
-- Module      : Amazonka.IoTEventsData.ListDetectors
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detectors (the instances of a detector model).
module Amazonka.IoTEventsData.ListDetectors
  ( -- * Creating a Request
    ListDetectors (..),
    newListDetectors,

    -- * Request Lenses
    listDetectors_nextToken,
    listDetectors_stateName,
    listDetectors_maxResults,
    listDetectors_detectorModelName,

    -- * Destructuring the Response
    ListDetectorsResponse (..),
    newListDetectorsResponse,

    -- * Response Lenses
    listDetectorsResponse_nextToken,
    listDetectorsResponse_detectorSummaries,
    listDetectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectors' smart constructor.
data ListDetectors = ListDetectors'
  { -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that limits results to those detectors (instances) in the given
    -- state.
    stateName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the detector model whose detectors (instances) are listed.
    detectorModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectors_nextToken' - The token that you can use to return the next set of results.
--
-- 'stateName', 'listDetectors_stateName' - A filter that limits results to those detectors (instances) in the given
-- state.
--
-- 'maxResults', 'listDetectors_maxResults' - The maximum number of results to be returned per request.
--
-- 'detectorModelName', 'listDetectors_detectorModelName' - The name of the detector model whose detectors (instances) are listed.
newListDetectors ::
  -- | 'detectorModelName'
  Prelude.Text ->
  ListDetectors
newListDetectors pDetectorModelName_ =
  ListDetectors'
    { nextToken = Prelude.Nothing,
      stateName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | The token that you can use to return the next set of results.
listDetectors_nextToken :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Text)
listDetectors_nextToken = Lens.lens (\ListDetectors' {nextToken} -> nextToken) (\s@ListDetectors' {} a -> s {nextToken = a} :: ListDetectors)

-- | A filter that limits results to those detectors (instances) in the given
-- state.
listDetectors_stateName :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Text)
listDetectors_stateName = Lens.lens (\ListDetectors' {stateName} -> stateName) (\s@ListDetectors' {} a -> s {stateName = a} :: ListDetectors)

-- | The maximum number of results to be returned per request.
listDetectors_maxResults :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Natural)
listDetectors_maxResults = Lens.lens (\ListDetectors' {maxResults} -> maxResults) (\s@ListDetectors' {} a -> s {maxResults = a} :: ListDetectors)

-- | The name of the detector model whose detectors (instances) are listed.
listDetectors_detectorModelName :: Lens.Lens' ListDetectors Prelude.Text
listDetectors_detectorModelName = Lens.lens (\ListDetectors' {detectorModelName} -> detectorModelName) (\s@ListDetectors' {} a -> s {detectorModelName = a} :: ListDetectors)

instance Core.AWSRequest ListDetectors where
  type
    AWSResponse ListDetectors =
      ListDetectorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDetectorsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "detectorSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDetectors where
  hashWithSalt _salt ListDetectors' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData ListDetectors where
  rnf ListDetectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf detectorModelName

instance Core.ToHeaders ListDetectors where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDetectors where
  toPath ListDetectors' {..} =
    Prelude.mconcat
      ["/detectors/", Core.toBS detectorModelName]

instance Core.ToQuery ListDetectors where
  toQuery ListDetectors' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "stateName" Core.=: stateName,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'
  { -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of summary information about the detectors (instances).
    detectorSummaries :: Prelude.Maybe [DetectorSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDetectorsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'detectorSummaries', 'listDetectorsResponse_detectorSummaries' - A list of summary information about the detectors (instances).
--
-- 'httpStatus', 'listDetectorsResponse_httpStatus' - The response's http status code.
newListDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectorsResponse
newListDetectorsResponse pHttpStatus_ =
  ListDetectorsResponse'
    { nextToken = Prelude.Nothing,
      detectorSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listDetectorsResponse_nextToken :: Lens.Lens' ListDetectorsResponse (Prelude.Maybe Prelude.Text)
listDetectorsResponse_nextToken = Lens.lens (\ListDetectorsResponse' {nextToken} -> nextToken) (\s@ListDetectorsResponse' {} a -> s {nextToken = a} :: ListDetectorsResponse)

-- | A list of summary information about the detectors (instances).
listDetectorsResponse_detectorSummaries :: Lens.Lens' ListDetectorsResponse (Prelude.Maybe [DetectorSummary])
listDetectorsResponse_detectorSummaries = Lens.lens (\ListDetectorsResponse' {detectorSummaries} -> detectorSummaries) (\s@ListDetectorsResponse' {} a -> s {detectorSummaries = a} :: ListDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDetectorsResponse_httpStatus :: Lens.Lens' ListDetectorsResponse Prelude.Int
listDetectorsResponse_httpStatus = Lens.lens (\ListDetectorsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorsResponse' {} a -> s {httpStatus = a} :: ListDetectorsResponse)

instance Prelude.NFData ListDetectorsResponse where
  rnf ListDetectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf detectorSummaries
      `Prelude.seq` Prelude.rnf httpStatus
