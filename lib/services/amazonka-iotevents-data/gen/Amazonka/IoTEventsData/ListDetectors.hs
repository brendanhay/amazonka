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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists detectors (the instances of a detector model).
module Amazonka.IoTEventsData.ListDetectors
  ( -- * Creating a Request
    ListDetectors (..),
    newListDetectors,

    -- * Request Lenses
    listDetectors_maxResults,
    listDetectors_nextToken,
    listDetectors_stateName,
    listDetectors_detectorModelName,

    -- * Destructuring the Response
    ListDetectorsResponse (..),
    newListDetectorsResponse,

    -- * Response Lenses
    listDetectorsResponse_detectorSummaries,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDetectors' smart constructor.
data ListDetectors = ListDetectors'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that you can use to return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that limits results to those detectors (instances) in the given
    -- state.
    stateName :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listDetectors_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listDetectors_nextToken' - The token that you can use to return the next set of results.
--
-- 'stateName', 'listDetectors_stateName' - A filter that limits results to those detectors (instances) in the given
-- state.
--
-- 'detectorModelName', 'listDetectors_detectorModelName' - The name of the detector model whose detectors (instances) are listed.
newListDetectors ::
  -- | 'detectorModelName'
  Prelude.Text ->
  ListDetectors
newListDetectors pDetectorModelName_ =
  ListDetectors'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateName = Prelude.Nothing,
      detectorModelName = pDetectorModelName_
    }

-- | The maximum number of results to be returned per request.
listDetectors_maxResults :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Natural)
listDetectors_maxResults = Lens.lens (\ListDetectors' {maxResults} -> maxResults) (\s@ListDetectors' {} a -> s {maxResults = a} :: ListDetectors)

-- | The token that you can use to return the next set of results.
listDetectors_nextToken :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Text)
listDetectors_nextToken = Lens.lens (\ListDetectors' {nextToken} -> nextToken) (\s@ListDetectors' {} a -> s {nextToken = a} :: ListDetectors)

-- | A filter that limits results to those detectors (instances) in the given
-- state.
listDetectors_stateName :: Lens.Lens' ListDetectors (Prelude.Maybe Prelude.Text)
listDetectors_stateName = Lens.lens (\ListDetectors' {stateName} -> stateName) (\s@ListDetectors' {} a -> s {stateName = a} :: ListDetectors)

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
            Prelude.<$> ( x
                            Data..?> "detectorSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDetectors where
  hashWithSalt _salt ListDetectors' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateName
      `Prelude.hashWithSalt` detectorModelName

instance Prelude.NFData ListDetectors where
  rnf ListDetectors' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf stateName `Prelude.seq`
          Prelude.rnf detectorModelName

instance Data.ToHeaders ListDetectors where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDetectors where
  toPath ListDetectors' {..} =
    Prelude.mconcat
      ["/detectors/", Data.toBS detectorModelName]

instance Data.ToQuery ListDetectors where
  toQuery ListDetectors' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "stateName" Data.=: stateName
      ]

-- | /See:/ 'newListDetectorsResponse' smart constructor.
data ListDetectorsResponse = ListDetectorsResponse'
  { -- | A list of summary information about the detectors (instances).
    detectorSummaries :: Prelude.Maybe [DetectorSummary],
    -- | The token that you can use to return the next set of results, or @null@
    -- if there are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'detectorSummaries', 'listDetectorsResponse_detectorSummaries' - A list of summary information about the detectors (instances).
--
-- 'nextToken', 'listDetectorsResponse_nextToken' - The token that you can use to return the next set of results, or @null@
-- if there are no more results.
--
-- 'httpStatus', 'listDetectorsResponse_httpStatus' - The response's http status code.
newListDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDetectorsResponse
newListDetectorsResponse pHttpStatus_ =
  ListDetectorsResponse'
    { detectorSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of summary information about the detectors (instances).
listDetectorsResponse_detectorSummaries :: Lens.Lens' ListDetectorsResponse (Prelude.Maybe [DetectorSummary])
listDetectorsResponse_detectorSummaries = Lens.lens (\ListDetectorsResponse' {detectorSummaries} -> detectorSummaries) (\s@ListDetectorsResponse' {} a -> s {detectorSummaries = a} :: ListDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that you can use to return the next set of results, or @null@
-- if there are no more results.
listDetectorsResponse_nextToken :: Lens.Lens' ListDetectorsResponse (Prelude.Maybe Prelude.Text)
listDetectorsResponse_nextToken = Lens.lens (\ListDetectorsResponse' {nextToken} -> nextToken) (\s@ListDetectorsResponse' {} a -> s {nextToken = a} :: ListDetectorsResponse)

-- | The response's http status code.
listDetectorsResponse_httpStatus :: Lens.Lens' ListDetectorsResponse Prelude.Int
listDetectorsResponse_httpStatus = Lens.lens (\ListDetectorsResponse' {httpStatus} -> httpStatus) (\s@ListDetectorsResponse' {} a -> s {httpStatus = a} :: ListDetectorsResponse)

instance Prelude.NFData ListDetectorsResponse where
  rnf ListDetectorsResponse' {..} =
    Prelude.rnf detectorSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
