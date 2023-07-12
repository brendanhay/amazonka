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
-- Module      : Amazonka.FraudDetector.GetDetectors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all detectors or a single detector if a @detectorId@ is specified.
-- This is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 10 records per page. If you provide a
-- @maxResults@, the value must be between 5 and 10. To get the next page
-- results, provide the pagination token from the @GetDetectorsResponse@ as
-- part of your request. A null pagination token fetches the records from
-- the beginning.
module Amazonka.FraudDetector.GetDetectors
  ( -- * Creating a Request
    GetDetectors (..),
    newGetDetectors,

    -- * Request Lenses
    getDetectors_detectorId,
    getDetectors_maxResults,
    getDetectors_nextToken,

    -- * Destructuring the Response
    GetDetectorsResponse (..),
    newGetDetectorsResponse,

    -- * Response Lenses
    getDetectorsResponse_detectors,
    getDetectorsResponse_nextToken,
    getDetectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDetectors' smart constructor.
data GetDetectors = GetDetectors'
  { -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectorId', 'getDetectors_detectorId' - The detector ID.
--
-- 'maxResults', 'getDetectors_maxResults' - The maximum number of objects to return for the request.
--
-- 'nextToken', 'getDetectors_nextToken' - The next token for the subsequent request.
newGetDetectors ::
  GetDetectors
newGetDetectors =
  GetDetectors'
    { detectorId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The detector ID.
getDetectors_detectorId :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Text)
getDetectors_detectorId = Lens.lens (\GetDetectors' {detectorId} -> detectorId) (\s@GetDetectors' {} a -> s {detectorId = a} :: GetDetectors)

-- | The maximum number of objects to return for the request.
getDetectors_maxResults :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Natural)
getDetectors_maxResults = Lens.lens (\GetDetectors' {maxResults} -> maxResults) (\s@GetDetectors' {} a -> s {maxResults = a} :: GetDetectors)

-- | The next token for the subsequent request.
getDetectors_nextToken :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Text)
getDetectors_nextToken = Lens.lens (\GetDetectors' {nextToken} -> nextToken) (\s@GetDetectors' {} a -> s {nextToken = a} :: GetDetectors)

instance Core.AWSRequest GetDetectors where
  type AWSResponse GetDetectors = GetDetectorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDetectorsResponse'
            Prelude.<$> (x Data..?> "detectors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDetectors where
  hashWithSalt _salt GetDetectors' {..} =
    _salt
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDetectors where
  rnf GetDetectors' {..} =
    Prelude.rnf detectorId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetDetectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDetectors where
  toJSON GetDetectors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("detectorId" Data..=) Prelude.<$> detectorId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetDetectors where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDetectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDetectorsResponse' smart constructor.
data GetDetectorsResponse = GetDetectorsResponse'
  { -- | The detectors.
    detectors :: Prelude.Maybe [Detector],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectors', 'getDetectorsResponse_detectors' - The detectors.
--
-- 'nextToken', 'getDetectorsResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getDetectorsResponse_httpStatus' - The response's http status code.
newGetDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDetectorsResponse
newGetDetectorsResponse pHttpStatus_ =
  GetDetectorsResponse'
    { detectors = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detectors.
getDetectorsResponse_detectors :: Lens.Lens' GetDetectorsResponse (Prelude.Maybe [Detector])
getDetectorsResponse_detectors = Lens.lens (\GetDetectorsResponse' {detectors} -> detectors) (\s@GetDetectorsResponse' {} a -> s {detectors = a} :: GetDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getDetectorsResponse_nextToken :: Lens.Lens' GetDetectorsResponse (Prelude.Maybe Prelude.Text)
getDetectorsResponse_nextToken = Lens.lens (\GetDetectorsResponse' {nextToken} -> nextToken) (\s@GetDetectorsResponse' {} a -> s {nextToken = a} :: GetDetectorsResponse)

-- | The response's http status code.
getDetectorsResponse_httpStatus :: Lens.Lens' GetDetectorsResponse Prelude.Int
getDetectorsResponse_httpStatus = Lens.lens (\GetDetectorsResponse' {httpStatus} -> httpStatus) (\s@GetDetectorsResponse' {} a -> s {httpStatus = a} :: GetDetectorsResponse)

instance Prelude.NFData GetDetectorsResponse where
  rnf GetDetectorsResponse' {..} =
    Prelude.rnf detectors
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
