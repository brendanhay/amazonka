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
-- Module      : Amazonka.Pinpoint.GetJourneyRuns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the runs of a journey.
module Amazonka.Pinpoint.GetJourneyRuns
  ( -- * Creating a Request
    GetJourneyRuns (..),
    newGetJourneyRuns,

    -- * Request Lenses
    getJourneyRuns_pageSize,
    getJourneyRuns_token,
    getJourneyRuns_applicationId,
    getJourneyRuns_journeyId,

    -- * Destructuring the Response
    GetJourneyRunsResponse (..),
    newGetJourneyRunsResponse,

    -- * Response Lenses
    getJourneyRunsResponse_httpStatus,
    getJourneyRunsResponse_journeyRunsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetJourneyRuns' smart constructor.
data GetJourneyRuns = GetJourneyRuns'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text,
    -- | The unique identifier for the journey.
    journeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRuns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getJourneyRuns_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getJourneyRuns_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getJourneyRuns_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'journeyId', 'getJourneyRuns_journeyId' - The unique identifier for the journey.
newGetJourneyRuns ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'journeyId'
  Prelude.Text ->
  GetJourneyRuns
newGetJourneyRuns pApplicationId_ pJourneyId_ =
  GetJourneyRuns'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      applicationId = pApplicationId_,
      journeyId = pJourneyId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getJourneyRuns_pageSize :: Lens.Lens' GetJourneyRuns (Prelude.Maybe Prelude.Text)
getJourneyRuns_pageSize = Lens.lens (\GetJourneyRuns' {pageSize} -> pageSize) (\s@GetJourneyRuns' {} a -> s {pageSize = a} :: GetJourneyRuns)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getJourneyRuns_token :: Lens.Lens' GetJourneyRuns (Prelude.Maybe Prelude.Text)
getJourneyRuns_token = Lens.lens (\GetJourneyRuns' {token} -> token) (\s@GetJourneyRuns' {} a -> s {token = a} :: GetJourneyRuns)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getJourneyRuns_applicationId :: Lens.Lens' GetJourneyRuns Prelude.Text
getJourneyRuns_applicationId = Lens.lens (\GetJourneyRuns' {applicationId} -> applicationId) (\s@GetJourneyRuns' {} a -> s {applicationId = a} :: GetJourneyRuns)

-- | The unique identifier for the journey.
getJourneyRuns_journeyId :: Lens.Lens' GetJourneyRuns Prelude.Text
getJourneyRuns_journeyId = Lens.lens (\GetJourneyRuns' {journeyId} -> journeyId) (\s@GetJourneyRuns' {} a -> s {journeyId = a} :: GetJourneyRuns)

instance Core.AWSRequest GetJourneyRuns where
  type
    AWSResponse GetJourneyRuns =
      GetJourneyRunsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetJourneyRunsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.eitherParseJSON x)
      )

instance Prelude.Hashable GetJourneyRuns where
  hashWithSalt _salt GetJourneyRuns' {..} =
    _salt
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` journeyId

instance Prelude.NFData GetJourneyRuns where
  rnf GetJourneyRuns' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf journeyId

instance Data.ToHeaders GetJourneyRuns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetJourneyRuns where
  toPath GetJourneyRuns' {..} =
    Prelude.mconcat
      [ "/v1/apps/",
        Data.toBS applicationId,
        "/journeys/",
        Data.toBS journeyId,
        "/runs"
      ]

instance Data.ToQuery GetJourneyRuns where
  toQuery GetJourneyRuns' {..} =
    Prelude.mconcat
      ["page-size" Data.=: pageSize, "token" Data.=: token]

-- | /See:/ 'newGetJourneyRunsResponse' smart constructor.
data GetJourneyRunsResponse = GetJourneyRunsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    journeyRunsResponse :: JourneyRunsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetJourneyRunsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getJourneyRunsResponse_httpStatus' - The response's http status code.
--
-- 'journeyRunsResponse', 'getJourneyRunsResponse_journeyRunsResponse' - Undocumented member.
newGetJourneyRunsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'journeyRunsResponse'
  JourneyRunsResponse ->
  GetJourneyRunsResponse
newGetJourneyRunsResponse
  pHttpStatus_
  pJourneyRunsResponse_ =
    GetJourneyRunsResponse'
      { httpStatus = pHttpStatus_,
        journeyRunsResponse = pJourneyRunsResponse_
      }

-- | The response's http status code.
getJourneyRunsResponse_httpStatus :: Lens.Lens' GetJourneyRunsResponse Prelude.Int
getJourneyRunsResponse_httpStatus = Lens.lens (\GetJourneyRunsResponse' {httpStatus} -> httpStatus) (\s@GetJourneyRunsResponse' {} a -> s {httpStatus = a} :: GetJourneyRunsResponse)

-- | Undocumented member.
getJourneyRunsResponse_journeyRunsResponse :: Lens.Lens' GetJourneyRunsResponse JourneyRunsResponse
getJourneyRunsResponse_journeyRunsResponse = Lens.lens (\GetJourneyRunsResponse' {journeyRunsResponse} -> journeyRunsResponse) (\s@GetJourneyRunsResponse' {} a -> s {journeyRunsResponse = a} :: GetJourneyRunsResponse)

instance Prelude.NFData GetJourneyRunsResponse where
  rnf GetJourneyRunsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf journeyRunsResponse
