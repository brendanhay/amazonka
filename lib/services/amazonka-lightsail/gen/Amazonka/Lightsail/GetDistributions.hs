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
-- Module      : Amazonka.Lightsail.GetDistributions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail content
-- delivery network (CDN) distributions.
module Amazonka.Lightsail.GetDistributions
  ( -- * Creating a Request
    GetDistributions (..),
    newGetDistributions,

    -- * Request Lenses
    getDistributions_distributionName,
    getDistributions_pageToken,

    -- * Destructuring the Response
    GetDistributionsResponse (..),
    newGetDistributionsResponse,

    -- * Response Lenses
    getDistributionsResponse_distributions,
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { -- | The name of the distribution for which to return information.
    --
    -- When omitted, the response includes all of your distributions in the
    -- Amazon Web Services Region where the request is made.
    distributionName :: Prelude.Maybe Prelude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDistributions@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'getDistributions_distributionName' - The name of the distribution for which to return information.
--
-- When omitted, the response includes all of your distributions in the
-- Amazon Web Services Region where the request is made.
--
-- 'pageToken', 'getDistributions_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
newGetDistributions ::
  GetDistributions
newGetDistributions =
  GetDistributions'
    { distributionName =
        Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | The name of the distribution for which to return information.
--
-- When omitted, the response includes all of your distributions in the
-- Amazon Web Services Region where the request is made.
getDistributions_distributionName :: Lens.Lens' GetDistributions (Prelude.Maybe Prelude.Text)
getDistributions_distributionName = Lens.lens (\GetDistributions' {distributionName} -> distributionName) (\s@GetDistributions' {} a -> s {distributionName = a} :: GetDistributions)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getDistributions_pageToken :: Lens.Lens' GetDistributions (Prelude.Maybe Prelude.Text)
getDistributions_pageToken = Lens.lens (\GetDistributions' {pageToken} -> pageToken) (\s@GetDistributions' {} a -> s {pageToken = a} :: GetDistributions)

instance Core.AWSRequest GetDistributions where
  type
    AWSResponse GetDistributions =
      GetDistributionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionsResponse'
            Prelude.<$> (x Data..?> "distributions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDistributions where
  hashWithSalt _salt GetDistributions' {..} =
    _salt
      `Prelude.hashWithSalt` distributionName
      `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetDistributions where
  rnf GetDistributions' {..} =
    Prelude.rnf distributionName
      `Prelude.seq` Prelude.rnf pageToken

instance Data.ToHeaders GetDistributions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetDistributions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDistributions where
  toJSON GetDistributions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("distributionName" Data..=)
              Prelude.<$> distributionName,
            ("pageToken" Data..=) Prelude.<$> pageToken
          ]
      )

instance Data.ToPath GetDistributions where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDistributions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { -- | An array of objects that describe your distributions.
    distributions :: Prelude.Maybe [LightsailDistribution],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDistributions@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDistributionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributions', 'getDistributionsResponse_distributions' - An array of objects that describe your distributions.
--
-- 'nextPageToken', 'getDistributionsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDistributions@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getDistributionsResponse_httpStatus' - The response's http status code.
newGetDistributionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionsResponse
newGetDistributionsResponse pHttpStatus_ =
  GetDistributionsResponse'
    { distributions =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe your distributions.
getDistributionsResponse_distributions :: Lens.Lens' GetDistributionsResponse (Prelude.Maybe [LightsailDistribution])
getDistributionsResponse_distributions = Lens.lens (\GetDistributionsResponse' {distributions} -> distributions) (\s@GetDistributionsResponse' {} a -> s {distributions = a} :: GetDistributionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDistributions@
-- request and specify the next page token using the @pageToken@ parameter.
getDistributionsResponse_nextPageToken :: Lens.Lens' GetDistributionsResponse (Prelude.Maybe Prelude.Text)
getDistributionsResponse_nextPageToken = Lens.lens (\GetDistributionsResponse' {nextPageToken} -> nextPageToken) (\s@GetDistributionsResponse' {} a -> s {nextPageToken = a} :: GetDistributionsResponse)

-- | The response's http status code.
getDistributionsResponse_httpStatus :: Lens.Lens' GetDistributionsResponse Prelude.Int
getDistributionsResponse_httpStatus = Lens.lens (\GetDistributionsResponse' {httpStatus} -> httpStatus) (\s@GetDistributionsResponse' {} a -> s {httpStatus = a} :: GetDistributionsResponse)

instance Prelude.NFData GetDistributionsResponse where
  rnf GetDistributionsResponse' {..} =
    Prelude.rnf distributions
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
