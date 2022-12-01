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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    getDistributions_pageToken,
    getDistributions_distributionName,

    -- * Destructuring the Response
    GetDistributionsResponse (..),
    newGetDistributionsResponse,

    -- * Response Lenses
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_distributions,
    getDistributionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDistributions@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution for which to return information.
    --
    -- When omitted, the response includes all of your distributions in the
    -- Amazon Web Services Region where the request is made.
    distributionName :: Prelude.Maybe Prelude.Text
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
-- 'pageToken', 'getDistributions_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
--
-- 'distributionName', 'getDistributions_distributionName' - The name of the distribution for which to return information.
--
-- When omitted, the response includes all of your distributions in the
-- Amazon Web Services Region where the request is made.
newGetDistributions ::
  GetDistributions
newGetDistributions =
  GetDistributions'
    { pageToken = Prelude.Nothing,
      distributionName = Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getDistributions_pageToken :: Lens.Lens' GetDistributions (Prelude.Maybe Prelude.Text)
getDistributions_pageToken = Lens.lens (\GetDistributions' {pageToken} -> pageToken) (\s@GetDistributions' {} a -> s {pageToken = a} :: GetDistributions)

-- | The name of the distribution for which to return information.
--
-- When omitted, the response includes all of your distributions in the
-- Amazon Web Services Region where the request is made.
getDistributions_distributionName :: Lens.Lens' GetDistributions (Prelude.Maybe Prelude.Text)
getDistributions_distributionName = Lens.lens (\GetDistributions' {distributionName} -> distributionName) (\s@GetDistributions' {} a -> s {distributionName = a} :: GetDistributions)

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
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (x Core..?> "distributions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDistributions where
  hashWithSalt _salt GetDistributions' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` distributionName

instance Prelude.NFData GetDistributions where
  rnf GetDistributions' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf distributionName

instance Core.ToHeaders GetDistributions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetDistributions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDistributions where
  toJSON GetDistributions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("pageToken" Core..=) Prelude.<$> pageToken,
            ("distributionName" Core..=)
              Prelude.<$> distributionName
          ]
      )

instance Core.ToPath GetDistributions where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDistributions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDistributions@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe your distributions.
    distributions :: Prelude.Maybe [LightsailDistribution],
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
-- 'nextPageToken', 'getDistributionsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDistributions@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'distributions', 'getDistributionsResponse_distributions' - An array of objects that describe your distributions.
--
-- 'httpStatus', 'getDistributionsResponse_httpStatus' - The response's http status code.
newGetDistributionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDistributionsResponse
newGetDistributionsResponse pHttpStatus_ =
  GetDistributionsResponse'
    { nextPageToken =
        Prelude.Nothing,
      distributions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDistributions@
-- request and specify the next page token using the @pageToken@ parameter.
getDistributionsResponse_nextPageToken :: Lens.Lens' GetDistributionsResponse (Prelude.Maybe Prelude.Text)
getDistributionsResponse_nextPageToken = Lens.lens (\GetDistributionsResponse' {nextPageToken} -> nextPageToken) (\s@GetDistributionsResponse' {} a -> s {nextPageToken = a} :: GetDistributionsResponse)

-- | An array of objects that describe your distributions.
getDistributionsResponse_distributions :: Lens.Lens' GetDistributionsResponse (Prelude.Maybe [LightsailDistribution])
getDistributionsResponse_distributions = Lens.lens (\GetDistributionsResponse' {distributions} -> distributions) (\s@GetDistributionsResponse' {} a -> s {distributions = a} :: GetDistributionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDistributionsResponse_httpStatus :: Lens.Lens' GetDistributionsResponse Prelude.Int
getDistributionsResponse_httpStatus = Lens.lens (\GetDistributionsResponse' {httpStatus} -> httpStatus) (\s@GetDistributionsResponse' {} a -> s {httpStatus = a} :: GetDistributionsResponse)

instance Prelude.NFData GetDistributionsResponse where
  rnf GetDistributionsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf distributions
      `Prelude.seq` Prelude.rnf httpStatus
