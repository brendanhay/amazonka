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
-- Module      : Network.AWS.Lightsail.GetDistributions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more of your Amazon Lightsail content
-- delivery network (CDN) distributions.
module Network.AWS.Lightsail.GetDistributions
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
    getDistributionsResponse_distributions,
    getDistributionsResponse_nextPageToken,
    getDistributionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDistributions' smart constructor.
data GetDistributions = GetDistributions'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDistributions@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text,
    -- | The name of the distribution for which to return information.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    --
    -- When omitted, the response includes all of your distributions in the AWS
    -- Region where the request is made.
    distributionName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- When omitted, the response includes all of your distributions in the AWS
-- Region where the request is made.
newGetDistributions ::
  GetDistributions
newGetDistributions =
  GetDistributions'
    { pageToken = Core.Nothing,
      distributionName = Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDistributions@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getDistributions_pageToken :: Lens.Lens' GetDistributions (Core.Maybe Core.Text)
getDistributions_pageToken = Lens.lens (\GetDistributions' {pageToken} -> pageToken) (\s@GetDistributions' {} a -> s {pageToken = a} :: GetDistributions)

-- | The name of the distribution for which to return information.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
--
-- When omitted, the response includes all of your distributions in the AWS
-- Region where the request is made.
getDistributions_distributionName :: Lens.Lens' GetDistributions (Core.Maybe Core.Text)
getDistributions_distributionName = Lens.lens (\GetDistributions' {distributionName} -> distributionName) (\s@GetDistributions' {} a -> s {distributionName = a} :: GetDistributions)

instance Core.AWSRequest GetDistributions where
  type
    AWSResponse GetDistributions =
      GetDistributionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDistributionsResponse'
            Core.<$> (x Core..?> "distributions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDistributions

instance Core.NFData GetDistributions

instance Core.ToHeaders GetDistributions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetDistributions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDistributions where
  toJSON GetDistributions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("pageToken" Core..=) Core.<$> pageToken,
            ("distributionName" Core..=)
              Core.<$> distributionName
          ]
      )

instance Core.ToPath GetDistributions where
  toPath = Core.const "/"

instance Core.ToQuery GetDistributions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDistributionsResponse' smart constructor.
data GetDistributionsResponse = GetDistributionsResponse'
  { -- | An array of objects that describe your distributions.
    distributions :: Core.Maybe [LightsailDistribution],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDistributions@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetDistributionsResponse
newGetDistributionsResponse pHttpStatus_ =
  GetDistributionsResponse'
    { distributions =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe your distributions.
getDistributionsResponse_distributions :: Lens.Lens' GetDistributionsResponse (Core.Maybe [LightsailDistribution])
getDistributionsResponse_distributions = Lens.lens (\GetDistributionsResponse' {distributions} -> distributions) (\s@GetDistributionsResponse' {} a -> s {distributions = a} :: GetDistributionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDistributions@
-- request and specify the next page token using the @pageToken@ parameter.
getDistributionsResponse_nextPageToken :: Lens.Lens' GetDistributionsResponse (Core.Maybe Core.Text)
getDistributionsResponse_nextPageToken = Lens.lens (\GetDistributionsResponse' {nextPageToken} -> nextPageToken) (\s@GetDistributionsResponse' {} a -> s {nextPageToken = a} :: GetDistributionsResponse)

-- | The response's http status code.
getDistributionsResponse_httpStatus :: Lens.Lens' GetDistributionsResponse Core.Int
getDistributionsResponse_httpStatus = Lens.lens (\GetDistributionsResponse' {httpStatus} -> httpStatus) (\s@GetDistributionsResponse' {} a -> s {httpStatus = a} :: GetDistributionsResponse)

instance Core.NFData GetDistributionsResponse
