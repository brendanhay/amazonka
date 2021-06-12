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
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the recommender model configurations
-- that are associated with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetRecommenderConfigurations
  ( -- * Creating a Request
    GetRecommenderConfigurations (..),
    newGetRecommenderConfigurations,

    -- * Request Lenses
    getRecommenderConfigurations_pageSize,
    getRecommenderConfigurations_token,

    -- * Destructuring the Response
    GetRecommenderConfigurationsResponse (..),
    newGetRecommenderConfigurationsResponse,

    -- * Response Lenses
    getRecommenderConfigurationsResponse_httpStatus,
    getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRecommenderConfigurations' smart constructor.
data GetRecommenderConfigurations = GetRecommenderConfigurations'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Core.Maybe Core.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRecommenderConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getRecommenderConfigurations_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getRecommenderConfigurations_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
newGetRecommenderConfigurations ::
  GetRecommenderConfigurations
newGetRecommenderConfigurations =
  GetRecommenderConfigurations'
    { pageSize =
        Core.Nothing,
      token = Core.Nothing
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getRecommenderConfigurations_pageSize :: Lens.Lens' GetRecommenderConfigurations (Core.Maybe Core.Text)
getRecommenderConfigurations_pageSize = Lens.lens (\GetRecommenderConfigurations' {pageSize} -> pageSize) (\s@GetRecommenderConfigurations' {} a -> s {pageSize = a} :: GetRecommenderConfigurations)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getRecommenderConfigurations_token :: Lens.Lens' GetRecommenderConfigurations (Core.Maybe Core.Text)
getRecommenderConfigurations_token = Lens.lens (\GetRecommenderConfigurations' {token} -> token) (\s@GetRecommenderConfigurations' {} a -> s {token = a} :: GetRecommenderConfigurations)

instance Core.AWSRequest GetRecommenderConfigurations where
  type
    AWSResponse GetRecommenderConfigurations =
      GetRecommenderConfigurationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetRecommenderConfigurations

instance Core.NFData GetRecommenderConfigurations

instance Core.ToHeaders GetRecommenderConfigurations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetRecommenderConfigurations where
  toPath = Core.const "/v1/recommenders"

instance Core.ToQuery GetRecommenderConfigurations where
  toQuery GetRecommenderConfigurations' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetRecommenderConfigurationsResponse' smart constructor.
data GetRecommenderConfigurationsResponse = GetRecommenderConfigurationsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    listRecommenderConfigurationsResponse :: ListRecommenderConfigurationsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRecommenderConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRecommenderConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'listRecommenderConfigurationsResponse', 'getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse' - Undocumented member.
newGetRecommenderConfigurationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'listRecommenderConfigurationsResponse'
  ListRecommenderConfigurationsResponse ->
  GetRecommenderConfigurationsResponse
newGetRecommenderConfigurationsResponse
  pHttpStatus_
  pListRecommenderConfigurationsResponse_ =
    GetRecommenderConfigurationsResponse'
      { httpStatus =
          pHttpStatus_,
        listRecommenderConfigurationsResponse =
          pListRecommenderConfigurationsResponse_
      }

-- | The response's http status code.
getRecommenderConfigurationsResponse_httpStatus :: Lens.Lens' GetRecommenderConfigurationsResponse Core.Int
getRecommenderConfigurationsResponse_httpStatus = Lens.lens (\GetRecommenderConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetRecommenderConfigurationsResponse' {} a -> s {httpStatus = a} :: GetRecommenderConfigurationsResponse)

-- | Undocumented member.
getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse :: Lens.Lens' GetRecommenderConfigurationsResponse ListRecommenderConfigurationsResponse
getRecommenderConfigurationsResponse_listRecommenderConfigurationsResponse = Lens.lens (\GetRecommenderConfigurationsResponse' {listRecommenderConfigurationsResponse} -> listRecommenderConfigurationsResponse) (\s@GetRecommenderConfigurationsResponse' {} a -> s {listRecommenderConfigurationsResponse = a} :: GetRecommenderConfigurationsResponse)

instance
  Core.NFData
    GetRecommenderConfigurationsResponse
