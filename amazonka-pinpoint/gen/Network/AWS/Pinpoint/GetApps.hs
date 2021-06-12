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
-- Module      : Network.AWS.Pinpoint.GetApps
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the applications that are associated
-- with your Amazon Pinpoint account.
module Network.AWS.Pinpoint.GetApps
  ( -- * Creating a Request
    GetApps (..),
    newGetApps,

    -- * Request Lenses
    getApps_pageSize,
    getApps_token,

    -- * Destructuring the Response
    GetAppsResponse (..),
    newGetAppsResponse,

    -- * Response Lenses
    getAppsResponse_httpStatus,
    getAppsResponse_applicationsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApps' smart constructor.
data GetApps = GetApps'
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
-- Create a value of 'GetApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getApps_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getApps_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
newGetApps ::
  GetApps
newGetApps =
  GetApps'
    { pageSize = Core.Nothing,
      token = Core.Nothing
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApps_pageSize :: Lens.Lens' GetApps (Core.Maybe Core.Text)
getApps_pageSize = Lens.lens (\GetApps' {pageSize} -> pageSize) (\s@GetApps' {} a -> s {pageSize = a} :: GetApps)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getApps_token :: Lens.Lens' GetApps (Core.Maybe Core.Text)
getApps_token = Lens.lens (\GetApps' {token} -> token) (\s@GetApps' {} a -> s {token = a} :: GetApps)

instance Core.AWSRequest GetApps where
  type AWSResponse GetApps = GetAppsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetApps

instance Core.NFData GetApps

instance Core.ToHeaders GetApps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApps where
  toPath = Core.const "/v1/apps"

instance Core.ToQuery GetApps where
  toQuery GetApps' {..} =
    Core.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetAppsResponse' smart constructor.
data GetAppsResponse = GetAppsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    applicationsResponse :: ApplicationsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAppsResponse_httpStatus' - The response's http status code.
--
-- 'applicationsResponse', 'getAppsResponse_applicationsResponse' - Undocumented member.
newGetAppsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'applicationsResponse'
  ApplicationsResponse ->
  GetAppsResponse
newGetAppsResponse
  pHttpStatus_
  pApplicationsResponse_ =
    GetAppsResponse'
      { httpStatus = pHttpStatus_,
        applicationsResponse = pApplicationsResponse_
      }

-- | The response's http status code.
getAppsResponse_httpStatus :: Lens.Lens' GetAppsResponse Core.Int
getAppsResponse_httpStatus = Lens.lens (\GetAppsResponse' {httpStatus} -> httpStatus) (\s@GetAppsResponse' {} a -> s {httpStatus = a} :: GetAppsResponse)

-- | Undocumented member.
getAppsResponse_applicationsResponse :: Lens.Lens' GetAppsResponse ApplicationsResponse
getAppsResponse_applicationsResponse = Lens.lens (\GetAppsResponse' {applicationsResponse} -> applicationsResponse) (\s@GetAppsResponse' {} a -> s {applicationsResponse = a} :: GetAppsResponse)

instance Core.NFData GetAppsResponse
