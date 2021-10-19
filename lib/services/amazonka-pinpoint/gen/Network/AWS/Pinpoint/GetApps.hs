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
    getApps_token,
    getApps_pageSize,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApps' smart constructor.
data GetApps = GetApps'
  { -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'token', 'getApps_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'pageSize', 'getApps_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
newGetApps ::
  GetApps
newGetApps =
  GetApps'
    { token = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getApps_token :: Lens.Lens' GetApps (Prelude.Maybe Prelude.Text)
getApps_token = Lens.lens (\GetApps' {token} -> token) (\s@GetApps' {} a -> s {token = a} :: GetApps)

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getApps_pageSize :: Lens.Lens' GetApps (Prelude.Maybe Prelude.Text)
getApps_pageSize = Lens.lens (\GetApps' {pageSize} -> pageSize) (\s@GetApps' {} a -> s {pageSize = a} :: GetApps)

instance Core.AWSRequest GetApps where
  type AWSResponse GetApps = GetAppsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetApps

instance Prelude.NFData GetApps

instance Core.ToHeaders GetApps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApps where
  toPath = Prelude.const "/v1/apps"

instance Core.ToQuery GetApps where
  toQuery GetApps' {..} =
    Prelude.mconcat
      ["token" Core.=: token, "page-size" Core.=: pageSize]

-- | /See:/ 'newGetAppsResponse' smart constructor.
data GetAppsResponse = GetAppsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    applicationsResponse :: ApplicationsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
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
getAppsResponse_httpStatus :: Lens.Lens' GetAppsResponse Prelude.Int
getAppsResponse_httpStatus = Lens.lens (\GetAppsResponse' {httpStatus} -> httpStatus) (\s@GetAppsResponse' {} a -> s {httpStatus = a} :: GetAppsResponse)

-- | Undocumented member.
getAppsResponse_applicationsResponse :: Lens.Lens' GetAppsResponse ApplicationsResponse
getAppsResponse_applicationsResponse = Lens.lens (\GetAppsResponse' {applicationsResponse} -> applicationsResponse) (\s@GetAppsResponse' {} a -> s {applicationsResponse = a} :: GetAppsResponse)

instance Prelude.NFData GetAppsResponse
