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
-- Module      : Amazonka.Pinpoint.GetCampaigns
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other
-- settings for all the campaigns that are associated with an application.
module Amazonka.Pinpoint.GetCampaigns
  ( -- * Creating a Request
    GetCampaigns (..),
    newGetCampaigns,

    -- * Request Lenses
    getCampaigns_pageSize,
    getCampaigns_token,
    getCampaigns_applicationId,

    -- * Destructuring the Response
    GetCampaignsResponse (..),
    newGetCampaignsResponse,

    -- * Response Lenses
    getCampaignsResponse_httpStatus,
    getCampaignsResponse_campaignsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCampaigns' smart constructor.
data GetCampaigns = GetCampaigns'
  { -- | The maximum number of items to include in each page of a paginated
    -- response. This parameter is not supported for application, campaign, and
    -- journey metrics.
    pageSize :: Prelude.Maybe Prelude.Text,
    -- | The NextToken string that specifies which page of results to return in a
    -- paginated response.
    token :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaigns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'getCampaigns_pageSize' - The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
--
-- 'token', 'getCampaigns_token' - The NextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'applicationId', 'getCampaigns_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetCampaigns ::
  -- | 'applicationId'
  Prelude.Text ->
  GetCampaigns
newGetCampaigns pApplicationId_ =
  GetCampaigns'
    { pageSize = Prelude.Nothing,
      token = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The maximum number of items to include in each page of a paginated
-- response. This parameter is not supported for application, campaign, and
-- journey metrics.
getCampaigns_pageSize :: Lens.Lens' GetCampaigns (Prelude.Maybe Prelude.Text)
getCampaigns_pageSize = Lens.lens (\GetCampaigns' {pageSize} -> pageSize) (\s@GetCampaigns' {} a -> s {pageSize = a} :: GetCampaigns)

-- | The NextToken string that specifies which page of results to return in a
-- paginated response.
getCampaigns_token :: Lens.Lens' GetCampaigns (Prelude.Maybe Prelude.Text)
getCampaigns_token = Lens.lens (\GetCampaigns' {token} -> token) (\s@GetCampaigns' {} a -> s {token = a} :: GetCampaigns)

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getCampaigns_applicationId :: Lens.Lens' GetCampaigns Prelude.Text
getCampaigns_applicationId = Lens.lens (\GetCampaigns' {applicationId} -> applicationId) (\s@GetCampaigns' {} a -> s {applicationId = a} :: GetCampaigns)

instance Core.AWSRequest GetCampaigns where
  type AWSResponse GetCampaigns = GetCampaignsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCampaignsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Core.eitherParseJSON x)
      )

instance Prelude.Hashable GetCampaigns where
  hashWithSalt _salt GetCampaigns' {..} =
    _salt `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetCampaigns where
  rnf GetCampaigns' {..} =
    Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders GetCampaigns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetCampaigns where
  toPath GetCampaigns' {..} =
    Prelude.mconcat
      ["/v1/apps/", Core.toBS applicationId, "/campaigns"]

instance Core.ToQuery GetCampaigns where
  toQuery GetCampaigns' {..} =
    Prelude.mconcat
      ["page-size" Core.=: pageSize, "token" Core.=: token]

-- | /See:/ 'newGetCampaignsResponse' smart constructor.
data GetCampaignsResponse = GetCampaignsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    campaignsResponse :: CampaignsResponse
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCampaignsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCampaignsResponse_httpStatus' - The response's http status code.
--
-- 'campaignsResponse', 'getCampaignsResponse_campaignsResponse' - Undocumented member.
newGetCampaignsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'campaignsResponse'
  CampaignsResponse ->
  GetCampaignsResponse
newGetCampaignsResponse
  pHttpStatus_
  pCampaignsResponse_ =
    GetCampaignsResponse'
      { httpStatus = pHttpStatus_,
        campaignsResponse = pCampaignsResponse_
      }

-- | The response's http status code.
getCampaignsResponse_httpStatus :: Lens.Lens' GetCampaignsResponse Prelude.Int
getCampaignsResponse_httpStatus = Lens.lens (\GetCampaignsResponse' {httpStatus} -> httpStatus) (\s@GetCampaignsResponse' {} a -> s {httpStatus = a} :: GetCampaignsResponse)

-- | Undocumented member.
getCampaignsResponse_campaignsResponse :: Lens.Lens' GetCampaignsResponse CampaignsResponse
getCampaignsResponse_campaignsResponse = Lens.lens (\GetCampaignsResponse' {campaignsResponse} -> campaignsResponse) (\s@GetCampaignsResponse' {} a -> s {campaignsResponse = a} :: GetCampaignsResponse)

instance Prelude.NFData GetCampaignsResponse where
  rnf GetCampaignsResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf campaignsResponse
