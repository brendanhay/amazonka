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
-- Module      : Network.AWS.Pinpoint.GetApp
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an application.
module Network.AWS.Pinpoint.GetApp
  ( -- * Creating a Request
    GetApp (..),
    newGetApp,

    -- * Request Lenses
    getApp_applicationId,

    -- * Destructuring the Response
    GetAppResponse (..),
    newGetAppResponse,

    -- * Response Lenses
    getAppResponse_httpStatus,
    getAppResponse_applicationResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetApp' smart constructor.
data GetApp = GetApp'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApp' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApp_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
newGetApp ::
  -- | 'applicationId'
  Core.Text ->
  GetApp
newGetApp pApplicationId_ =
  GetApp' {applicationId = pApplicationId_}

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getApp_applicationId :: Lens.Lens' GetApp Core.Text
getApp_applicationId = Lens.lens (\GetApp' {applicationId} -> applicationId) (\s@GetApp' {} a -> s {applicationId = a} :: GetApp)

instance Core.AWSRequest GetApp where
  type AWSResponse GetApp = GetAppResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetApp

instance Core.NFData GetApp

instance Core.ToHeaders GetApp where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApp where
  toPath GetApp' {..} =
    Core.mconcat ["/v1/apps/", Core.toBS applicationId]

instance Core.ToQuery GetApp where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetAppResponse' smart constructor.
data GetAppResponse = GetAppResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    applicationResponse :: ApplicationResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetAppResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAppResponse_httpStatus' - The response's http status code.
--
-- 'applicationResponse', 'getAppResponse_applicationResponse' - Undocumented member.
newGetAppResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'applicationResponse'
  ApplicationResponse ->
  GetAppResponse
newGetAppResponse pHttpStatus_ pApplicationResponse_ =
  GetAppResponse'
    { httpStatus = pHttpStatus_,
      applicationResponse = pApplicationResponse_
    }

-- | The response's http status code.
getAppResponse_httpStatus :: Lens.Lens' GetAppResponse Core.Int
getAppResponse_httpStatus = Lens.lens (\GetAppResponse' {httpStatus} -> httpStatus) (\s@GetAppResponse' {} a -> s {httpStatus = a} :: GetAppResponse)

-- | Undocumented member.
getAppResponse_applicationResponse :: Lens.Lens' GetAppResponse ApplicationResponse
getAppResponse_applicationResponse = Lens.lens (\GetAppResponse' {applicationResponse} -> applicationResponse) (\s@GetAppResponse' {} a -> s {applicationResponse = a} :: GetAppResponse)

instance Core.NFData GetAppResponse
