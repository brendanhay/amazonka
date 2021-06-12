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
-- Module      : Network.AWS.Pinpoint.GetUserEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about all the endpoints that are associated with a
-- specific user ID.
module Network.AWS.Pinpoint.GetUserEndpoints
  ( -- * Creating a Request
    GetUserEndpoints (..),
    newGetUserEndpoints,

    -- * Request Lenses
    getUserEndpoints_applicationId,
    getUserEndpoints_userId,

    -- * Destructuring the Response
    GetUserEndpointsResponse (..),
    newGetUserEndpointsResponse,

    -- * Response Lenses
    getUserEndpointsResponse_httpStatus,
    getUserEndpointsResponse_endpointsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetUserEndpoints' smart constructor.
data GetUserEndpoints = GetUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getUserEndpoints_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'userId', 'getUserEndpoints_userId' - The unique identifier for the user.
newGetUserEndpoints ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  GetUserEndpoints
newGetUserEndpoints pApplicationId_ pUserId_ =
  GetUserEndpoints'
    { applicationId = pApplicationId_,
      userId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
getUserEndpoints_applicationId :: Lens.Lens' GetUserEndpoints Core.Text
getUserEndpoints_applicationId = Lens.lens (\GetUserEndpoints' {applicationId} -> applicationId) (\s@GetUserEndpoints' {} a -> s {applicationId = a} :: GetUserEndpoints)

-- | The unique identifier for the user.
getUserEndpoints_userId :: Lens.Lens' GetUserEndpoints Core.Text
getUserEndpoints_userId = Lens.lens (\GetUserEndpoints' {userId} -> userId) (\s@GetUserEndpoints' {} a -> s {userId = a} :: GetUserEndpoints)

instance Core.AWSRequest GetUserEndpoints where
  type
    AWSResponse GetUserEndpoints =
      GetUserEndpointsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUserEndpointsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable GetUserEndpoints

instance Core.NFData GetUserEndpoints

instance Core.ToHeaders GetUserEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetUserEndpoints where
  toPath GetUserEndpoints' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/users/",
        Core.toBS userId
      ]

instance Core.ToQuery GetUserEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetUserEndpointsResponse' smart constructor.
data GetUserEndpointsResponse = GetUserEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    endpointsResponse :: EndpointsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetUserEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getUserEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpointsResponse', 'getUserEndpointsResponse_endpointsResponse' - Undocumented member.
newGetUserEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'endpointsResponse'
  EndpointsResponse ->
  GetUserEndpointsResponse
newGetUserEndpointsResponse
  pHttpStatus_
  pEndpointsResponse_ =
    GetUserEndpointsResponse'
      { httpStatus =
          pHttpStatus_,
        endpointsResponse = pEndpointsResponse_
      }

-- | The response's http status code.
getUserEndpointsResponse_httpStatus :: Lens.Lens' GetUserEndpointsResponse Core.Int
getUserEndpointsResponse_httpStatus = Lens.lens (\GetUserEndpointsResponse' {httpStatus} -> httpStatus) (\s@GetUserEndpointsResponse' {} a -> s {httpStatus = a} :: GetUserEndpointsResponse)

-- | Undocumented member.
getUserEndpointsResponse_endpointsResponse :: Lens.Lens' GetUserEndpointsResponse EndpointsResponse
getUserEndpointsResponse_endpointsResponse = Lens.lens (\GetUserEndpointsResponse' {endpointsResponse} -> endpointsResponse) (\s@GetUserEndpointsResponse' {} a -> s {endpointsResponse = a} :: GetUserEndpointsResponse)

instance Core.NFData GetUserEndpointsResponse
