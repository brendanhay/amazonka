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
-- Module      : Network.AWS.Pinpoint.DeleteUserEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all the endpoints that are associated with a specific user ID.
module Network.AWS.Pinpoint.DeleteUserEndpoints
  ( -- * Creating a Request
    DeleteUserEndpoints (..),
    newDeleteUserEndpoints,

    -- * Request Lenses
    deleteUserEndpoints_applicationId,
    deleteUserEndpoints_userId,

    -- * Destructuring the Response
    DeleteUserEndpointsResponse (..),
    newDeleteUserEndpointsResponse,

    -- * Response Lenses
    deleteUserEndpointsResponse_httpStatus,
    deleteUserEndpointsResponse_endpointsResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserEndpoints' smart constructor.
data DeleteUserEndpoints = DeleteUserEndpoints'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteUserEndpoints_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'userId', 'deleteUserEndpoints_userId' - The unique identifier for the user.
newDeleteUserEndpoints ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'userId'
  Core.Text ->
  DeleteUserEndpoints
newDeleteUserEndpoints pApplicationId_ pUserId_ =
  DeleteUserEndpoints'
    { applicationId =
        pApplicationId_,
      userId = pUserId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteUserEndpoints_applicationId :: Lens.Lens' DeleteUserEndpoints Core.Text
deleteUserEndpoints_applicationId = Lens.lens (\DeleteUserEndpoints' {applicationId} -> applicationId) (\s@DeleteUserEndpoints' {} a -> s {applicationId = a} :: DeleteUserEndpoints)

-- | The unique identifier for the user.
deleteUserEndpoints_userId :: Lens.Lens' DeleteUserEndpoints Core.Text
deleteUserEndpoints_userId = Lens.lens (\DeleteUserEndpoints' {userId} -> userId) (\s@DeleteUserEndpoints' {} a -> s {userId = a} :: DeleteUserEndpoints)

instance Core.AWSRequest DeleteUserEndpoints where
  type
    AWSResponse DeleteUserEndpoints =
      DeleteUserEndpointsResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteUserEndpointsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteUserEndpoints

instance Core.NFData DeleteUserEndpoints

instance Core.ToHeaders DeleteUserEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteUserEndpoints where
  toPath DeleteUserEndpoints' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/users/",
        Core.toBS userId
      ]

instance Core.ToQuery DeleteUserEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteUserEndpointsResponse' smart constructor.
data DeleteUserEndpointsResponse = DeleteUserEndpointsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    endpointsResponse :: EndpointsResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUserEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpointsResponse', 'deleteUserEndpointsResponse_endpointsResponse' - Undocumented member.
newDeleteUserEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'endpointsResponse'
  EndpointsResponse ->
  DeleteUserEndpointsResponse
newDeleteUserEndpointsResponse
  pHttpStatus_
  pEndpointsResponse_ =
    DeleteUserEndpointsResponse'
      { httpStatus =
          pHttpStatus_,
        endpointsResponse = pEndpointsResponse_
      }

-- | The response's http status code.
deleteUserEndpointsResponse_httpStatus :: Lens.Lens' DeleteUserEndpointsResponse Core.Int
deleteUserEndpointsResponse_httpStatus = Lens.lens (\DeleteUserEndpointsResponse' {httpStatus} -> httpStatus) (\s@DeleteUserEndpointsResponse' {} a -> s {httpStatus = a} :: DeleteUserEndpointsResponse)

-- | Undocumented member.
deleteUserEndpointsResponse_endpointsResponse :: Lens.Lens' DeleteUserEndpointsResponse EndpointsResponse
deleteUserEndpointsResponse_endpointsResponse = Lens.lens (\DeleteUserEndpointsResponse' {endpointsResponse} -> endpointsResponse) (\s@DeleteUserEndpointsResponse' {} a -> s {endpointsResponse = a} :: DeleteUserEndpointsResponse)

instance Core.NFData DeleteUserEndpointsResponse
