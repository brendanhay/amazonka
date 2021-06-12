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
-- Module      : Network.AWS.Pinpoint.DeleteEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an endpoint from an application.
module Network.AWS.Pinpoint.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_applicationId,
    deleteEndpoint_endpointId,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,

    -- * Response Lenses
    deleteEndpointResponse_httpStatus,
    deleteEndpointResponse_endpointResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The unique identifier for the application. This identifier is displayed
    -- as the __Project ID__ on the Amazon Pinpoint console.
    applicationId :: Core.Text,
    -- | The unique identifier for the endpoint.
    endpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteEndpoint_applicationId' - The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
--
-- 'endpointId', 'deleteEndpoint_endpointId' - The unique identifier for the endpoint.
newDeleteEndpoint ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'endpointId'
  Core.Text ->
  DeleteEndpoint
newDeleteEndpoint pApplicationId_ pEndpointId_ =
  DeleteEndpoint'
    { applicationId = pApplicationId_,
      endpointId = pEndpointId_
    }

-- | The unique identifier for the application. This identifier is displayed
-- as the __Project ID__ on the Amazon Pinpoint console.
deleteEndpoint_applicationId :: Lens.Lens' DeleteEndpoint Core.Text
deleteEndpoint_applicationId = Lens.lens (\DeleteEndpoint' {applicationId} -> applicationId) (\s@DeleteEndpoint' {} a -> s {applicationId = a} :: DeleteEndpoint)

-- | The unique identifier for the endpoint.
deleteEndpoint_endpointId :: Lens.Lens' DeleteEndpoint Core.Text
deleteEndpoint_endpointId = Lens.lens (\DeleteEndpoint' {endpointId} -> endpointId) (\s@DeleteEndpoint' {} a -> s {endpointId = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (Core.eitherParseJSON x)
      )

instance Core.Hashable DeleteEndpoint

instance Core.NFData DeleteEndpoint

instance Core.ToHeaders DeleteEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteEndpoint where
  toPath DeleteEndpoint' {..} =
    Core.mconcat
      [ "/v1/apps/",
        Core.toBS applicationId,
        "/endpoints/",
        Core.toBS endpointId
      ]

instance Core.ToQuery DeleteEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    endpointResponse :: EndpointResponse
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEndpointResponse_httpStatus' - The response's http status code.
--
-- 'endpointResponse', 'deleteEndpointResponse_endpointResponse' - Undocumented member.
newDeleteEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'endpointResponse'
  EndpointResponse ->
  DeleteEndpointResponse
newDeleteEndpointResponse
  pHttpStatus_
  pEndpointResponse_ =
    DeleteEndpointResponse'
      { httpStatus = pHttpStatus_,
        endpointResponse = pEndpointResponse_
      }

-- | The response's http status code.
deleteEndpointResponse_httpStatus :: Lens.Lens' DeleteEndpointResponse Core.Int
deleteEndpointResponse_httpStatus = Lens.lens (\DeleteEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointResponse' {} a -> s {httpStatus = a} :: DeleteEndpointResponse)

-- | Undocumented member.
deleteEndpointResponse_endpointResponse :: Lens.Lens' DeleteEndpointResponse EndpointResponse
deleteEndpointResponse_endpointResponse = Lens.lens (\DeleteEndpointResponse' {endpointResponse} -> endpointResponse) (\s@DeleteEndpointResponse' {} a -> s {endpointResponse = a} :: DeleteEndpointResponse)

instance Core.NFData DeleteEndpointResponse
