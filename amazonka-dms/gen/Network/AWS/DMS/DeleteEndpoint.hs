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
-- Module      : Network.AWS.DMS.DeleteEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified endpoint.
--
-- All tasks associated with the endpoint must be deleted before you can
-- delete the endpoint.
module Network.AWS.DMS.DeleteEndpoint
  ( -- * Creating a Request
    DeleteEndpoint (..),
    newDeleteEndpoint,

    -- * Request Lenses
    deleteEndpoint_endpointArn,

    -- * Destructuring the Response
    DeleteEndpointResponse (..),
    newDeleteEndpointResponse,

    -- * Response Lenses
    deleteEndpointResponse_endpoint,
    deleteEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDeleteEndpoint' smart constructor.
data DeleteEndpoint = DeleteEndpoint'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the
    -- endpoint.
    endpointArn :: Core.Text
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
-- 'endpointArn', 'deleteEndpoint_endpointArn' - The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
newDeleteEndpoint ::
  -- | 'endpointArn'
  Core.Text ->
  DeleteEndpoint
newDeleteEndpoint pEndpointArn_ =
  DeleteEndpoint' {endpointArn = pEndpointArn_}

-- | The Amazon Resource Name (ARN) string that uniquely identifies the
-- endpoint.
deleteEndpoint_endpointArn :: Lens.Lens' DeleteEndpoint Core.Text
deleteEndpoint_endpointArn = Lens.lens (\DeleteEndpoint' {endpointArn} -> endpointArn) (\s@DeleteEndpoint' {} a -> s {endpointArn = a} :: DeleteEndpoint)

instance Core.AWSRequest DeleteEndpoint where
  type
    AWSResponse DeleteEndpoint =
      DeleteEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEndpointResponse'
            Core.<$> (x Core..?> "Endpoint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteEndpoint

instance Core.NFData DeleteEndpoint

instance Core.ToHeaders DeleteEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DeleteEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteEndpoint where
  toJSON DeleteEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EndpointArn" Core..= endpointArn)]
      )

instance Core.ToPath DeleteEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DeleteEndpoint where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDeleteEndpointResponse' smart constructor.
data DeleteEndpointResponse = DeleteEndpointResponse'
  { -- | The endpoint that was deleted.
    endpoint :: Core.Maybe Endpoint,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'deleteEndpointResponse_endpoint' - The endpoint that was deleted.
--
-- 'httpStatus', 'deleteEndpointResponse_httpStatus' - The response's http status code.
newDeleteEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteEndpointResponse
newDeleteEndpointResponse pHttpStatus_ =
  DeleteEndpointResponse'
    { endpoint = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint that was deleted.
deleteEndpointResponse_endpoint :: Lens.Lens' DeleteEndpointResponse (Core.Maybe Endpoint)
deleteEndpointResponse_endpoint = Lens.lens (\DeleteEndpointResponse' {endpoint} -> endpoint) (\s@DeleteEndpointResponse' {} a -> s {endpoint = a} :: DeleteEndpointResponse)

-- | The response's http status code.
deleteEndpointResponse_httpStatus :: Lens.Lens' DeleteEndpointResponse Core.Int
deleteEndpointResponse_httpStatus = Lens.lens (\DeleteEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteEndpointResponse' {} a -> s {httpStatus = a} :: DeleteEndpointResponse)

instance Core.NFData DeleteEndpointResponse
