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
-- Module      : Network.AWS.MediaPackage.DeleteOriginEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing OriginEndpoint.
module Network.AWS.MediaPackage.DeleteOriginEndpoint
  ( -- * Creating a Request
    DeleteOriginEndpoint (..),
    newDeleteOriginEndpoint,

    -- * Request Lenses
    deleteOriginEndpoint_id,

    -- * Destructuring the Response
    DeleteOriginEndpointResponse (..),
    newDeleteOriginEndpointResponse,

    -- * Response Lenses
    deleteOriginEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOriginEndpoint' smart constructor.
data DeleteOriginEndpoint = DeleteOriginEndpoint'
  { -- | The ID of the OriginEndpoint to delete.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOriginEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteOriginEndpoint_id' - The ID of the OriginEndpoint to delete.
newDeleteOriginEndpoint ::
  -- | 'id'
  Core.Text ->
  DeleteOriginEndpoint
newDeleteOriginEndpoint pId_ =
  DeleteOriginEndpoint' {id = pId_}

-- | The ID of the OriginEndpoint to delete.
deleteOriginEndpoint_id :: Lens.Lens' DeleteOriginEndpoint Core.Text
deleteOriginEndpoint_id = Lens.lens (\DeleteOriginEndpoint' {id} -> id) (\s@DeleteOriginEndpoint' {} a -> s {id = a} :: DeleteOriginEndpoint)

instance Core.AWSRequest DeleteOriginEndpoint where
  type
    AWSResponse DeleteOriginEndpoint =
      DeleteOriginEndpointResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOriginEndpointResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteOriginEndpoint

instance Core.NFData DeleteOriginEndpoint

instance Core.ToHeaders DeleteOriginEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteOriginEndpoint where
  toPath DeleteOriginEndpoint' {..} =
    Core.mconcat ["/origin_endpoints/", Core.toBS id]

instance Core.ToQuery DeleteOriginEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteOriginEndpointResponse' smart constructor.
data DeleteOriginEndpointResponse = DeleteOriginEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteOriginEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOriginEndpointResponse_httpStatus' - The response's http status code.
newDeleteOriginEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteOriginEndpointResponse
newDeleteOriginEndpointResponse pHttpStatus_ =
  DeleteOriginEndpointResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteOriginEndpointResponse_httpStatus :: Lens.Lens' DeleteOriginEndpointResponse Core.Int
deleteOriginEndpointResponse_httpStatus = Lens.lens (\DeleteOriginEndpointResponse' {httpStatus} -> httpStatus) (\s@DeleteOriginEndpointResponse' {} a -> s {httpStatus = a} :: DeleteOriginEndpointResponse)

instance Core.NFData DeleteOriginEndpointResponse
