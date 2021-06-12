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
-- Module      : Network.AWS.WorkMail.DeleteResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource.
module Network.AWS.WorkMail.DeleteResource
  ( -- * Creating a Request
    DeleteResource (..),
    newDeleteResource,

    -- * Request Lenses
    deleteResource_organizationId,
    deleteResource_resourceId,

    -- * Destructuring the Response
    DeleteResourceResponse (..),
    newDeleteResourceResponse,

    -- * Response Lenses
    deleteResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { -- | The identifier associated with the organization from which the resource
    -- is deleted.
    organizationId :: Core.Text,
    -- | The identifier of the resource to be deleted.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'deleteResource_organizationId' - The identifier associated with the organization from which the resource
-- is deleted.
--
-- 'resourceId', 'deleteResource_resourceId' - The identifier of the resource to be deleted.
newDeleteResource ::
  -- | 'organizationId'
  Core.Text ->
  -- | 'resourceId'
  Core.Text ->
  DeleteResource
newDeleteResource pOrganizationId_ pResourceId_ =
  DeleteResource'
    { organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The identifier associated with the organization from which the resource
-- is deleted.
deleteResource_organizationId :: Lens.Lens' DeleteResource Core.Text
deleteResource_organizationId = Lens.lens (\DeleteResource' {organizationId} -> organizationId) (\s@DeleteResource' {} a -> s {organizationId = a} :: DeleteResource)

-- | The identifier of the resource to be deleted.
deleteResource_resourceId :: Lens.Lens' DeleteResource Core.Text
deleteResource_resourceId = Lens.lens (\DeleteResource' {resourceId} -> resourceId) (\s@DeleteResource' {} a -> s {resourceId = a} :: DeleteResource)

instance Core.AWSRequest DeleteResource where
  type
    AWSResponse DeleteResource =
      DeleteResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteResource

instance Core.NFData DeleteResource

instance Core.ToHeaders DeleteResource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.DeleteResource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResource where
  toJSON DeleteResource' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath DeleteResource where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourceResponse_httpStatus' - The response's http status code.
newDeleteResourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteResourceResponse
newDeleteResourceResponse pHttpStatus_ =
  DeleteResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteResourceResponse_httpStatus :: Lens.Lens' DeleteResourceResponse Core.Int
deleteResourceResponse_httpStatus = Lens.lens (\DeleteResourceResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceResponse' {} a -> s {httpStatus = a} :: DeleteResourceResponse)

instance Core.NFData DeleteResourceResponse
