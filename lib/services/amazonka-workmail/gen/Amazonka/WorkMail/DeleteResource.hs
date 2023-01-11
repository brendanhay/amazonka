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
-- Module      : Amazonka.WorkMail.DeleteResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified resource.
module Amazonka.WorkMail.DeleteResource
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newDeleteResource' smart constructor.
data DeleteResource = DeleteResource'
  { -- | The identifier associated with the organization from which the resource
    -- is deleted.
    organizationId :: Prelude.Text,
    -- | The identifier of the resource to be deleted.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  DeleteResource
newDeleteResource pOrganizationId_ pResourceId_ =
  DeleteResource'
    { organizationId = pOrganizationId_,
      resourceId = pResourceId_
    }

-- | The identifier associated with the organization from which the resource
-- is deleted.
deleteResource_organizationId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_organizationId = Lens.lens (\DeleteResource' {organizationId} -> organizationId) (\s@DeleteResource' {} a -> s {organizationId = a} :: DeleteResource)

-- | The identifier of the resource to be deleted.
deleteResource_resourceId :: Lens.Lens' DeleteResource Prelude.Text
deleteResource_resourceId = Lens.lens (\DeleteResource' {resourceId} -> resourceId) (\s@DeleteResource' {} a -> s {resourceId = a} :: DeleteResource)

instance Core.AWSRequest DeleteResource where
  type
    AWSResponse DeleteResource =
      DeleteResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteResource where
  hashWithSalt _salt DeleteResource' {..} =
    _salt `Prelude.hashWithSalt` organizationId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DeleteResource where
  rnf DeleteResource' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf resourceId

instance Data.ToHeaders DeleteResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.DeleteResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteResource where
  toJSON DeleteResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Data..= organizationId),
            Prelude.Just ("ResourceId" Data..= resourceId)
          ]
      )

instance Data.ToPath DeleteResource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResourceResponse' smart constructor.
data DeleteResourceResponse = DeleteResourceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteResourceResponse
newDeleteResourceResponse pHttpStatus_ =
  DeleteResourceResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteResourceResponse_httpStatus :: Lens.Lens' DeleteResourceResponse Prelude.Int
deleteResourceResponse_httpStatus = Lens.lens (\DeleteResourceResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceResponse' {} a -> s {httpStatus = a} :: DeleteResourceResponse)

instance Prelude.NFData DeleteResourceResponse where
  rnf DeleteResourceResponse' {..} =
    Prelude.rnf httpStatus
