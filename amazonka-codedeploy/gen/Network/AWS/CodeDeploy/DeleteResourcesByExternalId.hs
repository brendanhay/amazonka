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
-- Module      : Network.AWS.CodeDeploy.DeleteResourcesByExternalId
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes resources linked to an external ID.
module Network.AWS.CodeDeploy.DeleteResourcesByExternalId
  ( -- * Creating a Request
    DeleteResourcesByExternalId (..),
    newDeleteResourcesByExternalId,

    -- * Request Lenses
    deleteResourcesByExternalId_externalId,

    -- * Destructuring the Response
    DeleteResourcesByExternalIdResponse (..),
    newDeleteResourcesByExternalIdResponse,

    -- * Response Lenses
    deleteResourcesByExternalIdResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourcesByExternalId' smart constructor.
data DeleteResourcesByExternalId = DeleteResourcesByExternalId'
  { -- | The unique ID of an external resource (for example, a CloudFormation
    -- stack ID) that is linked to one or more CodeDeploy resources.
    externalId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcesByExternalId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalId', 'deleteResourcesByExternalId_externalId' - The unique ID of an external resource (for example, a CloudFormation
-- stack ID) that is linked to one or more CodeDeploy resources.
newDeleteResourcesByExternalId ::
  DeleteResourcesByExternalId
newDeleteResourcesByExternalId =
  DeleteResourcesByExternalId'
    { externalId =
        Core.Nothing
    }

-- | The unique ID of an external resource (for example, a CloudFormation
-- stack ID) that is linked to one or more CodeDeploy resources.
deleteResourcesByExternalId_externalId :: Lens.Lens' DeleteResourcesByExternalId (Core.Maybe Core.Text)
deleteResourcesByExternalId_externalId = Lens.lens (\DeleteResourcesByExternalId' {externalId} -> externalId) (\s@DeleteResourcesByExternalId' {} a -> s {externalId = a} :: DeleteResourcesByExternalId)

instance Core.AWSRequest DeleteResourcesByExternalId where
  type
    AWSResponse DeleteResourcesByExternalId =
      DeleteResourcesByExternalIdResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourcesByExternalIdResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteResourcesByExternalId

instance Core.NFData DeleteResourcesByExternalId

instance Core.ToHeaders DeleteResourcesByExternalId where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeleteResourcesByExternalId" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteResourcesByExternalId where
  toJSON DeleteResourcesByExternalId' {..} =
    Core.object
      ( Core.catMaybes
          [("externalId" Core..=) Core.<$> externalId]
      )

instance Core.ToPath DeleteResourcesByExternalId where
  toPath = Core.const "/"

instance Core.ToQuery DeleteResourcesByExternalId where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourcesByExternalIdResponse' smart constructor.
data DeleteResourcesByExternalIdResponse = DeleteResourcesByExternalIdResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourcesByExternalIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourcesByExternalIdResponse_httpStatus' - The response's http status code.
newDeleteResourcesByExternalIdResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteResourcesByExternalIdResponse
newDeleteResourcesByExternalIdResponse pHttpStatus_ =
  DeleteResourcesByExternalIdResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourcesByExternalIdResponse_httpStatus :: Lens.Lens' DeleteResourcesByExternalIdResponse Core.Int
deleteResourcesByExternalIdResponse_httpStatus = Lens.lens (\DeleteResourcesByExternalIdResponse' {httpStatus} -> httpStatus) (\s@DeleteResourcesByExternalIdResponse' {} a -> s {httpStatus = a} :: DeleteResourcesByExternalIdResponse)

instance
  Core.NFData
    DeleteResourcesByExternalIdResponse
