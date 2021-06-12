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
-- Module      : Network.AWS.Greengrass.DeleteResourceDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource definition.
module Network.AWS.Greengrass.DeleteResourceDefinition
  ( -- * Creating a Request
    DeleteResourceDefinition (..),
    newDeleteResourceDefinition,

    -- * Request Lenses
    deleteResourceDefinition_resourceDefinitionId,

    -- * Destructuring the Response
    DeleteResourceDefinitionResponse (..),
    newDeleteResourceDefinitionResponse,

    -- * Response Lenses
    deleteResourceDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteResourceDefinition' smart constructor.
data DeleteResourceDefinition = DeleteResourceDefinition'
  { -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDefinitionId', 'deleteResourceDefinition_resourceDefinitionId' - The ID of the resource definition.
newDeleteResourceDefinition ::
  -- | 'resourceDefinitionId'
  Core.Text ->
  DeleteResourceDefinition
newDeleteResourceDefinition pResourceDefinitionId_ =
  DeleteResourceDefinition'
    { resourceDefinitionId =
        pResourceDefinitionId_
    }

-- | The ID of the resource definition.
deleteResourceDefinition_resourceDefinitionId :: Lens.Lens' DeleteResourceDefinition Core.Text
deleteResourceDefinition_resourceDefinitionId = Lens.lens (\DeleteResourceDefinition' {resourceDefinitionId} -> resourceDefinitionId) (\s@DeleteResourceDefinition' {} a -> s {resourceDefinitionId = a} :: DeleteResourceDefinition)

instance Core.AWSRequest DeleteResourceDefinition where
  type
    AWSResponse DeleteResourceDefinition =
      DeleteResourceDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteResourceDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteResourceDefinition

instance Core.NFData DeleteResourceDefinition

instance Core.ToHeaders DeleteResourceDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteResourceDefinition where
  toPath DeleteResourceDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/resources/",
        Core.toBS resourceDefinitionId
      ]

instance Core.ToQuery DeleteResourceDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteResourceDefinitionResponse' smart constructor.
data DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteResourceDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteResourceDefinitionResponse_httpStatus' - The response's http status code.
newDeleteResourceDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteResourceDefinitionResponse
newDeleteResourceDefinitionResponse pHttpStatus_ =
  DeleteResourceDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteResourceDefinitionResponse_httpStatus :: Lens.Lens' DeleteResourceDefinitionResponse Core.Int
deleteResourceDefinitionResponse_httpStatus = Lens.lens (\DeleteResourceDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteResourceDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteResourceDefinitionResponse)

instance Core.NFData DeleteResourceDefinitionResponse
