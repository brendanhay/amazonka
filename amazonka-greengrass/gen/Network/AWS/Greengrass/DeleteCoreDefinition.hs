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
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
  ( -- * Creating a Request
    DeleteCoreDefinition (..),
    newDeleteCoreDefinition,

    -- * Request Lenses
    deleteCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    DeleteCoreDefinitionResponse (..),
    newDeleteCoreDefinitionResponse,

    -- * Response Lenses
    deleteCoreDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCoreDefinition' smart constructor.
data DeleteCoreDefinition = DeleteCoreDefinition'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDefinitionId', 'deleteCoreDefinition_coreDefinitionId' - The ID of the core definition.
newDeleteCoreDefinition ::
  -- | 'coreDefinitionId'
  Core.Text ->
  DeleteCoreDefinition
newDeleteCoreDefinition pCoreDefinitionId_ =
  DeleteCoreDefinition'
    { coreDefinitionId =
        pCoreDefinitionId_
    }

-- | The ID of the core definition.
deleteCoreDefinition_coreDefinitionId :: Lens.Lens' DeleteCoreDefinition Core.Text
deleteCoreDefinition_coreDefinitionId = Lens.lens (\DeleteCoreDefinition' {coreDefinitionId} -> coreDefinitionId) (\s@DeleteCoreDefinition' {} a -> s {coreDefinitionId = a} :: DeleteCoreDefinition)

instance Core.AWSRequest DeleteCoreDefinition where
  type
    AWSResponse DeleteCoreDefinition =
      DeleteCoreDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCoreDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCoreDefinition

instance Core.NFData DeleteCoreDefinition

instance Core.ToHeaders DeleteCoreDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteCoreDefinition where
  toPath DeleteCoreDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/cores/",
        Core.toBS coreDefinitionId
      ]

instance Core.ToQuery DeleteCoreDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteCoreDefinitionResponse' smart constructor.
data DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCoreDefinitionResponse_httpStatus' - The response's http status code.
newDeleteCoreDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCoreDefinitionResponse
newDeleteCoreDefinitionResponse pHttpStatus_ =
  DeleteCoreDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCoreDefinitionResponse_httpStatus :: Lens.Lens' DeleteCoreDefinitionResponse Core.Int
deleteCoreDefinitionResponse_httpStatus = Lens.lens (\DeleteCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteCoreDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteCoreDefinitionResponse)

instance Core.NFData DeleteCoreDefinitionResponse
