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
-- Module      : Network.AWS.Greengrass.DeleteLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logger definition.
module Network.AWS.Greengrass.DeleteLoggerDefinition
  ( -- * Creating a Request
    DeleteLoggerDefinition (..),
    newDeleteLoggerDefinition,

    -- * Request Lenses
    deleteLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    DeleteLoggerDefinitionResponse (..),
    newDeleteLoggerDefinitionResponse,

    -- * Response Lenses
    deleteLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoggerDefinition' smart constructor.
data DeleteLoggerDefinition = DeleteLoggerDefinition'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggerDefinitionId', 'deleteLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newDeleteLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  DeleteLoggerDefinition
newDeleteLoggerDefinition pLoggerDefinitionId_ =
  DeleteLoggerDefinition'
    { loggerDefinitionId =
        pLoggerDefinitionId_
    }

-- | The ID of the logger definition.
deleteLoggerDefinition_loggerDefinitionId :: Lens.Lens' DeleteLoggerDefinition Core.Text
deleteLoggerDefinition_loggerDefinitionId = Lens.lens (\DeleteLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@DeleteLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: DeleteLoggerDefinition)

instance Core.AWSRequest DeleteLoggerDefinition where
  type
    AWSResponse DeleteLoggerDefinition =
      DeleteLoggerDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLoggerDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLoggerDefinition

instance Core.NFData DeleteLoggerDefinition

instance Core.ToHeaders DeleteLoggerDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteLoggerDefinition where
  toPath DeleteLoggerDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId
      ]

instance Core.ToQuery DeleteLoggerDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLoggerDefinitionResponse' smart constructor.
data DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoggerDefinitionResponse_httpStatus' - The response's http status code.
newDeleteLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLoggerDefinitionResponse
newDeleteLoggerDefinitionResponse pHttpStatus_ =
  DeleteLoggerDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoggerDefinitionResponse_httpStatus :: Lens.Lens' DeleteLoggerDefinitionResponse Core.Int
deleteLoggerDefinitionResponse_httpStatus = Lens.lens (\DeleteLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteLoggerDefinitionResponse)

instance Core.NFData DeleteLoggerDefinitionResponse
