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
-- Module      : Network.AWS.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Network.AWS.Greengrass.UpdateLoggerDefinition
  ( -- * Creating a Request
    UpdateLoggerDefinition (..),
    newUpdateLoggerDefinition,

    -- * Request Lenses
    updateLoggerDefinition_name,
    updateLoggerDefinition_loggerDefinitionId,

    -- * Destructuring the Response
    UpdateLoggerDefinitionResponse (..),
    newUpdateLoggerDefinitionResponse,

    -- * Response Lenses
    updateLoggerDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLoggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateLoggerDefinition_name' - The name of the definition.
--
-- 'loggerDefinitionId', 'updateLoggerDefinition_loggerDefinitionId' - The ID of the logger definition.
newUpdateLoggerDefinition ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  UpdateLoggerDefinition
newUpdateLoggerDefinition pLoggerDefinitionId_ =
  UpdateLoggerDefinition'
    { name = Core.Nothing,
      loggerDefinitionId = pLoggerDefinitionId_
    }

-- | The name of the definition.
updateLoggerDefinition_name :: Lens.Lens' UpdateLoggerDefinition (Core.Maybe Core.Text)
updateLoggerDefinition_name = Lens.lens (\UpdateLoggerDefinition' {name} -> name) (\s@UpdateLoggerDefinition' {} a -> s {name = a} :: UpdateLoggerDefinition)

-- | The ID of the logger definition.
updateLoggerDefinition_loggerDefinitionId :: Lens.Lens' UpdateLoggerDefinition Core.Text
updateLoggerDefinition_loggerDefinitionId = Lens.lens (\UpdateLoggerDefinition' {loggerDefinitionId} -> loggerDefinitionId) (\s@UpdateLoggerDefinition' {} a -> s {loggerDefinitionId = a} :: UpdateLoggerDefinition)

instance Core.AWSRequest UpdateLoggerDefinition where
  type
    AWSResponse UpdateLoggerDefinition =
      UpdateLoggerDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLoggerDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateLoggerDefinition

instance Core.NFData UpdateLoggerDefinition

instance Core.ToHeaders UpdateLoggerDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateLoggerDefinition where
  toJSON UpdateLoggerDefinition' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateLoggerDefinition where
  toPath UpdateLoggerDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/loggers/",
        Core.toBS loggerDefinitionId
      ]

instance Core.ToQuery UpdateLoggerDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateLoggerDefinitionResponse' smart constructor.
data UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateLoggerDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLoggerDefinitionResponse_httpStatus' - The response's http status code.
newUpdateLoggerDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateLoggerDefinitionResponse
newUpdateLoggerDefinitionResponse pHttpStatus_ =
  UpdateLoggerDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLoggerDefinitionResponse_httpStatus :: Lens.Lens' UpdateLoggerDefinitionResponse Core.Int
updateLoggerDefinitionResponse_httpStatus = Lens.lens (\UpdateLoggerDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateLoggerDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateLoggerDefinitionResponse)

instance Core.NFData UpdateLoggerDefinitionResponse
