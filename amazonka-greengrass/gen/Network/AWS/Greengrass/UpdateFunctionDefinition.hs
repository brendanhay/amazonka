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
-- Module      : Network.AWS.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Network.AWS.Greengrass.UpdateFunctionDefinition
  ( -- * Creating a Request
    UpdateFunctionDefinition (..),
    newUpdateFunctionDefinition,

    -- * Request Lenses
    updateFunctionDefinition_name,
    updateFunctionDefinition_functionDefinitionId,

    -- * Destructuring the Response
    UpdateFunctionDefinitionResponse (..),
    newUpdateFunctionDefinitionResponse,

    -- * Response Lenses
    updateFunctionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFunctionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateFunctionDefinition_name' - The name of the definition.
--
-- 'functionDefinitionId', 'updateFunctionDefinition_functionDefinitionId' - The ID of the Lambda function definition.
newUpdateFunctionDefinition ::
  -- | 'functionDefinitionId'
  Core.Text ->
  UpdateFunctionDefinition
newUpdateFunctionDefinition pFunctionDefinitionId_ =
  UpdateFunctionDefinition'
    { name = Core.Nothing,
      functionDefinitionId = pFunctionDefinitionId_
    }

-- | The name of the definition.
updateFunctionDefinition_name :: Lens.Lens' UpdateFunctionDefinition (Core.Maybe Core.Text)
updateFunctionDefinition_name = Lens.lens (\UpdateFunctionDefinition' {name} -> name) (\s@UpdateFunctionDefinition' {} a -> s {name = a} :: UpdateFunctionDefinition)

-- | The ID of the Lambda function definition.
updateFunctionDefinition_functionDefinitionId :: Lens.Lens' UpdateFunctionDefinition Core.Text
updateFunctionDefinition_functionDefinitionId = Lens.lens (\UpdateFunctionDefinition' {functionDefinitionId} -> functionDefinitionId) (\s@UpdateFunctionDefinition' {} a -> s {functionDefinitionId = a} :: UpdateFunctionDefinition)

instance Core.AWSRequest UpdateFunctionDefinition where
  type
    AWSResponse UpdateFunctionDefinition =
      UpdateFunctionDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFunctionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateFunctionDefinition

instance Core.NFData UpdateFunctionDefinition

instance Core.ToHeaders UpdateFunctionDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateFunctionDefinition where
  toJSON UpdateFunctionDefinition' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateFunctionDefinition where
  toPath UpdateFunctionDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/functions/",
        Core.toBS functionDefinitionId
      ]

instance Core.ToQuery UpdateFunctionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateFunctionDefinitionResponse' smart constructor.
data UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateFunctionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateFunctionDefinitionResponse_httpStatus' - The response's http status code.
newUpdateFunctionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateFunctionDefinitionResponse
newUpdateFunctionDefinitionResponse pHttpStatus_ =
  UpdateFunctionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateFunctionDefinitionResponse_httpStatus :: Lens.Lens' UpdateFunctionDefinitionResponse Core.Int
updateFunctionDefinitionResponse_httpStatus = Lens.lens (\UpdateFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateFunctionDefinitionResponse)

instance Core.NFData UpdateFunctionDefinitionResponse
