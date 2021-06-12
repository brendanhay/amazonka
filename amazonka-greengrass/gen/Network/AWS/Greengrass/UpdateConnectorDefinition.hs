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
-- Module      : Network.AWS.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Network.AWS.Greengrass.UpdateConnectorDefinition
  ( -- * Creating a Request
    UpdateConnectorDefinition (..),
    newUpdateConnectorDefinition,

    -- * Request Lenses
    updateConnectorDefinition_name,
    updateConnectorDefinition_connectorDefinitionId,

    -- * Destructuring the Response
    UpdateConnectorDefinitionResponse (..),
    newUpdateConnectorDefinitionResponse,

    -- * Response Lenses
    updateConnectorDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the connector definition.
    connectorDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectorDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateConnectorDefinition_name' - The name of the definition.
--
-- 'connectorDefinitionId', 'updateConnectorDefinition_connectorDefinitionId' - The ID of the connector definition.
newUpdateConnectorDefinition ::
  -- | 'connectorDefinitionId'
  Core.Text ->
  UpdateConnectorDefinition
newUpdateConnectorDefinition pConnectorDefinitionId_ =
  UpdateConnectorDefinition'
    { name = Core.Nothing,
      connectorDefinitionId = pConnectorDefinitionId_
    }

-- | The name of the definition.
updateConnectorDefinition_name :: Lens.Lens' UpdateConnectorDefinition (Core.Maybe Core.Text)
updateConnectorDefinition_name = Lens.lens (\UpdateConnectorDefinition' {name} -> name) (\s@UpdateConnectorDefinition' {} a -> s {name = a} :: UpdateConnectorDefinition)

-- | The ID of the connector definition.
updateConnectorDefinition_connectorDefinitionId :: Lens.Lens' UpdateConnectorDefinition Core.Text
updateConnectorDefinition_connectorDefinitionId = Lens.lens (\UpdateConnectorDefinition' {connectorDefinitionId} -> connectorDefinitionId) (\s@UpdateConnectorDefinition' {} a -> s {connectorDefinitionId = a} :: UpdateConnectorDefinition)

instance Core.AWSRequest UpdateConnectorDefinition where
  type
    AWSResponse UpdateConnectorDefinition =
      UpdateConnectorDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectorDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateConnectorDefinition

instance Core.NFData UpdateConnectorDefinition

instance Core.ToHeaders UpdateConnectorDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateConnectorDefinition where
  toJSON UpdateConnectorDefinition' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateConnectorDefinition where
  toPath UpdateConnectorDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/connectors/",
        Core.toBS connectorDefinitionId
      ]

instance Core.ToQuery UpdateConnectorDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateConnectorDefinitionResponse' smart constructor.
data UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectorDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConnectorDefinitionResponse_httpStatus' - The response's http status code.
newUpdateConnectorDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateConnectorDefinitionResponse
newUpdateConnectorDefinitionResponse pHttpStatus_ =
  UpdateConnectorDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConnectorDefinitionResponse_httpStatus :: Lens.Lens' UpdateConnectorDefinitionResponse Core.Int
updateConnectorDefinitionResponse_httpStatus = Lens.lens (\UpdateConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateConnectorDefinitionResponse)

instance
  Core.NFData
    UpdateConnectorDefinitionResponse
