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
-- Module      : Amazonka.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Amazonka.Greengrass.UpdateConnectorDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connector definition.
    connectorDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateConnectorDefinition
newUpdateConnectorDefinition pConnectorDefinitionId_ =
  UpdateConnectorDefinition'
    { name = Prelude.Nothing,
      connectorDefinitionId = pConnectorDefinitionId_
    }

-- | The name of the definition.
updateConnectorDefinition_name :: Lens.Lens' UpdateConnectorDefinition (Prelude.Maybe Prelude.Text)
updateConnectorDefinition_name = Lens.lens (\UpdateConnectorDefinition' {name} -> name) (\s@UpdateConnectorDefinition' {} a -> s {name = a} :: UpdateConnectorDefinition)

-- | The ID of the connector definition.
updateConnectorDefinition_connectorDefinitionId :: Lens.Lens' UpdateConnectorDefinition Prelude.Text
updateConnectorDefinition_connectorDefinitionId = Lens.lens (\UpdateConnectorDefinition' {connectorDefinitionId} -> connectorDefinitionId) (\s@UpdateConnectorDefinition' {} a -> s {connectorDefinitionId = a} :: UpdateConnectorDefinition)

instance Core.AWSRequest UpdateConnectorDefinition where
  type
    AWSResponse UpdateConnectorDefinition =
      UpdateConnectorDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateConnectorDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateConnectorDefinition where
  hashWithSalt _salt UpdateConnectorDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` connectorDefinitionId

instance Prelude.NFData UpdateConnectorDefinition where
  rnf UpdateConnectorDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf connectorDefinitionId

instance Data.ToHeaders UpdateConnectorDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateConnectorDefinition where
  toJSON UpdateConnectorDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateConnectorDefinition where
  toPath UpdateConnectorDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/connectors/",
        Data.toBS connectorDefinitionId
      ]

instance Data.ToQuery UpdateConnectorDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConnectorDefinitionResponse' smart constructor.
data UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateConnectorDefinitionResponse
newUpdateConnectorDefinitionResponse pHttpStatus_ =
  UpdateConnectorDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateConnectorDefinitionResponse_httpStatus :: Lens.Lens' UpdateConnectorDefinitionResponse Prelude.Int
updateConnectorDefinitionResponse_httpStatus = Lens.lens (\UpdateConnectorDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateConnectorDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateConnectorDefinitionResponse)

instance
  Prelude.NFData
    UpdateConnectorDefinitionResponse
  where
  rnf UpdateConnectorDefinitionResponse' {..} =
    Prelude.rnf httpStatus
