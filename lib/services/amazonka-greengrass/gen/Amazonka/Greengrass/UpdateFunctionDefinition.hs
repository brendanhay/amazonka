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
-- Module      : Amazonka.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Amazonka.Greengrass.UpdateFunctionDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Lambda function definition.
    functionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateFunctionDefinition
newUpdateFunctionDefinition pFunctionDefinitionId_ =
  UpdateFunctionDefinition'
    { name = Prelude.Nothing,
      functionDefinitionId = pFunctionDefinitionId_
    }

-- | The name of the definition.
updateFunctionDefinition_name :: Lens.Lens' UpdateFunctionDefinition (Prelude.Maybe Prelude.Text)
updateFunctionDefinition_name = Lens.lens (\UpdateFunctionDefinition' {name} -> name) (\s@UpdateFunctionDefinition' {} a -> s {name = a} :: UpdateFunctionDefinition)

-- | The ID of the Lambda function definition.
updateFunctionDefinition_functionDefinitionId :: Lens.Lens' UpdateFunctionDefinition Prelude.Text
updateFunctionDefinition_functionDefinitionId = Lens.lens (\UpdateFunctionDefinition' {functionDefinitionId} -> functionDefinitionId) (\s@UpdateFunctionDefinition' {} a -> s {functionDefinitionId = a} :: UpdateFunctionDefinition)

instance Core.AWSRequest UpdateFunctionDefinition where
  type
    AWSResponse UpdateFunctionDefinition =
      UpdateFunctionDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateFunctionDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateFunctionDefinition where
  hashWithSalt _salt UpdateFunctionDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` functionDefinitionId

instance Prelude.NFData UpdateFunctionDefinition where
  rnf UpdateFunctionDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf functionDefinitionId

instance Data.ToHeaders UpdateFunctionDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFunctionDefinition where
  toJSON UpdateFunctionDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateFunctionDefinition where
  toPath UpdateFunctionDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/functions/",
        Data.toBS functionDefinitionId
      ]

instance Data.ToQuery UpdateFunctionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFunctionDefinitionResponse' smart constructor.
data UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateFunctionDefinitionResponse
newUpdateFunctionDefinitionResponse pHttpStatus_ =
  UpdateFunctionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateFunctionDefinitionResponse_httpStatus :: Lens.Lens' UpdateFunctionDefinitionResponse Prelude.Int
updateFunctionDefinitionResponse_httpStatus = Lens.lens (\UpdateFunctionDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateFunctionDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateFunctionDefinitionResponse)

instance
  Prelude.NFData
    UpdateFunctionDefinitionResponse
  where
  rnf UpdateFunctionDefinitionResponse' {..} =
    Prelude.rnf httpStatus
