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
-- Module      : Amazonka.Greengrass.UpdateCoreDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a core definition.
module Amazonka.Greengrass.UpdateCoreDefinition
  ( -- * Creating a Request
    UpdateCoreDefinition (..),
    newUpdateCoreDefinition,

    -- * Request Lenses
    updateCoreDefinition_name,
    updateCoreDefinition_coreDefinitionId,

    -- * Destructuring the Response
    UpdateCoreDefinitionResponse (..),
    newUpdateCoreDefinitionResponse,

    -- * Response Lenses
    updateCoreDefinitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCoreDefinition' smart constructor.
data UpdateCoreDefinition = UpdateCoreDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the core definition.
    coreDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCoreDefinition_name' - The name of the definition.
--
-- 'coreDefinitionId', 'updateCoreDefinition_coreDefinitionId' - The ID of the core definition.
newUpdateCoreDefinition ::
  -- | 'coreDefinitionId'
  Prelude.Text ->
  UpdateCoreDefinition
newUpdateCoreDefinition pCoreDefinitionId_ =
  UpdateCoreDefinition'
    { name = Prelude.Nothing,
      coreDefinitionId = pCoreDefinitionId_
    }

-- | The name of the definition.
updateCoreDefinition_name :: Lens.Lens' UpdateCoreDefinition (Prelude.Maybe Prelude.Text)
updateCoreDefinition_name = Lens.lens (\UpdateCoreDefinition' {name} -> name) (\s@UpdateCoreDefinition' {} a -> s {name = a} :: UpdateCoreDefinition)

-- | The ID of the core definition.
updateCoreDefinition_coreDefinitionId :: Lens.Lens' UpdateCoreDefinition Prelude.Text
updateCoreDefinition_coreDefinitionId = Lens.lens (\UpdateCoreDefinition' {coreDefinitionId} -> coreDefinitionId) (\s@UpdateCoreDefinition' {} a -> s {coreDefinitionId = a} :: UpdateCoreDefinition)

instance Core.AWSRequest UpdateCoreDefinition where
  type
    AWSResponse UpdateCoreDefinition =
      UpdateCoreDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateCoreDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCoreDefinition where
  hashWithSalt _salt UpdateCoreDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` coreDefinitionId

instance Prelude.NFData UpdateCoreDefinition where
  rnf UpdateCoreDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf coreDefinitionId

instance Data.ToHeaders UpdateCoreDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCoreDefinition where
  toJSON UpdateCoreDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath UpdateCoreDefinition where
  toPath UpdateCoreDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/cores/",
        Data.toBS coreDefinitionId
      ]

instance Data.ToQuery UpdateCoreDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCoreDefinitionResponse' smart constructor.
data UpdateCoreDefinitionResponse = UpdateCoreDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCoreDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCoreDefinitionResponse_httpStatus' - The response's http status code.
newUpdateCoreDefinitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCoreDefinitionResponse
newUpdateCoreDefinitionResponse pHttpStatus_ =
  UpdateCoreDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateCoreDefinitionResponse_httpStatus :: Lens.Lens' UpdateCoreDefinitionResponse Prelude.Int
updateCoreDefinitionResponse_httpStatus = Lens.lens (\UpdateCoreDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateCoreDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateCoreDefinitionResponse)

instance Prelude.NFData UpdateCoreDefinitionResponse where
  rnf UpdateCoreDefinitionResponse' {..} =
    Prelude.rnf httpStatus
