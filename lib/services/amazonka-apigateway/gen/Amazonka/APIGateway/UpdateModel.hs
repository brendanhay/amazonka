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
-- Module      : Amazonka.APIGateway.UpdateModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about a model.
module Amazonka.APIGateway.UpdateModel
  ( -- * Creating a Request
    UpdateModel (..),
    newUpdateModel,

    -- * Request Lenses
    updateModel_patchOperations,
    updateModel_restApiId,
    updateModel_modelName,

    -- * Destructuring the Response
    Model (..),
    newModel,

    -- * Response Lenses
    model_name,
    model_description,
    model_id,
    model_schema,
    model_contentType,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to update an existing model in an existing RestApi resource.
--
-- /See:/ 'newUpdateModel' smart constructor.
data UpdateModel = UpdateModel'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the model to update.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateModel_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateModel_restApiId' - The string identifier of the associated RestApi.
--
-- 'modelName', 'updateModel_modelName' - The name of the model to update.
newUpdateModel ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  UpdateModel
newUpdateModel pRestApiId_ pModelName_ =
  UpdateModel'
    { patchOperations = Prelude.Nothing,
      restApiId = pRestApiId_,
      modelName = pModelName_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateModel_patchOperations :: Lens.Lens' UpdateModel (Prelude.Maybe [PatchOperation])
updateModel_patchOperations = Lens.lens (\UpdateModel' {patchOperations} -> patchOperations) (\s@UpdateModel' {} a -> s {patchOperations = a} :: UpdateModel) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateModel_restApiId :: Lens.Lens' UpdateModel Prelude.Text
updateModel_restApiId = Lens.lens (\UpdateModel' {restApiId} -> restApiId) (\s@UpdateModel' {} a -> s {restApiId = a} :: UpdateModel)

-- | The name of the model to update.
updateModel_modelName :: Lens.Lens' UpdateModel Prelude.Text
updateModel_modelName = Lens.lens (\UpdateModel' {modelName} -> modelName) (\s@UpdateModel' {} a -> s {modelName = a} :: UpdateModel)

instance Core.AWSRequest UpdateModel where
  type AWSResponse UpdateModel = Model
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateModel where
  hashWithSalt _salt UpdateModel' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData UpdateModel where
  rnf UpdateModel' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf modelName

instance Core.ToHeaders UpdateModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateModel where
  toJSON UpdateModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateModel where
  toPath UpdateModel' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/models/",
        Core.toBS modelName
      ]

instance Core.ToQuery UpdateModel where
  toQuery = Prelude.const Prelude.mempty
