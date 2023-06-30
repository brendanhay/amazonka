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
-- Module      : Amazonka.APIGateway.DeleteModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model.
module Amazonka.APIGateway.DeleteModel
  ( -- * Creating a Request
    DeleteModel (..),
    newDeleteModel,

    -- * Request Lenses
    deleteModel_restApiId,
    deleteModel_modelName,

    -- * Destructuring the Response
    DeleteModelResponse (..),
    newDeleteModelResponse,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to delete an existing model in an existing RestApi resource.
--
-- /See:/ 'newDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The name of the model to delete.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteModel_restApiId' - The string identifier of the associated RestApi.
--
-- 'modelName', 'deleteModel_modelName' - The name of the model to delete.
newDeleteModel ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'modelName'
  Prelude.Text ->
  DeleteModel
newDeleteModel pRestApiId_ pModelName_ =
  DeleteModel'
    { restApiId = pRestApiId_,
      modelName = pModelName_
    }

-- | The string identifier of the associated RestApi.
deleteModel_restApiId :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_restApiId = Lens.lens (\DeleteModel' {restApiId} -> restApiId) (\s@DeleteModel' {} a -> s {restApiId = a} :: DeleteModel)

-- | The name of the model to delete.
deleteModel_modelName :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_modelName = Lens.lens (\DeleteModel' {modelName} -> modelName) (\s@DeleteModel' {} a -> s {modelName = a} :: DeleteModel)

instance Core.AWSRequest DeleteModel where
  type AWSResponse DeleteModel = DeleteModelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response = Response.receiveNull DeleteModelResponse'

instance Prelude.Hashable DeleteModel where
  hashWithSalt _salt DeleteModel' {..} =
    _salt
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` modelName

instance Prelude.NFData DeleteModel where
  rnf DeleteModel' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf modelName

instance Data.ToHeaders DeleteModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToPath DeleteModel where
  toPath DeleteModel' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/models/",
        Data.toBS modelName
      ]

instance Data.ToQuery DeleteModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelResponse ::
  DeleteModelResponse
newDeleteModelResponse = DeleteModelResponse'

instance Prelude.NFData DeleteModelResponse where
  rnf _ = ()
