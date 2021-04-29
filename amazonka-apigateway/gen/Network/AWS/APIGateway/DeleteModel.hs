{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.DeleteModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model.
module Network.AWS.APIGateway.DeleteModel
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

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an existing model in an existing RestApi resource.
--
-- /See:/ 'newDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the model to delete.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteModel_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'modelName', 'deleteModel_modelName' - [Required] The name of the model to delete.
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

-- | [Required] The string identifier of the associated RestApi.
deleteModel_restApiId :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_restApiId = Lens.lens (\DeleteModel' {restApiId} -> restApiId) (\s@DeleteModel' {} a -> s {restApiId = a} :: DeleteModel)

-- | [Required] The name of the model to delete.
deleteModel_modelName :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_modelName = Lens.lens (\DeleteModel' {modelName} -> modelName) (\s@DeleteModel' {} a -> s {modelName = a} :: DeleteModel)

instance Prelude.AWSRequest DeleteModel where
  type Rs DeleteModel = DeleteModelResponse
  request = Request.delete defaultService
  response = Response.receiveNull DeleteModelResponse'

instance Prelude.Hashable DeleteModel

instance Prelude.NFData DeleteModel

instance Prelude.ToHeaders DeleteModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteModel where
  toPath DeleteModel' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/models/",
        Prelude.toBS modelName
      ]

instance Prelude.ToQuery DeleteModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelResponse ::
  DeleteModelResponse
newDeleteModelResponse = DeleteModelResponse'

instance Prelude.NFData DeleteModelResponse
