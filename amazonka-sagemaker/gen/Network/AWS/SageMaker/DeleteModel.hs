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
-- Module      : Network.AWS.SageMaker.DeleteModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model. The @DeleteModel@ API deletes only the model entry that
-- was created in Amazon SageMaker when you called the CreateModel API. It
-- does not delete model artifacts, inference code, or the IAM role that
-- you specified when creating the model.
module Network.AWS.SageMaker.DeleteModel
  ( -- * Creating a Request
    DeleteModel (..),
    newDeleteModel,

    -- * Request Lenses
    deleteModel_modelName,

    -- * Destructuring the Response
    DeleteModelResponse (..),
    newDeleteModelResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | The name of the model to delete.
    modelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'deleteModel_modelName' - The name of the model to delete.
newDeleteModel ::
  -- | 'modelName'
  Core.Text ->
  DeleteModel
newDeleteModel pModelName_ =
  DeleteModel' {modelName = pModelName_}

-- | The name of the model to delete.
deleteModel_modelName :: Lens.Lens' DeleteModel Core.Text
deleteModel_modelName = Lens.lens (\DeleteModel' {modelName} -> modelName) (\s@DeleteModel' {} a -> s {modelName = a} :: DeleteModel)

instance Core.AWSRequest DeleteModel where
  type AWSResponse DeleteModel = DeleteModelResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteModelResponse'

instance Core.Hashable DeleteModel

instance Core.NFData DeleteModel

instance Core.ToHeaders DeleteModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteModel" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteModel where
  toJSON DeleteModel' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ModelName" Core..= modelName)]
      )

instance Core.ToPath DeleteModel where
  toPath = Core.const "/"

instance Core.ToQuery DeleteModel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteModelResponse' smart constructor.
data DeleteModelResponse = DeleteModelResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelResponse ::
  DeleteModelResponse
newDeleteModelResponse = DeleteModelResponse'

instance Core.NFData DeleteModelResponse
