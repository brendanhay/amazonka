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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteModel' smart constructor.
data DeleteModel = DeleteModel'
  { -- | The name of the model to delete.
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
-- 'modelName', 'deleteModel_modelName' - The name of the model to delete.
newDeleteModel ::
  -- | 'modelName'
  Prelude.Text ->
  DeleteModel
newDeleteModel pModelName_ =
  DeleteModel' {modelName = pModelName_}

-- | The name of the model to delete.
deleteModel_modelName :: Lens.Lens' DeleteModel Prelude.Text
deleteModel_modelName = Lens.lens (\DeleteModel' {modelName} -> modelName) (\s@DeleteModel' {} a -> s {modelName = a} :: DeleteModel)

instance Prelude.AWSRequest DeleteModel where
  type Rs DeleteModel = DeleteModelResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteModelResponse'

instance Prelude.Hashable DeleteModel

instance Prelude.NFData DeleteModel

instance Prelude.ToHeaders DeleteModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteModel" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteModel where
  toJSON DeleteModel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Prelude..= modelName)]
      )

instance Prelude.ToPath DeleteModel where
  toPath = Prelude.const "/"

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
