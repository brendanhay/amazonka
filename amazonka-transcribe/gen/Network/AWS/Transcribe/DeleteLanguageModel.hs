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
-- Module      : Network.AWS.Transcribe.DeleteLanguageModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom language model using its name.
module Network.AWS.Transcribe.DeleteLanguageModel
  ( -- * Creating a Request
    DeleteLanguageModel (..),
    newDeleteLanguageModel,

    -- * Request Lenses
    deleteLanguageModel_modelName,

    -- * Destructuring the Response
    DeleteLanguageModelResponse (..),
    newDeleteLanguageModelResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteLanguageModel' smart constructor.
data DeleteLanguageModel = DeleteLanguageModel'
  { -- | The name of the model you\'re choosing to delete.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'deleteLanguageModel_modelName' - The name of the model you\'re choosing to delete.
newDeleteLanguageModel ::
  -- | 'modelName'
  Prelude.Text ->
  DeleteLanguageModel
newDeleteLanguageModel pModelName_ =
  DeleteLanguageModel' {modelName = pModelName_}

-- | The name of the model you\'re choosing to delete.
deleteLanguageModel_modelName :: Lens.Lens' DeleteLanguageModel Prelude.Text
deleteLanguageModel_modelName = Lens.lens (\DeleteLanguageModel' {modelName} -> modelName) (\s@DeleteLanguageModel' {} a -> s {modelName = a} :: DeleteLanguageModel)

instance Prelude.AWSRequest DeleteLanguageModel where
  type
    Rs DeleteLanguageModel =
      DeleteLanguageModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLanguageModelResponse'

instance Prelude.Hashable DeleteLanguageModel

instance Prelude.NFData DeleteLanguageModel

instance Prelude.ToHeaders DeleteLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Transcribe.DeleteLanguageModel" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteLanguageModel where
  toJSON DeleteLanguageModel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Prelude..= modelName)]
      )

instance Prelude.ToPath DeleteLanguageModel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLanguageModelResponse ::
  DeleteLanguageModelResponse
newDeleteLanguageModelResponse =
  DeleteLanguageModelResponse'

instance Prelude.NFData DeleteLanguageModelResponse
