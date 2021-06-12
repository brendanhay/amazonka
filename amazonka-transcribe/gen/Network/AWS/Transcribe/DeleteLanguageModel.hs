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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDeleteLanguageModel' smart constructor.
data DeleteLanguageModel = DeleteLanguageModel'
  { -- | The name of the model you\'re choosing to delete.
    modelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DeleteLanguageModel
newDeleteLanguageModel pModelName_ =
  DeleteLanguageModel' {modelName = pModelName_}

-- | The name of the model you\'re choosing to delete.
deleteLanguageModel_modelName :: Lens.Lens' DeleteLanguageModel Core.Text
deleteLanguageModel_modelName = Lens.lens (\DeleteLanguageModel' {modelName} -> modelName) (\s@DeleteLanguageModel' {} a -> s {modelName = a} :: DeleteLanguageModel)

instance Core.AWSRequest DeleteLanguageModel where
  type
    AWSResponse DeleteLanguageModel =
      DeleteLanguageModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteLanguageModelResponse'

instance Core.Hashable DeleteLanguageModel

instance Core.NFData DeleteLanguageModel

instance Core.ToHeaders DeleteLanguageModel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DeleteLanguageModel" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLanguageModel where
  toJSON DeleteLanguageModel' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ModelName" Core..= modelName)]
      )

instance Core.ToPath DeleteLanguageModel where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLanguageModel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLanguageModelResponse ::
  DeleteLanguageModelResponse
newDeleteLanguageModelResponse =
  DeleteLanguageModelResponse'

instance Core.NFData DeleteLanguageModelResponse
