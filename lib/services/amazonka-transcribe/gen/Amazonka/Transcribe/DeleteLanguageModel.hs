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
-- Module      : Amazonka.Transcribe.DeleteLanguageModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a custom language model. To use this operation, specify the name
-- of the language model you want to delete using @ModelName@. custom
-- language model names are case sensitive.
module Amazonka.Transcribe.DeleteLanguageModel
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDeleteLanguageModel' smart constructor.
data DeleteLanguageModel = DeleteLanguageModel'
  { -- | The name of the custom language model you want to delete. Model names
    -- are case sensitive.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'deleteLanguageModel_modelName' - The name of the custom language model you want to delete. Model names
-- are case sensitive.
newDeleteLanguageModel ::
  -- | 'modelName'
  Prelude.Text ->
  DeleteLanguageModel
newDeleteLanguageModel pModelName_ =
  DeleteLanguageModel' {modelName = pModelName_}

-- | The name of the custom language model you want to delete. Model names
-- are case sensitive.
deleteLanguageModel_modelName :: Lens.Lens' DeleteLanguageModel Prelude.Text
deleteLanguageModel_modelName = Lens.lens (\DeleteLanguageModel' {modelName} -> modelName) (\s@DeleteLanguageModel' {} a -> s {modelName = a} :: DeleteLanguageModel)

instance Core.AWSRequest DeleteLanguageModel where
  type
    AWSResponse DeleteLanguageModel =
      DeleteLanguageModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteLanguageModelResponse'

instance Prelude.Hashable DeleteLanguageModel where
  hashWithSalt _salt DeleteLanguageModel' {..} =
    _salt `Prelude.hashWithSalt` modelName

instance Prelude.NFData DeleteLanguageModel where
  rnf DeleteLanguageModel' {..} = Prelude.rnf modelName

instance Data.ToHeaders DeleteLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.DeleteLanguageModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLanguageModel where
  toJSON DeleteLanguageModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Data..= modelName)]
      )

instance Data.ToPath DeleteLanguageModel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLanguageModelResponse' smart constructor.
data DeleteLanguageModelResponse = DeleteLanguageModelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLanguageModelResponse ::
  DeleteLanguageModelResponse
newDeleteLanguageModelResponse =
  DeleteLanguageModelResponse'

instance Prelude.NFData DeleteLanguageModelResponse where
  rnf _ = ()
