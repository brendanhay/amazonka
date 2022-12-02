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
-- Module      : Amazonka.Transcribe.DescribeLanguageModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the specified custom language model.
--
-- This operation also shows if the base language model you used to create
-- your custom language model has been updated. If Amazon Transcribe has
-- updated the base model, you can create a new custom language model using
-- the updated base model.
--
-- If you tried to create a new custom language model and the request
-- wasn\'t successful, you can use @DescribeLanguageModel@ to help identify
-- the reason for this failure.
--
-- To get a list of your custom language models, use the operation.
module Amazonka.Transcribe.DescribeLanguageModel
  ( -- * Creating a Request
    DescribeLanguageModel (..),
    newDescribeLanguageModel,

    -- * Request Lenses
    describeLanguageModel_modelName,

    -- * Destructuring the Response
    DescribeLanguageModelResponse (..),
    newDescribeLanguageModelResponse,

    -- * Response Lenses
    describeLanguageModelResponse_languageModel,
    describeLanguageModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newDescribeLanguageModel' smart constructor.
data DescribeLanguageModel = DescribeLanguageModel'
  { -- | The name of the custom language model you want information about. Model
    -- names are case sensitive.
    modelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'describeLanguageModel_modelName' - The name of the custom language model you want information about. Model
-- names are case sensitive.
newDescribeLanguageModel ::
  -- | 'modelName'
  Prelude.Text ->
  DescribeLanguageModel
newDescribeLanguageModel pModelName_ =
  DescribeLanguageModel' {modelName = pModelName_}

-- | The name of the custom language model you want information about. Model
-- names are case sensitive.
describeLanguageModel_modelName :: Lens.Lens' DescribeLanguageModel Prelude.Text
describeLanguageModel_modelName = Lens.lens (\DescribeLanguageModel' {modelName} -> modelName) (\s@DescribeLanguageModel' {} a -> s {modelName = a} :: DescribeLanguageModel)

instance Core.AWSRequest DescribeLanguageModel where
  type
    AWSResponse DescribeLanguageModel =
      DescribeLanguageModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLanguageModelResponse'
            Prelude.<$> (x Data..?> "LanguageModel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLanguageModel where
  hashWithSalt _salt DescribeLanguageModel' {..} =
    _salt `Prelude.hashWithSalt` modelName

instance Prelude.NFData DescribeLanguageModel where
  rnf DescribeLanguageModel' {..} =
    Prelude.rnf modelName

instance Data.ToHeaders DescribeLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.DescribeLanguageModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLanguageModel where
  toJSON DescribeLanguageModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Data..= modelName)]
      )

instance Data.ToPath DescribeLanguageModel where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLanguageModelResponse' smart constructor.
data DescribeLanguageModelResponse = DescribeLanguageModelResponse'
  { -- | Provides information about the specified custom language model.
    --
    -- This parameter also shows if the base language model you used to create
    -- your custom language model has been updated. If Amazon Transcribe has
    -- updated the base model, you can create a new custom language model using
    -- the updated base model.
    --
    -- If you tried to create a new custom language model and the request
    -- wasn\'t successful, you can use this @DescribeLanguageModel@ to help
    -- identify the reason for this failure.
    languageModel :: Prelude.Maybe LanguageModel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageModel', 'describeLanguageModelResponse_languageModel' - Provides information about the specified custom language model.
--
-- This parameter also shows if the base language model you used to create
-- your custom language model has been updated. If Amazon Transcribe has
-- updated the base model, you can create a new custom language model using
-- the updated base model.
--
-- If you tried to create a new custom language model and the request
-- wasn\'t successful, you can use this @DescribeLanguageModel@ to help
-- identify the reason for this failure.
--
-- 'httpStatus', 'describeLanguageModelResponse_httpStatus' - The response's http status code.
newDescribeLanguageModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLanguageModelResponse
newDescribeLanguageModelResponse pHttpStatus_ =
  DescribeLanguageModelResponse'
    { languageModel =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Provides information about the specified custom language model.
--
-- This parameter also shows if the base language model you used to create
-- your custom language model has been updated. If Amazon Transcribe has
-- updated the base model, you can create a new custom language model using
-- the updated base model.
--
-- If you tried to create a new custom language model and the request
-- wasn\'t successful, you can use this @DescribeLanguageModel@ to help
-- identify the reason for this failure.
describeLanguageModelResponse_languageModel :: Lens.Lens' DescribeLanguageModelResponse (Prelude.Maybe LanguageModel)
describeLanguageModelResponse_languageModel = Lens.lens (\DescribeLanguageModelResponse' {languageModel} -> languageModel) (\s@DescribeLanguageModelResponse' {} a -> s {languageModel = a} :: DescribeLanguageModelResponse)

-- | The response's http status code.
describeLanguageModelResponse_httpStatus :: Lens.Lens' DescribeLanguageModelResponse Prelude.Int
describeLanguageModelResponse_httpStatus = Lens.lens (\DescribeLanguageModelResponse' {httpStatus} -> httpStatus) (\s@DescribeLanguageModelResponse' {} a -> s {httpStatus = a} :: DescribeLanguageModelResponse)

instance Prelude.NFData DescribeLanguageModelResponse where
  rnf DescribeLanguageModelResponse' {..} =
    Prelude.rnf languageModel
      `Prelude.seq` Prelude.rnf httpStatus
