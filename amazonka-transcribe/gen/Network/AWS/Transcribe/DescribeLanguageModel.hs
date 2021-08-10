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
-- Module      : Network.AWS.Transcribe.DescribeLanguageModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a single custom language model. Use this
-- information to see details about the language model in your AWS account.
-- You can also see whether the base language model used to create your
-- custom language model has been updated. If Amazon Transcribe has updated
-- the base model, you can create a new custom language model using the
-- updated base model. If the language model wasn\'t created, you can use
-- this operation to understand why Amazon Transcribe couldn\'t create it.
module Network.AWS.Transcribe.DescribeLanguageModel
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newDescribeLanguageModel' smart constructor.
data DescribeLanguageModel = DescribeLanguageModel'
  { -- | The name of the custom language model you submit to get more
    -- information.
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
-- 'modelName', 'describeLanguageModel_modelName' - The name of the custom language model you submit to get more
-- information.
newDescribeLanguageModel ::
  -- | 'modelName'
  Prelude.Text ->
  DescribeLanguageModel
newDescribeLanguageModel pModelName_ =
  DescribeLanguageModel' {modelName = pModelName_}

-- | The name of the custom language model you submit to get more
-- information.
describeLanguageModel_modelName :: Lens.Lens' DescribeLanguageModel Prelude.Text
describeLanguageModel_modelName = Lens.lens (\DescribeLanguageModel' {modelName} -> modelName) (\s@DescribeLanguageModel' {} a -> s {modelName = a} :: DescribeLanguageModel)

instance Core.AWSRequest DescribeLanguageModel where
  type
    AWSResponse DescribeLanguageModel =
      DescribeLanguageModelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLanguageModelResponse'
            Prelude.<$> (x Core..?> "LanguageModel")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLanguageModel

instance Prelude.NFData DescribeLanguageModel

instance Core.ToHeaders DescribeLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.DescribeLanguageModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLanguageModel where
  toJSON DescribeLanguageModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ModelName" Core..= modelName)]
      )

instance Core.ToPath DescribeLanguageModel where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLanguageModelResponse' smart constructor.
data DescribeLanguageModelResponse = DescribeLanguageModelResponse'
  { -- | The name of the custom language model you requested more information
    -- about.
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
-- 'languageModel', 'describeLanguageModelResponse_languageModel' - The name of the custom language model you requested more information
-- about.
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

-- | The name of the custom language model you requested more information
-- about.
describeLanguageModelResponse_languageModel :: Lens.Lens' DescribeLanguageModelResponse (Prelude.Maybe LanguageModel)
describeLanguageModelResponse_languageModel = Lens.lens (\DescribeLanguageModelResponse' {languageModel} -> languageModel) (\s@DescribeLanguageModelResponse' {} a -> s {languageModel = a} :: DescribeLanguageModelResponse)

-- | The response's http status code.
describeLanguageModelResponse_httpStatus :: Lens.Lens' DescribeLanguageModelResponse Prelude.Int
describeLanguageModelResponse_httpStatus = Lens.lens (\DescribeLanguageModelResponse' {httpStatus} -> httpStatus) (\s@DescribeLanguageModelResponse' {} a -> s {httpStatus = a} :: DescribeLanguageModelResponse)

instance Prelude.NFData DescribeLanguageModelResponse
