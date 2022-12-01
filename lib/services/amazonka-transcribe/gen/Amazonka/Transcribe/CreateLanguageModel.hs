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
-- Module      : Amazonka.Transcribe.CreateLanguageModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom language model.
--
-- When creating a new language model, you must specify:
--
-- -   If you want a Wideband (audio sample rates over 16,000 Hz) or
--     Narrowband (audio sample rates under 16,000 Hz) base model
--
-- -   The location of your training and tuning files (this must be an
--     Amazon S3 URI)
--
-- -   The language of your model
--
-- -   A unique name for your model
--
-- For more information, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/custom-language-models.html Custom language models>.
module Amazonka.Transcribe.CreateLanguageModel
  ( -- * Creating a Request
    CreateLanguageModel (..),
    newCreateLanguageModel,

    -- * Request Lenses
    createLanguageModel_tags,
    createLanguageModel_languageCode,
    createLanguageModel_baseModelName,
    createLanguageModel_modelName,
    createLanguageModel_inputDataConfig,

    -- * Destructuring the Response
    CreateLanguageModelResponse (..),
    newCreateLanguageModelResponse,

    -- * Response Lenses
    createLanguageModelResponse_modelStatus,
    createLanguageModelResponse_languageCode,
    createLanguageModelResponse_modelName,
    createLanguageModelResponse_baseModelName,
    createLanguageModelResponse_inputDataConfig,
    createLanguageModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newCreateLanguageModel' smart constructor.
data CreateLanguageModel = CreateLanguageModel'
  { -- | Adds one or more custom tags, each in the form of a key:value pair, to a
    -- new custom language model at the time you create this new model.
    --
    -- To learn more about using tags with Amazon Transcribe, refer to
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The language code that represents the language of your model. Each
    -- language model must contain terms in only one language, and the language
    -- you select for your model must match the language of your training and
    -- tuning data.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table. Note that U.S. English (@en-US@) is the only language supported
    -- with Amazon Transcribe Medical.
    --
    -- A custom language model can only be used to transcribe files in the same
    -- language as the model. For example, if you create a language model using
    -- US English (@en-US@), you can only apply this model to files that
    -- contain English audio.
    languageCode :: CLMLanguageCode,
    -- | The Amazon Transcribe standard language model, or base model, used to
    -- create your custom language model. Amazon Transcribe offers two options
    -- for base models: Wideband and Narrowband.
    --
    -- If the audio you want to transcribe has a sample rate of 16,000 Hz or
    -- greater, choose @WideBand@. To transcribe audio with a sample rate less
    -- than 16,000 Hz, choose @NarrowBand@.
    baseModelName :: BaseModelName,
    -- | A unique name, chosen by you, for your custom language model.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account. If you try to create a new
    -- language model with the same name as an existing language model, you get
    -- a @ConflictException@ error.
    modelName :: Prelude.Text,
    -- | Contains the Amazon S3 location of the training data you want to use to
    -- create a new custom language model, and permissions to access this
    -- location.
    --
    -- When using @InputDataConfig@, you must include these sub-parameters:
    -- @S3Uri@, which is the Amazon S3 location of your training data, and
    -- @DataAccessRoleArn@, which is the Amazon Resource Name (ARN) of the role
    -- that has permission to access your specified Amazon S3 location. You can
    -- optionally include @TuningDataS3Uri@, which is the Amazon S3 location of
    -- your tuning data. If you specify different Amazon S3 locations for
    -- training and tuning data, the ARN you use must have permissions to
    -- access both locations.
    inputDataConfig :: InputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLanguageModel_tags' - Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom language model at the time you create this new model.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
--
-- 'languageCode', 'createLanguageModel_languageCode' - The language code that represents the language of your model. Each
-- language model must contain terms in only one language, and the language
-- you select for your model must match the language of your training and
-- tuning data.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table. Note that U.S. English (@en-US@) is the only language supported
-- with Amazon Transcribe Medical.
--
-- A custom language model can only be used to transcribe files in the same
-- language as the model. For example, if you create a language model using
-- US English (@en-US@), you can only apply this model to files that
-- contain English audio.
--
-- 'baseModelName', 'createLanguageModel_baseModelName' - The Amazon Transcribe standard language model, or base model, used to
-- create your custom language model. Amazon Transcribe offers two options
-- for base models: Wideband and Narrowband.
--
-- If the audio you want to transcribe has a sample rate of 16,000 Hz or
-- greater, choose @WideBand@. To transcribe audio with a sample rate less
-- than 16,000 Hz, choose @NarrowBand@.
--
-- 'modelName', 'createLanguageModel_modelName' - A unique name, chosen by you, for your custom language model.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new
-- language model with the same name as an existing language model, you get
-- a @ConflictException@ error.
--
-- 'inputDataConfig', 'createLanguageModel_inputDataConfig' - Contains the Amazon S3 location of the training data you want to use to
-- create a new custom language model, and permissions to access this
-- location.
--
-- When using @InputDataConfig@, you must include these sub-parameters:
-- @S3Uri@, which is the Amazon S3 location of your training data, and
-- @DataAccessRoleArn@, which is the Amazon Resource Name (ARN) of the role
-- that has permission to access your specified Amazon S3 location. You can
-- optionally include @TuningDataS3Uri@, which is the Amazon S3 location of
-- your tuning data. If you specify different Amazon S3 locations for
-- training and tuning data, the ARN you use must have permissions to
-- access both locations.
newCreateLanguageModel ::
  -- | 'languageCode'
  CLMLanguageCode ->
  -- | 'baseModelName'
  BaseModelName ->
  -- | 'modelName'
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  CreateLanguageModel
newCreateLanguageModel
  pLanguageCode_
  pBaseModelName_
  pModelName_
  pInputDataConfig_ =
    CreateLanguageModel'
      { tags = Prelude.Nothing,
        languageCode = pLanguageCode_,
        baseModelName = pBaseModelName_,
        modelName = pModelName_,
        inputDataConfig = pInputDataConfig_
      }

-- | Adds one or more custom tags, each in the form of a key:value pair, to a
-- new custom language model at the time you create this new model.
--
-- To learn more about using tags with Amazon Transcribe, refer to
-- <https://docs.aws.amazon.com/transcribe/latest/dg/tagging.html Tagging resources>.
createLanguageModel_tags :: Lens.Lens' CreateLanguageModel (Prelude.Maybe (Prelude.NonEmpty Tag))
createLanguageModel_tags = Lens.lens (\CreateLanguageModel' {tags} -> tags) (\s@CreateLanguageModel' {} a -> s {tags = a} :: CreateLanguageModel) Prelude.. Lens.mapping Lens.coerced

-- | The language code that represents the language of your model. Each
-- language model must contain terms in only one language, and the language
-- you select for your model must match the language of your training and
-- tuning data.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table. Note that U.S. English (@en-US@) is the only language supported
-- with Amazon Transcribe Medical.
--
-- A custom language model can only be used to transcribe files in the same
-- language as the model. For example, if you create a language model using
-- US English (@en-US@), you can only apply this model to files that
-- contain English audio.
createLanguageModel_languageCode :: Lens.Lens' CreateLanguageModel CLMLanguageCode
createLanguageModel_languageCode = Lens.lens (\CreateLanguageModel' {languageCode} -> languageCode) (\s@CreateLanguageModel' {} a -> s {languageCode = a} :: CreateLanguageModel)

-- | The Amazon Transcribe standard language model, or base model, used to
-- create your custom language model. Amazon Transcribe offers two options
-- for base models: Wideband and Narrowband.
--
-- If the audio you want to transcribe has a sample rate of 16,000 Hz or
-- greater, choose @WideBand@. To transcribe audio with a sample rate less
-- than 16,000 Hz, choose @NarrowBand@.
createLanguageModel_baseModelName :: Lens.Lens' CreateLanguageModel BaseModelName
createLanguageModel_baseModelName = Lens.lens (\CreateLanguageModel' {baseModelName} -> baseModelName) (\s@CreateLanguageModel' {} a -> s {baseModelName = a} :: CreateLanguageModel)

-- | A unique name, chosen by you, for your custom language model.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account. If you try to create a new
-- language model with the same name as an existing language model, you get
-- a @ConflictException@ error.
createLanguageModel_modelName :: Lens.Lens' CreateLanguageModel Prelude.Text
createLanguageModel_modelName = Lens.lens (\CreateLanguageModel' {modelName} -> modelName) (\s@CreateLanguageModel' {} a -> s {modelName = a} :: CreateLanguageModel)

-- | Contains the Amazon S3 location of the training data you want to use to
-- create a new custom language model, and permissions to access this
-- location.
--
-- When using @InputDataConfig@, you must include these sub-parameters:
-- @S3Uri@, which is the Amazon S3 location of your training data, and
-- @DataAccessRoleArn@, which is the Amazon Resource Name (ARN) of the role
-- that has permission to access your specified Amazon S3 location. You can
-- optionally include @TuningDataS3Uri@, which is the Amazon S3 location of
-- your tuning data. If you specify different Amazon S3 locations for
-- training and tuning data, the ARN you use must have permissions to
-- access both locations.
createLanguageModel_inputDataConfig :: Lens.Lens' CreateLanguageModel InputDataConfig
createLanguageModel_inputDataConfig = Lens.lens (\CreateLanguageModel' {inputDataConfig} -> inputDataConfig) (\s@CreateLanguageModel' {} a -> s {inputDataConfig = a} :: CreateLanguageModel)

instance Core.AWSRequest CreateLanguageModel where
  type
    AWSResponse CreateLanguageModel =
      CreateLanguageModelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLanguageModelResponse'
            Prelude.<$> (x Core..?> "ModelStatus")
            Prelude.<*> (x Core..?> "LanguageCode")
            Prelude.<*> (x Core..?> "ModelName")
            Prelude.<*> (x Core..?> "BaseModelName")
            Prelude.<*> (x Core..?> "InputDataConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLanguageModel where
  hashWithSalt _salt CreateLanguageModel' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` baseModelName
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` inputDataConfig

instance Prelude.NFData CreateLanguageModel where
  rnf CreateLanguageModel' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf baseModelName
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf inputDataConfig

instance Core.ToHeaders CreateLanguageModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.CreateLanguageModel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLanguageModel where
  toJSON CreateLanguageModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("LanguageCode" Core..= languageCode),
            Prelude.Just ("BaseModelName" Core..= baseModelName),
            Prelude.Just ("ModelName" Core..= modelName),
            Prelude.Just
              ("InputDataConfig" Core..= inputDataConfig)
          ]
      )

instance Core.ToPath CreateLanguageModel where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLanguageModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLanguageModelResponse' smart constructor.
data CreateLanguageModelResponse = CreateLanguageModelResponse'
  { -- | The status of your custom language model. When the status displays as
    -- @COMPLETED@, your model is ready to use.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | The language code you selected for your custom language model.
    languageCode :: Prelude.Maybe CLMLanguageCode,
    -- | The name of your custom language model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Transcribe standard language model, or base model, you
    -- specified when creating your custom language model.
    baseModelName :: Prelude.Maybe BaseModelName,
    -- | Lists your data access role ARN (Amazon Resource Name) and the Amazon S3
    -- locations you provided for your training (@S3Uri@) and tuning
    -- (@TuningDataS3Uri@) data.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLanguageModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelStatus', 'createLanguageModelResponse_modelStatus' - The status of your custom language model. When the status displays as
-- @COMPLETED@, your model is ready to use.
--
-- 'languageCode', 'createLanguageModelResponse_languageCode' - The language code you selected for your custom language model.
--
-- 'modelName', 'createLanguageModelResponse_modelName' - The name of your custom language model.
--
-- 'baseModelName', 'createLanguageModelResponse_baseModelName' - The Amazon Transcribe standard language model, or base model, you
-- specified when creating your custom language model.
--
-- 'inputDataConfig', 'createLanguageModelResponse_inputDataConfig' - Lists your data access role ARN (Amazon Resource Name) and the Amazon S3
-- locations you provided for your training (@S3Uri@) and tuning
-- (@TuningDataS3Uri@) data.
--
-- 'httpStatus', 'createLanguageModelResponse_httpStatus' - The response's http status code.
newCreateLanguageModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLanguageModelResponse
newCreateLanguageModelResponse pHttpStatus_ =
  CreateLanguageModelResponse'
    { modelStatus =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      modelName = Prelude.Nothing,
      baseModelName = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of your custom language model. When the status displays as
-- @COMPLETED@, your model is ready to use.
createLanguageModelResponse_modelStatus :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe ModelStatus)
createLanguageModelResponse_modelStatus = Lens.lens (\CreateLanguageModelResponse' {modelStatus} -> modelStatus) (\s@CreateLanguageModelResponse' {} a -> s {modelStatus = a} :: CreateLanguageModelResponse)

-- | The language code you selected for your custom language model.
createLanguageModelResponse_languageCode :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe CLMLanguageCode)
createLanguageModelResponse_languageCode = Lens.lens (\CreateLanguageModelResponse' {languageCode} -> languageCode) (\s@CreateLanguageModelResponse' {} a -> s {languageCode = a} :: CreateLanguageModelResponse)

-- | The name of your custom language model.
createLanguageModelResponse_modelName :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe Prelude.Text)
createLanguageModelResponse_modelName = Lens.lens (\CreateLanguageModelResponse' {modelName} -> modelName) (\s@CreateLanguageModelResponse' {} a -> s {modelName = a} :: CreateLanguageModelResponse)

-- | The Amazon Transcribe standard language model, or base model, you
-- specified when creating your custom language model.
createLanguageModelResponse_baseModelName :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe BaseModelName)
createLanguageModelResponse_baseModelName = Lens.lens (\CreateLanguageModelResponse' {baseModelName} -> baseModelName) (\s@CreateLanguageModelResponse' {} a -> s {baseModelName = a} :: CreateLanguageModelResponse)

-- | Lists your data access role ARN (Amazon Resource Name) and the Amazon S3
-- locations you provided for your training (@S3Uri@) and tuning
-- (@TuningDataS3Uri@) data.
createLanguageModelResponse_inputDataConfig :: Lens.Lens' CreateLanguageModelResponse (Prelude.Maybe InputDataConfig)
createLanguageModelResponse_inputDataConfig = Lens.lens (\CreateLanguageModelResponse' {inputDataConfig} -> inputDataConfig) (\s@CreateLanguageModelResponse' {} a -> s {inputDataConfig = a} :: CreateLanguageModelResponse)

-- | The response's http status code.
createLanguageModelResponse_httpStatus :: Lens.Lens' CreateLanguageModelResponse Prelude.Int
createLanguageModelResponse_httpStatus = Lens.lens (\CreateLanguageModelResponse' {httpStatus} -> httpStatus) (\s@CreateLanguageModelResponse' {} a -> s {httpStatus = a} :: CreateLanguageModelResponse)

instance Prelude.NFData CreateLanguageModelResponse where
  rnf CreateLanguageModelResponse' {..} =
    Prelude.rnf modelStatus
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf baseModelName
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf httpStatus
