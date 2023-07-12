{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transcribe.Types.LanguageModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.LanguageModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.BaseModelName
import Amazonka.Transcribe.Types.CLMLanguageCode
import Amazonka.Transcribe.Types.InputDataConfig
import Amazonka.Transcribe.Types.ModelStatus

-- | Provides information about a custom language model, including the base
-- model name, when the model was created, the location of the files used
-- to train the model, when the model was last modified, the name you chose
-- for the model, its language, its processing state, and if there is an
-- upgrade available for the base model.
--
-- /See:/ 'newLanguageModel' smart constructor.
data LanguageModel = LanguageModel'
  { -- | The Amazon Transcribe standard language model, or base model, used to
    -- create your custom language model.
    baseModelName :: Prelude.Maybe BaseModelName,
    -- | The date and time the specified custom language model was created.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | If @ModelStatus@ is @FAILED@, @FailureReason@ contains information about
    -- why the custom language model request failed. See also:
    -- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location of the input files used to train and tune your
    -- custom language model, in addition to the data access role ARN (Amazon
    -- Resource Name) that has permissions to access these data.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The language code used to create your custom language model. Each custom
    -- language model must contain terms in only one language, and the language
    -- you select for your custom language model must match the language of
    -- your training and tuning data.
    --
    -- For a list of supported languages and their associated language codes,
    -- refer to the
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
    -- table. Note that U.S. English (@en-US@) is the only language supported
    -- with Amazon Transcribe Medical.
    languageCode :: Prelude.Maybe CLMLanguageCode,
    -- | The date and time the specified custom language model was last modified.
    --
    -- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
    -- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
    -- May 4, 2022.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | A unique name, chosen by you, for your custom language model.
    --
    -- This name is case sensitive, cannot contain spaces, and must be unique
    -- within an Amazon Web Services account.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The status of the specified custom language model. When the status
    -- displays as @COMPLETED@ the model is ready for use.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | Shows if a more current base model is available for use with the
    -- specified custom language model.
    --
    -- If @false@, your custom language model is using the most up-to-date base
    -- model.
    --
    -- If @true@, there is a newer base model available than the one your
    -- language model is using.
    --
    -- Note that to update a base model, you must recreate the custom language
    -- model using the new base model. Base model upgrades for existing custom
    -- language models are not supported.
    upgradeAvailability :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseModelName', 'languageModel_baseModelName' - The Amazon Transcribe standard language model, or base model, used to
-- create your custom language model.
--
-- 'createTime', 'languageModel_createTime' - The date and time the specified custom language model was created.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'failureReason', 'languageModel_failureReason' - If @ModelStatus@ is @FAILED@, @FailureReason@ contains information about
-- why the custom language model request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
--
-- 'inputDataConfig', 'languageModel_inputDataConfig' - The Amazon S3 location of the input files used to train and tune your
-- custom language model, in addition to the data access role ARN (Amazon
-- Resource Name) that has permissions to access these data.
--
-- 'languageCode', 'languageModel_languageCode' - The language code used to create your custom language model. Each custom
-- language model must contain terms in only one language, and the language
-- you select for your custom language model must match the language of
-- your training and tuning data.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table. Note that U.S. English (@en-US@) is the only language supported
-- with Amazon Transcribe Medical.
--
-- 'lastModifiedTime', 'languageModel_lastModifiedTime' - The date and time the specified custom language model was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
--
-- 'modelName', 'languageModel_modelName' - A unique name, chosen by you, for your custom language model.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account.
--
-- 'modelStatus', 'languageModel_modelStatus' - The status of the specified custom language model. When the status
-- displays as @COMPLETED@ the model is ready for use.
--
-- 'upgradeAvailability', 'languageModel_upgradeAvailability' - Shows if a more current base model is available for use with the
-- specified custom language model.
--
-- If @false@, your custom language model is using the most up-to-date base
-- model.
--
-- If @true@, there is a newer base model available than the one your
-- language model is using.
--
-- Note that to update a base model, you must recreate the custom language
-- model using the new base model. Base model upgrades for existing custom
-- language models are not supported.
newLanguageModel ::
  LanguageModel
newLanguageModel =
  LanguageModel'
    { baseModelName = Prelude.Nothing,
      createTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      modelName = Prelude.Nothing,
      modelStatus = Prelude.Nothing,
      upgradeAvailability = Prelude.Nothing
    }

-- | The Amazon Transcribe standard language model, or base model, used to
-- create your custom language model.
languageModel_baseModelName :: Lens.Lens' LanguageModel (Prelude.Maybe BaseModelName)
languageModel_baseModelName = Lens.lens (\LanguageModel' {baseModelName} -> baseModelName) (\s@LanguageModel' {} a -> s {baseModelName = a} :: LanguageModel)

-- | The date and time the specified custom language model was created.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
languageModel_createTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_createTime = Lens.lens (\LanguageModel' {createTime} -> createTime) (\s@LanguageModel' {} a -> s {createTime = a} :: LanguageModel) Prelude.. Lens.mapping Data._Time

-- | If @ModelStatus@ is @FAILED@, @FailureReason@ contains information about
-- why the custom language model request failed. See also:
-- <https://docs.aws.amazon.com/transcribe/latest/APIReference/CommonErrors.html Common Errors>.
languageModel_failureReason :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_failureReason = Lens.lens (\LanguageModel' {failureReason} -> failureReason) (\s@LanguageModel' {} a -> s {failureReason = a} :: LanguageModel)

-- | The Amazon S3 location of the input files used to train and tune your
-- custom language model, in addition to the data access role ARN (Amazon
-- Resource Name) that has permissions to access these data.
languageModel_inputDataConfig :: Lens.Lens' LanguageModel (Prelude.Maybe InputDataConfig)
languageModel_inputDataConfig = Lens.lens (\LanguageModel' {inputDataConfig} -> inputDataConfig) (\s@LanguageModel' {} a -> s {inputDataConfig = a} :: LanguageModel)

-- | The language code used to create your custom language model. Each custom
-- language model must contain terms in only one language, and the language
-- you select for your custom language model must match the language of
-- your training and tuning data.
--
-- For a list of supported languages and their associated language codes,
-- refer to the
-- <https://docs.aws.amazon.com/transcribe/latest/dg/supported-languages.html Supported languages>
-- table. Note that U.S. English (@en-US@) is the only language supported
-- with Amazon Transcribe Medical.
languageModel_languageCode :: Lens.Lens' LanguageModel (Prelude.Maybe CLMLanguageCode)
languageModel_languageCode = Lens.lens (\LanguageModel' {languageCode} -> languageCode) (\s@LanguageModel' {} a -> s {languageCode = a} :: LanguageModel)

-- | The date and time the specified custom language model was last modified.
--
-- Timestamps are in the format @YYYY-MM-DD\'T\'HH:MM:SS.SSSSSS-UTC@. For
-- example, @2022-05-04T12:32:58.761000-07:00@ represents 12:32 PM UTC-7 on
-- May 4, 2022.
languageModel_lastModifiedTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_lastModifiedTime = Lens.lens (\LanguageModel' {lastModifiedTime} -> lastModifiedTime) (\s@LanguageModel' {} a -> s {lastModifiedTime = a} :: LanguageModel) Prelude.. Lens.mapping Data._Time

-- | A unique name, chosen by you, for your custom language model.
--
-- This name is case sensitive, cannot contain spaces, and must be unique
-- within an Amazon Web Services account.
languageModel_modelName :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_modelName = Lens.lens (\LanguageModel' {modelName} -> modelName) (\s@LanguageModel' {} a -> s {modelName = a} :: LanguageModel)

-- | The status of the specified custom language model. When the status
-- displays as @COMPLETED@ the model is ready for use.
languageModel_modelStatus :: Lens.Lens' LanguageModel (Prelude.Maybe ModelStatus)
languageModel_modelStatus = Lens.lens (\LanguageModel' {modelStatus} -> modelStatus) (\s@LanguageModel' {} a -> s {modelStatus = a} :: LanguageModel)

-- | Shows if a more current base model is available for use with the
-- specified custom language model.
--
-- If @false@, your custom language model is using the most up-to-date base
-- model.
--
-- If @true@, there is a newer base model available than the one your
-- language model is using.
--
-- Note that to update a base model, you must recreate the custom language
-- model using the new base model. Base model upgrades for existing custom
-- language models are not supported.
languageModel_upgradeAvailability :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Bool)
languageModel_upgradeAvailability = Lens.lens (\LanguageModel' {upgradeAvailability} -> upgradeAvailability) (\s@LanguageModel' {} a -> s {upgradeAvailability = a} :: LanguageModel)

instance Data.FromJSON LanguageModel where
  parseJSON =
    Data.withObject
      "LanguageModel"
      ( \x ->
          LanguageModel'
            Prelude.<$> (x Data..:? "BaseModelName")
            Prelude.<*> (x Data..:? "CreateTime")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "InputDataConfig")
            Prelude.<*> (x Data..:? "LanguageCode")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "ModelName")
            Prelude.<*> (x Data..:? "ModelStatus")
            Prelude.<*> (x Data..:? "UpgradeAvailability")
      )

instance Prelude.Hashable LanguageModel where
  hashWithSalt _salt LanguageModel' {..} =
    _salt
      `Prelude.hashWithSalt` baseModelName
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelStatus
      `Prelude.hashWithSalt` upgradeAvailability

instance Prelude.NFData LanguageModel where
  rnf LanguageModel' {..} =
    Prelude.rnf baseModelName
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelStatus
      `Prelude.seq` Prelude.rnf upgradeAvailability
