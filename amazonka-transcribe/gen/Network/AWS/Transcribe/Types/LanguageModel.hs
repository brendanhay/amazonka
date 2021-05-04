{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Transcribe.Types.LanguageModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageModel where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.ModelStatus

-- | The structure used to describe a custom language model.
--
-- /See:/ 'newLanguageModel' smart constructor.
data LanguageModel = LanguageModel'
  { -- | The language code you used to create your custom language model.
    languageCode :: Prelude.Maybe CLMLanguageCode,
    -- | The data access role and Amazon S3 prefixes for the input files used to
    -- train the custom language model.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The creation status of a custom language model. When the status is
    -- @COMPLETED@ the model is ready for use.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | The reason why the custom language model couldn\'t be created.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Whether the base model used for the custom language model is up to date.
    -- If this field is @true@ then you are running the most up-to-date version
    -- of the base model in your custom language model.
    upgradeAvailability :: Prelude.Maybe Prelude.Bool,
    -- | The time the custom language model was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | The most recent time the custom language model was modified.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the custom language model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Transcribe standard language model, or base model used to
    -- create the custom language model.
    baseModelName :: Prelude.Maybe BaseModelName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LanguageModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'languageModel_languageCode' - The language code you used to create your custom language model.
--
-- 'inputDataConfig', 'languageModel_inputDataConfig' - The data access role and Amazon S3 prefixes for the input files used to
-- train the custom language model.
--
-- 'modelStatus', 'languageModel_modelStatus' - The creation status of a custom language model. When the status is
-- @COMPLETED@ the model is ready for use.
--
-- 'failureReason', 'languageModel_failureReason' - The reason why the custom language model couldn\'t be created.
--
-- 'upgradeAvailability', 'languageModel_upgradeAvailability' - Whether the base model used for the custom language model is up to date.
-- If this field is @true@ then you are running the most up-to-date version
-- of the base model in your custom language model.
--
-- 'createTime', 'languageModel_createTime' - The time the custom language model was created.
--
-- 'lastModifiedTime', 'languageModel_lastModifiedTime' - The most recent time the custom language model was modified.
--
-- 'modelName', 'languageModel_modelName' - The name of the custom language model.
--
-- 'baseModelName', 'languageModel_baseModelName' - The Amazon Transcribe standard language model, or base model used to
-- create the custom language model.
newLanguageModel ::
  LanguageModel
newLanguageModel =
  LanguageModel'
    { languageCode = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      modelStatus = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      upgradeAvailability = Prelude.Nothing,
      createTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      modelName = Prelude.Nothing,
      baseModelName = Prelude.Nothing
    }

-- | The language code you used to create your custom language model.
languageModel_languageCode :: Lens.Lens' LanguageModel (Prelude.Maybe CLMLanguageCode)
languageModel_languageCode = Lens.lens (\LanguageModel' {languageCode} -> languageCode) (\s@LanguageModel' {} a -> s {languageCode = a} :: LanguageModel)

-- | The data access role and Amazon S3 prefixes for the input files used to
-- train the custom language model.
languageModel_inputDataConfig :: Lens.Lens' LanguageModel (Prelude.Maybe InputDataConfig)
languageModel_inputDataConfig = Lens.lens (\LanguageModel' {inputDataConfig} -> inputDataConfig) (\s@LanguageModel' {} a -> s {inputDataConfig = a} :: LanguageModel)

-- | The creation status of a custom language model. When the status is
-- @COMPLETED@ the model is ready for use.
languageModel_modelStatus :: Lens.Lens' LanguageModel (Prelude.Maybe ModelStatus)
languageModel_modelStatus = Lens.lens (\LanguageModel' {modelStatus} -> modelStatus) (\s@LanguageModel' {} a -> s {modelStatus = a} :: LanguageModel)

-- | The reason why the custom language model couldn\'t be created.
languageModel_failureReason :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_failureReason = Lens.lens (\LanguageModel' {failureReason} -> failureReason) (\s@LanguageModel' {} a -> s {failureReason = a} :: LanguageModel)

-- | Whether the base model used for the custom language model is up to date.
-- If this field is @true@ then you are running the most up-to-date version
-- of the base model in your custom language model.
languageModel_upgradeAvailability :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Bool)
languageModel_upgradeAvailability = Lens.lens (\LanguageModel' {upgradeAvailability} -> upgradeAvailability) (\s@LanguageModel' {} a -> s {upgradeAvailability = a} :: LanguageModel)

-- | The time the custom language model was created.
languageModel_createTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_createTime = Lens.lens (\LanguageModel' {createTime} -> createTime) (\s@LanguageModel' {} a -> s {createTime = a} :: LanguageModel) Prelude.. Lens.mapping Prelude._Time

-- | The most recent time the custom language model was modified.
languageModel_lastModifiedTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_lastModifiedTime = Lens.lens (\LanguageModel' {lastModifiedTime} -> lastModifiedTime) (\s@LanguageModel' {} a -> s {lastModifiedTime = a} :: LanguageModel) Prelude.. Lens.mapping Prelude._Time

-- | The name of the custom language model.
languageModel_modelName :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_modelName = Lens.lens (\LanguageModel' {modelName} -> modelName) (\s@LanguageModel' {} a -> s {modelName = a} :: LanguageModel)

-- | The Amazon Transcribe standard language model, or base model used to
-- create the custom language model.
languageModel_baseModelName :: Lens.Lens' LanguageModel (Prelude.Maybe BaseModelName)
languageModel_baseModelName = Lens.lens (\LanguageModel' {baseModelName} -> baseModelName) (\s@LanguageModel' {} a -> s {baseModelName = a} :: LanguageModel)

instance Prelude.FromJSON LanguageModel where
  parseJSON =
    Prelude.withObject
      "LanguageModel"
      ( \x ->
          LanguageModel'
            Prelude.<$> (x Prelude..:? "LanguageCode")
            Prelude.<*> (x Prelude..:? "InputDataConfig")
            Prelude.<*> (x Prelude..:? "ModelStatus")
            Prelude.<*> (x Prelude..:? "FailureReason")
            Prelude.<*> (x Prelude..:? "UpgradeAvailability")
            Prelude.<*> (x Prelude..:? "CreateTime")
            Prelude.<*> (x Prelude..:? "LastModifiedTime")
            Prelude.<*> (x Prelude..:? "ModelName")
            Prelude.<*> (x Prelude..:? "BaseModelName")
      )

instance Prelude.Hashable LanguageModel

instance Prelude.NFData LanguageModel
