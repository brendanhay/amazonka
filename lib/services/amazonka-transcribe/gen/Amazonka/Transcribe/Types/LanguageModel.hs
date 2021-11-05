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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.LanguageModel where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.BaseModelName
import Amazonka.Transcribe.Types.CLMLanguageCode
import Amazonka.Transcribe.Types.InputDataConfig
import Amazonka.Transcribe.Types.ModelStatus

-- | The structure used to describe a custom language model.
--
-- /See:/ 'newLanguageModel' smart constructor.
data LanguageModel = LanguageModel'
  { -- | The reason why the custom language model couldn\'t be created.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The language code you used to create your custom language model.
    languageCode :: Prelude.Maybe CLMLanguageCode,
    -- | The name of the custom language model.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The most recent time the custom language model was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Whether the base model used for the custom language model is up to date.
    -- If this field is @true@ then you are running the most up-to-date version
    -- of the base model in your custom language model.
    upgradeAvailability :: Prelude.Maybe Prelude.Bool,
    -- | The data access role and Amazon S3 prefixes for the input files used to
    -- train the custom language model.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The Amazon Transcribe standard language model, or base model used to
    -- create the custom language model.
    baseModelName :: Prelude.Maybe BaseModelName,
    -- | The creation status of a custom language model. When the status is
    -- @COMPLETED@ the model is ready for use.
    modelStatus :: Prelude.Maybe ModelStatus,
    -- | The time the custom language model was created.
    createTime :: Prelude.Maybe Core.POSIX
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
-- 'failureReason', 'languageModel_failureReason' - The reason why the custom language model couldn\'t be created.
--
-- 'languageCode', 'languageModel_languageCode' - The language code you used to create your custom language model.
--
-- 'modelName', 'languageModel_modelName' - The name of the custom language model.
--
-- 'lastModifiedTime', 'languageModel_lastModifiedTime' - The most recent time the custom language model was modified.
--
-- 'upgradeAvailability', 'languageModel_upgradeAvailability' - Whether the base model used for the custom language model is up to date.
-- If this field is @true@ then you are running the most up-to-date version
-- of the base model in your custom language model.
--
-- 'inputDataConfig', 'languageModel_inputDataConfig' - The data access role and Amazon S3 prefixes for the input files used to
-- train the custom language model.
--
-- 'baseModelName', 'languageModel_baseModelName' - The Amazon Transcribe standard language model, or base model used to
-- create the custom language model.
--
-- 'modelStatus', 'languageModel_modelStatus' - The creation status of a custom language model. When the status is
-- @COMPLETED@ the model is ready for use.
--
-- 'createTime', 'languageModel_createTime' - The time the custom language model was created.
newLanguageModel ::
  LanguageModel
newLanguageModel =
  LanguageModel'
    { failureReason = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      modelName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      upgradeAvailability = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      baseModelName = Prelude.Nothing,
      modelStatus = Prelude.Nothing,
      createTime = Prelude.Nothing
    }

-- | The reason why the custom language model couldn\'t be created.
languageModel_failureReason :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_failureReason = Lens.lens (\LanguageModel' {failureReason} -> failureReason) (\s@LanguageModel' {} a -> s {failureReason = a} :: LanguageModel)

-- | The language code you used to create your custom language model.
languageModel_languageCode :: Lens.Lens' LanguageModel (Prelude.Maybe CLMLanguageCode)
languageModel_languageCode = Lens.lens (\LanguageModel' {languageCode} -> languageCode) (\s@LanguageModel' {} a -> s {languageCode = a} :: LanguageModel)

-- | The name of the custom language model.
languageModel_modelName :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Text)
languageModel_modelName = Lens.lens (\LanguageModel' {modelName} -> modelName) (\s@LanguageModel' {} a -> s {modelName = a} :: LanguageModel)

-- | The most recent time the custom language model was modified.
languageModel_lastModifiedTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_lastModifiedTime = Lens.lens (\LanguageModel' {lastModifiedTime} -> lastModifiedTime) (\s@LanguageModel' {} a -> s {lastModifiedTime = a} :: LanguageModel) Prelude.. Lens.mapping Core._Time

-- | Whether the base model used for the custom language model is up to date.
-- If this field is @true@ then you are running the most up-to-date version
-- of the base model in your custom language model.
languageModel_upgradeAvailability :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.Bool)
languageModel_upgradeAvailability = Lens.lens (\LanguageModel' {upgradeAvailability} -> upgradeAvailability) (\s@LanguageModel' {} a -> s {upgradeAvailability = a} :: LanguageModel)

-- | The data access role and Amazon S3 prefixes for the input files used to
-- train the custom language model.
languageModel_inputDataConfig :: Lens.Lens' LanguageModel (Prelude.Maybe InputDataConfig)
languageModel_inputDataConfig = Lens.lens (\LanguageModel' {inputDataConfig} -> inputDataConfig) (\s@LanguageModel' {} a -> s {inputDataConfig = a} :: LanguageModel)

-- | The Amazon Transcribe standard language model, or base model used to
-- create the custom language model.
languageModel_baseModelName :: Lens.Lens' LanguageModel (Prelude.Maybe BaseModelName)
languageModel_baseModelName = Lens.lens (\LanguageModel' {baseModelName} -> baseModelName) (\s@LanguageModel' {} a -> s {baseModelName = a} :: LanguageModel)

-- | The creation status of a custom language model. When the status is
-- @COMPLETED@ the model is ready for use.
languageModel_modelStatus :: Lens.Lens' LanguageModel (Prelude.Maybe ModelStatus)
languageModel_modelStatus = Lens.lens (\LanguageModel' {modelStatus} -> modelStatus) (\s@LanguageModel' {} a -> s {modelStatus = a} :: LanguageModel)

-- | The time the custom language model was created.
languageModel_createTime :: Lens.Lens' LanguageModel (Prelude.Maybe Prelude.UTCTime)
languageModel_createTime = Lens.lens (\LanguageModel' {createTime} -> createTime) (\s@LanguageModel' {} a -> s {createTime = a} :: LanguageModel) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LanguageModel where
  parseJSON =
    Core.withObject
      "LanguageModel"
      ( \x ->
          LanguageModel'
            Prelude.<$> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "ModelName")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "UpgradeAvailability")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "BaseModelName")
            Prelude.<*> (x Core..:? "ModelStatus")
            Prelude.<*> (x Core..:? "CreateTime")
      )

instance Prelude.Hashable LanguageModel

instance Prelude.NFData LanguageModel
