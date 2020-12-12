{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Transcribe.Types.LanguageModel
  ( LanguageModel (..),

    -- * Smart constructor
    mkLanguageModel,

    -- * Lenses
    lmFailureReason,
    lmLanguageCode,
    lmModelName,
    lmLastModifiedTime,
    lmUpgradeAvailability,
    lmInputDataConfig,
    lmBaseModelName,
    lmModelStatus,
    lmCreateTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Transcribe.Types.BaseModelName
import Network.AWS.Transcribe.Types.CLMLanguageCode
import Network.AWS.Transcribe.Types.InputDataConfig
import Network.AWS.Transcribe.Types.ModelStatus

-- | The structure used to describe a custom language model.
--
-- /See:/ 'mkLanguageModel' smart constructor.
data LanguageModel = LanguageModel'
  { failureReason ::
      Lude.Maybe Lude.Text,
    languageCode :: Lude.Maybe CLMLanguageCode,
    modelName :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    upgradeAvailability :: Lude.Maybe Lude.Bool,
    inputDataConfig :: Lude.Maybe InputDataConfig,
    baseModelName :: Lude.Maybe BaseModelName,
    modelStatus :: Lude.Maybe ModelStatus,
    createTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LanguageModel' with the minimum fields required to make a request.
--
-- * 'baseModelName' - The Amazon Transcribe standard language model, or base model used to create the custom language model.
-- * 'createTime' - The time the custom language model was created.
-- * 'failureReason' - The reason why the custom language model couldn't be created.
-- * 'inputDataConfig' - The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
-- * 'languageCode' - The language code you used to create your custom language model.
-- * 'lastModifiedTime' - The most recent time the custom language model was modified.
-- * 'modelName' - The name of the custom language model.
-- * 'modelStatus' - The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
-- * 'upgradeAvailability' - Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
mkLanguageModel ::
  LanguageModel
mkLanguageModel =
  LanguageModel'
    { failureReason = Lude.Nothing,
      languageCode = Lude.Nothing,
      modelName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      upgradeAvailability = Lude.Nothing,
      inputDataConfig = Lude.Nothing,
      baseModelName = Lude.Nothing,
      modelStatus = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The reason why the custom language model couldn't be created.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmFailureReason :: Lens.Lens' LanguageModel (Lude.Maybe Lude.Text)
lmFailureReason = Lens.lens (failureReason :: LanguageModel -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: LanguageModel)
{-# DEPRECATED lmFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The language code you used to create your custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmLanguageCode :: Lens.Lens' LanguageModel (Lude.Maybe CLMLanguageCode)
lmLanguageCode = Lens.lens (languageCode :: LanguageModel -> Lude.Maybe CLMLanguageCode) (\s a -> s {languageCode = a} :: LanguageModel)
{-# DEPRECATED lmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The name of the custom language model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmModelName :: Lens.Lens' LanguageModel (Lude.Maybe Lude.Text)
lmModelName = Lens.lens (modelName :: LanguageModel -> Lude.Maybe Lude.Text) (\s a -> s {modelName = a} :: LanguageModel)
{-# DEPRECATED lmModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The most recent time the custom language model was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmLastModifiedTime :: Lens.Lens' LanguageModel (Lude.Maybe Lude.Timestamp)
lmLastModifiedTime = Lens.lens (lastModifiedTime :: LanguageModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: LanguageModel)
{-# DEPRECATED lmLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
--
-- /Note:/ Consider using 'upgradeAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmUpgradeAvailability :: Lens.Lens' LanguageModel (Lude.Maybe Lude.Bool)
lmUpgradeAvailability = Lens.lens (upgradeAvailability :: LanguageModel -> Lude.Maybe Lude.Bool) (\s a -> s {upgradeAvailability = a} :: LanguageModel)
{-# DEPRECATED lmUpgradeAvailability "Use generic-lens or generic-optics with 'upgradeAvailability' instead." #-}

-- | The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmInputDataConfig :: Lens.Lens' LanguageModel (Lude.Maybe InputDataConfig)
lmInputDataConfig = Lens.lens (inputDataConfig :: LanguageModel -> Lude.Maybe InputDataConfig) (\s a -> s {inputDataConfig = a} :: LanguageModel)
{-# DEPRECATED lmInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | The Amazon Transcribe standard language model, or base model used to create the custom language model.
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmBaseModelName :: Lens.Lens' LanguageModel (Lude.Maybe BaseModelName)
lmBaseModelName = Lens.lens (baseModelName :: LanguageModel -> Lude.Maybe BaseModelName) (\s a -> s {baseModelName = a} :: LanguageModel)
{-# DEPRECATED lmBaseModelName "Use generic-lens or generic-optics with 'baseModelName' instead." #-}

-- | The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
--
-- /Note:/ Consider using 'modelStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmModelStatus :: Lens.Lens' LanguageModel (Lude.Maybe ModelStatus)
lmModelStatus = Lens.lens (modelStatus :: LanguageModel -> Lude.Maybe ModelStatus) (\s a -> s {modelStatus = a} :: LanguageModel)
{-# DEPRECATED lmModelStatus "Use generic-lens or generic-optics with 'modelStatus' instead." #-}

-- | The time the custom language model was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreateTime :: Lens.Lens' LanguageModel (Lude.Maybe Lude.Timestamp)
lmCreateTime = Lens.lens (createTime :: LanguageModel -> Lude.Maybe Lude.Timestamp) (\s a -> s {createTime = a} :: LanguageModel)
{-# DEPRECATED lmCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromJSON LanguageModel where
  parseJSON =
    Lude.withObject
      "LanguageModel"
      ( \x ->
          LanguageModel'
            Lude.<$> (x Lude..:? "FailureReason")
            Lude.<*> (x Lude..:? "LanguageCode")
            Lude.<*> (x Lude..:? "ModelName")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "UpgradeAvailability")
            Lude.<*> (x Lude..:? "InputDataConfig")
            Lude.<*> (x Lude..:? "BaseModelName")
            Lude.<*> (x Lude..:? "ModelStatus")
            Lude.<*> (x Lude..:? "CreateTime")
      )
