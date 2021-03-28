{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Transcribe.Types.LanguageModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Transcribe.Types.LanguageModel
  ( LanguageModel (..)
  -- * Smart constructor
  , mkLanguageModel
  -- * Lenses
  , lmBaseModelName
  , lmCreateTime
  , lmFailureReason
  , lmInputDataConfig
  , lmLanguageCode
  , lmLastModifiedTime
  , lmModelName
  , lmModelStatus
  , lmUpgradeAvailability
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Transcribe.Types.BaseModelName as Types
import qualified Network.AWS.Transcribe.Types.CLMLanguageCode as Types
import qualified Network.AWS.Transcribe.Types.FailureReason as Types
import qualified Network.AWS.Transcribe.Types.InputDataConfig as Types
import qualified Network.AWS.Transcribe.Types.ModelName as Types
import qualified Network.AWS.Transcribe.Types.ModelStatus as Types

-- | The structure used to describe a custom language model.
--
-- /See:/ 'mkLanguageModel' smart constructor.
data LanguageModel = LanguageModel'
  { baseModelName :: Core.Maybe Types.BaseModelName
    -- ^ The Amazon Transcribe standard language model, or base model used to create the custom language model.
  , createTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the custom language model was created.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The reason why the custom language model couldn't be created.
  , inputDataConfig :: Core.Maybe Types.InputDataConfig
    -- ^ The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
  , languageCode :: Core.Maybe Types.CLMLanguageCode
    -- ^ The language code you used to create your custom language model.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The most recent time the custom language model was modified.
  , modelName :: Core.Maybe Types.ModelName
    -- ^ The name of the custom language model.
  , modelStatus :: Core.Maybe Types.ModelStatus
    -- ^ The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
  , upgradeAvailability :: Core.Maybe Core.Bool
    -- ^ Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LanguageModel' value with any optional fields omitted.
mkLanguageModel
    :: LanguageModel
mkLanguageModel
  = LanguageModel'{baseModelName = Core.Nothing,
                   createTime = Core.Nothing, failureReason = Core.Nothing,
                   inputDataConfig = Core.Nothing, languageCode = Core.Nothing,
                   lastModifiedTime = Core.Nothing, modelName = Core.Nothing,
                   modelStatus = Core.Nothing, upgradeAvailability = Core.Nothing}

-- | The Amazon Transcribe standard language model, or base model used to create the custom language model.
--
-- /Note:/ Consider using 'baseModelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmBaseModelName :: Lens.Lens' LanguageModel (Core.Maybe Types.BaseModelName)
lmBaseModelName = Lens.field @"baseModelName"
{-# INLINEABLE lmBaseModelName #-}
{-# DEPRECATED baseModelName "Use generic-lens or generic-optics with 'baseModelName' instead"  #-}

-- | The time the custom language model was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreateTime :: Lens.Lens' LanguageModel (Core.Maybe Core.NominalDiffTime)
lmCreateTime = Lens.field @"createTime"
{-# INLINEABLE lmCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | The reason why the custom language model couldn't be created.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmFailureReason :: Lens.Lens' LanguageModel (Core.Maybe Types.FailureReason)
lmFailureReason = Lens.field @"failureReason"
{-# INLINEABLE lmFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The data access role and Amazon S3 prefixes for the input files used to train the custom language model.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmInputDataConfig :: Lens.Lens' LanguageModel (Core.Maybe Types.InputDataConfig)
lmInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE lmInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | The language code you used to create your custom language model.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmLanguageCode :: Lens.Lens' LanguageModel (Core.Maybe Types.CLMLanguageCode)
lmLanguageCode = Lens.field @"languageCode"
{-# INLINEABLE lmLanguageCode #-}
{-# DEPRECATED languageCode "Use generic-lens or generic-optics with 'languageCode' instead"  #-}

-- | The most recent time the custom language model was modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmLastModifiedTime :: Lens.Lens' LanguageModel (Core.Maybe Core.NominalDiffTime)
lmLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE lmLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The name of the custom language model.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmModelName :: Lens.Lens' LanguageModel (Core.Maybe Types.ModelName)
lmModelName = Lens.field @"modelName"
{-# INLINEABLE lmModelName #-}
{-# DEPRECATED modelName "Use generic-lens or generic-optics with 'modelName' instead"  #-}

-- | The creation status of a custom language model. When the status is @COMPLETED@ the model is ready for use.
--
-- /Note:/ Consider using 'modelStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmModelStatus :: Lens.Lens' LanguageModel (Core.Maybe Types.ModelStatus)
lmModelStatus = Lens.field @"modelStatus"
{-# INLINEABLE lmModelStatus #-}
{-# DEPRECATED modelStatus "Use generic-lens or generic-optics with 'modelStatus' instead"  #-}

-- | Whether the base model used for the custom language model is up to date. If this field is @true@ then you are running the most up-to-date version of the base model in your custom language model.
--
-- /Note:/ Consider using 'upgradeAvailability' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmUpgradeAvailability :: Lens.Lens' LanguageModel (Core.Maybe Core.Bool)
lmUpgradeAvailability = Lens.field @"upgradeAvailability"
{-# INLINEABLE lmUpgradeAvailability #-}
{-# DEPRECATED upgradeAvailability "Use generic-lens or generic-optics with 'upgradeAvailability' instead"  #-}

instance Core.FromJSON LanguageModel where
        parseJSON
          = Core.withObject "LanguageModel" Core.$
              \ x ->
                LanguageModel' Core.<$>
                  (x Core..:? "BaseModelName") Core.<*> x Core..:? "CreateTime"
                    Core.<*> x Core..:? "FailureReason"
                    Core.<*> x Core..:? "InputDataConfig"
                    Core.<*> x Core..:? "LanguageCode"
                    Core.<*> x Core..:? "LastModifiedTime"
                    Core.<*> x Core..:? "ModelName"
                    Core.<*> x Core..:? "ModelStatus"
                    Core.<*> x Core..:? "UpgradeAvailability"
