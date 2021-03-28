{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Greengrass.Types.SageMakerMachineLearningModelResourceData
  ( SageMakerMachineLearningModelResourceData (..)
  -- * Smart constructor
  , mkSageMakerMachineLearningModelResourceData
  -- * Lenses
  , smmlmrdDestinationPath
  , smmlmrdOwnerSetting
  , smmlmrdSageMakerJobArn
  ) where

import qualified Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Attributes that define an Amazon SageMaker machine learning resource.
--
-- /See:/ 'mkSageMakerMachineLearningModelResourceData' smart constructor.
data SageMakerMachineLearningModelResourceData = SageMakerMachineLearningModelResourceData'
  { destinationPath :: Core.Maybe Core.Text
    -- ^ The absolute local path of the resource inside the Lambda environment.
  , ownerSetting :: Core.Maybe Types.ResourceDownloadOwnerSetting
  , sageMakerJobArn :: Core.Maybe Core.Text
    -- ^ The ARN of the Amazon SageMaker training job that represents the source model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SageMakerMachineLearningModelResourceData' value with any optional fields omitted.
mkSageMakerMachineLearningModelResourceData
    :: SageMakerMachineLearningModelResourceData
mkSageMakerMachineLearningModelResourceData
  = SageMakerMachineLearningModelResourceData'{destinationPath =
                                                 Core.Nothing,
                                               ownerSetting = Core.Nothing,
                                               sageMakerJobArn = Core.Nothing}

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdDestinationPath :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe Core.Text)
smmlmrdDestinationPath = Lens.field @"destinationPath"
{-# INLINEABLE smmlmrdDestinationPath #-}
{-# DEPRECATED destinationPath "Use generic-lens or generic-optics with 'destinationPath' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdOwnerSetting :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe Types.ResourceDownloadOwnerSetting)
smmlmrdOwnerSetting = Lens.field @"ownerSetting"
{-# INLINEABLE smmlmrdOwnerSetting #-}
{-# DEPRECATED ownerSetting "Use generic-lens or generic-optics with 'ownerSetting' instead"  #-}

-- | The ARN of the Amazon SageMaker training job that represents the source model.
--
-- /Note:/ Consider using 'sageMakerJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smmlmrdSageMakerJobArn :: Lens.Lens' SageMakerMachineLearningModelResourceData (Core.Maybe Core.Text)
smmlmrdSageMakerJobArn = Lens.field @"sageMakerJobArn"
{-# INLINEABLE smmlmrdSageMakerJobArn #-}
{-# DEPRECATED sageMakerJobArn "Use generic-lens or generic-optics with 'sageMakerJobArn' instead"  #-}

instance Core.FromJSON SageMakerMachineLearningModelResourceData
         where
        toJSON SageMakerMachineLearningModelResourceData{..}
          = Core.object
              (Core.catMaybes
                 [("DestinationPath" Core..=) Core.<$> destinationPath,
                  ("OwnerSetting" Core..=) Core.<$> ownerSetting,
                  ("SageMakerJobArn" Core..=) Core.<$> sageMakerJobArn])

instance Core.FromJSON SageMakerMachineLearningModelResourceData
         where
        parseJSON
          = Core.withObject "SageMakerMachineLearningModelResourceData"
              Core.$
              \ x ->
                SageMakerMachineLearningModelResourceData' Core.<$>
                  (x Core..:? "DestinationPath") Core.<*> x Core..:? "OwnerSetting"
                    Core.<*> x Core..:? "SageMakerJobArn"
