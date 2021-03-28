{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLContainerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AutoMLContainerDefinition
  ( AutoMLContainerDefinition (..)
  -- * Smart constructor
  , mkAutoMLContainerDefinition
  -- * Lenses
  , amlcdImage
  , amlcdModelDataUrl
  , amlcdEnvironment
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContainerImage as Types
import qualified Network.AWS.SageMaker.Types.EnvironmentKey as Types
import qualified Network.AWS.SageMaker.Types.EnvironmentValue as Types
import qualified Network.AWS.SageMaker.Types.ModelDataUrl as Types

-- | A list of container definitions that describe the different containers that make up one AutoML candidate. Refer to ContainerDefinition for more details.
--
-- /See:/ 'mkAutoMLContainerDefinition' smart constructor.
data AutoMLContainerDefinition = AutoMLContainerDefinition'
  { image :: Types.ContainerImage
    -- ^ The ECR path of the container. Refer to ContainerDefinition for more details.
  , modelDataUrl :: Types.ModelDataUrl
    -- ^ The location of the model artifacts. Refer to ContainerDefinition for more details.
  , environment :: Core.Maybe (Core.HashMap Types.EnvironmentKey Types.EnvironmentValue)
    -- ^ Environment variables to set in the container. Refer to ContainerDefinition for more details.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoMLContainerDefinition' value with any optional fields omitted.
mkAutoMLContainerDefinition
    :: Types.ContainerImage -- ^ 'image'
    -> Types.ModelDataUrl -- ^ 'modelDataUrl'
    -> AutoMLContainerDefinition
mkAutoMLContainerDefinition image modelDataUrl
  = AutoMLContainerDefinition'{image, modelDataUrl,
                               environment = Core.Nothing}

-- | The ECR path of the container. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdImage :: Lens.Lens' AutoMLContainerDefinition Types.ContainerImage
amlcdImage = Lens.field @"image"
{-# INLINEABLE amlcdImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The location of the model artifacts. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'modelDataUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdModelDataUrl :: Lens.Lens' AutoMLContainerDefinition Types.ModelDataUrl
amlcdModelDataUrl = Lens.field @"modelDataUrl"
{-# INLINEABLE amlcdModelDataUrl #-}
{-# DEPRECATED modelDataUrl "Use generic-lens or generic-optics with 'modelDataUrl' instead"  #-}

-- | Environment variables to set in the container. Refer to ContainerDefinition for more details.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlcdEnvironment :: Lens.Lens' AutoMLContainerDefinition (Core.Maybe (Core.HashMap Types.EnvironmentKey Types.EnvironmentValue))
amlcdEnvironment = Lens.field @"environment"
{-# INLINEABLE amlcdEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

instance Core.FromJSON AutoMLContainerDefinition where
        parseJSON
          = Core.withObject "AutoMLContainerDefinition" Core.$
              \ x ->
                AutoMLContainerDefinition' Core.<$>
                  (x Core..: "Image") Core.<*> x Core..: "ModelDataUrl" Core.<*>
                    x Core..:? "Environment"
