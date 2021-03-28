{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
  ( ContainerDatasetAction (..)
  -- * Smart constructor
  , mkContainerDatasetAction
  -- * Lenses
  , cdaImage
  , cdaExecutionRoleArn
  , cdaResourceConfiguration
  , cdaVariables
  ) where

import qualified Network.AWS.IoTAnalytics.Types.ExecutionRoleArn as Types
import qualified Network.AWS.IoTAnalytics.Types.Image as Types
import qualified Network.AWS.IoTAnalytics.Types.ResourceConfiguration as Types
import qualified Network.AWS.IoTAnalytics.Types.Variable as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information required to run the @containerAction@ to produce dataset contents.
--
-- /See:/ 'mkContainerDatasetAction' smart constructor.
data ContainerDatasetAction = ContainerDatasetAction'
  { image :: Types.Image
    -- ^ The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
  , executionRoleArn :: Types.ExecutionRoleArn
    -- ^ The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
  , resourceConfiguration :: Types.ResourceConfiguration
    -- ^ Configuration of the resource that executes the @containerAction@ .
  , variables :: Core.Maybe [Types.Variable]
    -- ^ The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerDatasetAction' value with any optional fields omitted.
mkContainerDatasetAction
    :: Types.Image -- ^ 'image'
    -> Types.ExecutionRoleArn -- ^ 'executionRoleArn'
    -> Types.ResourceConfiguration -- ^ 'resourceConfiguration'
    -> ContainerDatasetAction
mkContainerDatasetAction image executionRoleArn
  resourceConfiguration
  = ContainerDatasetAction'{image, executionRoleArn,
                            resourceConfiguration, variables = Core.Nothing}

-- | The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
--
-- /Note:/ Consider using 'image' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaImage :: Lens.Lens' ContainerDatasetAction Types.Image
cdaImage = Lens.field @"image"
{-# INLINEABLE cdaImage #-}
{-# DEPRECATED image "Use generic-lens or generic-optics with 'image' instead"  #-}

-- | The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
--
-- /Note:/ Consider using 'executionRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaExecutionRoleArn :: Lens.Lens' ContainerDatasetAction Types.ExecutionRoleArn
cdaExecutionRoleArn = Lens.field @"executionRoleArn"
{-# INLINEABLE cdaExecutionRoleArn #-}
{-# DEPRECATED executionRoleArn "Use generic-lens or generic-optics with 'executionRoleArn' instead"  #-}

-- | Configuration of the resource that executes the @containerAction@ .
--
-- /Note:/ Consider using 'resourceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaResourceConfiguration :: Lens.Lens' ContainerDatasetAction Types.ResourceConfiguration
cdaResourceConfiguration = Lens.field @"resourceConfiguration"
{-# INLINEABLE cdaResourceConfiguration #-}
{-# DEPRECATED resourceConfiguration "Use generic-lens or generic-optics with 'resourceConfiguration' instead"  #-}

-- | The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdaVariables :: Lens.Lens' ContainerDatasetAction (Core.Maybe [Types.Variable])
cdaVariables = Lens.field @"variables"
{-# INLINEABLE cdaVariables #-}
{-# DEPRECATED variables "Use generic-lens or generic-optics with 'variables' instead"  #-}

instance Core.FromJSON ContainerDatasetAction where
        toJSON ContainerDatasetAction{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("image" Core..= image),
                  Core.Just ("executionRoleArn" Core..= executionRoleArn),
                  Core.Just ("resourceConfiguration" Core..= resourceConfiguration),
                  ("variables" Core..=) Core.<$> variables])

instance Core.FromJSON ContainerDatasetAction where
        parseJSON
          = Core.withObject "ContainerDatasetAction" Core.$
              \ x ->
                ContainerDatasetAction' Core.<$>
                  (x Core..: "image") Core.<*> x Core..: "executionRoleArn" Core.<*>
                    x Core..: "resourceConfiguration"
                    Core.<*> x Core..:? "variables"
