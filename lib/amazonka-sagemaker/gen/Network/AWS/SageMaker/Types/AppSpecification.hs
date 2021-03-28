{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AppSpecification
  ( AppSpecification (..)
  -- * Smart constructor
  , mkAppSpecification
  -- * Lenses
  , asImageUri
  , asContainerArguments
  , asContainerEntrypoint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ContainerArgument as Types
import qualified Network.AWS.SageMaker.Types.ContainerEntrypointString as Types
import qualified Network.AWS.SageMaker.Types.ImageUri as Types

-- | Configuration to run a processing job in a specified container image.
--
-- /See:/ 'mkAppSpecification' smart constructor.
data AppSpecification = AppSpecification'
  { imageUri :: Types.ImageUri
    -- ^ The container image to be run by the processing job.
  , containerArguments :: Core.Maybe (Core.NonEmpty Types.ContainerArgument)
    -- ^ The arguments for a container used to run a processing job.
  , containerEntrypoint :: Core.Maybe (Core.NonEmpty Types.ContainerEntrypointString)
    -- ^ The entrypoint for a container used to run a processing job.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppSpecification' value with any optional fields omitted.
mkAppSpecification
    :: Types.ImageUri -- ^ 'imageUri'
    -> AppSpecification
mkAppSpecification imageUri
  = AppSpecification'{imageUri, containerArguments = Core.Nothing,
                      containerEntrypoint = Core.Nothing}

-- | The container image to be run by the processing job.
--
-- /Note:/ Consider using 'imageUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asImageUri :: Lens.Lens' AppSpecification Types.ImageUri
asImageUri = Lens.field @"imageUri"
{-# INLINEABLE asImageUri #-}
{-# DEPRECATED imageUri "Use generic-lens or generic-optics with 'imageUri' instead"  #-}

-- | The arguments for a container used to run a processing job.
--
-- /Note:/ Consider using 'containerArguments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asContainerArguments :: Lens.Lens' AppSpecification (Core.Maybe (Core.NonEmpty Types.ContainerArgument))
asContainerArguments = Lens.field @"containerArguments"
{-# INLINEABLE asContainerArguments #-}
{-# DEPRECATED containerArguments "Use generic-lens or generic-optics with 'containerArguments' instead"  #-}

-- | The entrypoint for a container used to run a processing job.
--
-- /Note:/ Consider using 'containerEntrypoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asContainerEntrypoint :: Lens.Lens' AppSpecification (Core.Maybe (Core.NonEmpty Types.ContainerEntrypointString))
asContainerEntrypoint = Lens.field @"containerEntrypoint"
{-# INLINEABLE asContainerEntrypoint #-}
{-# DEPRECATED containerEntrypoint "Use generic-lens or generic-optics with 'containerEntrypoint' instead"  #-}

instance Core.FromJSON AppSpecification where
        toJSON AppSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ImageUri" Core..= imageUri),
                  ("ContainerArguments" Core..=) Core.<$> containerArguments,
                  ("ContainerEntrypoint" Core..=) Core.<$> containerEntrypoint])

instance Core.FromJSON AppSpecification where
        parseJSON
          = Core.withObject "AppSpecification" Core.$
              \ x ->
                AppSpecification' Core.<$>
                  (x Core..: "ImageUri") Core.<*> x Core..:? "ContainerArguments"
                    Core.<*> x Core..:? "ContainerEntrypoint"
