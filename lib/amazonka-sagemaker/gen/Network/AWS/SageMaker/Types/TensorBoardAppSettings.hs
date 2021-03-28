{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TensorBoardAppSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.TensorBoardAppSettings
  ( TensorBoardAppSettings (..)
  -- * Smart constructor
  , mkTensorBoardAppSettings
  -- * Lenses
  , tbasDefaultResourceSpec
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ResourceSpec as Types

-- | The TensorBoard app settings.
--
-- /See:/ 'mkTensorBoardAppSettings' smart constructor.
newtype TensorBoardAppSettings = TensorBoardAppSettings'
  { defaultResourceSpec :: Core.Maybe Types.ResourceSpec
    -- ^ The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TensorBoardAppSettings' value with any optional fields omitted.
mkTensorBoardAppSettings
    :: TensorBoardAppSettings
mkTensorBoardAppSettings
  = TensorBoardAppSettings'{defaultResourceSpec = Core.Nothing}

-- | The default instance type and the Amazon Resource Name (ARN) of the SageMaker image created on the instance.
--
-- /Note:/ Consider using 'defaultResourceSpec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tbasDefaultResourceSpec :: Lens.Lens' TensorBoardAppSettings (Core.Maybe Types.ResourceSpec)
tbasDefaultResourceSpec = Lens.field @"defaultResourceSpec"
{-# INLINEABLE tbasDefaultResourceSpec #-}
{-# DEPRECATED defaultResourceSpec "Use generic-lens or generic-optics with 'defaultResourceSpec' instead"  #-}

instance Core.FromJSON TensorBoardAppSettings where
        toJSON TensorBoardAppSettings{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultResourceSpec" Core..=) Core.<$> defaultResourceSpec])

instance Core.FromJSON TensorBoardAppSettings where
        parseJSON
          = Core.withObject "TensorBoardAppSettings" Core.$
              \ x ->
                TensorBoardAppSettings' Core.<$> (x Core..:? "DefaultResourceSpec")
