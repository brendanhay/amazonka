{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageFailure
  ( ImageFailure (..),

    -- * Smart constructor
    mkImageFailure,

    -- * Lenses
    ifFailureCode,
    ifFailureReason,
    ifImageId,
  )
where

import qualified Network.AWS.ECR.Types.FailureReason as Types
import qualified Network.AWS.ECR.Types.ImageFailureCode as Types
import qualified Network.AWS.ECR.Types.ImageIdentifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object representing an Amazon ECR image failure.
--
-- /See:/ 'mkImageFailure' smart constructor.
data ImageFailure = ImageFailure'
  { -- | The code associated with the failure.
    failureCode :: Core.Maybe Types.ImageFailureCode,
    -- | The reason for the failure.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | The image ID associated with the failure.
    imageId :: Core.Maybe Types.ImageIdentifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageFailure' value with any optional fields omitted.
mkImageFailure ::
  ImageFailure
mkImageFailure =
  ImageFailure'
    { failureCode = Core.Nothing,
      failureReason = Core.Nothing,
      imageId = Core.Nothing
    }

-- | The code associated with the failure.
--
-- /Note:/ Consider using 'failureCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifFailureCode :: Lens.Lens' ImageFailure (Core.Maybe Types.ImageFailureCode)
ifFailureCode = Lens.field @"failureCode"
{-# DEPRECATED ifFailureCode "Use generic-lens or generic-optics with 'failureCode' instead." #-}

-- | The reason for the failure.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifFailureReason :: Lens.Lens' ImageFailure (Core.Maybe Types.FailureReason)
ifFailureReason = Lens.field @"failureReason"
{-# DEPRECATED ifFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The image ID associated with the failure.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageId :: Lens.Lens' ImageFailure (Core.Maybe Types.ImageIdentifier)
ifImageId = Lens.field @"imageId"
{-# DEPRECATED ifImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Core.FromJSON ImageFailure where
  parseJSON =
    Core.withObject "ImageFailure" Core.$
      \x ->
        ImageFailure'
          Core.<$> (x Core..:? "failureCode")
          Core.<*> (x Core..:? "failureReason")
          Core.<*> (x Core..:? "imageId")
