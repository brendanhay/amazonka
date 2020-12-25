{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types.FailedResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types.FailedResource
  ( FailedResource (..),

    -- * Smart constructor
    mkFailedResource,

    -- * Lenses
    frErrorCode,
    frErrorMessage,
    frResourceArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ResourceGroups.Types.ErrorCode as Types
import qualified Network.AWS.ResourceGroups.Types.ErrorMessage as Types
import qualified Network.AWS.ResourceGroups.Types.ResourceArn as Types

-- | A resource that failed to be added to or removed from a group.
--
-- /See:/ 'mkFailedResource' smart constructor.
data FailedResource = FailedResource'
  { -- | The error code associated with the failure.
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | The error message text associated with the failure.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The ARN of the resource that failed to be added or removed.
    resourceArn :: Core.Maybe Types.ResourceArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailedResource' value with any optional fields omitted.
mkFailedResource ::
  FailedResource
mkFailedResource =
  FailedResource'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      resourceArn = Core.Nothing
    }

-- | The error code associated with the failure.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frErrorCode :: Lens.Lens' FailedResource (Core.Maybe Types.ErrorCode)
frErrorCode = Lens.field @"errorCode"
{-# DEPRECATED frErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message text associated with the failure.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frErrorMessage :: Lens.Lens' FailedResource (Core.Maybe Types.ErrorMessage)
frErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED frErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ARN of the resource that failed to be added or removed.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
frResourceArn :: Lens.Lens' FailedResource (Core.Maybe Types.ResourceArn)
frResourceArn = Lens.field @"resourceArn"
{-# DEPRECATED frResourceArn "Use generic-lens or generic-optics with 'resourceArn' instead." #-}

instance Core.FromJSON FailedResource where
  parseJSON =
    Core.withObject "FailedResource" Core.$
      \x ->
        FailedResource'
          Core.<$> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "ResourceArn")
