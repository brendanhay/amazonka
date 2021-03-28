{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ResourceError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.ResourceError
  ( ResourceError (..)
  -- * Smart constructor
  , mkResourceError
  -- * Lenses
  , reErrorCode
  , reErrorMessage
  , reErrorTimestamp
  ) where

import qualified Network.AWS.AppStream.Types.FleetErrorCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a resource error.
--
-- /See:/ 'mkResourceError' smart constructor.
data ResourceError = ResourceError'
  { errorCode :: Core.Maybe Types.FleetErrorCode
    -- ^ The error code.
  , errorMessage :: Core.Maybe Core.Text
    -- ^ The error message.
  , errorTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time the error occurred.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResourceError' value with any optional fields omitted.
mkResourceError
    :: ResourceError
mkResourceError
  = ResourceError'{errorCode = Core.Nothing,
                   errorMessage = Core.Nothing, errorTimestamp = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorCode :: Lens.Lens' ResourceError (Core.Maybe Types.FleetErrorCode)
reErrorCode = Lens.field @"errorCode"
{-# INLINEABLE reErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorMessage :: Lens.Lens' ResourceError (Core.Maybe Core.Text)
reErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE reErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The time the error occurred.
--
-- /Note:/ Consider using 'errorTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reErrorTimestamp :: Lens.Lens' ResourceError (Core.Maybe Core.NominalDiffTime)
reErrorTimestamp = Lens.field @"errorTimestamp"
{-# INLINEABLE reErrorTimestamp #-}
{-# DEPRECATED errorTimestamp "Use generic-lens or generic-optics with 'errorTimestamp' instead"  #-}

instance Core.FromJSON ResourceError where
        parseJSON
          = Core.withObject "ResourceError" Core.$
              \ x ->
                ResourceError' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "ErrorTimestamp"
