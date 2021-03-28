{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ErrorDetails
  ( ErrorDetails (..)
  -- * Smart constructor
  , mkErrorDetails
  -- * Lenses
  , eErrorCode
  , eErrorMessage
  ) where

import qualified Network.AWS.Glue.Types.ErrorCode as Types
import qualified Network.AWS.Glue.Types.ErrorMessageString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object containing error details.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { errorCode :: Core.Maybe Types.ErrorCode
    -- ^ The error code for an error.
  , errorMessage :: Core.Maybe Types.ErrorMessageString
    -- ^ The error message for an error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetails' value with any optional fields omitted.
mkErrorDetails
    :: ErrorDetails
mkErrorDetails
  = ErrorDetails'{errorCode = Core.Nothing,
                  errorMessage = Core.Nothing}

-- | The error code for an error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eErrorCode :: Lens.Lens' ErrorDetails (Core.Maybe Types.ErrorCode)
eErrorCode = Lens.field @"errorCode"
{-# INLINEABLE eErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error message for an error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eErrorMessage :: Lens.Lens' ErrorDetails (Core.Maybe Types.ErrorMessageString)
eErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE eErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

instance Core.FromJSON ErrorDetails where
        parseJSON
          = Core.withObject "ErrorDetails" Core.$
              \ x ->
                ErrorDetails' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
