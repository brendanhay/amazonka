{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ErrorDetail
  ( ErrorDetail (..)
  -- * Smart constructor
  , mkErrorDetail
  -- * Lenses
  , edErrorCode
  , edErrorMessage
  ) where

import qualified Network.AWS.Glue.Types.DescriptionString as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains details about an error.
--
-- /See:/ 'mkErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { errorCode :: Core.Maybe Types.NameString
    -- ^ The code associated with this error.
  , errorMessage :: Core.Maybe Types.DescriptionString
    -- ^ A message describing the error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetail' value with any optional fields omitted.
mkErrorDetail
    :: ErrorDetail
mkErrorDetail
  = ErrorDetail'{errorCode = Core.Nothing,
                 errorMessage = Core.Nothing}

-- | The code associated with this error.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorCode :: Lens.Lens' ErrorDetail (Core.Maybe Types.NameString)
edErrorCode = Lens.field @"errorCode"
{-# INLINEABLE edErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | A message describing the error.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edErrorMessage :: Lens.Lens' ErrorDetail (Core.Maybe Types.DescriptionString)
edErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE edErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

instance Core.FromJSON ErrorDetail where
        parseJSON
          = Core.withObject "ErrorDetail" Core.$
              \ x ->
                ErrorDetail' Core.<$>
                  (x Core..:? "ErrorCode") Core.<*> x Core..:? "ErrorMessage"
