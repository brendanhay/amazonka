{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.StackError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.StackError
  ( StackError (..),

    -- * Smart constructor
    mkStackError,

    -- * Lenses
    seErrorCode,
    seErrorMessage,
  )
where

import qualified Network.AWS.AppStream.Types.StackErrorCode as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a stack error.
--
-- /See:/ 'mkStackError' smart constructor.
data StackError = StackError'
  { -- | The error code.
    errorCode :: Core.Maybe Types.StackErrorCode,
    -- | The error message.
    errorMessage :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StackError' value with any optional fields omitted.
mkStackError ::
  StackError
mkStackError =
  StackError'
    { errorCode = Core.Nothing,
      errorMessage = Core.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seErrorCode :: Lens.Lens' StackError (Core.Maybe Types.StackErrorCode)
seErrorCode = Lens.field @"errorCode"
{-# DEPRECATED seErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seErrorMessage :: Lens.Lens' StackError (Core.Maybe Types.String)
seErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED seErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Core.FromJSON StackError where
  parseJSON =
    Core.withObject "StackError" Core.$
      \x ->
        StackError'
          Core.<$> (x Core..:? "ErrorCode") Core.<*> (x Core..:? "ErrorMessage")
