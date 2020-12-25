{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ErrorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ErrorInfo
  ( ErrorInfo (..),

    -- * Smart constructor
    mkErrorInfo,

    -- * Lenses
    eiCode,
    eiMessage,
  )
where

import qualified Network.AWS.IoT.Types.Code as Types
import qualified Network.AWS.IoT.Types.OTAUpdateErrorMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Error information.
--
-- /See:/ 'mkErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | The error code.
    code :: Core.Maybe Types.Code,
    -- | The error message.
    message :: Core.Maybe Types.OTAUpdateErrorMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorInfo' value with any optional fields omitted.
mkErrorInfo ::
  ErrorInfo
mkErrorInfo =
  ErrorInfo' {code = Core.Nothing, message = Core.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiCode :: Lens.Lens' ErrorInfo (Core.Maybe Types.Code)
eiCode = Lens.field @"code"
{-# DEPRECATED eiCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiMessage :: Lens.Lens' ErrorInfo (Core.Maybe Types.OTAUpdateErrorMessage)
eiMessage = Lens.field @"message"
{-# DEPRECATED eiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ErrorInfo where
  parseJSON =
    Core.withObject "ErrorInfo" Core.$
      \x ->
        ErrorInfo'
          Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "message")
