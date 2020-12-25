{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ErrorDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ErrorDetails
  ( ErrorDetails (..),

    -- * Smart constructor
    mkErrorDetails,

    -- * Lenses
    edCode,
    edMessage,
  )
where

import qualified Network.AWS.CodePipeline.Types.Code as Types
import qualified Network.AWS.CodePipeline.Types.Message as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about an error in AWS CodePipeline.
--
-- /See:/ 'mkErrorDetails' smart constructor.
data ErrorDetails = ErrorDetails'
  { -- | The system ID or number code of the error.
    code :: Core.Maybe Types.Code,
    -- | The text of the error message.
    message :: Core.Maybe Types.Message
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetails' value with any optional fields omitted.
mkErrorDetails ::
  ErrorDetails
mkErrorDetails =
  ErrorDetails' {code = Core.Nothing, message = Core.Nothing}

-- | The system ID or number code of the error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edCode :: Lens.Lens' ErrorDetails (Core.Maybe Types.Code)
edCode = Lens.field @"code"
{-# DEPRECATED edCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The text of the error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edMessage :: Lens.Lens' ErrorDetails (Core.Maybe Types.Message)
edMessage = Lens.field @"message"
{-# DEPRECATED edMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ErrorDetails where
  parseJSON =
    Core.withObject "ErrorDetails" Core.$
      \x ->
        ErrorDetails'
          Core.<$> (x Core..:? "code") Core.<*> (x Core..:? "message")
