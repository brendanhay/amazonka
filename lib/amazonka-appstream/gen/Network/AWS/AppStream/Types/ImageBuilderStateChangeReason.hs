{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderStateChangeReason
  ( ImageBuilderStateChangeReason (..),

    -- * Smart constructor
    mkImageBuilderStateChangeReason,

    -- * Lenses
    ibscrCode,
    ibscrMessage,
  )
where

import qualified Network.AWS.AppStream.Types.ImageBuilderStateChangeReasonCode as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the reason why the last image builder state change occurred.
--
-- /See:/ 'mkImageBuilderStateChangeReason' smart constructor.
data ImageBuilderStateChangeReason = ImageBuilderStateChangeReason'
  { -- | The state change reason code.
    code :: Core.Maybe Types.ImageBuilderStateChangeReasonCode,
    -- | The state change reason message.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageBuilderStateChangeReason' value with any optional fields omitted.
mkImageBuilderStateChangeReason ::
  ImageBuilderStateChangeReason
mkImageBuilderStateChangeReason =
  ImageBuilderStateChangeReason'
    { code = Core.Nothing,
      message = Core.Nothing
    }

-- | The state change reason code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibscrCode :: Lens.Lens' ImageBuilderStateChangeReason (Core.Maybe Types.ImageBuilderStateChangeReasonCode)
ibscrCode = Lens.field @"code"
{-# DEPRECATED ibscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The state change reason message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibscrMessage :: Lens.Lens' ImageBuilderStateChangeReason (Core.Maybe Types.String)
ibscrMessage = Lens.field @"message"
{-# DEPRECATED ibscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ImageBuilderStateChangeReason where
  parseJSON =
    Core.withObject "ImageBuilderStateChangeReason" Core.$
      \x ->
        ImageBuilderStateChangeReason'
          Core.<$> (x Core..:? "Code") Core.<*> (x Core..:? "Message")
