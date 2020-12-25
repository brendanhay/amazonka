{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageStateChangeReason
  ( ImageStateChangeReason (..),

    -- * Smart constructor
    mkImageStateChangeReason,

    -- * Lenses
    iscrCode,
    iscrMessage,
  )
where

import qualified Network.AWS.AppStream.Types.ImageStateChangeReasonCode as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the reason why the last image state change occurred.
--
-- /See:/ 'mkImageStateChangeReason' smart constructor.
data ImageStateChangeReason = ImageStateChangeReason'
  { -- | The state change reason code.
    code :: Core.Maybe Types.ImageStateChangeReasonCode,
    -- | The state change reason message.
    message :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ImageStateChangeReason' value with any optional fields omitted.
mkImageStateChangeReason ::
  ImageStateChangeReason
mkImageStateChangeReason =
  ImageStateChangeReason'
    { code = Core.Nothing,
      message = Core.Nothing
    }

-- | The state change reason code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrCode :: Lens.Lens' ImageStateChangeReason (Core.Maybe Types.ImageStateChangeReasonCode)
iscrCode = Lens.field @"code"
{-# DEPRECATED iscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The state change reason message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrMessage :: Lens.Lens' ImageStateChangeReason (Core.Maybe Types.String)
iscrMessage = Lens.field @"message"
{-# DEPRECATED iscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON ImageStateChangeReason where
  parseJSON =
    Core.withObject "ImageStateChangeReason" Core.$
      \x ->
        ImageStateChangeReason'
          Core.<$> (x Core..:? "Code") Core.<*> (x Core..:? "Message")
