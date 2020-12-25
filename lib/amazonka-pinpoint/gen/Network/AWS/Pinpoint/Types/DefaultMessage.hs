{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.DefaultMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.DefaultMessage
  ( DefaultMessage (..),

    -- * Smart constructor
    mkDefaultMessage,

    -- * Lenses
    dmBody,
    dmSubstitutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the default message for all channels.
--
-- /See:/ 'mkDefaultMessage' smart constructor.
data DefaultMessage = DefaultMessage'
  { -- | The default body of the message.
    body :: Core.Maybe Core.Text,
    -- | The default message variables to use in the message. You can override these default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DefaultMessage' value with any optional fields omitted.
mkDefaultMessage ::
  DefaultMessage
mkDefaultMessage =
  DefaultMessage'
    { body = Core.Nothing,
      substitutions = Core.Nothing
    }

-- | The default body of the message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmBody :: Lens.Lens' DefaultMessage (Core.Maybe Core.Text)
dmBody = Lens.field @"body"
{-# DEPRECATED dmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The default message variables to use in the message. You can override these default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmSubstitutions :: Lens.Lens' DefaultMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
dmSubstitutions = Lens.field @"substitutions"
{-# DEPRECATED dmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

instance Core.FromJSON DefaultMessage where
  toJSON DefaultMessage {..} =
    Core.object
      ( Core.catMaybes
          [ ("Body" Core..=) Core.<$> body,
            ("Substitutions" Core..=) Core.<$> substitutions
          ]
      )
