{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneyEmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyEmailMessage
  ( JourneyEmailMessage (..),

    -- * Smart constructor
    mkJourneyEmailMessage,

    -- * Lenses
    jemFromAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the "From" address for an email message that's sent to participants in a journey.
--
-- /See:/ 'mkJourneyEmailMessage' smart constructor.
newtype JourneyEmailMessage = JourneyEmailMessage'
  { -- | The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
    fromAddress :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'JourneyEmailMessage' value with any optional fields omitted.
mkJourneyEmailMessage ::
  JourneyEmailMessage
mkJourneyEmailMessage =
  JourneyEmailMessage' {fromAddress = Core.Nothing}

-- | The verified email address to send the email message from. The default address is the FromAddress specified for the email channel for the application.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jemFromAddress :: Lens.Lens' JourneyEmailMessage (Core.Maybe Core.Text)
jemFromAddress = Lens.field @"fromAddress"
{-# DEPRECATED jemFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

instance Core.FromJSON JourneyEmailMessage where
  toJSON JourneyEmailMessage {..} =
    Core.object
      (Core.catMaybes [("FromAddress" Core..=) Core.<$> fromAddress])

instance Core.FromJSON JourneyEmailMessage where
  parseJSON =
    Core.withObject "JourneyEmailMessage" Core.$
      \x -> JourneyEmailMessage' Core.<$> (x Core..:? "FromAddress")
