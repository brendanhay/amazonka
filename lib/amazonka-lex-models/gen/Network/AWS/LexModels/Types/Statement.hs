{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Statement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Statement
  ( Statement (..),

    -- * Smart constructor
    mkStatement,

    -- * Lenses
    sMessages,
    sResponseCard,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Message as Types
import qualified Network.AWS.LexModels.Types.ResponseCard as Types
import qualified Network.AWS.Prelude as Core

-- | A collection of messages that convey information to the user. At runtime, Amazon Lex selects the message to convey.
--
-- /See:/ 'mkStatement' smart constructor.
data Statement = Statement'
  { -- | A collection of message objects.
    messages :: Core.NonEmpty Types.Message,
    -- | At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
    responseCard :: Core.Maybe Types.ResponseCard
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Statement' value with any optional fields omitted.
mkStatement ::
  -- | 'messages'
  Core.NonEmpty Types.Message ->
  Statement
mkStatement messages =
  Statement' {messages, responseCard = Core.Nothing}

-- | A collection of message objects.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sMessages :: Lens.Lens' Statement (Core.NonEmpty Types.Message)
sMessages = Lens.field @"messages"
{-# DEPRECATED sMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sResponseCard :: Lens.Lens' Statement (Core.Maybe Types.ResponseCard)
sResponseCard = Lens.field @"responseCard"
{-# DEPRECATED sResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

instance Core.FromJSON Statement where
  toJSON Statement {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("messages" Core..= messages),
            ("responseCard" Core..=) Core.<$> responseCard
          ]
      )

instance Core.FromJSON Statement where
  parseJSON =
    Core.withObject "Statement" Core.$
      \x ->
        Statement'
          Core.<$> (x Core..: "messages") Core.<*> (x Core..:? "responseCard")
