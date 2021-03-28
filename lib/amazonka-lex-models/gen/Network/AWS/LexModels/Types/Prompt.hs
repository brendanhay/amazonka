{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Prompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.Prompt
  ( Prompt (..)
  -- * Smart constructor
  , mkPrompt
  -- * Lenses
  , pMessages
  , pMaxAttempts
  , pResponseCard
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Message as Types
import qualified Network.AWS.LexModels.Types.ResponseCard as Types
import qualified Network.AWS.Prelude as Core

-- | Obtains information from the user. To define a prompt, provide one or more messages and specify the number of attempts to get information from the user. If you provide more than one message, Amazon Lex chooses one of the messages to use to prompt the user. For more information, see 'how-it-works' .
--
-- /See:/ 'mkPrompt' smart constructor.
data Prompt = Prompt'
  { messages :: Core.NonEmpty Types.Message
    -- ^ An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
  , maxAttempts :: Core.Natural
    -- ^ The number of times to prompt the user for information.
  , responseCard :: Core.Maybe Types.ResponseCard
    -- ^ A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Prompt' value with any optional fields omitted.
mkPrompt
    :: Core.NonEmpty Types.Message -- ^ 'messages'
    -> Core.Natural -- ^ 'maxAttempts'
    -> Prompt
mkPrompt messages maxAttempts
  = Prompt'{messages, maxAttempts, responseCard = Core.Nothing}

-- | An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessages :: Lens.Lens' Prompt (Core.NonEmpty Types.Message)
pMessages = Lens.field @"messages"
{-# INLINEABLE pMessages #-}
{-# DEPRECATED messages "Use generic-lens or generic-optics with 'messages' instead"  #-}

-- | The number of times to prompt the user for information.
--
-- /Note:/ Consider using 'maxAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMaxAttempts :: Lens.Lens' Prompt Core.Natural
pMaxAttempts = Lens.field @"maxAttempts"
{-# INLINEABLE pMaxAttempts #-}
{-# DEPRECATED maxAttempts "Use generic-lens or generic-optics with 'maxAttempts' instead"  #-}

-- | A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' . 
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResponseCard :: Lens.Lens' Prompt (Core.Maybe Types.ResponseCard)
pResponseCard = Lens.field @"responseCard"
{-# INLINEABLE pResponseCard #-}
{-# DEPRECATED responseCard "Use generic-lens or generic-optics with 'responseCard' instead"  #-}

instance Core.FromJSON Prompt where
        toJSON Prompt{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("messages" Core..= messages),
                  Core.Just ("maxAttempts" Core..= maxAttempts),
                  ("responseCard" Core..=) Core.<$> responseCard])

instance Core.FromJSON Prompt where
        parseJSON
          = Core.withObject "Prompt" Core.$
              \ x ->
                Prompt' Core.<$>
                  (x Core..: "messages") Core.<*> x Core..: "maxAttempts" Core.<*>
                    x Core..:? "responseCard"
