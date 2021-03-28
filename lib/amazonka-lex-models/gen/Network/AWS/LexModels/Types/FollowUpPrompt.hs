{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FollowUpPrompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.FollowUpPrompt
  ( FollowUpPrompt (..)
  -- * Smart constructor
  , mkFollowUpPrompt
  -- * Lenses
  , fupPrompt
  , fupRejectionStatement
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Prompt as Types
import qualified Network.AWS.LexModels.Types.Statement as Types
import qualified Network.AWS.Prelude as Core

-- | A prompt for additional activity after an intent is fulfilled. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to find out whether the user wants to order drinks.
--
-- /See:/ 'mkFollowUpPrompt' smart constructor.
data FollowUpPrompt = FollowUpPrompt'
  { prompt :: Types.Prompt
    -- ^ Prompts for information from the user. 
  , rejectionStatement :: Types.Statement
    -- ^ If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FollowUpPrompt' value with any optional fields omitted.
mkFollowUpPrompt
    :: Types.Prompt -- ^ 'prompt'
    -> Types.Statement -- ^ 'rejectionStatement'
    -> FollowUpPrompt
mkFollowUpPrompt prompt rejectionStatement
  = FollowUpPrompt'{prompt, rejectionStatement}

-- | Prompts for information from the user. 
--
-- /Note:/ Consider using 'prompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fupPrompt :: Lens.Lens' FollowUpPrompt Types.Prompt
fupPrompt = Lens.field @"prompt"
{-# INLINEABLE fupPrompt #-}
{-# DEPRECATED prompt "Use generic-lens or generic-optics with 'prompt' instead"  #-}

-- | If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled. 
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fupRejectionStatement :: Lens.Lens' FollowUpPrompt Types.Statement
fupRejectionStatement = Lens.field @"rejectionStatement"
{-# INLINEABLE fupRejectionStatement #-}
{-# DEPRECATED rejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead"  #-}

instance Core.FromJSON FollowUpPrompt where
        toJSON FollowUpPrompt{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("prompt" Core..= prompt),
                  Core.Just ("rejectionStatement" Core..= rejectionStatement)])

instance Core.FromJSON FollowUpPrompt where
        parseJSON
          = Core.withObject "FollowUpPrompt" Core.$
              \ x ->
                FollowUpPrompt' Core.<$>
                  (x Core..: "prompt") Core.<*> x Core..: "rejectionStatement"
