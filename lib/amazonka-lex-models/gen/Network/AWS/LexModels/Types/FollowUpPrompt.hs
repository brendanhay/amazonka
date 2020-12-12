{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.FollowUpPrompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.FollowUpPrompt
  ( FollowUpPrompt (..),

    -- * Smart constructor
    mkFollowUpPrompt,

    -- * Lenses
    fupPrompt,
    fupRejectionStatement,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Prompt
import Network.AWS.LexModels.Types.Statement
import qualified Network.AWS.Prelude as Lude

-- | A prompt for additional activity after an intent is fulfilled. For example, after the @OrderPizza@ intent is fulfilled, you might prompt the user to find out whether the user wants to order drinks.
--
-- /See:/ 'mkFollowUpPrompt' smart constructor.
data FollowUpPrompt = FollowUpPrompt'
  { prompt :: Prompt,
    rejectionStatement :: Statement
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FollowUpPrompt' with the minimum fields required to make a request.
--
-- * 'prompt' - Prompts for information from the user.
-- * 'rejectionStatement' - If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
mkFollowUpPrompt ::
  -- | 'prompt'
  Prompt ->
  -- | 'rejectionStatement'
  Statement ->
  FollowUpPrompt
mkFollowUpPrompt pPrompt_ pRejectionStatement_ =
  FollowUpPrompt'
    { prompt = pPrompt_,
      rejectionStatement = pRejectionStatement_
    }

-- | Prompts for information from the user.
--
-- /Note:/ Consider using 'prompt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fupPrompt :: Lens.Lens' FollowUpPrompt Prompt
fupPrompt = Lens.lens (prompt :: FollowUpPrompt -> Prompt) (\s a -> s {prompt = a} :: FollowUpPrompt)
{-# DEPRECATED fupPrompt "Use generic-lens or generic-optics with 'prompt' instead." #-}

-- | If the user answers "no" to the question defined in the @prompt@ field, Amazon Lex responds with this statement to acknowledge that the intent was canceled.
--
-- /Note:/ Consider using 'rejectionStatement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fupRejectionStatement :: Lens.Lens' FollowUpPrompt Statement
fupRejectionStatement = Lens.lens (rejectionStatement :: FollowUpPrompt -> Statement) (\s a -> s {rejectionStatement = a} :: FollowUpPrompt)
{-# DEPRECATED fupRejectionStatement "Use generic-lens or generic-optics with 'rejectionStatement' instead." #-}

instance Lude.FromJSON FollowUpPrompt where
  parseJSON =
    Lude.withObject
      "FollowUpPrompt"
      ( \x ->
          FollowUpPrompt'
            Lude.<$> (x Lude..: "prompt") Lude.<*> (x Lude..: "rejectionStatement")
      )

instance Lude.ToJSON FollowUpPrompt where
  toJSON FollowUpPrompt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("prompt" Lude..= prompt),
            Lude.Just ("rejectionStatement" Lude..= rejectionStatement)
          ]
      )
