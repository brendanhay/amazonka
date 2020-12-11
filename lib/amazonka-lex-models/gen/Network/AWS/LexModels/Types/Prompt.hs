-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.Prompt
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.Prompt
  ( Prompt (..),

    -- * Smart constructor
    mkPrompt,

    -- * Lenses
    pResponseCard,
    pMessages,
    pMaxAttempts,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Message
import qualified Network.AWS.Prelude as Lude

-- | Obtains information from the user. To define a prompt, provide one or more messages and specify the number of attempts to get information from the user. If you provide more than one message, Amazon Lex chooses one of the messages to use to prompt the user. For more information, see 'how-it-works' .
--
-- /See:/ 'mkPrompt' smart constructor.
data Prompt = Prompt'
  { responseCard :: Lude.Maybe Lude.Text,
    messages :: Lude.NonEmpty Message,
    maxAttempts :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Prompt' with the minimum fields required to make a request.
--
-- * 'maxAttempts' - The number of times to prompt the user for information.
-- * 'messages' - An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
-- * 'responseCard' - A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
mkPrompt ::
  -- | 'messages'
  Lude.NonEmpty Message ->
  -- | 'maxAttempts'
  Lude.Natural ->
  Prompt
mkPrompt pMessages_ pMaxAttempts_ =
  Prompt'
    { responseCard = Lude.Nothing,
      messages = pMessages_,
      maxAttempts = pMaxAttempts_
    }

-- | A response card. Amazon Lex uses this prompt at runtime, in the @PostText@ API response. It substitutes session attributes and slot values for placeholders in the response card. For more information, see 'ex-resp-card' .
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pResponseCard :: Lens.Lens' Prompt (Lude.Maybe Lude.Text)
pResponseCard = Lens.lens (responseCard :: Prompt -> Lude.Maybe Lude.Text) (\s a -> s {responseCard = a} :: Prompt)
{-# DEPRECATED pResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

-- | An array of objects, each of which provides a message string and its type. You can specify the message string in plain text or in Speech Synthesis Markup Language (SSML).
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMessages :: Lens.Lens' Prompt (Lude.NonEmpty Message)
pMessages = Lens.lens (messages :: Prompt -> Lude.NonEmpty Message) (\s a -> s {messages = a} :: Prompt)
{-# DEPRECATED pMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The number of times to prompt the user for information.
--
-- /Note:/ Consider using 'maxAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pMaxAttempts :: Lens.Lens' Prompt Lude.Natural
pMaxAttempts = Lens.lens (maxAttempts :: Prompt -> Lude.Natural) (\s a -> s {maxAttempts = a} :: Prompt)
{-# DEPRECATED pMaxAttempts "Use generic-lens or generic-optics with 'maxAttempts' instead." #-}

instance Lude.FromJSON Prompt where
  parseJSON =
    Lude.withObject
      "Prompt"
      ( \x ->
          Prompt'
            Lude.<$> (x Lude..:? "responseCard")
            Lude.<*> (x Lude..: "messages")
            Lude.<*> (x Lude..: "maxAttempts")
      )

instance Lude.ToJSON Prompt where
  toJSON Prompt' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("responseCard" Lude..=) Lude.<$> responseCard,
            Lude.Just ("messages" Lude..= messages),
            Lude.Just ("maxAttempts" Lude..= maxAttempts)
          ]
      )
