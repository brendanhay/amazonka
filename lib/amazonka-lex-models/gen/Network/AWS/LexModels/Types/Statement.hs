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
    staResponseCard,
    staMessages,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.Message
import qualified Network.AWS.Prelude as Lude

-- | A collection of messages that convey information to the user. At runtime, Amazon Lex selects the message to convey.
--
-- /See:/ 'mkStatement' smart constructor.
data Statement = Statement'
  { responseCard :: Lude.Maybe Lude.Text,
    messages :: Lude.NonEmpty Message
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Statement' with the minimum fields required to make a request.
--
-- * 'messages' - A collection of message objects.
-- * 'responseCard' - At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
mkStatement ::
  -- | 'messages'
  Lude.NonEmpty Message ->
  Statement
mkStatement pMessages_ =
  Statement' {responseCard = Lude.Nothing, messages = pMessages_}

-- | At runtime, if the client is using the <http://docs.aws.amazon.com/lex/latest/dg/API_runtime_PostText.html PostText> API, Amazon Lex includes the response card in the response. It substitutes all of the session attributes and slot values for placeholders in the response card.
--
-- /Note:/ Consider using 'responseCard' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staResponseCard :: Lens.Lens' Statement (Lude.Maybe Lude.Text)
staResponseCard = Lens.lens (responseCard :: Statement -> Lude.Maybe Lude.Text) (\s a -> s {responseCard = a} :: Statement)
{-# DEPRECATED staResponseCard "Use generic-lens or generic-optics with 'responseCard' instead." #-}

-- | A collection of message objects.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
staMessages :: Lens.Lens' Statement (Lude.NonEmpty Message)
staMessages = Lens.lens (messages :: Statement -> Lude.NonEmpty Message) (\s a -> s {messages = a} :: Statement)
{-# DEPRECATED staMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

instance Lude.FromJSON Statement where
  parseJSON =
    Lude.withObject
      "Statement"
      ( \x ->
          Statement'
            Lude.<$> (x Lude..:? "responseCard") Lude.<*> (x Lude..: "messages")
      )

instance Lude.ToJSON Statement where
  toJSON Statement' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("responseCard" Lude..=) Lude.<$> responseCard,
            Lude.Just ("messages" Lude..= messages)
          ]
      )
