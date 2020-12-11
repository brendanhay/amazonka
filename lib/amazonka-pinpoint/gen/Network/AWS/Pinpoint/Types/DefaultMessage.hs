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
    dmSubstitutions,
    dmBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default message for all channels.
--
-- /See:/ 'mkDefaultMessage' smart constructor.
data DefaultMessage = DefaultMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    body :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DefaultMessage' with the minimum fields required to make a request.
--
-- * 'body' - The default body of the message.
-- * 'substitutions' - The default message variables to use in the message. You can override these default variables with individual address variables.
mkDefaultMessage ::
  DefaultMessage
mkDefaultMessage =
  DefaultMessage'
    { substitutions = Lude.Nothing,
      body = Lude.Nothing
    }

-- | The default message variables to use in the message. You can override these default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmSubstitutions :: Lens.Lens' DefaultMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
dmSubstitutions = Lens.lens (substitutions :: DefaultMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: DefaultMessage)
{-# DEPRECATED dmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The default body of the message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmBody :: Lens.Lens' DefaultMessage (Lude.Maybe Lude.Text)
dmBody = Lens.lens (body :: DefaultMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: DefaultMessage)
{-# DEPRECATED dmBody "Use generic-lens or generic-optics with 'body' instead." #-}

instance Lude.ToJSON DefaultMessage where
  toJSON DefaultMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("Body" Lude..=) Lude.<$> body
          ]
      )
