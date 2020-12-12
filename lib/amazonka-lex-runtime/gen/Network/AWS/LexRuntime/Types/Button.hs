{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.Button
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.Button
  ( Button (..),

    -- * Smart constructor
    mkButton,

    -- * Lenses
    bText,
    bValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an option to be shown on the client platform (Facebook, Slack, etc.)
--
-- /See:/ 'mkButton' smart constructor.
data Button = Button' {text :: Lude.Text, value :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Button' with the minimum fields required to make a request.
--
-- * 'text' - Text that is visible to the user on the button.
-- * 'value' - The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
mkButton ::
  -- | 'text'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  Button
mkButton pText_ pValue_ = Button' {text = pText_, value = pValue_}

-- | Text that is visible to the user on the button.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bText :: Lens.Lens' Button Lude.Text
bText = Lens.lens (text :: Button -> Lude.Text) (\s a -> s {text = a} :: Button)
{-# DEPRECATED bText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Button Lude.Text
bValue = Lens.lens (value :: Button -> Lude.Text) (\s a -> s {value = a} :: Button)
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON Button where
  parseJSON =
    Lude.withObject
      "Button"
      ( \x ->
          Button' Lude.<$> (x Lude..: "text") Lude.<*> (x Lude..: "value")
      )
