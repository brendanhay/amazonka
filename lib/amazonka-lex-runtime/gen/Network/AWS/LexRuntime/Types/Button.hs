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
import qualified Network.AWS.LexRuntime.Types.ButtonValueStringWithLength as Types
import qualified Network.AWS.LexRuntime.Types.Text as Types
import qualified Network.AWS.Prelude as Core

-- | Represents an option to be shown on the client platform (Facebook, Slack, etc.)
--
-- /See:/ 'mkButton' smart constructor.
data Button = Button'
  { -- | Text that is visible to the user on the button.
    text :: Types.Text,
    -- | The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
    value :: Types.ButtonValueStringWithLength
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Button' value with any optional fields omitted.
mkButton ::
  -- | 'text'
  Types.Text ->
  -- | 'value'
  Types.ButtonValueStringWithLength ->
  Button
mkButton text value = Button' {text, value}

-- | Text that is visible to the user on the button.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bText :: Lens.Lens' Button Types.Text
bText = Lens.field @"text"
{-# DEPRECATED bText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The value sent to Amazon Lex when a user chooses the button. For example, consider button text "NYC." When the user chooses the button, the value sent can be "New York City."
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bValue :: Lens.Lens' Button Types.ButtonValueStringWithLength
bValue = Lens.field @"value"
{-# DEPRECATED bValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Button where
  parseJSON =
    Core.withObject "Button" Core.$
      \x ->
        Button' Core.<$> (x Core..: "text") Core.<*> (x Core..: "value")
