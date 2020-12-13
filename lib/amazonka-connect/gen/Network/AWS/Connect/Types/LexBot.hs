{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.LexBot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.LexBot
  ( LexBot (..),

    -- * Smart constructor
    mkLexBot,

    -- * Lenses
    lbLexRegion,
    lbName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information of an Amazon Lex bot.
--
-- /See:/ 'mkLexBot' smart constructor.
data LexBot = LexBot'
  { -- | The Region the Amazon Lex bot was created in.
    lexRegion :: Lude.Maybe Lude.Text,
    -- | The name of the Amazon Lex bot.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LexBot' with the minimum fields required to make a request.
--
-- * 'lexRegion' - The Region the Amazon Lex bot was created in.
-- * 'name' - The name of the Amazon Lex bot.
mkLexBot ::
  LexBot
mkLexBot = LexBot' {lexRegion = Lude.Nothing, name = Lude.Nothing}

-- | The Region the Amazon Lex bot was created in.
--
-- /Note:/ Consider using 'lexRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLexRegion :: Lens.Lens' LexBot (Lude.Maybe Lude.Text)
lbLexRegion = Lens.lens (lexRegion :: LexBot -> Lude.Maybe Lude.Text) (\s a -> s {lexRegion = a} :: LexBot)
{-# DEPRECATED lbLexRegion "Use generic-lens or generic-optics with 'lexRegion' instead." #-}

-- | The name of the Amazon Lex bot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbName :: Lens.Lens' LexBot (Lude.Maybe Lude.Text)
lbName = Lens.lens (name :: LexBot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LexBot)
{-# DEPRECATED lbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON LexBot where
  parseJSON =
    Lude.withObject
      "LexBot"
      ( \x ->
          LexBot'
            Lude.<$> (x Lude..:? "LexRegion") Lude.<*> (x Lude..:? "Name")
      )

instance Lude.ToJSON LexBot where
  toJSON LexBot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LexRegion" Lude..=) Lude.<$> lexRegion,
            ("Name" Lude..=) Lude.<$> name
          ]
      )
