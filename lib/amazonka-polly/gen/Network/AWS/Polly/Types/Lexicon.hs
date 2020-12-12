{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Lexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.Lexicon
  ( Lexicon (..),

    -- * Smart constructor
    mkLexicon,

    -- * Lenses
    lContent,
    lName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides lexicon name and lexicon content in string format. For more information, see <https://www.w3.org/TR/pronunciation-lexicon/ Pronunciation Lexicon Specification (PLS) Version 1.0> .
--
-- /See:/ 'mkLexicon' smart constructor.
data Lexicon = Lexicon'
  { content ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Lexicon' with the minimum fields required to make a request.
--
-- * 'content' - Lexicon content in string format. The content of a lexicon must be in PLS format.
-- * 'name' - Name of the lexicon.
mkLexicon ::
  Lexicon
mkLexicon = Lexicon' {content = Lude.Nothing, name = Lude.Nothing}

-- | Lexicon content in string format. The content of a lexicon must be in PLS format.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lContent :: Lens.Lens' Lexicon (Lude.Maybe (Lude.Sensitive Lude.Text))
lContent = Lens.lens (content :: Lexicon -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {content = a} :: Lexicon)
{-# DEPRECATED lContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Lexicon (Lude.Maybe Lude.Text)
lName = Lens.lens (name :: Lexicon -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Lexicon)
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON Lexicon where
  parseJSON =
    Lude.withObject
      "Lexicon"
      ( \x ->
          Lexicon'
            Lude.<$> (x Lude..:? "Content") Lude.<*> (x Lude..:? "Name")
      )
