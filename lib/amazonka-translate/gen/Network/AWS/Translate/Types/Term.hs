{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.Term
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.Term
  ( Term (..),

    -- * Smart constructor
    mkTerm,

    -- * Lenses
    tTargetText,
    tSourceText,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The term being translated by the custom terminology.
--
-- /See:/ 'mkTerm' smart constructor.
data Term = Term'
  { targetText :: Lude.Maybe Lude.Text,
    sourceText :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Term' with the minimum fields required to make a request.
--
-- * 'sourceText' - The source text of the term being translated by the custom terminology.
-- * 'targetText' - The target text of the term being translated by the custom terminology.
mkTerm ::
  Term
mkTerm =
  Term' {targetText = Lude.Nothing, sourceText = Lude.Nothing}

-- | The target text of the term being translated by the custom terminology.
--
-- /Note:/ Consider using 'targetText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTargetText :: Lens.Lens' Term (Lude.Maybe Lude.Text)
tTargetText = Lens.lens (targetText :: Term -> Lude.Maybe Lude.Text) (\s a -> s {targetText = a} :: Term)
{-# DEPRECATED tTargetText "Use generic-lens or generic-optics with 'targetText' instead." #-}

-- | The source text of the term being translated by the custom terminology.
--
-- /Note:/ Consider using 'sourceText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSourceText :: Lens.Lens' Term (Lude.Maybe Lude.Text)
tSourceText = Lens.lens (sourceText :: Term -> Lude.Maybe Lude.Text) (\s a -> s {sourceText = a} :: Term)
{-# DEPRECATED tSourceText "Use generic-lens or generic-optics with 'sourceText' instead." #-}

instance Lude.FromJSON Term where
  parseJSON =
    Lude.withObject
      "Term"
      ( \x ->
          Term'
            Lude.<$> (x Lude..:? "TargetText") Lude.<*> (x Lude..:? "SourceText")
      )
