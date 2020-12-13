{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LexiconDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconDescription
  ( LexiconDescription (..),

    -- * Smart constructor
    mkLexiconDescription,

    -- * Lenses
    ldAttributes,
    ldName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Polly.Types.LexiconAttributes
import qualified Network.AWS.Prelude as Lude

-- | Describes the content of the lexicon.
--
-- /See:/ 'mkLexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { -- | Provides lexicon metadata.
    attributes :: Lude.Maybe LexiconAttributes,
    -- | Name of the lexicon.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LexiconDescription' with the minimum fields required to make a request.
--
-- * 'attributes' - Provides lexicon metadata.
-- * 'name' - Name of the lexicon.
mkLexiconDescription ::
  LexiconDescription
mkLexiconDescription =
  LexiconDescription'
    { attributes = Lude.Nothing,
      name = Lude.Nothing
    }

-- | Provides lexicon metadata.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAttributes :: Lens.Lens' LexiconDescription (Lude.Maybe LexiconAttributes)
ldAttributes = Lens.lens (attributes :: LexiconDescription -> Lude.Maybe LexiconAttributes) (\s a -> s {attributes = a} :: LexiconDescription)
{-# DEPRECATED ldAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldName :: Lens.Lens' LexiconDescription (Lude.Maybe Lude.Text)
ldName = Lens.lens (name :: LexiconDescription -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LexiconDescription)
{-# DEPRECATED ldName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON LexiconDescription where
  parseJSON =
    Lude.withObject
      "LexiconDescription"
      ( \x ->
          LexiconDescription'
            Lude.<$> (x Lude..:? "Attributes") Lude.<*> (x Lude..:? "Name")
      )
