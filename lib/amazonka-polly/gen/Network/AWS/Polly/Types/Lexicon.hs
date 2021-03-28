{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Lexicon
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.Lexicon
  ( Lexicon (..)
  -- * Smart constructor
  , mkLexicon
  -- * Lenses
  , lContent
  , lName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types.Content as Types
import qualified Network.AWS.Polly.Types.LexiconName as Types
import qualified Network.AWS.Prelude as Core

-- | Provides lexicon name and lexicon content in string format. For more information, see <https://www.w3.org/TR/pronunciation-lexicon/ Pronunciation Lexicon Specification (PLS) Version 1.0> .
--
-- /See:/ 'mkLexicon' smart constructor.
data Lexicon = Lexicon'
  { content :: Core.Maybe Types.Content
    -- ^ Lexicon content in string format. The content of a lexicon must be in PLS format.
  , name :: Core.Maybe Types.LexiconName
    -- ^ Name of the lexicon.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Lexicon' value with any optional fields omitted.
mkLexicon
    :: Lexicon
mkLexicon = Lexicon'{content = Core.Nothing, name = Core.Nothing}

-- | Lexicon content in string format. The content of a lexicon must be in PLS format.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lContent :: Lens.Lens' Lexicon (Core.Maybe Types.Content)
lContent = Lens.field @"content"
{-# INLINEABLE lContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Lexicon (Core.Maybe Types.LexiconName)
lName = Lens.field @"name"
{-# INLINEABLE lName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Lexicon where
        parseJSON
          = Core.withObject "Lexicon" Core.$
              \ x ->
                Lexicon' Core.<$> (x Core..:? "Content") Core.<*> x Core..:? "Name"
