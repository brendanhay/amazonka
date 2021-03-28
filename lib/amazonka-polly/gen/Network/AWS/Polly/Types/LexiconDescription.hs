{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LexiconDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.LexiconDescription
  ( LexiconDescription (..)
  -- * Smart constructor
  , mkLexiconDescription
  -- * Lenses
  , ldAttributes
  , ldName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Polly.Types.LexiconAttributes as Types
import qualified Network.AWS.Polly.Types.LexiconName as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the content of the lexicon.
--
-- /See:/ 'mkLexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { attributes :: Core.Maybe Types.LexiconAttributes
    -- ^ Provides lexicon metadata.
  , name :: Core.Maybe Types.LexiconName
    -- ^ Name of the lexicon.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LexiconDescription' value with any optional fields omitted.
mkLexiconDescription
    :: LexiconDescription
mkLexiconDescription
  = LexiconDescription'{attributes = Core.Nothing,
                        name = Core.Nothing}

-- | Provides lexicon metadata.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldAttributes :: Lens.Lens' LexiconDescription (Core.Maybe Types.LexiconAttributes)
ldAttributes = Lens.field @"attributes"
{-# INLINEABLE ldAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | Name of the lexicon.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldName :: Lens.Lens' LexiconDescription (Core.Maybe Types.LexiconName)
ldName = Lens.field @"name"
{-# INLINEABLE ldName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON LexiconDescription where
        parseJSON
          = Core.withObject "LexiconDescription" Core.$
              \ x ->
                LexiconDescription' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "Name"
