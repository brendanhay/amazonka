{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.Term
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Translate.Types.Term
  ( Term (..)
  -- * Smart constructor
  , mkTerm
  -- * Lenses
  , tSourceText
  , tTargetText
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The term being translated by the custom terminology.
--
-- /See:/ 'mkTerm' smart constructor.
data Term = Term'
  { sourceText :: Core.Maybe Core.Text
    -- ^ The source text of the term being translated by the custom terminology.
  , targetText :: Core.Maybe Core.Text
    -- ^ The target text of the term being translated by the custom terminology.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Term' value with any optional fields omitted.
mkTerm
    :: Term
mkTerm
  = Term'{sourceText = Core.Nothing, targetText = Core.Nothing}

-- | The source text of the term being translated by the custom terminology.
--
-- /Note:/ Consider using 'sourceText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSourceText :: Lens.Lens' Term (Core.Maybe Core.Text)
tSourceText = Lens.field @"sourceText"
{-# INLINEABLE tSourceText #-}
{-# DEPRECATED sourceText "Use generic-lens or generic-optics with 'sourceText' instead"  #-}

-- | The target text of the term being translated by the custom terminology.
--
-- /Note:/ Consider using 'targetText' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTargetText :: Lens.Lens' Term (Core.Maybe Core.Text)
tTargetText = Lens.field @"targetText"
{-# INLINEABLE tTargetText #-}
{-# DEPRECATED targetText "Use generic-lens or generic-optics with 'targetText' instead"  #-}

instance Core.FromJSON Term where
        parseJSON
          = Core.withObject "Term" Core.$
              \ x ->
                Term' Core.<$>
                  (x Core..:? "SourceText") Core.<*> x Core..:? "TargetText"
